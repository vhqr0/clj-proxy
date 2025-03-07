(ns clj-proxy.net
  (:require [clojure.core.async :as a]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]
            [aleph.http :as http]
            [clj-proxy.core :as core])
  (:import [java.net URI]))

;;; manifold utils

(defn md-connect
  "Connect manifold deferred to core.async chan."
  [ch d xf]
  (-> d
      (d/on-realized
       (fn [data] (a/>!! ch (xf data)) (a/close! ch))
       (fn [_error] (a/close! ch)))))

(defn md->ch
  "Convert manifold deferred to core.async chan."
  ([d]
   (md->ch d identity))
  ([d xf]
   (doto (a/chan) (md-connect d xf))))

(defn ms-connect
  "Connect manifold stream to core.async chan pair."
  [[ich och] s]
  (s/connect s ich)
  (s/connect och s))

(defn ms->ch-pair
  "Convert manifold stream to core.async chan pair."
  ([s]
   (ms->ch-pair s nil nil))
  ([s ixf oxf]
   (doto [(a/chan 1024 ixf) (a/chan 1024 oxf)]
     (ms-connect s))))

;;; aleph wrappers

;;;; connect

(defn tcp-connect
  "Connect by TCP."
  [opts]
  (-> (tcp/client opts)
      (md->ch ms->ch-pair)))

(defn ws-connect
  "Connect by WebSocket."
  [uri opts]
  (-> (http/websocket-client uri opts)
      (md->ch ms->ch-pair)))

(defmethod core/net-connect :tcp [opts]
  (let [{:keys [addr tls? tls-context]} opts
        [host port] addr
        tcp-opts (cond-> {:host host :port port :ssl? tls?}
                   (some? tls-context) (assoc :ssl-context tls-context))]
    (tcp-connect tcp-opts)))

(defmethod core/net-connect :ws [opts]
  (let [{:keys [addr http-path http-host tls? tls-context]} opts
        [host port] addr
        scheme (if tls? "wss" "ws")
        uri (str (URI. scheme nil host port http-path nil nil))
        headers (cond-> {}
                  (some? http-host) (assoc :host http-host))
        ws-opts (cond-> {:headers headers}
                  (some? tls-context) (assoc :ssl-context tls-context))]
    (ws-connect uri ws-opts)))

;;;; start-server

(defn tcp-start-server
  "Start TCP server."
  [handler opts]
  (-> (fn [s _info] (-> s ms->ch-pair handler))
      (tcp/start-server opts)))

(defn ws-start-server
  "Start WS server."
  [handler opts]
  (-> (fn [req]
        (let [s @(http/websocket-connection req)]
          (-> s ms->ch-pair handler)))
      (http/start-server opts)))

(defmethod core/net-start-server :tcp [handler opts]
  (let [{:keys [port tls-context]} opts
        tcp-opts (cond-> {:port port}
                   (some? tls-context) (assoc :ssl-context tls-context))]
    (a/thread
      (tcp-start-server handler tcp-opts))))

(defmethod core/net-start-server :ws [handler opts]
  (let [{:keys [port tls-context]} opts
        ws-opts (cond-> {:port port}
                  (some? tls-context) (assoc :ssl-context tls-context))]
    (a/thread
      (ws-start-server handler ws-opts))))
