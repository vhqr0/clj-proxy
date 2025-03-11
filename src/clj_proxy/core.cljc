(ns clj-proxy.core
  (:require [clojure.string :as str]
            [clojure.core.async :as a]
            [clj-bytes.core :as b]))

(defn log
  [context msg]
  (let [{:keys [log-fn]} context]
    (when (some? log-fn)
      (log-fn (merge {:level :info} msg (select-keys context [:uuid :addr :client-info :server-info])))
      nil)))

;;; network

(defmulti net-connect
  "Network connect."
  (fn [opts] (:type opts)))

(comment
  ;; spec example:
  {:tcp {:addr ["localhost" 80]}
   :tls {:addr ["localhost" 443] :tls? true}
   :ws  {:addr ["localhost" 80] :http-path "/" :http-host "localhost"}
   :wss {:addr ["localhost" 443] :http-path "/" :http-host "localhost" :tls? true}}
  ;; tls/wss on JVM: optional tls-context opt.
  )

(defmulti net-start-server
  "Network start server."
  (fn [_handle-fn opts] (:type opts)))

;;; handshake

(defprotocol HandShake
  (hs-update [_ b])
  (hs-advance [_])
  (hs-info [_]))

(defmulti ->proxy-client
  "Construct a proxy client handshake."
  (fn [_addr opts] (:type opts)))

(defmulti ->proxy-server
  "Construct a proxy server handshake."
  (fn [opts] (:type opts)))

(defn handshake
  "Handshake on peer."
  [[ich och] context state]
  (a/go-loop [state state]
    (let [[b state] (hs-advance state)]
      (if-not (or (nil? b) (b/empty? b) (a/>! och b))
        (log context {:level :error :type :handshake-error :reason :handshake/write})
        (if-let [info (hs-info state)]
          (case (:type info)
            :error (let [{:keys [ex]} info]
                     (log context {:level :error :type :handshake-error :reason :handshake/ex :msg (ex-message ex) :data (ex-data ex)}))
            :ok (dissoc info :type))
          (if-let [b (a/<! ich)]
            (recur (hs-update state b))
            (log context {:level :error :type :handshake-error :reason :handshake/read})))))))

(defn ch-ex-handle
  "Handle exception in chan."
  [ex context]
  (let [msg (ex-message ex)
        data (ex-data ex)]
    (log context {:level :error :type :pipe-error :reason :pipe/xform :msg msg :data data})))

(defn wrap-xf-pair
  "Wrap lower connetion by xf-pair."
  [[ich och] [ixf oxf] context]
  (let [exh #(ch-ex-handle % context)
        nich (a/chan 1024 ixf exh)
        noch (a/chan 1024 oxf exh)]
    (a/pipe ich nich)
    (a/pipe noch och)
    [nich noch]))

(defn proxy-client
  "Proxy handshake on server."
  [server context addr opts]
  (a/go
    (let [state (->proxy-client addr opts)]
      (if-let [{:keys [buffer server-xf-pair server-info]} (a/<! (handshake server context state))]
        (let [server (cond-> server (some? server-xf-pair) (wrap-xf-pair server-xf-pair context))]
          {:buffer buffer :server server :server-info server-info})
        (log context {:level :error :type :handshake-error :reason :client/handshake})))))

(defn proxy-server
  "Proxy handshake on client."
  [client context opts]
  (a/go
    (let [state (->proxy-server opts)]
      (if-let [{:keys [addr buffer client-xf-pair client-info]} (a/<! (handshake client context state))]
        (let [client (cond-> client (some? client-xf-pair) (wrap-xf-pair client-xf-pair context))]
          {:addr addr :buffer buffer :client client :client-info client-info})
        (log context {:level :error :type :handshake-error :reason :server/handshake})))))

;;; server

;;;; connect

(defmulti connect
  "Proxy connect."
  (fn [_context opts] (:type opts)))

(defmethod connect :proxy [{:keys [addr] :as context} {:keys [name proxy-opts net-opts]}]
  (log context {:type :connect :via name})
  (a/go
    (if-let [server (a/<! (net-connect net-opts))]
      (if-let [{:keys [buffer server server-info]} (a/<! (proxy-client server context addr proxy-opts))]
        {:buffer buffer :server server :server-info server-info}
        (log context {:level :error :type :connect-error :reason :proxy-connect/handshake}))
      (log context {:level :error :type :connect-error :reason :proxy-connect/net}))))

(defmethod connect :direct [{:keys [addr] :as context} {:keys [name]}]
  (log context {:type :connect :via name})
  (a/go
    (if-let [server (a/<! (net-connect {:type :tcp :addr addr}))]
      {:server server}
      (log context {:level :error :type :connect-error :reason :direct-connect/net}))))

(defmethod connect :block [context {:keys [name]}]
  (log context {:type :connect :via name}))

(defmethod connect :rand-dispatch [context {:keys [name sub-opts]}]
  (log context {:level :debug :type :dispatch :via name})
  (connect context (rand-nth sub-opts)))

(defn match-tag
  "Match host's tag in tag-map."
  [host tag-map]
  (when (not (str/blank? host))
    (if-let [tag (get tag-map host)]
      tag
      (when-let [host (second (str/split host #"\." 2))]
        (recur host tag-map)))))

^:rct/test
(comment
  (match-tag "google.com" {"google.com" :proxy}) ; => :proxy
  (match-tag "www.google.com" {"google.com" :proxy}) ; => :proxy
  (match-tag "www.a.google.com" {"google.com" :proxy}) ; => :proxy
  (match-tag "ads.google.com" {"google.com" :proxy "ads.google.com" :block}) ; => :block
  (match-tag "baidu.com" {"google.com" :proxy}) ; => nil
  )

(defmethod connect :tag-dispatch [{:keys [addr] :as context} {:keys [name tag-map default-tag sub-opts]}]
  (log context {:level :debug :type :dispatch :via name})
  (let [tag (or (match-tag (first addr) tag-map) default-tag)]
    (connect context (get sub-opts tag))))

;;;; handle

(defn pipe
  "Proxy pipe."
  [{:keys [client server client-buffer server-buffer] :as context}]
  (log context {:level :debug :type :pipe})
  (if (and (or (nil? server-buffer) (b/empty? server-buffer) (a/>! (second client) server-buffer))
           (or (nil? client-buffer) (b/empty? client-buffer) (a/>! (second server) client-buffer)))
    (do
      (a/pipe (first client) (second server))
      (a/pipe (first server) (second client)))
    (log context {:level :error :type :pipe-error :reason :pipe/write-first})))

(defn handle
  "Proxy handle."
  [client {:keys [proxy-server-opts connect-opts] :as context}]
  (a/go
    (let [context (assoc context :uuid (random-uuid))]
      (log context {:level :debug :type :handle})
      (if-let [{:keys [addr client client-info] client-buffer :buffer} (a/<! (proxy-server client context proxy-server-opts))]
        (let [context (assoc context :addr addr :client client :client-info client-info :client-buffer client-buffer)]
          (when-let [server-ch (connect context connect-opts)]
            (if-let [{:keys [server server-info] server-buffer :buffer} (a/<! server-ch)]
              (let [context (assoc context :server server :server-info server-info :server-buffer server-buffer)]
                (pipe context))
              (log context {:level :error :type :handle-error :reason :handle/connect}))))
        (log context {:level :error :type :handle-error :reason :handle/handshake})))))

(defn start-server
  "Start proxy server."
  [{:keys [net-server-opts] :as context}]
  (a/go
    (let [context (assoc context :uuid (random-uuid))]
      (if-let [server (a/<! (net-start-server #(handle % context) net-server-opts))]
        (do
          (log context {:level :debug :type :start})
          server)
        (log context {:level :error :type :start-error :reason :start/start})))))
