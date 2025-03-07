(ns clj-proxy.trojan
  (:require [clj-bytes.core :as b]
            [clj-bytes.struct :as st]
            [clj-proxy.core :as core]
            [clj-proxy.socks5 :as socks5]))

;; trojan: https://trojan-gfw.github.io/trojan/protocol

;;; structs

(def st-rsv
  (-> st/http-line
      (st/wrap-validator #(= % ""))))

(def st-req
  (st/keys
   :auth st/http-line
   :cmd socks5/st-cmd
   :addr socks5/st-addr
   :rsv st-rsv))

;;; states

;;;; client

(defn ->client
  [opts]
  (let [{:keys [addr auth]} opts]
    {:stage :req :addr addr :auth auth :buffer (b/empty)}))

(defmulti client-advance
  "DOC"
  (fn [state] (:stage state)))

(defmethod client-advance :req [state]
  (let [{:keys [addr auth]} state
        b (-> {:auth auth :cmd :connect :addr addr :rsv ""} (st/pack st-req))]
    [b (assoc state :stage :ok)]))

(defmethod client-advance :ok [state] [nil state])

;;;; server

(defn ->server
  "DOC"
  [opts]
  (let [{:keys [auths]} opts]
    {:stage :wait-req :auths auths :buffer (b/empty)}))

(defmulti server-advance
  "DOC"
  (fn [state] (:stage state)))

(defmethod server-advance :wait-req [state]
  (let [{:keys [buffer auths]} state
        unpack-res (-> buffer (st/unpack st-req))]
    (if (nil? unpack-res)
      [nil state]
      (let [[{:keys [auth addr]} buffer] unpack-res]
        (if-not (contains? auths auth)
          (let [ex (ex-info "invalid trojan auth req auth" {:reason :trojan/req.auth :auth auth})]
            [nil (assoc state :stage :error :buffer buffer :ex ex)])
          [nil (assoc state :stage :ok :buffer buffer :addr addr :auth auth)])))))

(defmethod server-advance :ok [state] [nil state])
(defmethod server-advance :error [state] [nil state])

;;; register

(defrecord TrojanClient [state]
  core/HandShake
  (hs-update [_ b]
    (->TrojanClient (update state :buffer b/concat! b)))
  (hs-advance [_]
    (let [[b state] (client-advance state)]
      [b (->TrojanClient state)]))
  (hs-info [_]
    (case (:stage state)
      :error (let [{:keys [buffer ex]} state]
               {:type :error :buffer buffer :ex ex})
      :ok    (let [{:keys [buffer]} state]
               {:type :ok :buffer buffer})
      nil)))

(defrecord TrojanServer [state]
  core/HandShake
  (hs-update [_ b]
    (->TrojanServer (update state :buffer b/concat! b)))
  (hs-advance [_]
    (let [[b state] (server-advance state)]
      [b (->TrojanServer state)]))
  (hs-info [_]
    (case (:stage state)
      :error (let [{:keys [buffer ex]} state]
               {:type :error :buffer buffer :ex ex})
      :ok    (let [{:keys [addr buffer auth]} state]
               {:type :ok :addr addr :buffer buffer :client-info {:auth auth}})
      nil)))

(defmethod core/->proxy-client :trojan [addr opts]
  (-> opts (assoc :addr addr) ->client ->TrojanClient))

(defmethod core/->proxy-server :trojan [opts]
  (-> opts ->server ->TrojanServer))
