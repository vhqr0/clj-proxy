(ns clj-proxy.core-test
  (:require [clojure.core.async :as a]
            [clj-bytes.core :as b]
            [clj-proxy.core :as core]
            clj-proxy.socks5
            clj-proxy.trojan)
  (:import [java.util Date]))

(defn proxy-handshake-local
  [client server]
  (loop [client client server server]
    (println :client client)
    (println :server server)
    (println "---")
    (let [[b1 client] (core/hs-advance client)
          [b2 server] (core/hs-advance server)
          client-updated? (and (some? b1) (not (b/empty? b1)))
          server-updated? (and (some? b2) (not (b/empty? b2)))
          client (cond-> client server-updated? (core/hs-update b2))
          server (cond-> server client-updated? (core/hs-update b1))
          client-info (core/hs-info client)
          server-info (core/hs-info server)]
      (if (and (some? client-info) (some? server-info))
        [client-info server-info]
        (do
          (assert (or client-updated? server-updated?))
          (recur client server))))))

(comment
  (proxy-handshake-local
   (core/->proxy-client ["www.baidu.com" 80] {:type :socks5 :auth ["vhqr" "123"]})
   (core/->proxy-server {:type :socks5 :auths {"vhqr" #{"123"}}}))
  (proxy-handshake-local
   (core/->proxy-client ["www.baidu.com" 80] {:type :trojan :auth "vhqr"})
   (core/->proxy-server {:type :trojan :auths #{"vhqr"}})))

(defn log
  [msg opts]
  (let [{:keys [levels prefer-info-keys]
         :or {levels #{:debug :info :error}
              prefer-info-keys [:uuid :level :type :addr]}}
        opts]
    (when (contains? levels (:level msg))
      (let [timestamp (Date.)
            data (->> prefer-info-keys
                      (reduce
                       (fn [data k]
                         (let [v (get msg k)]
                           (cond-> data (some? v) (conj v))))
                       [timestamp]))
            msg (->> prefer-info-keys
                     (reduce dissoc msg)
                     (remove #(nil? (val %)))
                     (into {}))
            data (cond-> data
                   (seq msg) (conj msg))]
        (prn data)))))

(comment
  (def log-msg-ch (a/chan 1024))
  (def log-opts nil)
  (a/go-loop []
    (when-let [msg (a/<! log-msg-ch)]
      (log msg log-opts)
      (recur)))

  (def context {:log-fn #(a/put! log-msg-ch %)
                :net-server-opts {:type :tcp :port 1080}
                :proxy-server-opts {:type :socks5}
                :connect-opts {:type :direct :name :direct}})
  (def server (a/<!! (core/start-server context))))
