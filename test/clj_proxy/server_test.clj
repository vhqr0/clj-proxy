(ns clj-proxy.server-test
  (:require [clojure.core.async :as a]
            [clj-proxy.core :as core]
            clj-proxy.socks5
            clj-proxy.trojan)
  (:import [java.util Date]))

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
