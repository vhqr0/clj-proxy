(ns clj-proxy.net-test
  (:require [clojure.core.async :as a]
            [clj-bytes.core :as b]
            [clj-proxy.core :as core]
            clj-proxy.net))

(defn http-request
  [host]
  (a/go
    (if-let [[ich och] (a/<! (core/net-connect {:type :tcp :addr [host 80]}))]
      (if (a/>! och (b/of-str (str "GET / HTTP/1.1\r\nHost: " host "\r\n\r\n")))
        (if-let [b (a/<! ich)]
          (println (b/str b))
          (println "read error:" host))
        (println "write error:" host))
      (println "connect error:" host))))

(comment
  (http-request "www.baidu.com"))

(defn ws-ping
  []
  (a/go
    (if-let [[ich _och] (a/<! (core/net-connect {:type :ws :addr ["echo.websocket.org" 443] :tls? true}))]
      (if-let [res (a/<! ich)]
        (println res)
        (println "read error"))
      (println "connect error"))))

(comment
  (ws-ping))
