(ns clj-proxy.core-test
  (:require [clj-bytes.core :as b]
            [clj-proxy.core :as core]
            clj-proxy.socks5
            clj-proxy.trojan))

(defn test-proxy-handshake
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
  (test-proxy-handshake
   (core/->proxy-client ["www.baidu.com" 80] {:type :socks5 :auth ["vhqr" "123"]})
   (core/->proxy-server {:type :socks5 :auths {"vhqr" #{"123"}}}))
  (test-proxy-handshake
   (core/->proxy-client ["www.baidu.com" 80] {:type :trojan :auth "vhqr"})
   (core/->proxy-server {:type :trojan :auths #{"vhqr"}})))
