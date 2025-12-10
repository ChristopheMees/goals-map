(ns server.core
  (:require [org.httpkit.server :as hk-server]
            [ring.middleware.file :refer [wrap-file]])
  (:gen-class))

(defn app [req]
  {:status 404})

(defn start-server []
  (hk-server/run-server (wrap-file app "public") {:port 8080}))

(defn -main [& args]
  (start-server))



