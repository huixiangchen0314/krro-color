(ns build
    (:require [clojure.tools.build.api :as b]))

(def lib 'krro-color)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
      (b/delete {:path "target"}))

(defn jar [_]
      (clean nil)
      (b/write-pom {:class-dir class-dir
                    :lib lib
                    :version version
                    :basis basis
                    :src-dirs ["src"]})
      (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file (format "target/%s-%s.jar" (name lib) version)})
      (println "Jar created."))

(defn install [_]
      (jar nil)
      (b/install {:basis basis
                  :lib lib
                  :version version
                  :class-dir class-dir})
      (println "Installed to local Maven repository."))

(defn deploy [_]
      (jar nil)
      (b/deploy {:basis basis
                 :lib lib
                 :version version
                 :class-dir class-dir
                 :repository {:url "https://clojars.org/repo"
                              :username (System/getenv "CLOJARS_USERNAME")
                              :password (System/getenv "CLOJARS_PASSWORD")}})
      (println "Deployed to Clojars."))

(defn test-all [_]
      (b/process {:command-args ["clojure" "-M:test"]}))