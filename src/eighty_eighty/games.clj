(ns eighty-eighty.games)

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [f]
  (clojure.core/with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream f) out)
    (->> out .toByteArray
         (mapv #(bit-and 0xff %)))))

(def invaders-h
  (-> "invaders/invaders.h"
      clojure.java.io/resource
      slurp-bytes))

(def invaders
  (-> "invaders/invaders.bin"
      clojure.java.io/resource
      slurp-bytes))
