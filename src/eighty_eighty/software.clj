(ns eighty-eighty.software)

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

(def test
  (let [raw (-> "test.com"
                clojure.java.io/resource
                slurp-bytes)
        padding (take 0x100 (repeat 0))]
    (vec (concat padding raw))))

;; something fucky, again. when i load this up, the instruction at
;; address 0x100 looks totally wrong.
(def cpudiag
  (let [raw (->> "cpudiag/cpudiag.bin"
                 clojure.java.io/resource
                 slurp-bytes)
        padding (take 0x100 (repeat 0))]
    (vec (concat padding raw))))
