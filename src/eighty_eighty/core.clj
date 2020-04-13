(ns eighty-eighty.core
  (:gen-class))

(def debug true)

(def flags
  {:z 0
   :s 0
   :p 0
   :cy 0
   :ac 0
   :pad 0})

(def cpu
  {:a 0
   :b 0
   :c 0
   :d 0
   :e 0
   :pc 0
   :sp 0})

(def initial-state
  {:cpu cpu
   :flags flags
   :interrupt-enabled true})

(defn flag-z [n]
  (if (= 0 (bit-and n 0xff))
    1
    0))

(defn flag-s [n]
  (if (not= 0 (bit-and n 0x80))
    1
    0))

(defn flag-cy [n]
  (if (> n 0xff)
    1
    0))

(defn flag-p [n]
  ;; FIXME
  0)

(defmacro add-a-r [r]
  `(let [{:keys [a ~r]} ~'cpu
          result# (+ ~'a ~r)]
     (when ~'debug (println (str "ADD " ~(clojure.string/upper-case (str r)))))
     (recur (-> ~'state
                ;; "I am emulating the 8-bit math
                ;; instructions by using a 16-bit
                ;; number. That makes it easy to figure out
                ;; if the math generated a carry out of it."
                ;; hence the (bit-and ... 0xff)
                (assoc-in [:cpu :a] (bit-and result# 0xff))
                (update-in [:cpu :pc] inc)
                (assoc :flags {:z (flag-z result#)
                               :s (flag-s result#)
                               :cy (flag-cy result#)
                               :p (flag-p result#)
                               :pad 0})))))

(defmacro inx [r0 r1]
  `(let [{msb# ~r0
          lsb# ~r1} ~'cpu
         d16# (+ (bit-shift-left msb# 8)
                 lsb#)
         result# (-> d16#
                     inc
                     (bit-and 0xffff))
         msb'# (-> result#
                   (bit-shift-right 8)
                   (bit-and 0xff))
         lsb'# (-> result#
                   (bit-and 0xff))]
     (println "INX B")
     (recur (-> ~'state
                (update [:cpu] merge {~r0 msb'#
                                      ~r1 lsb'#})
                (update-in [:cpu :pc] inc)))))

;; (lxi-r16-d16 :b :c) will load byte 3 into b and byte 2 into c
(defn lxi-r16-d16 [r0 r1]
  `(let [lsb# (nth ~'memory (+ 1 ~'pc))
         msb# (nth ~'memory (+ 2 ~'pc))
         d16# (+ (bit-shift-left msb# 8)
                 lsb#)]
     (when ~'debug (println (str "LXI" (str "LXI " ~(clojure.string/upper-case (str r0))) "," (format "#$%x" d16#))))
     (recur (-> ~'state
                (assoc-in [:cpu r0] ~'msb)
                (assoc-in [:cpu r1] ~'lsb)
                (update-in [:cpu :pc] + 3)))))

;; TODO: continue implementing arithmetic operations
;; http://www.emulator101.com/arithmetic-group.html
(defn emulate [memory & {:keys [debug]}]
  (loop [{flags :flags
          interrupt-enabled :interrupt-enabled
          memory :memory
          {:keys [pc] :as cpu} :cpu
          :as state} (-> initial-state
                         (assoc :memory memory))]
    (let [opcode (nth memory pc)]
      (case opcode
        0x00
        #_=> (recur state)

        0x01
        #_=> (lxi-r16-d16 :b :c)
        ;; #_=> (let [lsb (nth memory (+ 1 pc))
        ;;            msb (nth memory (+ 2 pc))
        ;;            d16 (+ (bit-shift-left msb 8)
        ;;                   lsb)]
        ;;        (when debug (println "LXI B," (format "#$%x" d16)))
        ;;        (recur (-> state
        ;;                   (assoc-in [:cpu :b] msb)
        ;;                   (assoc-in [:cpu :c] lsb)
        ;;                   (update-in [:cpu :pc] + 3))))

        0x02
        #_=> (let [{a :a
                    msb :b
                    lsb :c} cpu
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (when debug (println "STAX B"))
               (recur (-> state
                          (assoc-in [:cpu :memory adr] a)
                          (update-in [:cpu :pc] inc))))

        0x03
        #_=> (inx :b :c)
        ;; #_=> (let [{msb :b
        ;;             lsb :c} cpu
        ;;            d16 (+ (bit-shift-left msb 8)
        ;;                   lsb)
        ;;            result (-> d16
        ;;                       inc
        ;;                       (bit-and 0xffff))
        ;;            msb' (-> result
        ;;                  (bit-shift-right 8)
        ;;                  (bit-and 0xff))
        ;;            lsb' (-> result
        ;;                  (bit-and 0xff))]
        ;;        (println "INX B")
        ;;        (recur (-> state
        ;;                   (update [:cpu] merge {:b msb'
        ;;                                         :c lsb'})
        ;;                   (update-in [:cpu :pc] inc))))

        ;; 0x04
        ;; #_=> nil

        ;; 0x05
        ;; #_=> nil

        ;; 0x06
        ;; #_=> nil

        ;; 0x07
        ;; #_=> nil

        ;; 0x09
        ;; #_=> nil

        ;; 0x08
        ;; #_=> nil

        ;; 0xa
        ;; #_=> nil

        ;; 0xb
        ;; #_=> nil

        ;; 0x0c
        ;; #_=> nil

        ;; 0x0d
        ;; #_=> nil

        ;; 0x0e
        ;; #_=> nil

        ;; 0x0f
        ;; #_=> nil

        ;; 0x10
        ;; #_=> nil

        0x11
        #_=> (lxi-r16-d16 :d :e)

        ;; 0x12
        ;; #_=> nil

        ;; 0x13
        ;; #_=> nil

        ;; 0x14
        ;; #_=> nil

        ;; 0x15
        ;; #_=> nil

        ;; 0x16
        ;; #_=> nil

        ;; 0x18
        ;; #_=> nil

        ;; 0x19
        ;; #_=> nil

        ;; 0x1a
        ;; #_=> nil

        ;; 0x1b
        ;; #_=> nil

        ;; 0x1c
        ;; #_=> nil

        ;; 0x1d
        ;; #_=> nil

        ;; 0x1e
        ;; #_=> nil

        ;; 0x1f
        ;; #_=> nil

        ;; 0x20
        ;; #_=> nil

        0x21
        #_=> (lxi-r16-d16 :h :l)

        ;; 0x22
        ;; #_=> nil

        ;; 0x23
        ;; #_=> nil

        ;; 0x24
        ;; #_=> nil

        ;; 0x25
        ;; #_=> nil

        ;; 0x26
        ;; #_=> nil

        ;; 0x27
        ;; #_=> nil

        ;; 0x28
        ;; #_=> nil 

        ;; 0x29
        ;; #_=> nil

        ;; 0x2a
        ;; #_=> nil

        ;; 0x2b
        ;; #_=> nil

        ;; 0x2c
        ;; #_=> nil

        ;; 0x2e
        ;; #_=> nil

        ;; 0x2f
        ;; #_=> nil

        ;; 0x30
        ;; #_=> nil

        0x31
        #_=> (let [lsb (nth memory (+ 1 pc))
                   msb (nth memory (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (when debug (println "LXI B," (format "#$%x" d16)))
               (recur (-> state
                          (assoc-in [:cpu :sp] d16)
                          (update-in [:cpu :pc] + 3))))

        ;; 0x32
        ;; #_=> nil

        ;; 0x34
        ;; #_=> nil

        ;; 0x35
        ;; #_=> nil

        ;; 0x36
        ;; #_=> nil

        ;; 0x37
        ;; #_=> nil

        ;; 0x38
        ;; #_=> nil

        ;; 0x39
        ;; #_=> nil

        ;; 0x3a
        ;; #_=> nil

        ;; 0x3b nil

        ;; 0x3c
        ;; #_=> nil

        ;; 0x3d
        ;; #_=> nil

        ;; 0x3e
        ;; #_=> nil

        ;; 0x3f
        ;; #_=> nil

        ;; 0x40
        ;; #_=> nil

        ;; 0x41
        ;; #_=> nil

        ;; 0x42
        ;; #_=> nil

        ;; 0x43
        ;; #_=> nil

        ;; 0x44
        ;; #_=> nil

        ;; 0x45
        ;; #_=> nil

        ;; 0x46
        ;; #_=> nil

        ;; 0x47
        ;; #_=> nil

        ;; 0x48
        ;; #_=> nil

        ;; 0x49
        ;; #_=> nil

        ;; 0x4a
        ;; #_=> nil

        ;; 0x4b
        ;; #_=> nil

        ;; 0x4c
        ;; #_=> nil

        ;; 0x4d
        ;; #_=> nil

        ;; 0x4e
        ;; #_=> nil

        ;; 0x4f
        ;; #_=> nil

        ;; 0x50
        ;; #_=> nil

        ;; 0x51
        ;; #_=> nil

        ;; 0x54
        ;; #_=> nil

        ;; 0x56
        ;; #_=> nil

        ;; 0x57
        ;; #_=> nil

        ;; 0x59
        ;; #_=> nil

        ;; 0x5b
        ;; #_=> nil

        ;; 0x5e
        ;; #_=> nil

        ;; 0x5f nil

        ;; 0x60
        ;; #_=> nil

        ;; 0x61
        ;; #_=> nil

        ;; 0x62
        ;; #_=> nil

        ;; 0x63
        ;; #_=> nil

        ;; 0x64
        ;; #_=> nil

        ;; 0x65
        ;; #_=> nil

        ;; 0x66
        ;; #_=> nil

        ;; 0x67
        ;; #_=> nil

        ;; 0x68
        ;; #_=> nil

        ;; 0x69
        ;; #_=> nil

        ;; 0x6c
        ;; #_=> nil

        ;; 0x6d
        ;; #_=> nil

        ;; 0x6e
        ;; #_=> nil

        ;; 0x6f
        ;; #_=> nil

        ;; 0x70
        ;; #_=> nil

        ;; 0x71
        ;; #_=> nil

        ;; 0x72
        ;; #_=> nil

        ;; 0x73
        ;; #_=> nil

        ;; 0x74
        ;; #_=> nil

        ;; 0x76
        ;; #_=> nil

        ;; 0x77
        ;; #_=> nil

        ;; 0x78 nil

        ;; 0x79
        ;; #_=> nil

        ;; 0x7a
        ;; #_=> nil

        ;; 0x7b
        ;; #_=> nil

        ;; 0x7c
        ;; #_=> nil

        ;; 0x7d
        ;; #_=> nil

        ;; 0x7e
        ;; #_=> nil

        ;; 0x7f
        ;; #_=> nil

        0x80
        #_=> (add-a-r b)

        0x81
        #_=> (add-a-r c)

        0x82
        #_=> (add-a-r d)

        0x83
        #_=> (add-a-r e)

        0x84
        #_=> (add-a-r h)

        0x85
        #_=> (add-a-r l)

        0x86
        #_=> (let [{a :a
                    msb :h
                    lsb :l} cpu
                   m (+ (bit-shift-left msb 8)
                        lsb)
                   result (+ a m)]
               (when debug (println "ADD M"))
               (recur (-> state
                          (assoc-in [:cpu :a] (bit-and result 0xff))
                          (update-in [:cpu :pc] + 2)
                          (assoc :flags {:z (flag-z result)
                                         :s (flag-s result)
                                         :cy (flag-cy result)
                                         :p (flag-p result)
                                         :pad 0}))))

        0x87
        #_=> (add-a-r a)

        ;; 0x88
        ;; #_=> nil

        ;; 0x8a
        ;; #_=> nil

        ;; 0x8b
        ;; #_=> nil

        ;; 0x8e
        ;; #_=> nil

        ;; 0x90
        ;; #_=> nil

        ;; 0x94
        ;; #_=> nil

        ;; 0x97
        ;; #_=> nil

        ;; 0x98
        ;; #_=> nil

        ;; 0x99
        ;; #_=> nil

        ;; 0x9a
        ;; #_=> nil

        ;; 0x9b
        ;; #_=> nil

        ;; 0x9d
        ;; #_=> nil

        ;; 0x9e
        ;; #_=> nil

        ;; 0xa0
        ;; #_=> nil

        ;; 0xa3
        ;; #_=> nil

        ;; 0xa6
        ;; #_=> nil

        ;; 0xa7
        ;; #_=> nil

        ;; 0xa8
        ;; #_=> nil

        ;; 0xaa
        ;; #_=> nil

        ;; 0xaf
        ;; #_=> nil

        ;; 0xb0
        ;; #_=> nil

        ;; 0xb3
        ;; #_=> nil

        ;; 0xb4
        ;; #_=> nil

        ;; 0xb6
        ;; #_=> nil

        ;; 0xb8
        ;; #_=> nil

        ;; 0xbb
        ;; #_=> nil

        ;; 0xbc
        ;; #_=> nil

        ;; 0xbe
        ;; #_=> nil

        ;; 0xc0 nil

        ;; 0xc1
        ;; #_=> nil

        ;; 0xc2
        ;; #_=> nil

        ;; 0xc3
        ;; #_=> nil

        ;; 0xc4 nil

        ;; 0xc5
        ;; #_=> nil

        0xc6
        #_=> (let [a (:a cpu)
                   d8 (nth memory (inc pc))
                   result (+ a d8)]
               (when debug (println "ADI " d8))
               (recur (-> state
                          (assoc-in [:cpu :a] (bit-and result 0xff))
                          (update-in [:cpu :pc] + 2)
                          (assoc :flags {:z (flag-z result)
                                         :s (flag-s result)
                                         :cy (flag-cy result)
                                         :p (flag-p result)
                                         :pad 0}))))

        ;; 0xc8
        ;; #_=> nil

        ;; 0xc9
        ;; #_=> nil

        ;; 0xca
        ;; #_=> nil

        ;; 0xcc
        ;; #_=> nil

        ;; 0xcd
        ;; #_=> nil

        ;; 0xd0
        ;; #_=> nil

        ;; 0xd1
        ;; #_=> nil

        ;; 0xd2
        ;; #_=> nil

        ;; 0xd3
        ;; #_=> nil

        ;; 0xd5
        ;; #_=> nil

        ;; 0xd4
        ;; #_=> nil

        ;; 0xd6
        ;; #_=> nil

        ;; 0xd8
        ;; #_=> nil

        ;; 0xda
        ;; #_=> nil

        ;; 0xdb
        ;; #_=> nil

        ;; 0xde
        ;; #_=> nil

        ;; 0xe0
        ;; #_=> nil

        ;; 0xe1
        ;; #_=> nil

        ;; 0xe2
        ;; #_=> nil

        ;; 0xe3
        ;; #_=> nil

        ;; 0xe5
        ;; #_=> nil

        ;; 0xe6
        ;; #_=> nil

        ;; 0xe9
        ;; #_=> nil

        ;; 0xeb nil

        ;; 0xec
        ;; #_=> nil

        ;; 0xee
        ;; #_=> nil

        ;; 0xf0
        ;; #_=> nil

        ;; 0xf1
        ;; #_=> nil

        ;; 0xf5
        ;; #_=> nil

        ;; 0xf6
        ;; #_=> nil

        ;; 0xf8
        ;; #_=> nil

        ;; 0xfa
        ;; #_=> nil

        ;; 0xfb
        ;; #_=> nil

        ;; 0xfc
        ;; #_=> nil

        ;; 0xfe
        ;; #_=> nil

        ;; 0xff
        ;; #_=> nil

        (throw (Exception. (str "unknown opcode: " (format "0x%x" opcode))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
