(ns eighty-eighty.core
  (:gen-class))

(def debug true)

(def flags
  {:z 0
   :s 0
   :p 0
   :cy 0
   :ac 0})

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

(defn get-r16
  "Get a 16-bit register."
  [r state]  
  (let [[r-msb r-lsb] (case r
                        :b [:b :c]
                        :d [:d :e]
                        :h [:h :l]
                        (throw (Exception. (str "Unknown register: " (name r)))))
        {msb r-msb
         lsb r-lsb} (:cpu state)
        r16 (-> (+ (bit-shift-left msb 8)
                   lsb)
                (bit-and 0xffff))]
    r16))

;;;;;;;;;;;
;; flags ;;
;;;;;;;;;;;

(defn flag-z [n]
  (if (= 0 (bit-and n 0xff))
    1
    0))

(defn flag-s [n]
  ;; grab the MSB
  (if (not= 0 (bit-and n 0x80))
    1
    0))

(defn flag-cy [a0 a1]
  (if (> (+ a0 a1) 0xff)
    1
    0))

(defn flag-p [n]
  ;; FIXME
  0)

(defn flag-ac [n]
  ;; FIXME
  0)

(defn add [r state]
  (let [{a :a
         v r} (:cpu state)
        ;; "I am emulating the 8-bit math instructions by using a
        ;; 16-bit number. That makes it easy to figure out if the math
        ;; generated a carry out of it."
        ;; hence the (bit-and 0xff)
        result (-> (+ a v)
                   (bit-and 0xff))]
    (when debug (println "ADD" (-> r name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu :a] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac result)}))))
                              :cy (flag-cy a v)

;; (lxi :b :c state) will load byte 3 into b and byte 2 into c
(defn lxi [r-msb r-lsb state]
  (let [pc (-> state :cpu :pc)
        memory (:memory state)
        lsb (nth memory (+ 1 pc))
        msb (nth memory (+ 2 pc))
        d16 (+ (bit-shift-left msb 8)
               lsb)]
    (when debug (println (str "LXI " (clojure.string/upper-case (str r-msb))) "," (format "#$%x" d16)))
    (-> state
        (assoc-in [:cpu r-msb] msb)
        (assoc-in [:cpu r-lsb] lsb)
        (update-in [:cpu :pc] + 3))))

(defn stax [r-msb r-lsb state]
  (let [{a :a
         msb r-msb
         lsb r-lsb} (state :cpu)
        adr (bit-and 0xff
                     (+ (bit-shift-left msb 8)
                        lsb))]
    (when debug (println "STAX" (name r-msb)))
    (-> state
        (assoc-in [:cpu :memory adr] a)
        (update-in [:cpu :pc] inc))))

(defn inx [r-msb r-lsb state]
  (let [{msb r-msb
         lsb r-lsb} cpu
        d16 (+ (bit-shift-left msb 8)
               lsb)
        result (-> d16
                   inc
                   (bit-and 0xffff))
        msb' (bit-shift-right result 8)
        lsb' (bit-and result 0xff)]
    (when debug (println "INX" r-msb))
    (-> state
        (update [:cpu] merge {r-msb msb'
                              r-lsb lsb'})
        (update-in [:cpu :pc] inc))))

(defn inr [r state]
  (let [v (-> state :cpu r)
        result (-> v inc (bit-and 0xff))]
    (when debug (println "INR" (-> r name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac result)}))))                 

(defn dcr [r state]
  (let [v (-> state :cpu r)
        result (-> v dec (bit-and 0xff))]
    (when debug (println "DCR" (-> r name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac result)}))))

(defn mvi [r state]
  (let [pc (-> state :cpu :pc)
        d8 (-> state :memory (nth (inc pc)))]
    (when debug (println "MVI" (str (-> r name clojure.string/upper-case)
                                    ","
                                    d8)))
    (-> state
        (assoc-in [:cpu r] d8)
        (update-in [:cpu :pc] + 2))))

;;;;;;;;;;;;
;; rotate ;;
;;;;;;;;;;;;

(defn rlc [state]
  (let [a (-> state :cpu :a)
        cy (-> a
               (bit-and 2r10000000)
               (bit-shift-right 7))
        result (+ (-> a
                      (bit-shift-left 1)
                      (bit-and 0xff))
                  cy)]
    (when debug (println "RLC"))
    (-> state
        (assoc-in [:cpu :a] result)
        (assoc-in [:flags :cy] cy)
        (update-in [:cpu :pc] inc))))

(defn rrc [state]
  (let [a (-> state
              :cpu
              :a)
        cy (bit-and a 2r00000001)
        result (+ (-> a
                      (bit-shift-right 1)
                      (bit-and 0xff))
                  (bit-shift-left cy 7))]
    (when debug (println "RRC"))
    (-> state
        (assoc-in [:cpu :a] result)
        (assoc-in [:flags :cy] cy)
        (update-in [:cpu :pc] inc))))

(defn ral [state]
  (let [a (-> state :cpu :a)
        cy (-> state :flags :cy)
        cy' (-> a
                (bit-and 2r10000000)
                (bit-shift-right 7))
        result (-> a
                   (bit-shift-left 1)
                   (bit-and 0xff)
                   (+ cy))]
    (when debug (println "RAL"))
    (-> state
        (assoc-in [:cpu :a] result)
        (assoc-in [:flags :cy] cy')
        (update-in [:cpu :pc] inc))))

(defn rar [state]
  (let [a (-> state :cpu :a)
        cy (-> state :flags :cy)
        cy' (bit-and a 1)
        result (+ (bit-shift-left cy 7)
                  (-> a
                      (bit-shift-right 1)
                      (bit-and 0xff)))]
    (when debug (println "RAR"))
    (-> state
        (assoc-in [:cpu :a] result)
        (assoc-in [:flags :cy] cy')
        (update-in [:cpu :pc] inc))))

(defn dad [r-msb state]
  (let [r16 (get-r16 r-msb state)
        hl (get-r16 :h state)
        result (bit-and (+ hl r16)
                        0xffff)
        h' (bit-shift-right result 8)
        l' (bit-and result 0xff)
        ;; 16-bit carry; extract into fn?
        cy (-> (+ hl r16)
               (bit-and 0xffff0000)
               (> 0))]
    (when debug (println "DAD" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu :h] h')
        (assoc-in [:cpu :l] l')
        (update-in [:cpu :pc] inc)
        (update :flags merge {:cy cy}))))

(defn dad-sp [state]
  (let [sp (-> state :cpu :sp)
        hl (get-r16 :h state)
        result (bit-and (+ hl sp)
                        0xffff)
        h' (bit-shift-right result 8)
        l' (bit-and result 0xff)
        
        ;; 16-bit carry; extract into fn?
        cy (-> (+ hl sp)
               (bit-and 0xffff0000)
               (> 0))]
    (when debug (println "DAD SP"))
    (-> state
        (assoc-in [:cpu :h] h')
        (assoc-in [:cpu :l] l')
        (update-in [:cpu :pc] inc)
        (update :flags merge {:cy cy}))))

(defn ldax [r-msb state]
  (let [r16 (get-r16 r-msb state)
        v (-> state :memory (nth r16))]
    (when debug (println "LDAX" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu :a] v)
        (update-in [:cpu :pc] inc))))

(defn dcx [r-msb state]
  (let [r16 (get-r16 r-msb state)
        r-lsb (case r-msb
                :b :c
                :d :e
                :h :l)
        result (-> r16
                   dec 
                   (bit-and 0xffff))
        r-msb' (bit-shift-right result 8)
        r-lsb' (bit-and result 0xff)]
    (when debug (println "DCX" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r-msb] r-msb')
        (assoc-in [:cpu r-lsb] r-lsb')
        (update-in [:cpu :pc] inc))))

(defn dcx-sp [state]
  (let [result (-> state
                   :cpu
                   :sp
                   dec 
                   (bit-and 0xffff))]
    (when debug (println "DCX SP"))
    (-> state
        (assoc-in [:cpu :sp] result)
        (update-in [:cpu :pc] inc))))

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
        #_=> (recur (lxi :b :c state))

        0x02
        #_=> (recur (stax :b :c state))

        0x03
        #_=> (recur (inx :b :c state))

        0x04
        #_=> (recur (inr :b state))

        0x05
        #_=> (recur (dcr :b state))

        0x06
        #_=> (recur (mvi :b state))

        0x07
        #_=> (recur (rlc state))

        ;; 0x08
        ;; deliberately undefined

        0x09
        #_=> (recur (dad :b state))

        0x0a
        #_=> (recur (ldax :b state))

        0x0b
        #_=> (recur (dcx :b state))

        0x0c
        #_=> (recur (inr :c state))

        0x0d
        #_=> (recur (dcr :c state))

        0x0e
        #_=> (recur (mvi :c state))

        0x0f
        #_=> (recur (rrc state))

        ;; 0x10
        ;; deliberately undefined

        0x11
        #_=> (recur (lxi :d :e state))

        0x12
        #_=> (recur (stax :d :e state))

        0x13
        #_=> (recur (inx :d :e state))

        0x14
        #_=> (recur (inr :d state))

        0x15
        #_=> (recur (dcr :d state))

        0x16
        #_=> (recur (mvi :d state))

        0x17
        #_=> (recur (ral state))

        ;; 0x18
        ;; deliberately undefined

        0x19
        #_=> (recur (dad :d state))

        0x1a
        #_=> (recur (ldax :d state))

        0x1b
        #_=> (recur (dcx :d state))

        0x1c
        #_=> (recur (inr :e state))

        0x1d
        #_=> (recur (dcr :e state))

        0x1e
        #_=> (recur (mvi :e state))

        0x1f
        #_=> (recur (rar state))

        ;; 0x20
        ;; #_=> nil

        0x21
        #_=> (recur (lxi :h :l state))

        ;; 0x22
        ;; #_=> nil

        0x23
        #_=> (recur (inx :h :l state))

        0x24
        #_=> (recur (inr :h state))

        0x25
        #_=> (recur (dcr :h state))

        0x26
        #_=> (recur (mvi :h state))

        ;; 0x27
        ;; #_=> nil

        ;; 0x28
        ;; #_=> nil 

        0x29
        #_=> (recur (dad :h state))

        ;; 0x2a
        ;; #_=> nil

        0x2b
        #_=> (recur (dcx :h state))

        0x2c
        #_=> (recur (inr :l state))

        0x2d
        #_=> (recur (dcr :l state))

        0x2e
        #_=> (recur (mvi :l state))

        ;; 0x2f
        ;; #_=> nil

        ;; 0x30
        ;; #_=> nil

        0x31
        #_=> (let [lsb (nth memory (+ 1 pc))
                   msb (nth memory (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (when debug (println "LXI SP," (format "#$%x" d16)))
               (recur (-> state
                          (assoc-in [:cpu :sp] (bit-and d16 0xffff))
                          (update-in [:cpu :pc] + 3))))

        ;; 0x32
        ;; #_=> nil

        0x33
        #_=> (let [sp (:sp cpu)
                   result (-> sp
                              inc
                              (bit-and 0xffff))]                   
               (println "INX SP")
               (recur (-> state
                          (assoc [:cpu :sp] sp)
                          (update-in [:cpu :pc] inc))))

        0x34
        #_=> (let [hl (get-r16 :h state)
                   v (-> state :memory (nth hl))
                   result (-> hl inc (bit-and 0xff))]
               (when debug (println "INR M"))
               (-> state
                   (assoc-in [:memory hl] result)
                   (update-in [:cpu :pc] inc)
                   (update :flags merge {:z (flag-z result)
                                         :s (flag-s result)
                                         :p (flag-p result)
                                         :ac (flag-ac result)})))

        0x35
        #_=> (let [hl (get-r16 :h state)
                   v (-> state :memory (nth hl))
                   result (-> hl dec (bit-and 0xff))]
               (when debug (println "DCR M"))
               (-> state
                   (assoc-in [:memory hl] result)
                   (update-in [:cpu :pc] inc)
                   (update :flags merge {:z (flag-z result)
                                         :s (flag-s result)
                                         :p (flag-p result)
                                         :ac (flag-ac result)})))

        0x36
        #_=> (let [hl (get-r16 :h state)
                   d8 (-> state :memory (nth (inc pc)))]
               (when debug (println "MVI" (str "M,"
                                               d8)))
               (-> state
                   (assoc-in [:memory hl] d8)
                   (update-in [:cpu :pc] + 2)))

        ;; 0x37
        ;; #_=> nil

        ;; 0x38
        ;; #_=> nil

        0x39
        #_=> (recur (dad-sp state))

        ;; 0x3a
        ;; #_=> nil

        0x3b
        #_=> (recur (dcx-sp state))

        0x3c
        #_=> (recur (inr :a state))

        0x3d
        #_=> (recur (dcr :a state))

        0x3e
        #_=> (recur (mvi :a state))

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
        #_=> (recur (add :b state))

        0x81
        #_=> (recur (add :c state))

        0x82
        #_=> (recur (add :d state))

        0x83
        #_=> (recur (add :e state))

        0x84
        #_=> (recur (add :h state))

        0x85
        #_=> (recur (add :l state))

        0x86
        #_=> (let [a (-> state :cpu :a)
                   hl (get-r16 :h state)
                   m (-> state :cpu (nth hl))
                   result (-> (+ a m)
                              (bit-and 0xff))]
               (when debug (println "ADD M"))
               (recur (-> state
                          (assoc-in [:cpu :a] result)
                          (update-in [:cpu :pc] inc)
                          (assoc :flags {:z (flag-z result)
                                         :s (flag-s result)
                                         :p (flag-p result)
                                         :cy (flag-cy a m)
                                         :ac (flag-ac result)}))))

        0x87
        #_=> (recur (add :a state))

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
                                         :cy (flag-cy a d8)
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
