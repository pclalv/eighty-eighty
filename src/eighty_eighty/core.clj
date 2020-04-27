(ns eighty-eighty.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math]))

(def debug true)

(def flags
  "from pg 22 of 8080 Programmer's Manual:
  7 6 5 4 3 2 1 0
        a       c
  s z 0 c 0 p 1 y"
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

;; debug output

(defn d16-str [n]
  (format "$%04x" n))

(defn byte-str-with-padding
  "prints a string like 01010101"
  [i]
  (clojure.pprint/cl-format nil "~8'0b" i))

;; getters

(defn get-r-lsb [r-msb]
  (case r-msb
    :a :f
    :b :c
    :d :e
    :h :l
    (throw (Exception. (str "Unknown register: " r-msb)))))

(defmulti get-r16 
  "Get a 16-bit register."
  (fn [r-msb _state & _opts] r-msb))

(defmethod get-r16 :psw
  [_ state & {split? :split?}]
  (let [flags (:flags state)
        msb (-> state :cpu :a)
        lsb (+ (* (math/expt 2 7) (flags :s))
               (* (math/expt 2 6) (flags :z))
               (* (math/expt 2 4) (flags :ac))
               (* (math/expt 2 2) (flags :p))
               (* (math/expt 2 1))
               (* (math/expt 2 0) (flags :cy)))]
    (if split?
      [msb lsb]
      (-> lsb
          (+ (bit-shift-left msb 8))
          (bit-and 0xffff)))))

(defmethod get-r16 :sp
  [_ state & {split? :split?}]
  (if split?
    (throw (Exception. "not implemented: (get-r16 :sp {...} :split? true)"))
    (-> state :cpu :sp)))

(defmethod get-r16 :default
  [r-msb state & {split? :split?}]
  (let [r-lsb (get-r-lsb r-msb)
        {msb r-msb
         lsb r-lsb} (:cpu state)]
    (if split?
      [msb lsb]
      (-> (+ (bit-shift-left msb 8)
             lsb)
          (bit-and 0xffff)))))

(defmulti get-r8 (fn [r _state] r))

(defmethod get-r8 :m
  [_ state]
  (let [hl (get-r16 :h state)]
    (-> state :memory (nth hl))))

(defmethod get-r8 :default
  [r state]
  (-> state :cpu r))

(defn get-d16-from-pc
  "returns the two bytes following PC in memory as a 16-bit integer"
  [state]
  (let [pc (-> state :cpu :pc)
        memory (:memory state)
        adr-lsb (-> state :memory (nth (-> pc inc)))
        adr-msb (-> state :memory (nth (-> pc inc inc)))]
    (+ (bit-shift-left adr-msb 8)
       adr-lsb)))

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

(def flag-p-bits
  [2r10000000
   2r01000000
   2r00100000
   2r00010000
   2r00001000
   2r00000100
   2r00000010
   2r00000001])

(defn flag-p [n]
  (let [parity (apply + (map #(if (= 0 (bit-and n %))
                                0
                                1)
                             flag-p-bits))]
    (if (even? parity)
      1
      0)))

;; TODO: factor out a carry function that accepts the byte at which
;; we're looking for a carry. this code needs to check for 4-bit,
;; 8-bit, and 16-bit carries.

(defn flag-cy-16 [& addends]
  ;; TODO: does this only work for addition?
  (let [sum (->> addends
                 (map #(bit-and % 0xffff))
                 (apply +))]
    (if (> sum 0xffff)
      1
      0)))

(defn flag-cy [& addends]
  ;; TODO: does this only work for addition?
  (let [sum (->> addends
                 (map #(bit-and % 0xff))
                 (apply +))]
    (if (> sum 0xff)
      1
      0)))

(defn flag-ac [& addends]
  ;; TODO: does this only work for addition?  
  (let [sum (->> addends
                 (map #(bit-and % 0xf))
                 (apply +))]
    (if (> sum 0xf)
      1
      0)))

;; ops

(defn add* [v state & {with-carry :with-carry}]
  (let [addends (if with-carry
                  [(-> state :cpu :a) v (-> state :flags :cy)]
                  [(-> state :cpu :a) v])
        ;; "I am emulating the 8-bit math instructions by using a
        ;; 16-bit number. That makes it easy to figure out if the math
        ;; generated a carry out of it."
        ;; hence the (bit-and 0xff)]
        result (->> addends
                    (apply +)
                    (bit-and 0xff))]
    (-> state
        (assoc-in [:cpu :a] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :cy (apply flag-cy addends)
                              :ac (apply flag-ac addends)}))))

(defn add [r state]
  (when debug (println "ADD" (-> r name clojure.string/upper-case)))
  (let [v (get-r8 r state)]
    (add* v state)))

(defn adc [r state]
  (when debug (println "ADC" (-> r name clojure.string/upper-case)))
  (let [v (get-r8 r state)]
    (add* v state
          :with-carry true)))

(defn bit-flip-lsb [b]
  (bit-flip b 0))

(defn two's-complement [v]
  (-> v
      bit-not
      (bit-and 0xff)
      inc))

(defn sub* [v state & {with-carry :with-carry}]
  (let [carry-increment (if with-carry
                          (-> state :flags :cy)
                          0)]
    (-> (add* (-> v (+ carry-increment) two's-complement)
              state)
        ;; If there is _no_ carry out of the high-order bit position,
        ;; indicating that a borrow occurred, the Carry bit is _set_;
        ;; otherwise it is reset. (Note that this differs from an add
        ;; operation, which resets the carry if no overflow occurs.)
        ;; - 8080 Programmer's Manual, pg. 18
        (update-in [:flags :cy] bit-flip-lsb))))

(defn sub [r state]
  (let [v (get-r8 r state)]
    (when debug (println "SUB" (-> r name clojure.string/upper-case)))
    (sub* v state)))

(defn sbb [r state]
  (let [v (get-r8 r state)]
    (when debug (println "SSB" (-> r name clojure.string/upper-case)))
    (sub* v state :with-carry true)))

(defmulti lxi (fn [r _state] r))
(defmethod lxi :sp
  [_ state]
  (let [memory (:memory state)
        pc (-> state :cpu :pc)
        lsb (nth memory (+ 1 pc))
        msb (nth memory (+ 2 pc))
        d16 (+ (bit-shift-left msb 8)
               lsb)]
    (when debug (println (str "LXI SP," (d16-str d16))))
    (-> state
        (assoc-in [:cpu :sp] (bit-and d16 0xffff))
        (update-in [:cpu :pc] + 3))))

(defmethod lxi :default
  [r-msb state]
  (let [pc (-> state :cpu :pc)
        r-lsb (get-r-lsb r-msb)
        memory (:memory state)
        lsb (nth memory (+ 1 pc))
        msb (nth memory (+ 2 pc))
        d16 (+ (bit-shift-left msb 8)
               lsb)]
    (when debug (println "LXI" (str (-> r-msb name clojure.string/upper-case)
                                    ","
                                    (d16-str d16)))) 
    (-> state
        (assoc-in [:cpu r-msb] msb)
        (assoc-in [:cpu r-lsb] lsb)
        (update-in [:cpu :pc] + 3))))

(defn stax [r-msb state]
  (let [a (-> state :cpu :a)
        adr (get-r16 r-msb state)]
    (when debug (println "STAX" (name r-msb)))
    (-> state
        (assoc-in [:memory adr] a)
        (update-in [:cpu :pc] inc))))

(defmulti inx (fn [r-msb _state] r-msb))
(defmethod inx :sp
  [_ state]
  (let [sp (-> state :cpu :sp)
        result (-> sp
                   inc
                   (bit-and 0xffff))]
    (println "INX SP")
    (-> state
        (assoc-in [:cpu :sp] result)
        (update-in [:cpu :pc] inc))))

(defmethod inx :default
  [r-msb state]
  (let [r16 (get-r16 r-msb state)
        r-lsb (get-r-lsb r-msb)
        result (-> r16
                   inc
                   (bit-and 0xffff))
        msb' (bit-shift-right result 8)
        lsb' (bit-and result 0xff)]
    (when debug (println "INX" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r-msb] msb')
        (assoc-in [:cpu r-lsb] lsb')
        (update-in [:cpu :pc] inc))))

(defmulti inr (fn [r _state] r))
(defmethod inr :m
  [_ state]
  (let [hl (get-r16 :h state)
        v (-> state :memory (nth hl))
        result (-> hl inc (bit-and 0xff))]
    (when debug (println "INR M"))
    (-> state
        (assoc-in [:memory hl] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac result)}))))

(defmethod inr :default
  [r state]
  (let [v (-> state :cpu r)
        result (-> v inc (bit-and 0xff))]
    (when debug (println "INR" (-> r name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac v 1)}))))                 

(defmulti dcr (fn [r _state] r))

(defmethod dcr :m
  [_ state]
  (let [hl (get-r16 :h state)
        v (-> state :memory (nth hl))
        result (-> v dec (bit-and 0xff))]
    (when debug (println "DCR M"))
    (-> state
        (assoc-in [:memory hl] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac v -1)}))))

(defmethod dcr :default
  [r state]
  (let [v (-> state :cpu r)
        result (-> v dec (bit-and 0xff))]
    (when debug (println "DCR" (-> r name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu r] result)
        (update-in [:cpu :pc] inc)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              :ac (flag-ac v -1)}))))

(defmulti mvi (fn [r _state] r))
(defmethod mvi :m
  [_ state]
  (let [hl (get-r16 :h state)
        pc (-> state :cpu :pc)
        d8 (-> state :memory (nth (inc pc)))]
    (when debug (println "MVI" (str "M,"
                                    d8)))
    (-> state
        (assoc-in [:memory hl] d8)
        (update-in [:cpu :pc] + 2))))

(defmethod mvi :default
  [r state]
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
        l' (bit-and result 0xff)]
    (when debug (println "DAD" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu :h] h')
        (assoc-in [:cpu :l] l')
        (update-in [:cpu :pc] inc)
        (update :flags merge {:cy (flag-cy-16 hl r16)}))))

(defn ldax [r-msb state]
  (let [r16 (get-r16 r-msb state)
        v (-> state :memory (nth r16))]
    (when debug (println "LDAX" (-> r-msb name clojure.string/upper-case)))
    (-> state
        (assoc-in [:cpu :a] v)
        (update-in [:cpu :pc] inc))))

(defmulti dcx (fn [r _state] r))
(defmethod dcx :sp
  [_ state]
  (let [result (-> state
                   :cpu
                   :sp
                   dec 
                   (bit-and 0xffff))]
    (when debug (println "DCX SP"))
    (-> state
        (assoc-in [:cpu :sp] result)
        (update-in [:cpu :pc] inc))))

(defmethod dcx :default
  [r-msb state]
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

(defn shld [state]
  (let [{:keys [h l pc]} (-> state :cpu)
        adr-lsb (-> state :memory (nth (-> pc inc)))
        adr-msb (-> state :memory (nth (-> pc inc inc)))
        adr (+ (bit-shift-left adr-msb 8)
               adr-lsb)]
    (when debug (println "SHLD" (format "%04x" adr)))
    (-> state
        (assoc-in [:memory adr] l)
        (assoc-in [:memory (inc adr)] h)
        (update-in [:cpu :pc] + 3))))

(defn daa-0 [state]
  (let [a (-> state :cpu :a)
        ac (-> state :flags :ac)
        low-nybble (bit-and a 2r00001111)
        increment (if (or (> low-nybble 9)
                          (= ac 1))
                    6
                    0)
        a' (+ a increment)]
    (-> state
        (assoc-in [:cpu :a] (bit-and a' 0xff))
        (assoc-in [:flags :ac] (flag-ac low-nybble increment)))))

(defn daa-1 [state]
  (let [a (-> state :cpu :a)
        cy (-> state :flags :cy)
        low-nybble (bit-and a 2r00001111)
        high-nybble (bit-and a 2r11110000)
        increment (if (or (> high-nybble 9)
                          (= cy 1))
                    (bit-shift-left 6 4)
                    0)
        high-nybble' (+ high-nybble increment)
        a' (+ (bit-and high-nybble' 2r11110000)
              low-nybble)]
    (-> state
        (assoc-in [:cpu :a] a')
        (assoc-in [:flags :cy] (flag-cy high-nybble increment)))))
      
(defn daa [state]
  (-> state
      daa-0
      daa-1
      (update-in [:cpu :pc] inc)))

(defn lhld [state]
  (let [{:keys [h l pc]} (-> state :cpu)
        memory (:memory state)
        adr-lsb (-> state :memory (nth (-> pc inc)))
        adr-msb (-> state :memory (nth (-> pc inc inc)))
        adr (+ (bit-shift-left adr-msb 8)
               adr-lsb)]
    (when debug (println "LHLD"  (format "%04x" adr)))
    (-> state
        (assoc-in [:cpu :l] (nth memory adr))
        (assoc-in [:cpu :h] (nth memory (inc adr)))
        (update-in [:cpu :pc] + 3))))

(defn cma [state]
  (let [a' (-> state :cpu :a bit-not (bit-and 0xff))]
    (when debug (println "CMA"))
    (-> state
        (assoc-in [:cpu :a] a')
        (update-in [:cpu :pc] inc))))

(defn stc [state]
  (let []
    (when debug (println "STC"))
    (-> state
        (assoc-in [:flags :cy] 1)
        (update-in [:cpu :pc] inc))))

(defn cmc [state]
  (let []
    (when debug (println "CMC"))
    (-> state
        (assoc-in [:flags :cy] (-> state :flags :cy bit-flip-lsb))
        (update-in [:cpu :pc] inc))))

(defn sta [state]
  (let [pc (-> state :cpu :pc)
        memory (:memory state)
        adr-lsb (-> state :memory (nth (-> pc inc)))
        adr-msb (-> state :memory (nth (-> pc inc inc)))
        adr (+ (bit-shift-left adr-msb 8)
               adr-lsb)]
    (when debug (println "STA" (format "%04x" adr)))
    (-> state
        (assoc-in [:memory adr] (-> state :cpu :a))
        (update-in [:cpu :pc] + 3))))

(defn lda [state]
  (let [adr (get-d16-from-pc state)
        a' (-> state :memory (nth adr))]
    (when debug (println "LDA" (format "%04x" adr)))
    (-> state
        (assoc-in [:cpu :a] a')
        (update-in [:cpu :pc] + 3))))

(defmulti mov (fn [r-dst r-src _state]
                (cond (= r-dst :m)
                      #_=> :to-m

                      (= r-src :m)
                      #_=> :from-m)))

(defmethod mov :to-m
  [_ r-src state]
  (let [adr (get-r16 :h state)]
    (when debug (println "MOV" (str "M,"
                                    (-> r-src name clojure.string/upper-case))))
    (-> state
        (assoc-in [:memory adr] (-> state :cpu r-src))
        (update-in [:cpu :pc] inc))))

(defmethod mov :from-m
  [r-dst _ state]
  (let [adr (get-r16 :h state)]
    (when debug (println "MOV" (str (-> r-dst name clojure.string/upper-case)
                                    ",M")))
    (-> state
        (assoc-in [:cpu r-dst] (-> state :memory (nth adr)))
        (update-in [:cpu :pc] inc))))

(defmethod mov :default
  [r-dst r-src state]
  (let []
    (when debug (println "MOV" (str (-> r-dst name clojure.string/upper-case)
                                    ","
                                    (-> r-src name clojure.string/upper-case))))
    (-> state
        (assoc-in [:cpu r-dst] (-> state :cpu r-src))
        (update-in [:cpu :pc] inc))))

(defn get-r8-from-pc [state]
  (-> state
      :memory
      (nth (-> state :cpu :pc inc))))

(defn bitwise-a [state bitwise-fn op & {r :register
                                        immediate? :immediate?}]
  (let [pc-increment (if immediate? 2 1)
        a (get-r8 :a state)
        v (cond r (get-r8 r state)
                immediate? (get-r8-from-pc state)
                :else (throw (Exception. "Must pass either :register or :immediate?")))
        result (bitwise-fn a v)]
    (when debug (if immediate?
                  (println op (format "0x%02x" v))
                  (println op (-> r name clojure.string/upper-case))))
    (-> state
        (assoc-in [:cpu :a] result)
        (update :flags merge {:z (flag-z result)
                              :s (flag-s result)
                              :p (flag-p result)
                              ;; TODO: figure out if/how cy and ac are
                              ;; affected. docs are unclear.
                              :cy 0})
        (update-in [:cpu :pc] + pc-increment))))

(defn ana [r state]
  (bitwise-a state bit-and "ANA"
             :register r))

(defn xra [r state]
  (bitwise-a state bit-xor "XRA"
             :register r))

(defn ora [r state]
  (bitwise-a state bit-or "ORA"
             :register r))

(defn ani [state]
  (bitwise-a state bit-and "ANI"
             :immediate? true))

(defn xri [state]
  (bitwise-a state bit-xor "XRI"
             :immediate? true))

(defn ori [state]
  (bitwise-a state bit-or "ORI"
             :immediate? true))

(defn cmp [r state]
  (let [a (get-r8 :a state)
        v (get-r8 r state)]
    (when debug (println "CMP" (-> r name clojure.string/upper-case)))
    (-> (sub* v state)
        ;; unlike subtraction operations, cmp does not affect a
        (assoc-in [:cpu :a] a))))

(defn ret [state & {op :op :or {op "RET"}}]
  (let [sp (get-r16 :sp state)
        memory (:memory state)
        pc-lsb (-> state :memory (nth sp))
        pc-msb (-> state :memory (nth (inc sp)))
        pc' (+ (bit-shift-left pc-msb 8)
               pc-lsb)]
    (when debug (println op))
    (-> state
        (assoc-in [:cpu :pc] pc')
        (update-in [:cpu :sp] + 2))))

(defn flag-c? [state]
  (= 1 (-> state :flags :cy)))

(defn rc [state]
  (if (flag-c? state)
    (ret state)
    state))

(defn rnc [state]
  (if (not (flag-c? state))
    (ret state :op "RNC")
    state))

(defn flag-z? [state]
  (= 1 (-> state :flags :z)))

(defn rz [state]
  (if (flag-z? state)
    (ret state :op "RZ")
    state))

(defn rnz [state]
  (if (not (flag-z? state))
    (ret state :op "RNZ")
    state))

(defn rm [state]
  (if (= 1 (-> state :flags :s))
    (ret state :op "RM")
    state))

(defn rp [state]
  (if (= 0 (-> state :flags :s))
    (ret state :op "RP")
    state))

(defn rpe [state]
  (if (= 1 (-> state :flags :p))
    (ret state :op "RPE")
    state))

(defn rpo [state]
  (if (= 0 (-> state :flags :p))
    (ret state :op "RPO")
    state))

(defn adi* [state & {:keys [with-carry? subtract? op]
                     :or {with-carry? false
                          subtract? false}}]
  (let [{a :a
         pc :pc} (:cpu state)
        carry-increment (if with-carry?
                          (-> state :flags :cy)
                          0)
        d8 (-> state :memory (nth (inc pc)))
        addends (if subtract?
                  ;; with subtraction, we must two's complement and
                  ;; sum `d8` and `cy` _BEFORE_ the arithmetic
                  ;; happens, so that we only pass its result and `a`
                  ;; to any downstream add operations; if we were to
                  ;; pass `a`, `d8` and `cy`, we'd get the wrong
                  ;; result because `flag-cy`/`flag-ac` might see a
                  ;; carry where there is none.
                  [(apply + (map two's-complement [d8 carry-increment]))]
                  [d8 carry-increment])
        addends (conj addends a)
        result (apply + addends)
        op (if op
             op
             (({true {true "SBI"
                      false "SUI"}
                false {true "ACI"
                       false "ADI"}} subtract?) with-carry?))]
    (when debug (println op d8))
    (-> state
        (assoc-in [:cpu :a] (bit-and result 0xff))
        (update-in [:cpu :pc] + 2)
        (assoc :flags {:z (flag-z result)
                       :s (flag-s result)
                       :cy (if subtract?
                             (bit-flip-lsb (apply flag-cy addends))
                             (apply flag-cy addends))
                       :p (flag-p result)
                       :ac (apply flag-ac addends)}))))

(defn adi [state]
  (adi* state))

(defn aci [state]
  (adi* state :with-carry? true))

(defn sui [state]
  ;; might be able to piggy-back off internals of adi much like
  ;; add/sub piggy-back off add*
  (adi* state :subtract? true))

(defn sbi [state]
  ;; might be able to piggy-back off internals of adi much like
  ;; add/sub piggy-back off add*
  (adi* state
        :subtract? true
        :with-carry? true))

(defn cpi [state]
  (let [a (get-r8 :a state)]
    (-> state
        (adi* :subtract? true
              :op "CPI")
        (assoc-in [:cpu :a] a))))

(defmulti assoc-in-cpu-r16 (fn [_ r16 _ _] r16))
(defmethod assoc-in-cpu-r16 :psw
  [state _ d16-msb d16-lsb]
  (let [s (if (bit-test d16-lsb 7) 1 0)
        z (if (bit-test d16-lsb 6) 1 0)
        ac (if (bit-test d16-lsb 4) 1 0)
        p (if (bit-test d16-lsb 2) 1 0)
        cy (if (bit-test d16-lsb 0) 1 0)]
    (-> state
        (assoc-in [:cpu :a] d16-msb)
        (assoc :flags {:z z
                       :s s
                       :p p
                       :cy cy
                       :ac ac}))))

(defmethod assoc-in-cpu-r16 :default
  [state r-msb d16-msb d16-lsb]
  (let [r-lsb (get-r-lsb r-msb)]
    (-> state
        (assoc-in [:cpu r-msb] d16-msb)
        (assoc-in [:cpu r-lsb] d16-lsb))))

(defn pop [r16 state]
  (let [sp (get-r16 :sp state)
        d16-lsb (-> state :memory (nth sp))
        d16-msb (-> state :memory (nth (inc sp)))]
    (when debug (println "POP" (-> r16 name clojure.string/upper-case)))
    (-> state
        ;; this assoc-in-cpu-r16 thing is experimental. i just wanna
        ;; give it a shot to push the abstraction down a level.

        ;; still, it seems necessary to test the :psw case specially,
        ;; so maybe pop is best left as the multimethod.

        ;; probably it'd've been best to implement :flags as the :f
        ;; register, and use `bit-set` and `bit-clear` accordingly.
        (assoc-in-cpu-r16 r16 d16-msb d16-lsb)
        (update-in [:cpu :sp] + 2)
        (update-in [:cpu :pc] inc))))

(defn jmp [state & {op :op :or {op "JMP"}}]
  (let [pc' (get-d16-from-pc state)]
    (when debug (println op (d16-str pc')))
    (-> state
        (assoc-in [:cpu :pc] pc'))))

(defn jc [state]
  (if (flag-c? state)
    (jmp state :op "JC")
    state))

(defn jnc [state]
  (if (not (flag-c? state))
    (jmp state :op "JNC")
    state))

(defn jz [state]
  (if (flag-z? state)
    (jmp state :op "JZ")
    state))

(defn jnz [state]
  (if (not (flag-z? state))
    (jmp state :op "JNZ")
    state))

(defn jm [state]
  (if (= 1 (-> state :flags :s))
    (jmp state :op "JM")
    state))

(defn jp [state]
  (if (= 0 (-> state :flags :s))
    (jmp state :op "JP")
    state))

(defn jpe [state]
  (if (= 1 (-> state :flags :p))
    (jmp state :op "JPE")
    state))

(defn jpo [state]
  (if (= 0 (-> state :flags :p))
    (jmp state :op "JPO")
    state))

(defn call [state & {:keys [op interrupt-handler-adr]
                     :or {op "CALL"}}]
  (let [sp (-> state :cpu :sp)
        adr (if interrupt-handler-adr
              interrupt-handler-adr
              (-> state :cpu :pc))
        pc-lo-nybble (bit-shift-right adr 8)
        pc-hi-nybble (bit-and adr 0xff)]
    (when debug (println op (d16-str adr)))
    (-> state
        (assoc-in [:memory (-> sp dec)] pc-hi-nybble)
        (assoc-in [:memory (-> sp dec dec)] pc-lo-nybble)
        (assoc-in [:cpu :pc] (get-d16-from-pc state))
        (update-in [:cpu :sp] + 2))))

(defn cc [state]
  (if (flag-c? state)
    (call state :op "CC")
    state))

(defn cnc [state]
  (if (not (flag-c? state))
    (call state :op "CNC")
    state))

(defn cz [state]
  (if (flag-z? state)
    (call state :op "CZ")
    state))

(defn cnz [state]
  (if (not (flag-z? state))
    (call state :op "CNZ")
    state))

(defn cm [state]
  (if (= 1 (-> state :flags :s))
    (call state :op "CM")
    state))

(defn cp [state]
  (if (= 0 (-> state :flags :s))
    (call state :op "CP")
    state))

(defn cpe [state]
  (if (= 1 (-> state :flags :p))
    (call state :op "CPE")
    state))

(defn cpo [state]
  (if (= 0 (-> state :flags :p))
    (call state :op "CPO")
    state))

(defn push
  [r16 state]
  (let [sp (-> state :cpu :sp)
        [msb lsb] (get-r16 r16 state :split? true)]
    (when debug (println "PUSH" (-> r16 name clojure.string/upper-case)))
    (-> state
        (assoc-in [:memory (-> sp dec)] msb)
        (assoc-in [:memory (-> sp dec dec)] lsb)
        (update-in [:cpu :sp] - 2)
        (update-in [:cpu :pc] inc))))

(defn rst [exp state]
  (call state
        :op (str "RST " exp)
        :interrupt-handler-adr (* exp 0x08)))

(defn out [state]
  ;; FIXME
  (let []
    (when debug (println "OUT"))
    (-> state
        (update-in [:cpu :pc] + 2))))

(defn in [state]
  ;; FIXME
  (let []
    (when debug (println "IN"))
    (-> state
        (update-in [:cpu :pc] + 2))))

(defn di [state]
  (when debug (println "DI"))
  (-> state
      (assoc-in [:cpu :interrupt-enabled] false)
      (update-in [:cpu :pc] inc)))

(defn ei [state]
  (when debug (println "EI"))
  (-> state
      (assoc-in [:cpu :interrupt-enabled] true)
      (update-in [:cpu :pc] inc)))

(defn xthl [state]
  (let [sp (-> state :cpu :sp)]
    (when debug (println "XTHL"))
    (-> state
        (assoc-in [:cpu :l] (-> state :memory (nth sp)))
        (assoc-in [:cpu :h] (-> state :memory (nth (inc sp))))
        (assoc-in [:memory sp] (-> state :cpu :l))
        (assoc-in [:memory (inc sp)] (-> state :cpu :h))
        (update-in [:cpu :pc] inc))))

(defn pchl [state]
  (let [hl (get-r16 :h state)]
    (when debug (println "PCHL"))
    (-> state
        (assoc-in [:cpu :pc] hl))))

(defn xchg [state]
  (let [sp (-> state :cpu :sp)]
    (when debug (println "XCHG"))
    (-> state
        (assoc-in [:cpu :l] (-> state :cpu :e))
        (assoc-in [:cpu :h] (-> state :cpu :d))
        (assoc-in [:cpu :e] (-> state :cpu :l))
        (assoc-in [:cpu :d] (-> state :cpu :h))
        (update-in [:cpu :pc] inc))))

(defn sphl [state]
  (let [hl (get-r16 :h state)]
    (when debug (println "SPHL"))
    (-> state
        (assoc-in [:cpu :sp] hl)
        (update-in [:cpu :pc] inc))))

(defn hlt []
  (println "HLT"))

;; TODO: continue implementing arithmetic operations
;; http://www.emulator101.com/arithmetic-group.html
(defn nop [state]
  (when debug (println "NOP"))
  (-> state
      (update-in [:cpu :pc] inc)))

(defn emulate [memory & {:keys [debug]}]
  (loop [{memory :memory
          {:keys [pc] :as cpu} :cpu
          :as state} (-> initial-state
                         ;; extend memory so that it fills a 16-bit address space?
                         (assoc :memory (->> (concat memory (repeat 0x00))
                                             (take 0xffff)
                                             vec)))]
    (let [opcode (nth memory pc)]
      (case opcode
        0x00
        #_=> (recur (nop state))

        0x01
        #_=> (recur (lxi :b state))

        0x02
        #_=> (recur (stax :b state))

        0x03
        #_=> (recur (inx :b state))

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
        #_=> (recur (lxi :d state))

        0x12
        #_=> (recur (stax :d state))

        0x13
        #_=> (recur (inx :d state))

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

        ;; 0x20 ;; RIM
        ;; deliberately undefined

        0x21
        #_=> (recur (lxi :h state))

        0x22
        #_=> (recur (shld state))

        0x23
        #_=> (recur (inx :h state))

        0x24
        #_=> (recur (inr :h state))

        0x25
        #_=> (recur (dcr :h state))

        0x26
        #_=> (recur (mvi :h state))

        0x27
        #_=> (recur (daa state))

        ;; 0x28
        ;; deliberately undefined

        0x29
        #_=> (recur (dad :h state))

        0x2a
        #_=> (recur (lhld state))

        0x2b
        #_=> (recur (dcx :h state))

        0x2c
        #_=> (recur (inr :l state))

        0x2d
        #_=> (recur (dcr :l state))

        0x2e
        #_=> (recur (mvi :l state))

        0x2f
        #_=> (recur (cma state))

        ;; 0x30 ;; SIM
        ;; deliberately undefined

        0x31
        #_=> (recur (lxi :sp state))

        0x32
        #_=> (recur (sta state))

        0x33
        #_=> (recur (inx :sp state))

        0x34
        #_=> (recur (inr :m state))

        0x35
        #_=> (recur (dcr :m state))

        0x36
        #_=> (recur (mvi :m state))

        0x37
        #_=> (recur (stc state))

        ;; 0x38
        ;; deliberately undefined

        0x39
        #_=> (recur (dad :sp state))

        0x3a
        #_=> (recur (lda state))

        0x3b
        #_=> (recur (dcx :sp state))

        0x3c
        #_=> (recur (inr :a state))

        0x3d
        #_=> (recur (dcr :a state))

        0x3e
        #_=> (recur (mvi :a state))

        0x3f
        #_=> (recur (cmc state))

        0x40
        #_=> (recur (mov :b :b state))

        0x41
        #_=> (recur (mov :b :c state))

        0x42
        #_=> (recur (mov :b :d state))

        0x43
        #_=> (recur (mov :b :e state))

        0x44
        #_=> (recur (mov :b :h state))

        0x45
        #_=> (recur (mov :b :l state))

        0x46
        #_=> (recur (mov :b :m state))

        0x47
        #_=> (recur (mov :b :a state))

        0x48
        #_=> (recur (mov :c :b state))

        0x49
        #_=> (recur (mov :c :c state))

        0x4a
        #_=> (recur (mov :c :d state))

        0x4b
        #_=> (recur (mov :c :e state))

        0x4c
        #_=> (recur (mov :c :h state))

        0x4d
        #_=> (recur (mov :c :l state))

        0x4e
        #_=> (recur (mov :c :m state))

        0x4f
        #_=> (recur (mov :c :a state))

        0x50
        #_=> (recur (mov :d :b state))

        0x51
        #_=> (recur (mov :d :c state))

        0x52
        #_=> (recur (mov :d :d state))

        0x53
        #_=> (recur (mov :d :e state))

        0x54
        #_=> (recur (mov :d :h state))

        0x55
        #_=> (recur (mov :d :l state))

        0x56
        #_=> (recur (mov :d :m state))

        0x57
        #_=> (recur (mov :d :a state))

        0x58
        #_=> (recur (mov :e :b state))

        0x59
        #_=> (recur (mov :e :c state))

        0x5a
        #_=> (recur (mov :e :d state))

        0x5b
        #_=> (recur (mov :e :e state))

        0x5c
        #_=> (recur (mov :e :h state))

        0x5d
        #_=> (recur (mov :e :l state))

        0x5e
        #_=> (recur (mov :e :m state))

        0x5f
        #_=> (recur (mov :e :a state))

        0x60
        #_=> (recur (mov :h :b state))

        0x61
        #_=> (recur (mov :h :c state))

        0x62
        #_=> (recur (mov :h :d state))

        0x63
        #_=> (recur (mov :h :e state))

        0x64
        #_=> (recur (mov :h :h state))

        0x65
        #_=> (recur (mov :h :l state))

        0x66
        #_=> (recur (mov :h :m state))

        0x67
        #_=> (recur (mov :h :a state))

        0x68
        #_=> (recur (mov :l :b state))

        0x69
        #_=> (recur (mov :l :c state))

        0x6a
        #_=> (recur (mov :l :d state))

        0x6b
        #_=> (recur (mov :l :e state))

        0x6c
        #_=> (recur (mov :l :h state))

        0x6d
        #_=> (recur (mov :l :l state))

        0x6e
        #_=> (recur (mov :l :m state))

        0x6f
        #_=> (recur (mov :l :a state))

        0x70
        #_=> (recur (mov :m :b state))

        0x71
        #_=> (recur (mov :m :c state))

        0x72
        #_=> (recur (mov :m :d state))

        0x73
        #_=> (recur (mov :m :e state))

        0x74
        #_=> (recur (mov :m :h state))

        0x75
        #_=> (recur (mov :m :l state))

        0x76
        #_=> (hlt)

        0x77
        #_=> (recur (mov :m :a state))

        0x78
        #_=> (recur (mov :a :b state))

        0x79
        #_=> (recur (mov :a :c state))

        0x7a
        #_=> (recur (mov :a :d state))

        0x7b
        #_=> (recur (mov :a :e state))

        0x7c
        #_=> (recur (mov :a :h state))

        0x7d
        #_=> (recur (mov :a :l state))

        0x7e
        #_=> (recur (mov :a :m state))

        0x7f
        #_=> (recur (mov :a :a state))

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
        #_=> (recur (add :m state))

        0x87
        #_=> (recur (add :a state))

        0x88
        #_=> (recur (adc :b state))

        0x89
        #_=> (recur (adc :c state))

        0x8a
        #_=> (recur (adc :d state))

        0x8b
        #_=> (recur (adc :e state))

        0x8c
        #_=> (recur (adc :h state))

        0x8d
        #_=> (recur (adc :l state))

        0x8e
        #_=> (recur (adc :m state))

        0x8f
        #_=> (recur (adc :a state))

        0x90
        #_=> (recur (sub :b state))

        0x91
        #_=> (recur (sub :c state))

        0x92
        #_=> (recur (sub :d state))

        0x93
        #_=> (recur (sub :e state))

        0x94
        #_=> (recur (sub :h state))

        0x95
        #_=> (recur (sub :l state))

        0x96
        #_=> (recur (sub :m state))

        0x97
        #_=> (recur (sub :a state))

        0x98
        #_=> (recur (sbb :b state))

        0x99
        #_=> (recur (sbb :c state))

        0x9a
        #_=> (recur (sbb :d state))

        0x9b
        #_=> (recur (sbb :e state))

        0x9c
        #_=> (recur (sbb :h state))

        0x9d
        #_=> (recur (sbb :l state))

        0x9e
        #_=> (recur (sbb :m state))

        0x9f
        #_=> (recur (sbb :a state))

        0xa0
        #_=> (recur (ana :b state))

        0xa1
        #_=> (recur (ana :c state))

        0xa2
        #_=> (recur (ana :d state))

        0xa3
        #_=> (recur (ana :e state))

        0xa4
        #_=> (recur (ana :h state))

        0xa5
        #_=> (recur (ana :l state))

        0xa6
        #_=> (recur (ana :m state))

        0xa7
        #_=> (recur (ana :a state))

        0xa8
        #_=> (recur (xra :b state))

        0xa9
        #_=> (recur (xra :c state))

        0xaa
        #_=> (recur (xra :d state))

        0xab
        #_=> (recur (xra :e state))

        0xac
        #_=> (recur (xra :h state))

        0xad
        #_=> (recur (xra :l state))

        0xae
        #_=> (recur (xra :m state))

        0xaf
        #_=> (recur (xra :a state))

        0xb0
        #_=> (recur (ora :b state))

        0xb1
        #_=> (recur (ora :c state))

        0xb2
        #_=> (recur (ora :d state))

        0xb3
        #_=> (recur (ora :e state))

        0xb4
        #_=> (recur (ora :h state))

        0xb5
        #_=> (recur (ora :l state))

        0xb6
        #_=> (recur (ora :m state))

        0xb7
        #_=> (recur (ora :a state))

        0xb8
        #_=> (recur (cmp :b state))

        0xb9
        #_=> (recur (cmp :c state))

        0xba
        #_=> (recur (cmp :d state))

        0xbb
        #_=> (recur (cmp :e state))

        0xbc
        #_=> (recur (cmp :h state))

        0xbd
        #_=> (recur (cmp :l state))

        0xbe
        #_=> (recur (cmp :m state))

        0xbf
        #_=> (recur (cmp :a state))

        0xc0
        #_=> (recur (rnz state))

        0xc1
        #_=> (recur (pop :b state))

        0xc2
        #_=> (recur (jnz state))

        0xc3
        #_=> (recur (jmp state))

        0xc4
        #_=> (recur (cnz state))

        0xc5
        #_=> (recur (push :b state))

        0xc6
        #_=> (recur (adi state))

        0xc7
        #_=> (recur (rst 0 state))

        0xc8
        #_=> (recur (rz state))

        0xc9
        #_=> (recur (ret state))

        0xca
        #_=> (recur (jz state))

        ;; 0xcb
        ;; deliberately undefined

        0xcc
        #_=> (recur (cz state))

        0xcd
        #_=> (recur (call state))

        0xce
        #_=> (recur (aci state))

        0xcf
        #_=> (recur (rst 1 state))

        0xd0
        #_=> (recur (rnc state))

        0xd1
        #_=> (recur (pop :d state))

        0xd2
        #_=> (recur (jnc state))

        0xd3
        #_=> (recur (out state))

        0xd4
        #_=> (recur (cnc state))

        0xd5
        #_=> (recur (push :d state))

        0xd6
        #_=> (recur (sui state))

        0xd7
        #_=> (recur (rst 2 state))

        0xd8
        #_=> (recur (rc state))

        ;; 0xd9
        ;; deliberately undefined

        0xda
        #_=> (recur (jc state))

        0xdb
        #_=> (recur (in state))

        0xdc
        #_=> (recur (cc state))

        ;; 0xdd
        ;; deliberately undefined

        0xde
        #_=> (recur (sbi state))

        0xdf
        #_=> (recur (rst 3 state))

        0xe0
        #_=> (recur (rpo state))

        0xe1
        #_=> (recur (pop :h state))

        0xe2
        #_=> (recur (jpo state))

        0xe3
        #_=> (recur (xthl state))

        0xe4
        #_=> (recur (cpo state))

        0xe5
        #_=> (recur (push :h state))

        0xe6
        #_=> (recur (ani state))

        0xe7
        #_=> (recur (rst 4 state))

        0xe8
        #_=> (recur (rpe state))

        0xe9
        #_=> (recur (pchl state))

        0xea
        #_=> (recur (jpe state))

        0xeb
        #_=> (recur (xchg state))

        0xec
        #_=> (recur (cpe state))

        ;; 0xed
        ;; deliberately undefined

        0xee
        #_=> (recur (xri state))

        0xef
        #_=> (recur (rst 5 state))

        0xf0
        #_=> (recur (rp state))

        0xf1
        #_=> (recur (pop :psw state))

        0xf2
        #_=> (recur (jp state))

        0xf3
        #_=> (recur (di state))

        0xf4
        #_=> (recur (cp state))

        0xf5
        #_=> (recur (push :psw state))

        0xf6
        #_=> (recur (ori state))

        0xf7
        #_=> (recur (rst 6 state))

        0xf8
        #_=> (recur (rm state))

        0xf9
        #_=> (recur (sphl state))

        0xfa
        #_=> (recur (jm state))

        0xfb
        #_=> (recur (ei state))

        0xfc
        #_=> (recur (cm state))

        ;; 0xfd
        ;; deliberately undefined

        0xfe
        #_=> (recur (cpi state))

        0xff
        #_=> (recur (rst 7 state))

        (throw (Exception. (str "unknown opcode: " (format "0x%x" opcode))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
