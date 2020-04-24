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

(defn byte-str-with-padding
  "prints a string like 01010101"
  [i]
  (clojure.pprint/cl-format nil "~8'0b" i))

(defn get-r-lsb [r-msb]
  (case r-msb
    :b :c
    :d :e
    :h :l
    (throw (Exception. (str "Unknown register: " (name r-msb))))))

(defn get-r16
  "Get a 16-bit register."
  [r-msb state]  
  (let [r-lsb (get-r-lsb r-msb)
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

(defmulti add (fn [r _state] r))
(defmethod add :m
  [_ state]
  (when debug (println "ADD M"))
  (let [a (-> state :cpu :a)
        hl (get-r16 :h state)
        m (-> state :memory (nth hl))]
    (add* m state)))

(defmethod add :default
  [r state]
  (when debug (println "ADD" (-> r name clojure.string/upper-case)))
  (let [v (-> state :cpu r)]
    (add* v state)))

(defmulti adc (fn [r _state] r))
(defmethod adc :m
  [_ state]
  (when debug (println "ADC M"))
  (let [hl (get-r16 :h state)
        m (-> state :memory (nth hl))]
    (add* m state
          :with-carry true)))

(defmethod adc :default
  [r state]
  (when debug (println "ADC" (-> r name clojure.string/upper-case)))
  (let [v (-> state :cpu r)]
    (add* v state
          :with-carry true)))

(defn bit-flip-lsb [b]
  (bit-flip b 0))

(defn sub* [v state & {with-carry :with-carry}]
  (let [carry-increment (if with-carry
                          (-> state :flags :cy)
                          0)
        v-two's-complement (-> v
                               (+ carry-increment)
                               bit-not
                               (bit-and 0xff)
                               inc)]
    (-> (add* v-two's-complement
              state)
        ;; If there is _no_ carry out of the high-order bit position,
        ;; indicating that a borrow occurred, the Carry bit is _set_;
        ;; otherwise it is reset. (Note that this differs from an add
        ;; operation, which resets the carry if no overflow occurs.)
        ;; - 8080 Programmer's Manual, pg. 18
        (update-in [:flags :cy] bit-flip-lsb))))

(defmulti sub (fn [r _state] r))
(defmethod sub :m
  [_ state]
  (let [hl (get-r16 :h state)
        m (-> state :memory (nth hl))]
    (when debug (println "SUB M"))
    (sub* m state)))

(defmethod sub :default
  [r state]
  (let [v (-> state :cpu r)]
    (when debug (println "SUB" (-> r name clojure.string/upper-case)))
    (sub* v state)))

(defmulti lxi (fn [r _state] r))
(defmethod lxi :sp
  [_ state]
  (let [memory (:memory state)
        pc (-> state :cpu :pc)
        lsb (nth memory (+ 1 pc))
        msb (nth memory (+ 2 pc))
        d16 (+ (bit-shift-left msb 8)
               lsb)]
    (when debug (println (str "LXI SP," (format "#$%x" d16))))
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
                                    (format "#$%x" d16)))) 
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

(defmulti dad (fn [r _state] r))
(defmethod dad :sp
  [_ state]
  (let [sp (-> state :cpu :sp)
        hl (get-r16 :h state)
        result (bit-and (+ hl sp)
                        0xffff)
        h' (bit-shift-right result 8)
        l' (bit-and result 0xff)]
    (when debug (println "DAD SP"))
    (-> state
        (assoc-in [:cpu :h] h')
        (assoc-in [:cpu :l] l')
        (update-in [:cpu :pc] inc)
        (update :flags merge {:cy (flag-cy-16 hl sp)}))))

(defmethod dad :default
  [r-msb state]
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
  (let [pc (-> state :cpu :pc)
        memory (:memory state)
        adr-lsb (-> state :memory (nth (-> pc inc)))
        adr-msb (-> state :memory (nth (-> pc inc inc)))
        adr (+ (bit-shift-left adr-msb 8)
               adr-lsb)
        a' (nth memory adr)]
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
    (when debug (println (str "MOV M," (-> r-src name clojure.string/upper-case))))
    (-> state
        (assoc-in [:memory adr] (-> state :cpu r-src))
        (update-in [:cpu :pc] inc))))

(defmethod mov :from-m
  [r-dst _ state]
  (let [adr (get-r16 :h state)]
    (when debug (println (str "MOV "
                              (-> r-dst name clojure.string/upper-case)
                              ",M")))
    (-> state
        (assoc-in [:cpu r-dst] (-> state :memory (nth adr)))
        (update-in [:cpu :pc] inc))))

(defmethod mov :default
  [r-dst r-src state]
  (let []
    (when debug (println (str "MOV "
                              (-> r-dst name clojure.string/upper-case)
                              ","
                              (-> r-src name clojure.string/upper-case))))
    (-> state
        (assoc-in [:cpu r-dst] (-> state :cpu r-src))
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

        0x20
        #_=> (recur state)

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

        ;; 0x30
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
        ;; MOV B,B
        #_=> (recur state)

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
        #_=> (recur state)

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
        #_=> (recur state)

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
        #_=> (recur state)

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
        #_=> (recur state)

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
        #_=> (recur state)

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

        ;; 0x76
        ;; #_=> nil

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
        #_=> (recur state)

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
