(ns eighty-eighty.core-test
  (:require [clojure.test :refer :all]
            [eighty-eighty.core :refer :all]))

;; TODO: test more opcode fns. this site seems to have good examples:
;; https://www.geeksforgeeks.org/rotate-instructions-in-8085/

(deftest flag-p-test
  (testing "0 bits"
    (is (= 1 (flag-p 2r00000000))))
  (testing "3 bites"
    (is (= 0 (flag-p 2r01010001))))
  (testing "4 bits"
    (is (= 1 (flag-p 2r01101001))))
  (testing "7 bits"
    (is (= 0 (flag-p 2r01111111)))))

(deftest flag-ac-test
  (testing "return value"
    (is (= 1 (flag-ac 0x2B 0x39)))
    (is (= 1 (flag-ac 0x3e 0x22)))))

(deftest flag-cy-test
  (testing "return value"
    (is (= 1 (flag-ac 0x2B 0x39)))
    (is (= 1 (flag-ac 0x3e 0x22)))))

(deftest rlc-test
  (testing "once"
    (is (= {:flags {:cy 1}
            :cpu {:a 2r01010101
                  :pc 1}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10101010
                      :pc 0}}
               rlc))))
  (testing "twice"
    (is (= {:flags {:cy 0}
            :cpu {:a 2r10101010
                  :pc 2}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10101010
                      :pc 0}}
               rlc
               rlc)))))

(deftest rrc-test
  (testing "once"
    (is (= {:flags {:cy 1}
            :cpu {:a 2r11000000
                  :pc 1}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10000001
                      :pc 0}}
               rrc))))
  (testing "twice"
    (is (= {:flags {:cy 0}
            :cpu {:a 2r01100000
                  :pc 2}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10000001
                      :pc 0}}
               rrc
               rrc)))))

(deftest ral-test
  (testing "once"
    (is (= {:flags {:cy 1}
            :cpu {:a 2r01010100
                  :pc 1}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10101010
                      :pc 0}}
               ral))))
  (testing "twice"
    (is (= {:flags {:cy 0}
            :cpu {:a 2r10101001
                  :pc 2}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10101010
                      :pc 0}}
               ral
               ral)))))

(deftest rar-test
  (testing "once"
    (is (= {:flags {:cy 1}
            :cpu {:a 2r01000000
                  :pc 1}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10000001
                      :pc 0}}
               rar))))
  (testing "twice"
    (is (= {:flags {:cy 0}
            :cpu {:a 2r10100000
                  :pc 2}}
           (-> {:flags {:cy 0}
                :cpu {:a 2r10000001
                      :pc 0}}
               rar
               rar)))))

(deftest shld-test
  ;; example taken from pg. 30 of the 8080 Programmer's Manual
  (testing "return value"
    (is (= '(0 41 174 0) ;; address in range 0x0109 - 0x010c
           (->> (shld {:cpu {:h 0xae
                             :l 0x29
                             :pc 0}
                       :memory (->> (-> 0x010a (repeat 0))
                                    (concat [0x22 0x0a 0x01])
                                    vec)})
                :memory
                (drop 0x0109))))))

(deftest lhld-test
  ;; taken from pg 31 of the 8080 Programmer's Manual
  (testing "return value"
      (is (= {:h 3, :l 255, :pc 3}
             (->> (lhld {:cpu {:h 0xae
                               :l 0x29
                               :pc 0}
                         :memory (-> [0x2a 0x5b 0x02]
                                     (concat (-> 0x025b (repeat 0)))
                                     vec
                                     (assoc 0x025b 0xff)
                                     (assoc 0x025c 0x03))})
                  :cpu)))))

(deftest daa-test
  ;; taken from pg. 16 of the 8080 Programmer's Manual
  (testing "return value"
    (is (= {:cpu {:a 1
                  :pc 1}
            :flags {:ac 1
                    :cy 1}}
           (daa {:cpu {:a 0x9b
                       :pc 0}
                 :flags {:ac 0
                         :cy 0}})))))

(deftest cma-test
  ;; taken from pg. 15 of the 8080 Programmer's Manual
  (testing "return value"
    (is (= {:cpu {:a 2r10101110
                  :pc 1}}
           (cma {:cpu {:a 2r01010001
                       :pc 0}})))))

(deftest stc-test
  (testing "return value"
    (is (= {:cpu {:pc 1}, :flags {:cy 1}}
           (stc {:cpu {:pc 0}
                 :flags {:cy 0}})))))

(deftest cmc-test
  (testing "return value"
    (is (= {:cpu {:pc 1}
            :flags {:cy 0}}
           (cmc {:cpu {:pc 0}
                 :flags {:cy 1}})))
    (is (= {:cpu {:pc 1}
            :flags {:cy 1}}
           (cmc {:cpu {:pc 0}
                 :flags {:cy 0}})))))

(deftest sta-test
  (testing "return value"
    (is (= {:cpu {:a 0x69
                  :pc 3}
            :memory [0x32 0x03 0x00 0x69]}
           (sta {:cpu {:a 0x69
                       :pc 0}
                 :memory [0x32 0x03 0x00 0x00]})))))

(deftest lda-test
  (testing "return value"
    (is (= {:pc 3
            :a 0x04}
           (:cpu (lda {:cpu {:pc 0}
                       :memory [0x3a 0x03 0x00 0x04]}))))))

(deftest stax-test
  (testing "return value"
    (is (= {:cpu {:a 0x69
                  :b 0x00
                  :c 0x01
                  :pc 1}
            :memory [0x02 0x69]}
           (stax :b {:cpu {:a 0x69
                           :b 0x00
                           :c 0x01
                           :pc 0}
                     :memory [0x02 0x00]})))))

(deftest inx-test
  (testing "return value"
    (is (= {:cpu {:sp 0x00
                  :pc 1}}
           (inx :sp {:cpu {:sp 0xffff
                           :pc 0}})))
    (is (= {:cpu {:d 0x39
                  :e 0x00
                  :pc 1}}
           (inx :d {:cpu {:d 0x38
                          :e 0xff
                          :pc 0}})))))

(deftest inr-test
  (testing "return value"
    (is (= {:cpu {:c 0x9a
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 1
                    :ac 0}}
           (inr :c {:cpu {:c 0x99
                          :pc 0}})))))

(deftest dcr-test
  (testing "return value"
    (is (= {:cpu {:h 0x00
                  :l 0x01
                  :pc 1}
            :memory [0x00 0x3f]
            :flags {:z 0
                    :s 0
                    :p 1
                    :ac 0}}
           (dcr :m {:cpu {:h 0x00
                          :l 0x01
                          :pc 0}
                    :memory [0x00 0x40]})))))

(deftest dad-test
  (testing "return value"
    (is (= {:cpu {:h 0x08
                  :l 0x64
                  :pc 1}
            :flags {:cy 0}}
           (dad :h {:cpu {:h 0x04
                          :l 0x32
                          :pc 0}})))
    (is (= {:cpu {:h 0xd5
                  :l 0x1a
                  :sp 0x339f
                  :pc 1}
            :flags {:cy 0}}
           (dad :sp {:cpu {:h 0xa1
                           :l 0x7b
                           :sp 0x339f
                           :pc 0}})))
    (is (= {:cpu {:b 0x33
                  :c 0x9f
                  :h 0xd5
                  :l 0x1a
                  :pc 1}
            :flags {:cy 0}}
           (dad :b {:cpu {:b 0x33
                          :c 0x9f
                          :h 0xa1
                          :l 0x7b
                          :pc 0}})))))

(deftest add-test
  (testing "return value"
    (is (= {:cpu {:a 0x08
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 0
                    :cy 0
                    :ac 0}}
           (add :a {:cpu {:a 0x04
                          :pc 0}})))
    (is (= {:cpu {:a 0x6e
                  :h 0x00
                  :l 0x02
                  :pc 1}
            :flags {:z 0, :cy 0, :ac 0, :p 0, :s 0},
            :memory [0 0 2]}
           (add :m {:cpu {:a 0x6c
                          :h 0x00
                          :l 0x02
                          :pc 0}
                    :memory [0x00 0x00 0x02]})))
    (is (= {:cpu {:a 0x9a
                  :d 0x2e
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 1
                    :cy 0
                    :ac 1}}
           (add :d {:cpu {:a 0x6c
                          :d 0x2e
                          :pc 0}})))))
(deftest adc-test
  (testing "return value"    
    (is (= {:cpu {:a 0x80
                  :c 0x3d
                  :pc 1}
            :flags {:cy 0
                    :s 1
                    :z 0
                    :p 0
                    :ac 1}}
           (adc :c {:cpu {:a 0x42
                          :c 0x3d
                          :pc 0}
                    :flags {:cy 1}})))
    (is (= {:cpu {:a 0x7f
                  :c 0x3d
                  :pc 1}
            :flags {:cy 0
                    :s 0
                    :z 0
                    :p 0
                    :ac 0}}
           (adc :c {:cpu {:a 0x42
                          :c 0x3d
                          :pc 0}
                    :flags {:cy 0}})))))

(deftest sub-test
  (testing "return value"
    (is (= {:cpu {:a 0
                  :pc 1}
            :flags {:z 1
                    :s 0
                    :p 1
                    :cy 0
                    :ac 1}}
           (sub :a {:cpu {:a 0x3e
                          :pc 0}})))))

(deftest sbb-test
  (testing "return value"
    (is (= {:cpu {:a 0x01
                  :l 0x02
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 0
                    :cy 0
                    :ac 1}}
           (sbb :l {:cpu {:a 0x04
                          :l 0x02
                          :pc 0}
                    :flags {:cy 1}})))))

(deftest ana-test
  (testing "return value"
    (is (= {:cpu {:a 2r00001100
                  :h 0x00
                  :l 0x01
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 0}
            :memory [0x00 2r00001111]}
           (ana :m {:cpu {:a 2r11111100
                          :h 0x00
                          :l 0x01
                          :pc 0}
                    :memory [0x00 2r00001111]})))
    (is (= {:cpu {:a 2r00001100
                  :c 2r00001111
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 0}}
           (ana :c {:cpu {:a 2r11111100
                          :c 2r00001111
                          :pc 0}})))))

(deftest xra-test
  (testing "return value"
    (is (= {:cpu {:a 0
                  :pc 1}
            :flags {:z 1
                    :s 0
                    :p 1
                    :cy 0}}
           (xra :a {:cpu {:a 0x88
                          :pc 0}})))
    ;; a's value is the one's complement of b's value
    (is (= {:cpu {:a 2r11000101
                  :b 2r00111010
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 1
                    :cy 0}}
           (xra :b {:cpu {:a 0xff
                          :b 2r00111010
                          :pc 0}})))))

(deftest ora-test
  (testing "return value"
    (is (= {:cpu {:a 0x5f
                  :b 0x5f
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 0}}
           (ora :b {:cpu {:a 0
                          :b 0x5f
                          :pc 0}})))
    (is (= {:cpu {:a 2r10101111
                  :b 2r10100000
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 1
                    :cy 0}}
           (ora :b {:cpu {:a 2r00001111
                          :b 2r10100000
                          :pc 0}})))))

(deftest cmp-test
  (testing "return value"
    ;; all examples taken from pg 20-21, 8080 Programmer's Manual
    (is (= {:cpu {:a 0x0a
                  :e 0x05
                  :pc 1}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 0
                    :ac 1}}
           (cmp :e {:cpu {:a 0x0a
                          :e 0x05
                          :pc 0}})))
    (is (= {:cpu {:a 0x02
                  :e 0x05
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 0
                    :cy 1
                    :ac 0}}
           (cmp :e {:cpu {:a 0x02
                          :e 0x05
                          :pc 0}})))
    (is (= {:cpu {:a (two's-complement 27)
                  :e 0x05
                  :pc 1}
            :flags {:z 0
                    :s 1
                    :p 0
                    :cy 0
                    :ac 1}}
           (cmp :e {:cpu {:a (two's-complement 27)
                          :e 0x05
                          :pc 0}})))))

(deftest ret-test
  (testing "return value"
    (is (= {:cpu {:sp 3
                  :pc 0x0403}
            :memory [0x00 0x03 0x04]}
           (ret {:cpu {:sp 0x01}
                 :memory [0x00 0x03 0x04]})))))

(deftest pop-test
  (testing "default"
    (is (= {:h 0x93
            :l 0x3d
            :pc 1
            :sp 0x123b}
           (:cpu (pop :h {:cpu {:sp 0x1239
                                :pc 0}
                          :memory (-> (repeat 0x1239 0)
                                      (vec)
                                      (conj 0x3d 0x93))})))))
  (testing "psw"
    (is (= {:cpu {:sp 0x2c02
                  :pc 1
                  :a 0xff},          
            :flags {:s 1
                    :z 1
                    :ac 0
                    :p 0
                    :cy 1}}
           (-> (pop :psw {:cpu {:sp 0x2c00
                                :pc 0}
                          :memory (-> (repeat 0x2c00 0)
                                      (vec)
                                      (conj 0xc3 0xff))})
               (dissoc :memory))))))

(deftest jmp-test
  (testing "return value"
    (is (= {:cpu {:pc 0x1234}
            :memory [0xc3 0x34 0x12]}
           (jmp {:cpu {:pc 0}
                 :memory [0xc3 0x34 0x12]})))))

(deftest call-test
  (testing "return value"
    (is (= {:cpu {:pc 0x1234
                  :sp 0x0009}
            ;;                               pc-hi pc-lo
            :memory [0x00 0x00 0xcd 0x34 0x12 0x00 0x02 0x00]}
           (call {:cpu {:pc 0x0002
                        :sp 0x0007}
                  :memory [0x00 0x00 0xcd 0x34 0x12 0x00 0x00 0x00]})))))

(deftest push-test
  (testing "default"
    (is (= {:cpu {:d 0x8f
                  :e 0x9d
                  :sp 0x0002
                  :pc 1}
            :memory [0x00 0x00 0x9d 0x8f 0x00]}
           (push :d {:cpu {:d 0x8f
                           :e 0x9d
                           :sp 0x0004
                           :pc 0}
                     :memory [0x00 0x00 0x00 0x00 0x00]}))))
  ;; is the fact that this has to be tested separately an indication
  ;; that push should be a multimethod?
  (testing "psw"
    (is (= {:cpu {:a 0x1f
                  :sp 2
                  :pc 1},            
            :flags {:z 1
                    :s 0
                    :p 1
                    :cy 1
                    :ac 0}
             :memory [0 0 0x47 0x1f 0]}
           (push :psw {:cpu {:a 0x1f
                             :sp 0x0004
                             :pc 0}
                       :flags {:z 1 
                               :s 0
                               :p 1
                               :cy 1
                               :ac 0}
                       :memory [0x00 0x00 0x00 0x00 0x00]})))))

(deftest adi-test
  (testing "return value"
    (is (= {:cpu {:a 0x56
                  :pc 2}
            :memory [0xc6 0x42]
            :flags {:z 0
                    :s 0
                    :cy 0
                    :p 1
                    :ac 0}}
           (adi {:cpu {:a 0x14
                       :pc 0}
                 :memory [0xc6 0x42]})))
    (is (= {:cpu {:a 0x14
                  :pc 2}
            :memory [0xc6 (two's-complement 0x42)]
            :flags {:z 0
                    :s 0
                    :cy 1
                    :p 1
                    :ac 1}}
           (adi {:cpu {:a 0x56
                       :pc 0}
                 :memory [0xc6 (two's-complement 0x42)]})))))

(deftest aci-test
  (testing "return value"
    (is (= {:cpu {:a 0x14
                  :pc 2}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 1
                    :ac 1}
            :memory [0xc6 (two's-complement 0x42)]}
           (aci {:cpu {:a 0x56
                       :pc 0}
                 :flags {:cy 0}
                 :memory [0xc6 (two's-complement 0x42)]})))
    (is (= {:cpu {:a 0x57
                  :pc 2}
            :flags {:z 0
                    :s 0
                    :p 0
                    :cy 0
                    :ac 0}
            :memory [0xc6 0x42]}
           (aci {:cpu {:a 0x14
                       :pc 0}
                 :flags {:cy 1}
                 :memory [0xc6 0x42]})))))

(deftest sui-test
  (testing "return value"
    (is (= {:cpu {:a (two's-complement 0x01)
                  :pc 0x02}
            :flags {:z 0
                    :s 1
                    :p 1
                    :cy 1
                    :ac 0}
            :memory [0xd6 0x01]}
           (sui {:cpu {:a 0x00
                       :pc 0x00}
                 :memory [0xd6 0x01]})))))

(deftest sbi-test
  (testing "return value"
    (is (= {:cpu {:a (two's-complement 0x02)
                  :pc 2}
            :memory  [0xde 0x01]
            :flags {:z 0
                    :s 1
                    :p 0
                    :cy 1
                    :ac 0}}
           (sbi {:cpu {:a 0x00
                       :pc 0x00}
                 :flags {:cy 1}
                 :memory [0xde 0x01]})))
    (is (= {:cpu {:a (two's-complement 0x01)
                  :pc 2}
            :memory  [0xde 0x01]
            :flags {:z 0
                    :s 1
                    :p 1
                    :cy 1
                    :ac 0}}
           (sbi {:cpu {:a 0x00
                       :pc 0x00}
                 :flags {:cy 0}
                 :memory [0xde 0x01]})))))

(deftest xthl-test
  (testing "return value"
    (is (= {:cpu {:h 0x0d
                  :l 0xf0
                  :sp 0x00
                  :pc 0x01}
            :memory [0x3c 0x0b]}
           (xthl {:cpu {:h 0x0b
                        :l 0x3c
                        :sp 0x00
                        :pc 0x00}
                  :memory [0xf0 0x0d]})))))

(deftest ani-test
  (testing "return value"
    (is (= {:cpu {:a 2r00001010
                  :pc 2}
            :flags {:z 0
                    :s 0
                    :p 1
                    :cy 0}
            :memory [0xe6 2r00001111]}
           (ani {:cpu {:a 2r00111010
                       :pc 0x00}
                 :memory [0xe6 2r00001111]})))))

(deftest xri-test
  (testing "return value"
    (is (= {:cpu {:a 2r10111010
                  :pc 2}
            :flags {:z 0
                    :s 1
                    :p 0
                    :cy 0}
            :memory [0xee 2r10000001]}
           (xri {:cpu {:a 2r00111011
                       :pc 0x00}
                 :memory [0xee 2r10000001]})))))

(deftest ori-test
  (testing "return value"
    (is (= {:cpu {:a 2r10111111
                  :pc 2}
            :flags {:z 0
                    :s 1
                    :p 0
                    :cy 0}
            :memory [0xf6 2r00001111]}
           (ori {:cpu {:a 2r10110101
                       :pc 0x00}
                 :memory [0xf6 2r00001111]})))))

(deftest pchl-test
  (testing "return value"
    (is (= {:cpu {:h 0x41
                  :l 0x3e
                  :pc 0x413e}}
           (pchl {:cpu {:h 0x41
                        :l 0x3e
                        :pc 0x00}})))))
