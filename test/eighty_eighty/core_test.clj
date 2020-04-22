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
