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
  ;; example taken from pg. 16 of the 8080 Programmer's Manual
  (testing "return value"
    (is (= {:cpu {:a 1
                  :pc 1}
            :flags {:ac 1
                    :cy 1}}
           (daa {:cpu {:a 0x9b
                       :pc 0}
                 :flags {:ac 0
                         :cy 0}})))))
