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
