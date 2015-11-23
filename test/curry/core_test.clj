(ns curry.core-test
  (:refer-clojure :exclude [defn fn])
  (:require [clojure.test :refer :all]
            [curry.core :refer :all]))

(defn defn-under-test
  [x y z]
  (+ x y z))

(deftest test-defn
  (testing "curry/defn"
    (is (= 6 (defn-under-test 1 2 3)))
    (is (= 6 (((defn-under-test 1) 2) 3)))
    (is (= 6 ((defn-under-test 1 2) 3)))
    (is (= 6 ((defn-under-test 1) 2 3)))))


(def fn-under-test (fn [x y z] (+ x y z)))

(deftest test-fn
  (testing "curry/fn"
    (is (= 6 (fn-under-test 1 2 3)))
    (is (= 6 (((fn-under-test 1) 2) 3)))
    (is (= 6 ((fn-under-test 1 2) 3)))
    (is (= 6 ((fn-under-test 1) 2 3)))))

