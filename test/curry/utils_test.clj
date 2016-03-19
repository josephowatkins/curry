(ns curry.utils-test
  (:require [clojure.test :refer :all]
            [curry.utils :as c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fmap

(deftest fmap-identity
  (testing "fmap id = id"
    (let [l '(1 2 3)
          v [1 2 3]
          m {:a 1 :b 2 :c 3}]
      (is (= (c/fmap identity l) (identity l)))
      (is (= (c/fmap identity v) (identity v)))
      (is (= (c/fmap identity m) (identity m))))))

(deftest fmap-compose
  (testing "fmap (f . g) = fmap f . fmap g")
  (let [f (partial + 1)
        g (partial * 3)
        l '(1 2 3)
        v [1 2 3]
        m {:a 1 :b 2 :c 3}]
    (is (= (c/fmap (comp f g) l)
           ((comp (c/fmap f) (c/fmap g)) l)))
    (is (= (c/fmap (comp f g) v)
           ((comp (c/fmap f) (c/fmap g)) v)))
    (is (= (c/fmap (comp f g) m)
           ((comp (c/fmap f) (c/fmap g)) m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maths - Binary operators

(deftest plus-test
  (testing "+"
    (let [plus-one (c/+ 1)]
      (is (= 2 (plus-one 1))))))

(deftest divide-test
  (testing "/"
    (let [divide (c// 3)]
      (is (= 1 (divide 3))))))

(deftest minus-test
  (testing "-"
    (let [minus (c/- 3)]
      (is (= -2 (minus 5))))))

(deftest multiply-test
  (testing "*"
    (let [multiply (c/* 10)]
      (is (= 100 (multiply 10))))))

(deftest flip-test
  (testing "flip"
    (let [minus-10 ((c/flip c/-) 10)
          divide-by-7 ((c/flip c//) 7)]
      (is (= 3 (minus-10  13)))
      (is (= 2 (divide-by-7 14))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prop, prop-or, prop-eq, path, path-or, path-eq

(deftest prop-test
  (testing "prop"
    (let [m {:hello "world"}
          prop-fn (c/prop :hello)] 
      (is (= (prop-fn m) "world")))))

(deftest prop-or-test
  (testing "prop-or"
    (let [m  {:a 5}
          m' {:b 6}
          prop-or-fn (c/prop-or :default :a)]
      (is (= (prop-or-fn m) 5))
      (is (= (prop-or-fn m') :default)))))

(deftest prop-eq-test
  (testing "prop-or"
    (let [m  {:a 5}
          m' {:a 6}
          prop-eq-fn (c/prop-eq :a 5)]
      (is (= (prop-eq-fn m) true))
      (is (= (prop-eq-fn m') false)))))

(deftest path-test
  (testing "path"
    (let [m {:a {"b" [1 2 3]}}
          path-fn (c/path [:a "b" 0])]
      (is (= (path-fn m) 1)))))

(deftest path-or-test
  (testing "path-or"
    (let [m {:a {"b" [1 2 3]}}
          m' {:a {"b" []}}
          path-or-fn (c/path-or :default [:a "b" 0])]
      (is (= (path-or-fn m) 1))
      (is (= (path-or-fn m') :default)))))

(deftest path-eq-test
  (testing "path-eq"
    (let [m  {:a {"b" 5}}
          m' {:a 6}
          path-eq-fn (c/path-eq [:a "b"] 5)]
      (is (= (path-eq-fn m) true))
      (is (= (path-eq-fn m') false)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assoc, assoc-path, dissoc, dissoc-path

(deftest assoc-test
  (testing "assoc"
    (let [m {:a 1 :b 2}
          expected {:a 1 :b 2 :c 3}
          assoc-fn (c/assoc :c 3) 
          actual (assoc-fn m)]
      (is (= actual expected)))))

(deftest assoc-path-test
  (testing "assoc-path"
    (let [m {:a 1 :b {:c {:d 2}}}
          expected {:a 1 :b {:c {:d 2 :e 3}}}
          assoc-path-fn (c/assoc-path [:b :c :e] 3)
          actual (assoc-path-fn m)]
      (is (= actual expected)))))

(deftest dissoc-test
  (testing "dissoc"
    (let [m {:a 1 :b 2}
          expected {:a 1}
          dissoc-fn (c/dissoc :b)
          actual (dissoc-fn m)]
      (is (= actual expected)))))

(deftest dissoc-path-test
  (testing "dissoc-path"
    (let [m {:a 1 :b {:c {:d 2}}}
          expected {:a 1 :b {:c {}}}
          dissoc-path-fn (c/dissoc-path [:b :c :d])
          actual (dissoc-path-fn m)]
      (is (= actual expected)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip-with

(deftest zip-with-test
  (testing "zip-with seq"
    (let [s1 (range 5)
          s2 (range 5)
          expected '(0 2 4 6 8)
          zip-fn (c/zip-with +)
          actual (zip-fn s1 s2)]
      (is (= actual expected))))

  (testing "zip-with vector"
    (let [v1 [0 1 2 3 4]
          v2 [0 1 2 3 4]
          expected [0 2 4 6 8]
          zip-fn (c/zip-with +)
          actual (zip-fn v1 v2)]
      (is (= actual expected))
      (is (vector? actual)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; until

(deftest until-test
  (testing "until"
    (let [expected 11
          until-fn (c/until #(> % 10) inc)
          actual (until-fn 0)]
      (is (= actual expected)))))

(deftest unless-test
  (testing "unless"
    (let [expected [10]
          unless-fn (c/unless vector? vector)
          actual (unless-fn 10)]
      (is (= actual expected)))))
