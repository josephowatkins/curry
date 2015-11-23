(ns curry.utils-test
  (:require [clojure.test :refer :all]
            [curry.utils :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functor Law - fmap id = id

(deftest fmap-identity
  (testing "fmap id = id"
    (let [l '(1 2 3)
          v [1 2 3]
          m {:a 1 :b 2 :c 3}]
      (is (= (fmap identity l) (identity l)))
      (is (= (fmap identity v) (identity v)))
      (is (= (fmap identity m) (identity m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functor Law - fmap (f . g) = fmap f . fmap g

(deftest fmap-compose
  (testing "fmap (f . g) = fmap f . fmap g")
  (let [f (partial + 1)
        g (partial * 3)
        l '(1 2 3)
        v [1 2 3]
        m {:a 1 :b 2 :c 3}]
    (is (= (fmap (comp f g) l)
           ((comp (fmap f) (fmap g)) l)))
    (is (= (fmap (comp f g) v)
           ((comp (fmap f) (fmap g)) v)))
    (is (= (fmap (comp f g) m)
           ((comp (fmap f) (fmap g)) m)))))
