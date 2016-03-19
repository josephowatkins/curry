(ns curry.utils
  (:refer-clojure :exclude [defn fn = + - * / assoc dissoc])
  (:require [curry.core :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO:
;;
;; Fix the docs!
;;
;; Implement:
;;
;; until :: (a -> Bool) -> (a -> a) -> a -> a
;; zip-with :: (a -> b -> c) -> [a] -> [b] -> [c]
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fmap

(defprotocol Functor
  (-fmap [this f]))

(extend-protocol Functor
  clojure.lang.IPersistentList
  (-fmap [this f]
    (map f this))
  
  clojure.lang.IPersistentVector
  (-fmap [this f]
    (into (empty this) (map f this)))

  clojure.lang.IPersistentMap
  (-fmap [this f]
    (reduce (fn [acc [k v]] (clojure.core/assoc acc k (f v))) {} this))

  clojure.lang.LazySeq
  (-fmap [this f]
    (map f this)))

(defn fmap [f coll]
  (-fmap coll f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; foldl

(defprotocol Foldable
  (-foldl [this f v]))

(extend-protocol Foldable
  clojure.lang.IPersistentVector
  (-foldl [this f v]
    (reduce f v this))

  clojure.lang.IPersistentList
  (-foldl [this f v]
    (reduce f v this)))

(defn foldl [f v coll]
  (-foldl coll f v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maths - Binary operators

(defn + [x y]
  (clojure.core/+ x y))

(defn - [x y]
  (clojure.core/- x y))

(defn * [x y]
  (clojure.core/* x y))

(defn / [x y]
  (clojure.core// x y))

(defn = [x y]
  (clojure.core/= x y))

(defn flip [f]
  (fn [x y]
    (f y x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prop, prop-or, prop-eq, path, path-or, path-eq

(defprotocol Props
  (-prop    [this prop])
  (-prop-or [this default prop])
  (-prop-eq [this prop value]))

(extend-protocol Props
  clojure.lang.Associative
  (-prop [this prop]
    (get this prop))
  (-prop-or [this default prop]
    (get this prop default))
  (-prop-eq [this prop value]
    (= (get this prop) value)))

(defn prop [prop object]
  (-prop object prop))

(defn prop-or [default prop object]
  (-prop-or object default prop))

(defn prop-eq [prop value object]
  (-prop-eq object prop value))

(defprotocol Paths
  (-path    [this path])
  (-path-or [this default path])
  (-path-eq [this path value]))

(extend-protocol Paths
  clojure.lang.Associative
  (-path [this path]
    (get-in this path))
  (-path-or [this default path]
    (get-in this path default))
  (-path-eq [this path value]
    (= (get-in this path) value)))

(defn path [path object]
  (-path object path))

(defn path-or [default path object]
  (-path-or object default path))

(defn path-eq [path value object]
  (-path-eq object path value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assoc, assocPath, dissoc, dissocPath

(defprotocol Associative
  "Curried implementations of assoc & assoc-path"
  (-assoc       [this prop value])
  (-assoc-path  [this path value]))

(extend-protocol Associative
  clojure.lang.Associative
  (-assoc [this prop value]
    (clojure.core/assoc this prop value))
  (-assoc-path [this path value]
    (assoc-in this path value)))

(defn assoc [prop value object]
  (-assoc object prop value))

(defn assoc-path [path value object]
  (-assoc-path object path value))

(defprotocol Dissociative
  "Curried implementation of dissoc & dissoc-path. Will work on 
  all clojure.lang.Associative."
  (-dissoc      [this prop])
  (-dissoc-path [this path]))

(declare dissoc)

(extend-protocol Dissociative
  clojure.lang.IPersistentMap
  (-dissoc [this prop]
    (clojure.core/dissoc this prop))
  (-dissoc-path [this path]
    (update-in this (vec (butlast path)) (dissoc (last path))))

  clojure.lang.IPersistentVector
  (-dissoc [this prop]
    (vec (concat (subvec this 0 prop) (subvec this (inc prop)))))
  (-dissoc-path [this path]
    (if (= (count path) 1)
      (dissoc (last path) this)
      (update-in this (vec (butlast path)) (dissoc (last path))))))

(defn dissoc [prop object]
  (-dissoc object prop))

(defn dissoc-path [path object]
  (-dissoc-path object path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zip-with (a -> b -> c) -> [a] -> [b] -> [c]

(defprotocol Zippable
  (-zip-with [xs ys f]))

(extend-protocol Zippable
  clojure.lang.ISeq
  (-zip-with [this ys f]
    (map f this ys))
  
  clojure.lang.IPersistentVector
  (-zip-with [this ys f]
    (into (empty this) (map f this ys))))

(defn zip-with [f xs ys]
  (-zip-with xs ys f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; until :: (a -> Bool) -> (a -> a) -> a -> a

(defn until [pred f x]
  (if (pred x) x (recur pred f (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unless :: (a -> Bool) -> (a -> a) -> a -> a

(defn unless [pred f x]
  (if (pred x) x (f x)))
