(ns curry.utils
  (:refer-clojure :exclude [defn fn])
  (:require [curry.core :refer :all]))

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
    (reduce (fn [acc [k v]] (assoc acc k (f v))) {} this))

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
