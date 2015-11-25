(ns curry.utils
  (:refer-clojure :exclude [defn fn = + - * / assoc dissoc])
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
  clojure.lang.IPersistentMap
  (-prop [this prop]
    (get this prop))
  (-prop-or [this default prop]
    (get this prop default))
  (-prop-eq [this prop value]
    (= (get this prop) value))

  clojure.lang.IPersistentVector
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
  clojure.lang.IPersistentMap
  (-path [this path]
    (get-in this path))
  (-path-or [this default path]
    (get-in this path default))
  (-path-eq [this path value]
    (= (get-in this path) value))

  clojure.lang.IPersistentVector
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

(defprotocol Associate
  (-assoc      [this prop value])
  (-assoc-path [this path value]))

(extend-protocol Associate
  clojure.lang.IPersistentMap
  (-assoc [this prop value]
    (clojure.core/assoc this prop value))
  (-assoc-path [this path value]
    (assoc-in this path value)))

(defn assoc [prop value object]
  (-assoc object prop value))

(defn assoc-path [path value object]
  (-assoc-path object path value))


(defprotocol Dissociate
  (-dissoc      [this prop])
  (-dissoc-path [this path]))

(declare dissoc)

(extend-protocol Dissociate
  clojure.lang.IPersistentMap
  (-dissoc [this prop]
    (clojure.core/dissoc this prop))
  (-dissoc-path [this path]
    (update-in this (vec (butlast path)) (dissoc (last path)))))

(defn dissoc [prop object]
  (-dissoc object prop))

(defn dissoc-path [path object]
  (-dissoc-path object path))
