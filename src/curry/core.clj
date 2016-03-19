(ns curry.core
  (:refer-clojure :exclude [defn fn]))

(clojure.core/defn inits [xs]
  (for [n (range (count xs))] (take n xs)))

(clojure.core/defn tails [xs]
  (map vec (take (inc (count xs)) (iterate rest xs))))

(clojure.core/defn calculate-arity
  [binders]
  (map vector (rest (inits binders)) (rest (tails binders))))

(clojure.core/defn build-arity
  [fn-name [args remaining]]
  (let [next (rest (inits remaining))]
    `([~@args]
      (clojure.core/fn ~@(for [args2 next] `([~@args2] (~fn-name ~@args ~@args2)))
        ([~@remaining] (~fn-name ~@args ~@remaining))))))

(defmacro fn
  [binders & body]
  (when-not (vector? binders)
    (throw (IllegalArgumentException. "Second argument to defn must be in a vector")))
  (let [fn-name (gensym "curried")]
    `(clojure.core/fn ~fn-name ~@(map (partial build-arity fn-name) (calculate-arity binders))
       ([~@binders] ~@body))))

(defmacro defn [name binders & body]
  (when-not (instance? clojure.lang.Symbol name)
    (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
  (when-not (vector? binders)
    (throw (IllegalArgumentException. "Second argument to defn must be in a vector")))
  `(def ~name (fn [~@binders] ~@body)))
