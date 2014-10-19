(ns clog.core
  (:require [cont.core :refer [shift reset]]))

(defprotocol Term
  (objectify [term a]))

(extend-protocol Term
  clojure.lang.ISeq
  (objectify [seq a]
    (reduce (fn [s x] (map cons (objectify x a) s)) [()] (reverse seq)))
  clojure.lang.IPersistentVector
  (objectify [vec a]
    (reduce (fn [v x] (map conj v (objectify x a))) [[]] vec))
  Object
  (objectify [obj a] [obj])
  nil
  (objectify [_ a] [nil]))

(defrecord LVar [id constraint]
  Term
  (objectify [this a]
    (if (contains? a this)
      (mapcat #(if (constraint %) [%]) (objectify (get a this) a))
      [this])))

(defn lvar? [x]
  (instance? LVar x))

(defrecord Truth []
  clojure.lang.IFn
  (invoke [f x] true))

(def lvar (comp #(->LVar % (Truth.)) gensym))

(defprotocol Seq
  (head [seq])
  (tail [seq]))

(extend-protocol Seq
  clojure.lang.ISeq
  (head [seq] (first seq))
  (tail [seq] (if (first seq) (rest seq)))
  clojure.lang.IPersistentVector
  (head [vec] (first vec))
  (tail [vec] (if (first vec) (subvec vec 1))))

(declare lcons)

(defrecord LCons [head tail]
  Seq
  (head [lcons] head)
  (tail [lcons] tail)
  Term
  (objectify [this a]
    (map lcons (objectify head a) (objectify tail a))))

(defn lcons? [x]
  (instance? LCons x))

(defn lcons [x xs]
  (if (or (lvar? xs) (lcons? xs))
    (LCons. x xs)
    (cons x xs)))

(derive clojure.lang.Sequential ::seq)
(derive LCons ::seq)

(defmulti unify (fn [x y a] [(type x) (type y)]))

(defmethod unify [LVar LVar] [x y a]
  (cond (= x y) a
        (contains? a x) (unify (get a x) y a)
        (contains? a y) (unify x (get a y) a)
        :else (assoc a x y)))

(defmethod unify [LVar Object] [lvar obj a]
  (if (contains? a lvar)
    (unify (get a lvar) obj a)
    (assoc a lvar obj)))

(defmethod unify [Object LVar] [obj lvar a]
  (if (contains? a lvar)
    (unify obj (get a lvar) a)
    (assoc a lvar obj)))

(prefer-method unify [LVar Object] [Object LVar])

(defmethod unify [::seq ::seq] [xs ys a]
  (some->> a
           (unify (head xs) (head ys))
           (unify (tail xs) (tail ys))))

(defmethod unify :default [x y a]
  (if (= x y) a))

(defmacro fresh [syms & exprs]
  `(let [~@(interleave syms (map (fn [sym] `(lvar (quote ~sym))) syms))]
     ~@exprs))

(defmacro logic [sym expr]
  `(shift k#
     (fn [~sym]
       (mapcat #((k# %) %) ~expr))))

(def succeed (logic a [a]))

(def fail (logic a nil))

(defn is [x y]
  (logic a (some->> a (unify x y) list)))

(defmacro execute [a & goals]
  `((reset ~@goals list) ~a))

(defmacro all [& clauses]
  `(logic a# (execute a# ~@clauses)))

(defmacro any [& clauses]
  (let [k (gensym), a (gensym)]
    `(shift ~k
       (fn [~a]
         (concat ~@(map (fn [clause] `(lazy-seq (mapcat #((~k %) %) (execute ~a ~clause)))) clauses))))))

(defmacro match [e & clauses]
  (letfn [(unbounds [pattern]
            (cond (seq? pattern) (mapcat unbounds (next pattern))
                  (vector? pattern) (mapcat unbounds pattern)
                  (and (symbol? pattern) (not (contains? &env pattern))) [pattern]))]
    `(any ~@(map (fn [[pattern result]]
                   `(fresh [~@(unbounds pattern)]
                      (all (is ~pattern ~e) ~result)))
                 (partition 2 clauses)))))

(defn append [xs ys zs]
  (match [xs zs]
    [() ys] succeed
    [(lcons x xs') (lcons x zs')] (append xs' ys zs')))

(defn member [x xs]
  (match xs
    (lcons x _) succeed
    (lcons _ xs') (member x xs')))

(defn pred [lvar p]
  (is lvar (LVar. (gensym) p)))

(defn return [lvar]
  (fn [a]
    (objectify lvar a)))

(defmacro run [& exprs]
  `((reset ~@exprs) {}))
