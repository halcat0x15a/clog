(ns clog.core
  (:require [cont.core :refer [shift reset]]))

(defprotocol Term
  (objectify [term a]))

(extend-protocol Term
  clojure.lang.ISeq
  (objectify [seq a]
    (map #(objectify % a) seq))
  clojure.lang.IPersistentVector
  (objectify [vec a]
    (mapv #(objectify % a) vec))
  Object
  (objectify [obj a] obj)
  nil
  (objectify [_ a] nil))

(deftype LVar [id]
  Term
  (objectify [this a]
    (if (contains? a this)
      (objectify (get a this) a)
      this)))

(defn lvar? [x]
  (instance? LVar x))

(defn lvar
  ([]
     (LVar. (gensym)))
  ([id]
     (LVar. id)))

(defmethod print-method LVar [^LVar lvar ^java.io.Writer w]
  (.write w (name (.id lvar))))

(defmacro fresh [syms & exprs]
  `(let [~@(interleave syms (map (fn [sym] `(lvar (quote ~sym))) syms))]
     ~@exprs))

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

(deftype LCons [head tail]
  Seq
  (head [lcons] head)
  (tail [lcons] tail)
  Term
  (objectify [this a]
    (lcons (objectify head a) (objectify tail a))))

(defn lcons? [x]
  (instance? LCons x))

(defn lcons [x xs]
  (cond (vector? xs) (into [x] xs)
        (seq? xs) (cons x xs)
        :else (LCons. x xs)))

(defmethod print-method LCons [^LCons lcons ^java.io.Writer w]
  (.write w "(")
  (print-method (.head lcons) w)
  (.write w " . ")
  (print-method (.tail lcons) w)
  (.write w ")"))

(derive LCons ::seq)
(derive clojure.lang.Sequential ::seq)
(derive Object ::any)
(derive ::nil ::any)

(defmulti unify
  (fn [x y a]
    [(or (class x) ::nil) (or (class y) ::nil)]))

(defmethod unify [LVar LVar] [x y a]
  (cond (= x y) [a]
        (contains? a x) (unify (get a x) y a)
        (contains? a y) (unify x (get a y) a)
        :else [(assoc a x y)]))

(defmethod unify [LVar ::any] [lvar obj a]
  (if (contains? a lvar)
    (unify (get a lvar) obj a)
    [(assoc a lvar obj)]))

(defmethod unify [::any LVar] [obj lvar a]
  (if (contains? a lvar)
    (unify obj (get a lvar) a)
    [(assoc a lvar obj)]))

(defmethod unify [::seq ::seq] [xs ys a]
  (let [x (head xs)
        y (head ys)]
    (cond (and x y) (mapcat #(unify (tail xs) (tail ys) %) (unify x y a))
          (= xs ys) [a])))

(defmethod unify :default [x y a]
  (if (= x y) [a]))

(defn bind [k xs]
  (mapcat #((k (second %)) (first %)) xs))

(defmacro logic [sym expr]
  `(shift k#
     (fn [~sym]
       (bind k# ~expr))))

(def succeed (logic a [[a a]]))

(def fail (logic a nil))

(defn is [x y]
  (logic a (map #(list % %) (unify x y a))))

(defn return [x]
  (logic a [[a (objectify x a)]]))

(defmacro execute [a & exprs]
  `((reset (let* [x# (do ~@exprs)] (fn* [a#] [[a# x#]]))) ~a))

(defmacro run [& exprs]
  `(map second (execute {} ~@exprs)))

(defmacro all [& exprs]
  `(logic a# (execute a# ~@exprs)))

(defmacro any [& exprs]
  (let [k (gensym)
        a (gensym)]
    `(shift ~k
       (fn [~a]
         (lazy-cat ~@(map (fn [e] `(bind ~k (execute ~a ~e))) exprs))))))

(defn- unbounds [env pattern]
  (->> pattern
       (tree-seq sequential? #(if (seq? %) (next %) %))
       (filter #(and (symbol? %) (not (contains? env %))))))

(defmacro match [expr & clauses]
  `(any ~@(map (fn [[pattern result]]
                 `(fresh [~@(unbounds &env pattern)]
                    (all (is ~pattern ~expr) ~result)))
               (partition 2 clauses))))

(defn append [xs ys zs]
  (match [xs zs]
    [() ys] succeed
    [(lcons x xs') (lcons x zs')] (append xs' ys zs')))

(defn member [x xs]
  (match xs
    (lcons x _) succeed
    (lcons _ xs') (member x xs')))
