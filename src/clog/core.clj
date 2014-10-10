(ns clog.core
  (:require [cont.core :refer [shift reset]]
            [unapply.core :as u]))

(defprotocol Term
  (objectify [term rel]))

(extend-protocol Term
  clojure.lang.ISeq
  (objectify [seq rel]
    (map #(objectify % rel) seq))
  clojure.lang.IPersistentVector
  (objectify [vec rel]
    (mapv #(objectify % rel) vec))
  Object
  (objectify [obj rel] obj)
  nil
  (objectify [_ rel] nil))

(deftype LVar [name]
  Term
  (objectify [this rel]
    (if (contains? rel this)
      (objectify (get rel this) rel)
      this))
  Object
  (toString [this]
    (str name)))

(defn lvar? [x]
  (instance? LVar x))

(defn lvar [name]
  (LVar. name))

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
  (objectify [this rel]
    (lcons (objectify head rel) (objectify tail rel)))
  Object
  (toString [x] (str head " . " tail)))

(defn lcons? [x]
  (instance? LCons x))

(defn lcons [x xs]
  (if (or (lvar? xs) (lcons? xs))
    (LCons. x xs)
    (cons x xs)))

(derive clojure.lang.Sequential ::seq)
(derive LCons ::seq)

(defmulti unify (fn [x y rel] [(type x) (type y)]))

(defmethod unify [LVar Object] [lvar obj rel]
  (cond (= lvar obj) rel
        (contains? rel lvar) (unify (get rel lvar) obj rel)
        :else (assoc rel lvar obj)))

(defmethod unify [Object LVar] [obj lvar rel]
  (assoc rel lvar obj))

(prefer-method unify [LVar Object] [Object LVar])

(defmethod unify [::seq ::seq] [xs ys rel]
  (some->> rel
           (unify (head xs) (head ys))
           (unify (tail xs) (tail ys))))

(defmethod unify :default [x y rel]
  (if (= x y) rel))

(defmacro fresh [syms & exprs]
  `(let [~@(interleave syms (map (fn [sym] `(lvar (quote ~sym))) syms))]
     ~@exprs))

(defn logic [f]
  (shift k
    (fn [a]
      (mapcat #((k %) %) (f a)))))

(def succeed (logic (fn [a] [a])))

(def fail (logic (fn [a])))

(defn is [x y]
  (logic
    (fn [a]
      (some->> a (unify x y) list))))

(defmacro all [& clauses]
  `(logic
     (fn [a#]
       ((reset ~@clauses list) a#))))

(defmacro any [& clauses]
  (let [k (gensym), a (gensym)]
    `(shift ~k
       (fn [~a]
         (concat ~@(map (fn [clause] `(mapcat #((~k %) %) ((reset ~clause list) ~a))) clauses))))))

(defn append [xs ys zs]
  (any (all (is xs ()) (is zs ys))
       (fresh [x xs' z zs']
         (all (is xs (lcons x xs'))
              (is zs (lcons z zs'))
              (is x z)
              (append xs' ys zs')))))

(defn member [x xs]
  (any (fresh [xs']
         (is xs (lcons x xs')))
       (fresh [x' xs']
         (all (is xs (lcons x' xs'))
              (member x xs')))))

(defn return [lvar]
  (fn [a]
    [(objectify lvar a)]))

(defmacro run [& exprs]
  `((reset ~@exprs) {}))
