(ns clog.test
  (:require [clojure.test :refer :all]
            [clog.core :refer [run fresh return] :as l]))

(deftest append
  (testing "concatenation"
    (is (= (run (fresh [q] (l/append [1 2 3] [4 5] q) (return q)))
           [[1 2 3 4 5]])))
  (testing "difference"
    (is (= (run (fresh [q] (l/append [1 2 3] q [1 2 3 4 5]) (return q)))
           [[4 5]])))
  (testing "combination"
    (is (= (run (fresh [a b] (l/append a b [1 2 3 4 5]) (return [a b])))
           [[[] [1 2 3 4 5]] [[1] [2 3 4 5]] [[1 2] [3 4 5]] [[1 2 3] [4 5]] [[1 2 3 4] [5]] [[1 2 3 4 5] []]]))))

(deftest member
  (testing "enumeration"
    (is (= (run (fresh [q] (l/member q ['foo 'bar 'baz]) (return q)))
           ['foo 'bar 'baz])))
  (testing "construction"
    (is (= (:head (first (run (fresh [q] (l/member 'foo q) (return q)))))
           'foo))))

(with-test
  (defn sudoku [table]
    (letfn [(every [f xs]
              (if xs
                (l/all (f (first xs)) (every f xs))
                l/succeed))
            (digits [xs]
              (every #(l/member % (range 1 10)) xs))
            (satisfy [p x]
              (l/logic a (if (p (get a x)) [a])))]
      (l/all (every digits table)
             (every digits (apply map list table))
             (every digits (map flatten (partition 3 (apply mapcat list (map (partial partition 3) table)))))
             (every (partial satisfy (partial apply distinct?)) table))))
  (let [table [[(l/lvar) (l/lvar) (l/lvar) 2 6 (l/lvar) 7 (l/lvar) 1]
               [6 8 (l/lvar) (l/lvar) 7 (l/lvar) (l/lvar) 9 (l/lvar)]
               [1 9 (l/lvar) (l/lvar) (l/lvar) 4 5 (l/lvar) (l/lvar)]
               [8 2 (l/lvar) 1 (l/lvar) (l/lvar) (l/lvar) 4 (l/lvar)]
               [(l/lvar) (l/lvar) 4 6 (l/lvar) 2 9 (l/lvar) (l/lvar)]
               [(l/lvar) 5 (l/lvar) (l/lvar) (l/lvar) 3 (l/lvar) 2 8]
               [(l/lvar) (l/lvar) 9 3 (l/lvar) (l/lvar) (l/lvar) 7 4]
               [(l/lvar) 4 (l/lvar) (l/lvar) 5 (l/lvar) (l/lvar) 3 6]
               [7 (l/lvar) 3 (l/lvar) 1 8 (l/lvar) (l/lvar) (l/lvar)]]]
    (prn (time (run
      (fresh [q]
        (l/is q table)
        (sudoku table)
        (return q)))))))
