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
    (is (= (l/head (first (run (fresh [q] (l/member 'foo q) (return q)))))
           'foo))))
