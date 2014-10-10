(ns clog.test
  (:require [clojure.test :refer :all]
            [clog.core :refer [run fresh return] :as l]))

(deftest append
  (testing "append"
    (is (= (run (fresh [q] (l/append [1 2 3] [4 5] q) (return q)))
           [[1 2 3 4 5]])))
  (testing "difference"
    (is (= (run (fresh [q] (l/append [1 2 3] q [1 2 3 4 5]) (return q)))
           [[4 5]])))
  (testing "combination"
    (is (= (run (fresh [q a b] (l/append a b [1 2 3 4 5]) (l/is q [a b]) (return q)))
           [[[] [1 2 3 4 5]] [[1] [2 3 4 5]] [[1 2] [3 4 5]] [[1 2 3] [4 5]] [[1 2 3 4] [5]] [[1 2 3 4 5] []]]))))
