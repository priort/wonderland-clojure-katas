(ns magic-square.puzzle-test
  (:require [clojure.test :refer :all]
            [magic-square.puzzle :refer :all]))

(defn sum-rows [m]
  (map #(reduce + %) m))

(defn sum-cols [m]
  [(reduce + (map first m))
   (reduce + (map second m))
   (reduce + (map last m))])

(defn sum-diagonals [m]
  [(+ (get-in m [0 0]) (get-in m [1 1]) (get-in m [2 2]))
   (+ (get-in m [2 0]) (get-in m [1 1]) (get-in m [0 2]))])

(defn assert-magic-square [square]
  (is (= (set (sum-rows square))
         (set (sum-cols square))
         (set (sum-diagonals square))))
  (is (= 1
         (count (set (sum-rows square)))
         (count (set (sum-cols square)))
         (count (set (sum-diagonals square))))))

(deftest test-magic-square
  (testing "all the rows, columns, and diagonal add to the same number"
    (assert-magic-square (magic-square values))))

(def values-non-distinct [2 2 2 2 2 2 2 2 2])

(deftest magic-square-found-for-non-distinct-values
  (testing "magic square found for non distinct numbers non distinct numbers"
    (assert-magic-square (magic-square values-non-distinct))))

(def values-unsorted [9 6 8 7 1 4 2 3 5])

(deftest magic-square-found-for-unsorted-values
  (testing "magic square found for unsorted non distinct numbers"
    (assert-magic-square (magic-square values-unsorted))))
