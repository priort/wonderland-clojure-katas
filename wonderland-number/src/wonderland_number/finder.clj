(ns wonderland-number.finder)

(defn get-lazy-sequence-of-vectors-of-6-digit-multiples-2-3-4-5-6 []
  (iterate (fn [multiples]
             [(+ 2 (nth multiples 0))
              (+ 3 (nth multiples 1))
              (+ 4 (nth multiples 2))
              (+ 5 (nth multiples 3))
              (+ 6 (nth multiples 4))])
           [200000 300000 400000 500000 600000]))

(defn all-numbers-have-same-digits? [nums]
  (apply = (map #(set (str %)) nums)))

(defn find-first-6-digit-multiples-2-3-4-5-6-with-same-digits []
  (first (filter all-numbers-have-same-digits?
                 (get-lazy-sequence-of-vectors-of-6-digit-multiples-2-3-4-5-6))))


(defn wonderland-number []
  (/ (first (find-first-6-digit-multiples-2-3-4-5-6-with-same-digits)) 2))

