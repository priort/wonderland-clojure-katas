(ns magic-square.puzzle)


(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- prepare-side-of-vals-for-interleave [vals index-of-partitioned-pair-to-reverse]
  (update-in (vec (partition 2 vals)) [index-of-partitioned-pair-to-reverse] reverse))

(defn- get-vals-ordered-for-magic-square-without-mid-val [left-side right-side]
  (interleave
    (prepare-side-of-vals-for-interleave left-side 1)
    (prepare-side-of-vals-for-interleave right-side 0)))

(defn magic-square [vals]
  (let [sorted-vals (sort vals)
        mid-index (/ (count sorted-vals) 2)
        left-side (reverse (drop-last mid-index (sort sorted-vals)))
        right-side (reverse (drop mid-index (sort sorted-vals)))
        magic-square-ordering-without-mid-val (get-vals-ordered-for-magic-square-without-mid-val left-side right-side)]
    (->> (concat (drop-last 2 magic-square-ordering-without-mid-val)
                 [(nth sorted-vals mid-index)]
                 (drop 2 magic-square-ordering-without-mid-val))
         (flatten) (partition 3) (map vec) (vec)))
  )

(println (magic-square values))
(println (magic-square [2 2 2 2 2 2 2 2 2]))
(println (magic-square [9 6 8 7 1 4 2 3 5]))