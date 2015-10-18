(ns tiny-maze.solver)

(def open-cell-values #{:S 0 :E})

(defn convert-to-1D-id-val-pairs [numbers]
  (map #(conj [%1] %2) (range) numbers))

(defn convert-maze-to-1D-id-val-pairs [maze]
  (convert-to-1D-id-val-pairs (flatten maze)))

(defn get-1D-ids-for-open-cells [maze]
  (->> maze
       (convert-maze-to-1D-id-val-pairs)
       (filter #(open-cell-values (last %)))
       (map first)))

(defn convert-1D-id-to-2D-id [id-1D maze]
  (let [maze-side-length (count maze)
        row (int (/ id-1D maze-side-length))
        column (mod id-1D maze-side-length)]
    [row column]))

(defn convert-2D-id-to-1D-id [[row column] num-rows]
  (+ column (* num-rows row)))

(defn filter-out-closed-cells [ids-1D maze]
  (filter (set (get-1D-ids-for-open-cells maze)) ids-1D))

(defn get-1D-ids-for-adjacent-cells [[row column] maze]
  (map #(convert-2D-id-to-1D-id % (count maze))
       [[(- row 1) column]
        [(+ row 1) column]
        [row (- column 1)]
        [row (+ column 1)]]))

(defn find-adjacent-open-cells-1D-ids [id-2D maze]
  (-> (get-1D-ids-for-adjacent-cells id-2D maze)
       (filter-out-closed-cells maze)))

(defn get-parent-connection-pair [[parent _] connection-pairs]
  (get connection-pairs parent))

(defn trace-connection-pairs-from-exit [connection-pairs]
  (loop [curr-connection-pair (get connection-pairs (apply max (keys connection-pairs)))
         connection-pairs-path []]
    (if (nil? curr-connection-pair)
      connection-pairs-path
      (recur (get-parent-connection-pair curr-connection-pair connection-pairs)
             (conj connection-pairs-path curr-connection-pair)))))

(defn get-child-from-connection-pair [[_ child]] child)

(defn filter-out-adjacent-cells-not-already-connected [maze connection-pairs cell-2D-id]
  (filter (set (map get-child-from-connection-pair (vals connection-pairs)))
          (find-adjacent-open-cells-1D-ids cell-2D-id maze)))

(defn candidate-parents [maze connection-pairs current-1D-id]
  (filter-out-adjacent-cells-not-already-connected
    maze connection-pairs (convert-1D-id-to-2D-id current-1D-id maze)))

(defn union-open-cell-to-lowest-id-parent [maze connection-pairs current-1D-id]
  (if-let [candidate-parents
           (seq (candidate-parents maze connection-pairs current-1D-id))]
    (assoc connection-pairs current-1D-id [(apply min candidate-parents) current-1D-id])
    (assoc  connection-pairs current-1D-id [:no-parent current-1D-id])))


(defn union-open-cells-with-lowest-id-parents [maze]
  (reduce
    #(union-open-cell-to-lowest-id-parent maze %1 %2)
    {}
    (get-1D-ids-for-open-cells maze)))

(defn find-path-through-maze-in-2D-ids [maze]
  (->> maze
       (union-open-cells-with-lowest-id-parents)
       (trace-connection-pairs-from-exit)
       (map get-child-from-connection-pair)
       (map #(convert-1D-id-to-2D-id % maze))))

(defn solve-maze [maze]
  (reduce #(assoc-in %1 %2 :x) maze (find-path-through-maze-in-2D-ids maze)))

(println (solve-maze  [[:S 0 0 1]
                       [1  1 0 0]
                       [1  0  0 1]
                       [1  1  0 :E]]))
