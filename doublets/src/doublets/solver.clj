(ns doublets.solver
  (:require [clojure.java.io :as io]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn- find-matching-child [target children]
  (first (filter #(= target (first %)) children)))

(defn- remove-child-tree-with-root-matching-letter [letter children]
  (remove #(= % (find-matching-child letter children)) children))

(defn- add-letters-to-tree [[root words-ending-in-root & children] letters word]
  (let [[matching-child-root words-ending-in-matching-child-root children-of-matching-child :as existing-child-to-update]
        (find-matching-child (first letters) children)
        children-minus-child-to-update (vec (remove-child-tree-with-root-matching-letter (first letters) children))
        updated-children
        (if existing-child-to-update
          (if (= 1 (count letters))
            (conj children-minus-child-to-update [matching-child-root (conj words-ending-in-matching-child-root word) children-of-matching-child])
            (conj children-minus-child-to-update (add-letters-to-tree existing-child-to-update (drop 1 letters) word)))
          (if (= 1 (count letters))
            (conj children [(first letters) (conj words-ending-in-root word)])
            (conj children (add-letters-to-tree [(first letters) []] (drop 1 letters) word)))
          )]
    (apply conj [root words-ending-in-root] updated-children)))

(defn convert-words-to-letter-tree [ws]
  (reduce (fn [tree word-as-letters]
            (add-letters-to-tree tree word-as-letters (apply str word-as-letters)))
          [nil []] (map flatten (map (partial partition 1) ws)))
  )

(defn- find-node-from-letter-path [[_ _ & children :as node] letters]
  (if (seq letters)
    (let [matching-child (first (filter #(= (first letters) (first %)) children))]
      (if matching-child
        (find-node-from-letter-path  matching-child (drop 1 letters))
        nil))
    node))

(defn- letter-path-with-matching-word-from-node? [node candidate-val {:keys [start-letters rest-letters]}]
  (seq (filter
         #(= % (apply str (concat (conj start-letters candidate-val) (rest rest-letters))))
         (first (rest (find-node-from-letter-path node (concat [candidate-val] (vec (rest rest-letters)))))))))

(defn- find-candidate-subtree-for-alt-rest-of-word [[_ _ & children :as letter-tree] start-letters rest-letters]
  (let [node (find-node-from-letter-path letter-tree start-letters)]
    (first (filter
             (fn [[candidate-val _ _]]
               (and (not= (first rest-letters) candidate-val)
                    (letter-path-with-matching-word-from-node? node candidate-val
                                                               {:start-letters start-letters
                                                                :rest-letters rest-letters})))
        children))))

(defn- find-next-word [letter-tree word-as-letters]
  (loop [start-letters []
         letters word-as-letters]
    (if (seq letters)
      (let [candidate-sub-tree (find-candidate-subtree-for-alt-rest-of-word letter-tree start-letters letters)]
        (if (nil? candidate-sub-tree)
          (recur (conj start-letters (first letters)) (vec (drop 1 letters)))
          (apply str (apply conj start-letters (first candidate-sub-tree) (rest letters)))))
      nil
      )
    ))

(defn doublets [word1 word2]
  (loop [dictionary (filter #(not= word1 %) words)
         words-so-far [word1]
         next-word  (find-next-word
                      (convert-words-to-letter-tree dictionary)
                      (flatten (partition 1 word1)))]
    (if (or (= word2 (last words-so-far)) (empty? words-so-far))
      words-so-far
      (if (nil? (peek words-so-far))
        (recur (filter #(not= (last (butlast words-so-far)) %) dictionary)
               (vec (butlast (butlast words-so-far)))
               (find-next-word
                 (convert-words-to-letter-tree (filter #(not= (peek (pop words-so-far)) %) dictionary))
                 (flatten (partition 1 (peek (pop (pop words-so-far)))))))
        (recur (filter #(not= next-word %) dictionary)
               (conj words-so-far next-word)
               (find-next-word
                 (convert-words-to-letter-tree (filter #(not= next-word %) dictionary))
                 (flatten (partition 1 next-word)))))

      )))

(println (doublets "head" "tail"))

(println (doublets "door" "lock"))

(println (doublets "bank" "loan"))

(println (doublets "wheat" "bread"))

(println (doublets "ye" "freezer"))


