(ns card-game-war.game)

(def suit-value-by-name {:spade 1
                         :club 2
                         :diamond 3
                         :heart 4})

(def suits (keys suit-value-by-name))

(def rank-value-by-name {2 2
                         3 3
                         4 4
                         5 5
                         6 6
                         7 7
                         8 8
                         9 9
                         10 10
                         :jack 11
                         :queen 12
                         :king 13
                         :ace 14})

(def ranks (keys rank-value-by-name))

(defn- play-round-higher-rank-wins [[_ rank-1 :as player1-card]
                  [_ rank-2 :as player2-card]]
  (cond
    (> (get rank-value-by-name rank-2) (get rank-value-by-name rank-1))
    [[] [player2-card player1-card]]
    (< (get rank-value-by-name rank-2) (get rank-value-by-name rank-1))
    [[player1-card player2-card] []]))

(defn- play-round-higher-suit-wins [[suit-1 _ :as player1-card]
                                    [suit-2 _ :as player2-card]]
  (cond
    (> (get suit-value-by-name suit-2) (get suit-value-by-name suit-1))
    [[] [player2-card player1-card]]
    (< (get suit-value-by-name suit-2) (get suit-value-by-name suit-1))
    [[player1-card player2-card] []]))

(defn play-round [player1-card player2-card]
  (if-let [result (play-round-higher-rank-wins player1-card player2-card)]
    result (play-round-higher-suit-wins player1-card player2-card)))

(defn play-game [player1-cards player2-cards]
  (loop [cards1-queue player1-cards
         cards2-queue player2-cards]
    (if (and (seq cards1-queue) (seq cards2-queue))
      (let [[round-result-cards1 round-result-cards2]
            (play-round (first cards1-queue) (first cards2-queue))]
        (recur (vec (concat (rest cards1-queue) round-result-cards1))
               (vec (concat (rest cards2-queue) round-result-cards2))))
      [cards1-queue cards2-queue])))

