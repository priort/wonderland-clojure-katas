(ns card-game-war.game)

(def suits [:spade :club :diamond :heart])
(def suit-value-by-name {:spade 1
                         :club 2
                         :diamond 3
                         :heart 4})
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
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
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn- play-round-higher-rank-wins [[_ rank-1 :as player1-card]
                  [_ rank-2 :as player2-card]]
  (cond
    (> (get rank-value-by-name rank-2) (get rank-value-by-name rank-1))
    [[] [player1-card player2-card]]
    (< (get rank-value-by-name rank-2) (get rank-value-by-name rank-1))
    [[player1-card player2-card] []]))

(defn- play-round-higher-suit-wins [[suit-1 _ :as player1-card]
                                    [suit-2 _ :as player2-card]]
  (cond
    (> (get suit-value-by-name suit-2) (get suit-value-by-name suit-1))
    [[] [player1-card player2-card]]
    (< (get suit-value-by-name suit-2) (get suit-value-by-name suit-1))
    [[player1-card player2-card] []]))

(defn play-round [player1-card player2-card]

  (println [player1-card player2-card])
  (if-let [result (play-round-higher-rank-wins player1-card player2-card)]
    result (play-round-higher-suit-wins player1-card player2-card)))

(defn play-game [player1-cards player2-cards])

