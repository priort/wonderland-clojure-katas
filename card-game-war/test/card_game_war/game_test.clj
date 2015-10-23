(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defn- higher-card-wins? [lower-rank-card higher-rank-card]
  (and
    (= [[] [lower-rank-card higher-rank-card]] (play-round lower-rank-card higher-rank-card))
    (= [[higher-rank-card lower-rank-card] []] (play-round higher-rank-card lower-rank-card))))

(defn get-results-from-play-round-suits-combos-lower-to-higher-rank-combos [suits ranks]
  (for [suit-1 suits
        suit-2 suits
        rank-1-index (range 0 (count ranks))
        rank-2-index (range (inc rank-1-index) (count ranks))
        :let [rank-1 (nth ranks rank-1-index)
              rank-2 (nth ranks rank-2-index)]]
    (higher-card-wins? [suit-1 rank-1] [suit-2 rank-2])))

(defn get-results-from-play-round-equal-ranks-combos-lower-to-higher-suit-combos [suits ranks]
  (for [rank ranks
        suit-1-index (range 0 (count suits))
        suit-2-index (range (inc suit-1-index) (count suits))
        :let [suit-1 (nth suits suit-1-index)
              suit-2 (nth suits suit-2-index)]]
    (higher-card-wins? [suit-1 rank] [suit-2 rank])))


(deftest test-play-round
  (testing "the highest numeric rank wins the cards in the round"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-suits-combos-lower-to-higher-rank-combos
                     suits (vec (range 2 11)))))))

  (testing "queens are higher rank than jacks"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-suits-combos-lower-to-higher-rank-combos
                     suits [:jack :queen])))))

  (testing "kings are higher rank than queens"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-suits-combos-lower-to-higher-rank-combos
                     suits [:queen :king])))))

  (testing "aces are higher rank than kings"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-suits-combos-lower-to-higher-rank-combos
                     suits [:king :ace])))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-equal-ranks-combos-lower-to-higher-suit-combos
                     [:spade :club] ranks)))))

  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= true
           (reduce #(and %1 %2) true
                   (get-results-from-play-round-equal-ranks-combos-lower-to-higher-suit-combos
                     [:club :diamond] ranks)))))
  (testing "if the ranks are equal, hearts beat diamonds"))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [divided-shuffled-cards (partition 2 (shuffle cards))
          player1-cards (first divided-shuffled-cards)
          player2-cards (last divided-shuffled-cards)
          game-result (play-game player1-cards player2-cards)]
      (is (and (seq (filter (fn [player-cards] (empty? player-cards)) game-result))
               (seq (filter (fn [player-cards]
                              (= (set cards) (set player-cards))) game-result)))))))