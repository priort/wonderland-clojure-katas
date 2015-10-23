(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(defn- higher-card-wins? [lower-rank-card higher-rank-card]
  (and
    (= [[] [higher-rank-card lower-rank-card]] (play-round lower-rank-card higher-rank-card))
    (= [[higher-rank-card lower-rank-card] []] (play-round higher-rank-card lower-rank-card))))

(defn higher-card-wins-suits-combos-lower-to-higher-rank-combos? [suits ranks]
  (for [suit-1 suits
        suit-2 suits
        rank-1-index (range 0 (count ranks))
        rank-2-index (range (inc rank-1-index) (count ranks))
        :let [rank-1 (nth ranks rank-1-index)
              rank-2 (nth ranks rank-2-index)]]
    (higher-card-wins? [suit-1 rank-1] [suit-2 rank-2])))

(defn higher-card-wins-equal-ranks-combos-lower-to-higher-suit-combos? [suits ranks]
  (for [rank ranks
        suit-1-index (range 0 (count suits))
        suit-2-index (range (inc suit-1-index) (count suits))
        :let [suit-1 (nth suits suit-1-index)
              suit-2 (nth suits suit-2-index)]]
    (higher-card-wins? [suit-1 rank] [suit-2 rank])))

(defn all-results-true? [results]
  (is (= true (reduce #(and %1 %2) true results))))

(deftest test-play-round
  (testing "the highest numeric rank wins the cards in the round"
    (all-results-true? (higher-card-wins-suits-combos-lower-to-higher-rank-combos?
                        suits (vec (range 2 11)))))

  (testing "queens are higher rank than jacks"
    (all-results-true? (higher-card-wins-suits-combos-lower-to-higher-rank-combos?
                        suits [:jack :queen])))

  (testing "kings are higher rank than queens"
    (all-results-true? (higher-card-wins-suits-combos-lower-to-higher-rank-combos?
        suits [:queen :king])))

  (testing "aces are higher rank than kings"
    (all-results-true? (higher-card-wins-suits-combos-lower-to-higher-rank-combos?
        suits [:king :ace])))

  (testing "if the ranks are equal, clubs beat spades"
    (all-results-true? (higher-card-wins-equal-ranks-combos-lower-to-higher-suit-combos?
        [:spade :club] ranks)))

  (testing "if the ranks are equal, diamonds beat clubs"
    (all-results-true? (higher-card-wins-equal-ranks-combos-lower-to-higher-suit-combos?
        [:club :diamond] ranks)))
  (testing "if the ranks are equal, hearts beat diamonds"))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (let [player1-cards [[:diamond 9] [:heart :king] [:diamond 8]]
          player2-cards [[:diamond :king] [:spade 6] [:club :jack]]]

      (is (and (= [[[:diamond :king] [:diamond 8] [:heart :king] [:diamond 9] [:club :jack] [:spade 6]]
                   []]
                  (play-game player1-cards player2-cards))
               (= [[]
                   [[:diamond :king] [:diamond 8] [:heart :king] [:diamond 9] [:club :jack] [:spade 6]]]
                  (play-game player2-cards player1-cards)))))))