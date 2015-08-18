(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set]))

(def start-pos [[[:goose :fox :corn :you] [:boat] []]])

(defn river-crossing-plan []

  (let [seperate-queue-things-from-person
        (fn [v] (vec (butlast v)))

        right-bank-queue-safe
        (fn [v]
          (case (count v)
            2 (or (= v [:corn :goose]) (= v [:fox :corn]))
            1 (= v [:goose])))

        update-queues
        (fn [departure-bank-queue destination-bank-queue need-to-pop-queue?]
          (if (need-to-pop-queue? departure-bank-queue)
            {:popped-from-departure-queue (first departure-bank-queue)
             :updated-departure-queue (vec (rest departure-bank-queue))
             :updated-destination-queue (conj destination-bank-queue (first departure-bank-queue))
             }
            {:popped-from-departure-queue nil
             :updated-departure-queue departure-bank-queue
             :updated-destination-queue destination-bank-queue
             }))

        filter-nils-from-boat (fn [boat] (vec (filter #(not (nil? %)) boat)))

        travel-between-banks
        (fn [need-to-take-something? step]
          (let [[departure-bank _ destination-bank] step
                departure-queue (seperate-queue-things-from-person departure-bank)
                destination-queue destination-bank
                {:keys [popped-from-departure-queue updated-departure-queue updated-destination-queue]}
                (update-queues departure-queue destination-queue need-to-take-something?)]
            (conj [] [updated-departure-queue (filter-nils-from-boat [:boat popped-from-departure-queue :you]) destination-bank]
                  [updated-departure-queue [:boat] (conj updated-destination-queue :you)])))
        ]

    (loop [crossing-plan-so-far start-pos]
      (let [crossing-plan-after-right-trip
            (->> (last crossing-plan-so-far)
              (travel-between-banks (fn [_] true))
              (concat crossing-plan-so-far)
              vec)]

        (if (empty? (first (last crossing-plan-after-right-trip)))
          crossing-plan-after-right-trip
          (let [crossing-plan-after-left-trip
                (->> (last crossing-plan-after-right-trip)
                  reverse
                  (travel-between-banks (complement right-bank-queue-safe))
                  (map reverse)
                  (concat crossing-plan-after-right-trip)
                  vec)]

            (recur crossing-plan-after-left-trip)))))))


