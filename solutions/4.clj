(ns solutions.4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn add-score [{:keys [found winning] :as card}]
  (let [matches (count (set/intersection (set found)
                                         (set winning)))]
    (assoc card
           :matches matches
           :score (if (zero? matches)
                    0
                    (->> matches dec (Math/pow 2))))))

(defn parse-card-strings [card-string]
  (->> card-string
    (re-matches #"Card\s+(\d+): ([\d\s]+)\|([\d\s]+)")
    rest
    (map (comp
           #(remove empty? %)
           #(str/split % #" ")))
    (zipmap [:card-id :found :winning])
    add-score
    (#(update % :card-id (comp parse-long first)))))

(defn card-count [cards]
  (loop [rem-cards cards
         result 0
         multis {}]
    (let [[{:keys [card-id matches] :as card} & rest-cards] rem-cards
          this-card-multi (inc (get multis card-id 0))]
      (if card
        (recur
          rest-cards
          (+ result this-card-multi)
          (->> (repeat this-card-multi)
            (zipmap (range (inc card-id)
                      (+ matches (inc card-id))))
            (merge-with + multis)))
        result))))

(let [lines (-> (slurp "aoc/data/4.txt")
                (str/split #"\n"))]
    {:part1 (->> lines
              (map parse-card-strings)
              (map :score)
              (reduce +))
     :part2 (->> lines
              (map parse-card-strings)
              card-count)})
