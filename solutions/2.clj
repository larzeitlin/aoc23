(ns solutions.2
  (:require [clojure.string :as str]))

(defn parse-count [s]
  (let [[cnt color] (str/split s #" ")]
    [(keyword color) (parse-long cnt)]))

(defn parse-hand [s]
  (->> (str/split s #", ")
    (map parse-count)
    (into {})))

(defn game-maxes [s]
  (let [[game-name-str counts-str] (str/split s #": ")
        max-counts (-> counts-str
                     (str/split #"; ")
                     (->> (map parse-hand)
                       (apply merge-with max)))
        game-id (-> game-name-str (str/split #" ")
                  second
                  parse-long)]
    [game-id max-counts]))

(def thresholds {:red 12 :green 13 :blue 14})

(defn game-within-threshold? [[_game-id maxes]]
  (->> (merge-with >= thresholds maxes)
    vals
    (every? true?)))

(defn power-val [[_game-id maxes]]
  (->> maxes
    vals
    (reduce *)))

(let [lines (-> (slurp "aoc23/data/2.txt")
              (str/split #"\n"))]
  {:part1 (->>
            lines
            (map game-maxes)
            (filter game-within-threshold?)
            (map first)
            (reduce +))
   :part2 (->>
            lines
            (map game-maxes)
            (map power-val)
            (reduce +))})
