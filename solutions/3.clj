(ns solutions.3
  (:require [clojure.string :as str]))

(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res []]
    (if (.find m)
      (recur
       m
       (conj res {:start (.start m)
                  :v     (.group m)}))
      res)))

(defn cell-neighbourhood [row col]
  (for [c (range (dec col) (+ 2 col))
        r (range (dec row) (+ 2 row))
        :when (not (or (neg? c)
                     (neg? r)
                     (= [r c] [row col])))]
    [r c]))

(defn word-neighbourhood [row-idx start len]
  (let [cells (map vector
                (repeat row-idx)
                (range start (+ start len)))]
    (->> (for [[row-idx col-idx] cells]
           (cell-neighbourhood row-idx col-idx))
      (apply concat)
      (remove (set cells))
      distinct)))

(defn symb? [c]
  (and c (not (#{\. \0 \1 \2 \3 \4 \5 \6 \7 \8 \9} c))))

(defn adjacent-symbol [rows {:keys [start v row-idx]}]
  (some #(symb? (get-in rows %))
    (word-neighbourhood row-idx start (count v))))

(defn adjacent-* [rows {:keys [start v row-idx] :as num-run}]
  (assoc num-run
    :gear-coord
    (->> (word-neighbourhood row-idx start (count v))
      (drop-while #(not= \* (get-in rows %)))
      first)))

(defn row->num-runs [row-idx row-str]
  (map #(assoc % :row-idx row-idx)
    (re-pos #"\d+" row-str)))

(let [lines (-> (slurp "aoc23/data/3.txt")
              (str/split #"\n"))]
  {:part1 (->> lines
            (map-indexed row->num-runs)
            (apply concat)
            (filter (partial adjacent-symbol lines))
            (map :v)
            (map parse-long)
            (reduce +))
   :part2 (->> lines
            (map-indexed row->num-runs)
            (apply concat)
            (map (partial adjacent-* lines))
            (filter :gear-coord)
            (group-by :gear-coord)
            (map second)
            (filter #(= 2 (count %)))
            (map #(reduce * (map (comp parse-long :v) %)))
            (reduce +))})
