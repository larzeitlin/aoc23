(ns solutions.1
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(def name->num-part-1
  (into {} (map (fn [n] [(str n) n]) (range 10))))

(def name->num-part-2
  (into {} (map (fn [n] [(pp/cl-format nil "~r" n) n]) (range 10))))

(defn starts-with-num [num-map]
  (fn [s]
    (loop [ss s]
      (or (some (fn [[k v]]
                  (when (str/starts-with? ss k)
                    v))
            num-map)
        (recur (subs ss 1))))))

(defn ends-with-num [num-map]
  (fn [s]
    (loop [ss s]
      (or (some (fn [[k v]]
                  (when (str/ends-with? ss k)
                    v))
            num-map)
        (recur (subs ss 0 (dec (count ss))))))))

(defn cal-val [num-map]
  (fn [input]
    (->> input
      ((juxt
         (starts-with-num num-map)
         (ends-with-num num-map)))
      (apply str)
      read-string)))

(let [lines (-> "aoc23/data/1.txt"
              slurp
              (str/split #"\n"))]
  {:part1 (-> lines
             (->> (map (cal-val name->num-part-1))
               (reduce +)))
   :part2 (-> lines
             (->> (map (cal-val (merge name->num-part-1 name->num-part-2)))
               (reduce +)))})
