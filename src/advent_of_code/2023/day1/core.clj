(ns aoc.one
  (:require [clojure.pprint :refer [pprint]]))


;; Advent Of Code, Day 1
;; https://adventofcode.com/2023/day/1
;;
;; Clojure Documentation
;; https://jafingerhut.github.io/cheatsheet/clojuredocs/cheatsheet-tiptip-no-cdocs-summary.html

(defn f [val]

  (->> val
       (map (partial re-seq #"\d"))
       (map (juxt first last))
       (map (partial apply str))
       (map #(Integer. %))
       (apply +)))

(comment


  (+ 1 2 4)
  (partial + 1 3)

  (str "1" "2")
  (apply str '("1" "2"))


  ;; A
  (let [val '("1abc2"
              "pqr3stu8vwx"
              "a1b2c3d4e5f"
              "treb7uchet")]
    (f val))


  ;; B
  (let [val (clojure.string/split-lines (slurp "input"))]

    (f val)))
