(ns aoc.two
  (:require [clojure.string :as str]))


;; (def foo :a)
#_(def compare {:red 20
                :blue 15
                :green 13})

;; What is the best Clojure tutorial you can think of?
;; https://www.reddit.com/r/Clojure/comments/v321vj/what_is_the_best_clojure_tutorial_you_can_think_of/
;;
;; Clojure for the Brave and True
;; https://www.braveclojure.com/
;;
;; Clojure Destructuring
;; https://clojure.org/guides/destructuring
;; https://gist.github.com/john2x/e1dca953548bfdfb9844

(def games "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")

(defn ppi [v]
  (clojure.pprint/pprint v)
  v)

(defn format-score
  "Algorithm
   . trim whitespace
   . split k/v by space
   . reverse number & string
   . make keywork k, number v"
  [s]

  (let [f (fn [[l r]]
            (hash-map (keyword l)
                      (Integer. r)))]

    (-> (str/trim s)
        (str/split #" ")
        reverse
        f)))

;; NOTE Expected output
;; '(("Game 1" ({:red 4, :blue 3}
;;                {:green 2, :red 1, :blue 6}
;;                {:green 2}))
;;
;;     ("Game 2" ({:green 2, :blue 1}
;;                {:green 3, :red 1, :blue 4}
;;                {:green 1, :blue 1}))
;;
;;     ("Game 3" ({:green 8, :red 20, :blue 6}
;;                {:green 13, :red 4, :blue 5}
;;                {:green 5, :red 1}))
;;
;;     ("Game 4" ({:green 1, :red 3, :blue 6}
;;                {:green 3, :red 6}
;;                {:green 3, :red 14, :blue 15}))
;;
;;     ("Game 5" ({:green 3, :red 6, :blue 1}
;;                {:green 2, :red 1, :blue 2})))
(defn parse-rounds [rounds]

  (let [f (fn [as]
            (str/split as #"\:"))

        g (fn [[k v]]

            (let [ks (str/split v #"\;")
                  lf #(str/split %  #"\,")]

              (list k
                    (map lf ks))))

        h (fn [[k lss]]

            (list k
                  (map (fn [as]

                         (apply merge
                                (map #(format-score %) as)))
                       lss)))]

    (->> (clojure.string/split-lines rounds)
         (map f)
         (map g)
         (map h))))

;; NOTE expected output
;; '(("Game 1" {:green 4, :red 5, :blue 9})
;;   ("Game 2" {:green 6, :red 1, :blue 6})
;;   ("Game 3" {:green 26, :red 25, :blue 11})
;;   ("Game 4" {:green 7, :red 23, :blue 21})
;;   ("Game 5" {:green 5, :red 7, :blue 3}))
(defn aggregate-cube-colours [games]

  (map (fn [[k v]]
         (list k (apply merge-with + v)))
       games))

(defn game-matches-criteria? [aggregated-cube-colours]

  ;; NOTE expected output
  ;; '(("Game 1" true)
  ;;   ("Game 2" true)
  ;;   ("Game 3" false)
  ;;   ("Game 4" false)
  ;;   ("Game 5" true))
  (map (fn [[k v]]

         ;; NOTE example "v"
         ;; {:green 4, :red 5, :blue 9}
         (let [rgb-values ((juxt :red :green :blue) v)
               rgb-thresholds (vals {:red 12 :green 13 :blue 14})

               ;; NOTE expected output
               ;; '(("Game 1" (true true true))
               ;;   ("Game 2" (true true true))
               ;;   ("Game 3" (false false true))
               ;;   ("Game 4" (false true false))
               ;;   ("Game 5" (true true true)))
               matching-thresholds (map (fn [a b] (<= b a))
                                        rgb-thresholds
                                        rgb-values)]

           (list k (every? true? matching-thresholds))))
       aggregated-cube-colours))

(comment

  (-> (parse-rounds games)
      aggregate-cube-colours
      game-matches-criteria?
      ppi)

  ;; A
  '(("Game 1" ({:red 4, :blue 3}
               {:green 2, :red 1, :blue 6}
               {:green 2}))

    ("Game 2" ({:green 2, :blue 1}
               {:green 3, :red 1, :blue 4}
               {:green 1, :blue 1}))

    ("Game 3" ({:green 8, :red 20, :blue 6}
               {:green 13, :red 4, :blue 5}
               {:green 5, :red 1}))

    ("Game 4" ({:green 1, :red 3, :blue 6}
               {:green 3, :red 6}
               {:green 3, :red 14, :blue 15}))

    ("Game 5" ({:green 3, :red 6, :blue 1}
               {:green 2, :red 1, :blue 2})))

  ;; A
  ;; Game 1:
  ;;   3 blue, 4 red;
  ;;   1 red, 2 green, 6 blue;
  ;;   2 green

  ;; Game 1:
  ;;   9 blue
  ;;   5 red
  ;;   4 green

  ;; # Possible solution
  ;; Regex
  ;; List Comprehension
  ;; Set Comparison
  ;; => Merge-with


  (def foo [["Game 5"
             [[" 6 red" " 1 blue" " 3 green"]
              [" 2 blue" " 1 red" " 2 green"]]]])

  ;; ["Game 5"
  ;;  [[" 6 red" " 1 blue" " 3 green"]
  ;;   [" 2 blue" " 1 red" " 2 green"]]]
  (ppi (map (fn [[k lss]]

              (list k
                    (map (fn [as]

                           (apply merge
                                  (map #(format-score %) as)))

                         lss)))
            foo)))

(comment


  ((juxt :red :green :blue) {:green 4 :red 5 :blue 9})

  '(12 13 14)
  [5 4 9]

  ;; A
  (map (fn [a b]
         [(<= b a) a b])
         '(12 13 14)
           [5 4  9])

  ;; 12 red
  ;; 13 green
  ;; 14 blue

)

(comment  ;; 4ever-clojure, Problem 29, Get the Caps

  (def afn
    (fn [s]

      (apply str (re-seq #"\p{Upper}" s))

      #_(->> (re-seq #"\p{Upper}" s)
             (apply str))

      #_(as-> s foo
        (re-seq #"\p{Upper}" foo)
        (apply str foo)
        ('bar foo '123))))

  (fn [s]
      (apply str (re-seq #"\p{Upper}" s)))


  ;; Looping over list
  (reduce (fn [acc i]

            (ppi (str acc i)))
          "ZZ"
          '("H" "L" "O" "W" "R" "D"))

  (reduce (fn [acc i]

            (ppi (str acc i)))
          '("H" "L" "O" "W" "R" "D"))


  ;; Applying fn to 'n' argument
  (str "a" "b" "c" "ZZTop")
  (+ 1 56 6 7 789)

  '("H" "L" "O" "W" "R" "D")
  (str "H" "L" "O" "W" "R" "D")
  (apply str '("H" "L" "O" "W" "R" "D"))



  (re-seq #"\p{Upper}" "HeLlO, WoRlD!") ;; ("H" "L" "O" "W" "R" "D")
  (re-seq #"[A-Z]" "HeLlO, WoRlD!")
  (re-seq #"[A-Z]" "nothing")
  (re-seq #"[A-Z]" "$#A(*&987Zf")

  (afn "HeLlO, WoRlD!")
  (afn "$#A(*&987Zf")
  (afn "nothing")


  (= (
      (fn [s]
        (apply str (re-seq #"\p{Upper}" s)))

      "HeLlO, WoRlD!"
      )
     "HLOWRD")

  )
