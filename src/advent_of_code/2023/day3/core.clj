(ns advent-of-code.2023.day3.core
  (:require [clojure.string :refer [split-lines trim]]))


(defn ppi [v]
  (clojure.pprint/pprint v)
  v)

(def input "467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..")

;; => If you can add up all the part numbers in the engine schematic
;; => any number adjacent to a symbol... even diagonally, is a "part number"

; NOTE Target Data structure
;; Grid
;; Number Overlay (spans multiple columns)
;;   position (row, column), dimensions (width)
;; All grid points surrounding number overlay

;; NOTE Options
;; i. 467..114
;;    ...*....
;; ii. For each artifact record, position, dimensions

(defn ->grid-dimensions [rows]
  (hash-map :width (count (first rows))
            :height (count rows)))

(comment

  ;; A
  (let [input-lines (->> (split-lines input)
                         (map trim)
                         ppi)
        grid-dimensions (ppi (->grid-dimensions input-lines))]

    grid-dimensions)

  ;; B So far just dealing with a single line
  ;; ["4" "6" "7" "." "." "1" "1" "4" "." "."]
  (->> (clojure.string/split "467..114.." #"")
       (map-indexed (fn [i v]
                      {:i i
                       :v v
                       ;; :? (re-matches #"\d" v)
                       }))
       (partition 2 1)
       (map (fn [#_[{lv :v :as l}
                    {rv :v :as r}]
                 [l r :as ls]]

              ;; 'ls' input will be in this shape
              ;; ({:i 0, :v "4"} {:i 1, :v "6"})

              #_{:l l
                 :r r
                 :joined? (every? #(re-matches #"\d" %) [lv rv])}
              {:l l
               :r r
               :joined? (every? #(re-matches #"\d" (:v %))
                                ls)}))
       ppi)


  (ppi (partition 3 ["4" "6" "7" "." "." "1" "1" "4" "." "."]))
  (ppi (partition 2 ["4" "6" "7" "." "." "1" "1" "4" "." "."]))
  (ppi (partition 2 1 ["4" "6" "7" "." "." "1" "1" "4" "." "."]))

  (split-with true? [true true false false true false])
  (split-with false? '(false false true false))

  (split-by true? [true true false false true false]) '((true true) (false false true) (false))
  ;; (group-by true? [true true false false true false])
  ;; (take-while true? [true true false false true false])

  )

(defn split-by [pred coll]

  (lazy-seq

   (when-let [s (seq coll)]

      (let [[xs ys] (split-with pred s)]

        (when (seq xs)

          (cons xs (split-by (complement pred) ys)))))))

(comment

  ;; split-by Input
  (def one
    '({:l {:i 0 :v "4"} :r {:i 1 :v "6"} :joined? true}
      {:l {:i 1 :v "6"} :r {:i 2 :v "7"} :joined? true}
      {:l {:i 2 :v "7"} :r {:i 3 :v "."} :joined? false}
      {:l {:i 3 :v "."} :r {:i 4 :v "."} :joined? false}
      {:l {:i 4 :v "."} :r {:i 5 :v "1"} :joined? false}
      {:l {:i 5 :v "1"} :r {:i 6 :v "1"} :joined? true}
      {:l {:i 6 :v "1"} :r {:i 7 :v "4"} :joined? true}
      {:l {:i 7 :v "4"} :r {:i 8 :v "."} :joined? false}
      {:l {:i 8 :v "."} :r {:i 9 :v "."} :joined? false}))


  (ppi (take 5 (split-by #(true? (:joined? %)) one)))

  ;; split-by output
  (({:l {:i 0, :v "4"}, :r {:i 1, :v "6"}, :joined? true}
    {:l {:i 1, :v "6"}, :r {:i 2, :v "7"}, :joined? true})

   ({:l {:i 2, :v "7"}, :r {:i 3, :v "."}, :joined? false}
    {:l {:i 3, :v "."}, :r {:i 4, :v "."}, :joined? false}
    {:l {:i 4, :v "."}, :r {:i 5, :v "1"}, :joined? false})

   ({:l {:i 5, :v "1"}, :r {:i 6, :v "1"}, :joined? true}
    {:l {:i 6, :v "1"}, :r {:i 7, :v "4"}, :joined? true})

   ({:l {:i 7, :v "4"}, :r {:i 8, :v "."}, :joined? false}
    {:l {:i 8, :v "."}, :r {:i 9, :v "."}, :joined? false})
   )

  )
