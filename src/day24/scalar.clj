(ns day24.scalar
  (:require [cuerdas.core :as cs]))

(defn get-input []
  (slurp "data.txt"))

(defn machine [input]
  {:memory {:x 0 :y 0 :z 0 :w 0}
   :input input})

(def machine-ops {:mul *
                  :add +
                  :div (fn [a b] (int (/ a b)))
                  :mod mod
                  :eql (fn [a b] (if (= a b) 1 0))})

(defn exec [machine instr]
  (let [[a b c] instr
        get-val (fn [v]
                  (if (keyword? v)
                    (get-in machine [:memory v])
                    v))]
    (if
      (= :inp a)
      (-> machine
          (assoc-in [:memory b] (first (:input machine)))
          (update :input rest))
      (assoc-in machine [:memory b] ((machine-ops a) (get-val b) (get-val c))))))



(defn day-24 []
  (let [parse-instr (fn [s]
                      (mapv #(if (#{"add" "mul" "div" "mod" "eql" "inp" "x" "y" "z" "w"} %)
                               (keyword %)
                               (cs/parse-int %))
                            (cs/split s " ")))
        instrs (mapv parse-instr (cs/lines (get-input)))]
    (time (reduce exec (machine [4 1 1 7 7 1 8 7 1 4 5 8 9 1]) instrs))))