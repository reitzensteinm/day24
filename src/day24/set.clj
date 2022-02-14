(ns day24.set
  (:require [cuerdas.core :as cs]
            [com.climate.claypoole :as cp]))

(def tp (cp/threadpool (cp/ncpus)))

(defn get-input [d]
  (slurp "data.txt"))

(defn machine [input]
  {:memory {:x #{0} :y #{0} :z #{0} :w #{0}}
   :input input})

(def machine-ops {:mul *
                  :add +
                  :div (fn [a b] (int (/ a b)))
                  :mod mod
                  :eql (fn [a b] (if (= a b) 1 0))})

(defn cartesian-op [op va vb]
  (into #{}
        (for [a (if (int? va) #{va} va)
              b (if (int? vb) #{vb} vb)]
          (op a b))))

(defn advance [machine instr]
  (let [[a b c] instr
        get-val (fn [v]
                  (if (keyword? v)
                    (get-in machine [:memory v])
                    #{v}))]
    (if
      (= :inp a)
      (-> machine
          (assoc-in [:memory b] (first (:input machine)))
          (update :input rest))
      (assoc-in machine [:memory b]
                (cartesian-op (machine-ops a) (get-val b) (get-val c))))))

(defn search [instrs fixed free]
  (apply concat
         (filter identity
                 (for [i (first free)]
                   (let [inp (concat fixed [#{i}] (rest free))
                         m (reduce advance (machine inp) instrs)
                         mem (get-in m [:memory :z])]
                     (when (contains? mem 0)
                       (if (empty? (rest free))
                         [inp]
                         (search instrs
                                 (conj fixed #{i}) (rest free)))))))))

(defn run-test [instrs]
  (apply concat
         (filter identity
                 (cp/pfor tp [a (range 1 10)
                              b (range 1 10)
                              c (range 1 10)]
                          (search instrs [#{a} #{b} #{c}] (into [] (repeat 11 (into #{} (range 1 10)))))))))

(defn day-24 []
  (let [parse-instr (fn [s]
                      (mapv #(if (#{"add" "mul" "div" "mod" "eql" "inp" "x" "y" "z" "w"} %)
                               (keyword %)
                               (cs/parse-int %))
                            (cs/split s " ")))
        instrs (mapv parse-instr (cs/lines (get-input 24)))
        human-num #(apply str (map first %))]
    (time
      (mapv human-num
            (run-test instrs)))))