(ns genetic-change-calculator.core
  (:require [darwin.core :refer :all]
            [genetic-change-calculator.naive :refer :all]))


(defn total-money [coinset individual]
  (reduce + (map * coinset individual)))

(defn fitness [x coinset]
  (fn [individual] (+ (Math/abs (- x (* 1000000000 (total-money coinset individual))))
                     (reduce + individual))))

(defn rand-elem [coll]
  (int (Math/floor (rand (count coll)))))

(defn mutation [coll]
  (let [mutating-gene (rand-elem coll)]
    (concat (take mutating-gene coll)
            [((if (> 0.5 (rand)) inc dec) (nth coll mutating-gene))]
            (nthrest coll (inc mutating-gene)))))

(defn crossover [x1 x2]
  (let [n (rand-elem x1)]
    [(concat (take n x1) (nthrest x2 n))
     (concat (take n x2) (nthrest x1 n))]))

(defn average [coll]
  (/ (reduce + coll)
     (count coll)))

(defn initialization-aproximation [x coins]
  (* 1 (/ x (average coins))))

(defn random-individual [x coins]
  (let [ceiling (initialization-aproximation x coins)]
    (fn [] (repeatedly (count coins) (fn [] (int (Math/floor (rand ceiling)))) ))))

(defn evolve-coins [x coins gens]
  (evolve (fitness x coins) crossover mutation (random-individual x coins) gens))

(defn best-result [result]
  (first (sort-by :score (:rabble result))))

(defn solve-gen
  ([x coins gens]
   (let [r (:genotype (best-result (evolve-coins x coins gens)))]
     (if (= x (total-money coins r))
       (zipmap coins r)
       (throw (Exception. (str "Could not find a precise answer.\n"
                               "Maybe try with more generations?\n\n"
                               "Best result so far: " (zipmap coins r)))))))
  ([x coins] (solve-gen x coins 500)))
;(def r (evolve-coins 300 [2 5 8 7 6]))
