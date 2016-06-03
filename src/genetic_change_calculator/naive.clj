(ns genetic-change-calculator.naive
  (:require [clojure.math.combinatorics :refer :all]))

(defn how-many
  "Integer division"
  [x y]
  (int (Math/floor (/ x y))))

(defn changer-top
  "Amounts for each coin"
  [x coinset]
  (if (empty? coinset)
    (throw (Exception. "Cannot give complete change with this coinset")))
  (loop [remaining-coins (reverse (sort coinset))
         x x
         new-coins {}]
    (if (empty? remaining-coins)
      (if (zero? x)
        new-coins
        (throw (Exception. (str "Cannot give complete change with this coinset\n"
                                 "This is the closest aproximation:\n"
                               {:remainder x
                                :coins new-coins})))
        )
      (let [curr-coin (first remaining-coins)
            of-this-coin (how-many x curr-coin)]
        (recur (rest remaining-coins)
               (mod x curr-coin)
               (assoc new-coins curr-coin of-this-coin))))))

(defn relevant-subsets [coll]
  (remove #(or (= 0 (count %))
               (= coll %))
          (subsets coll)))

(defn changer-all-subsets [x coinset]
  (remove nil? (map #(try (changer-top x %)
                          (catch Exception e))
                    (relevant-subsets coinset))))

(defn reduce-change
  "Given a change map, get the total
  4 validation purposes"
  [change]
  (reduce + (map (partial apply *) change)))

(defn count-coins
  [change]
  (apply + (vals change)))

(defn changer
  [x coinset]
  (try (changer-top x coinset)
       (catch Exception e (first (sort-by count-coins (changer-all-subsets x coinset))))))
