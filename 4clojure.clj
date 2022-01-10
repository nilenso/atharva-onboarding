;; 4clojure solutions

;; https://4clojure.oxal.org/#/problem/21
(defn takenth [coll n] (first (drop n coll)))

;; https://4clojure.oxal.org/#/problem/22
#(reduce inc 0 %)

;; https://4clojure.oxal.org/#/problem/31
#(partition-by identity %)

;; https://4clojure.oxal.org/#/problem/33
(defn rep [s n]
  (mapcat #(repeat n %) s))

;; https://4clojure.oxal.org/#/problem/43
;; shorter, found in solutions: #(apply map list (partition %2 %1))
(defn reverse-interleave
  [coll step]
  (->> coll
       (iterate rest)
       (take step)
       (map #(take-nth step %))))

;; https://4clojure.oxal.org/#/problem/46
(defn flip [func]
  (fn [& args] (apply func (reverse args))))

;; https://4clojure.oxal.org/#/problem/50
(defn split-by-type [coll]
  (vals (group-by type coll)))

;; https://4clojure.oxal.org/#/problem/54
(defn part [step coll]
  (->> coll
       (iterate #(drop step %))
       (take (quot (count coll) step))
       (map #(take step %))))

;; same as above, but trying out transducers
;; this is slightly slower, why?
;; NOTE: also seems to fail on 4clojure only.
(defn part2 [step coll]
  (eduction
   (take (quot (count coll) step))
   (map #(take step %))
   (iterate #(drop step %) coll)))

;; https://4clojure.oxal.org/#/problem/55
(defn freqs [s]
  (apply (partial merge-with +) (map #(hash-map %1 1) s)))

;; https://4clojure.oxal.org/#/problem/56
;;
;; NOTE: this solution does not work on 4clojure,
;; but passes all the cases in a local clj repl
(defn ord-dist [coll]
  (into []
        (apply sorted-set-by
               #(< (.indexOf coll %1) (.indexOf coll %2)) coll)))

;; https://4clojure.oxal.org/#/problem/58
(defn comp [& fs]
  (fn [& args]
    (reduce #(%1 (apply %2 args)) fs)))

;; https://4clojure.oxal.org/#/problem/59
;; Juxt is just a comp, with map instead of reduce!
(defn juxt [& fs]
  (fn [& args]
    (map #(apply % args) fs)))
