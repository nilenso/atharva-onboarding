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

;; same as above, but trying out transducers.
;; this is much slower, why?
;; ran with criterium's quickbench: (__ 3 (range 8))
;;
;; with threading:
;; Execution time mean : 48.871371 ns
;; Execution time std-deviation : 0.587785 ns
;; Execution time lower quantile : 48.168608 ns ( 2.5%)
;; Execution time upper quantile : 49.437318 ns (97.5%)
;; Overhead used : 6.262402 ns
;;
;; with transducers:
;; Execution time mean : 290.292177 ns
;; Execution time std-deviation : 6.409654 ns
;; Execution time lower quantile : 284.113340 ns ( 2.5%)
;; Execution time upper quantile : 300.254526 ns (97.5%)
;; Overhead used : 6.262402 ns
;;
;; Time not written here, but using `sequence` instead of
;; `eduction` was still faster, but much slower than plain
;; ol' threading
;;
;; NOTE: also seems to fail on 4clojure only.
(defn part2 [step coll]
  (eduction
   (take (quot (count coll) step))
   (map #(take step %))
   (iterate #(drop step %) coll)))

(defn part3 [step coll]
  (sequence
   (comp (take (quot (count coll) step))
         (map #(take step %)))
   (iterate #(drop step %) coll)))

(defn part4 [step coll]
  (into []
   (comp (take (quot (count coll) step))
         (map #(take step %)))
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
(defn komp [& fs]
  (fn [& args]
    (reduce #(%1 (apply %2 args)) fs)))

;; https://4clojure.oxal.org/#/problem/59
;; Juxt is just a comp, with map instead of reduce!
(defn juxt [& fs]
  (fn [& args]
    (map #(apply % args) fs)))

;; https://4clojure.oxal.org/#/problem/60
(defn reductionz [func acc coll]
  (let [n (count coll)]
    (->> coll
         (repeat (inc n))
         (map #(take %1 %2)
              (range (inc n)))
         (map #(reduce func acc %)))))

;; https://4clojure.oxal.org/#/problem/69
;; wew, this one took a while
;; is this easy to understand?
(defn murge-with [func & maps]
  (apply merge
         (for [k (distinct (mapcat keys maps))]
           {k (reduce func (filter identity (map k maps)))})))

;; https://4clojure.oxal.org/#/problem/70
(defn splort [line]
  (sort-by clojure.string/lower-case
           (clojure.string/split line #"\W")))

;; https://4clojure.oxal.org/#/problem/73
;; Tic Tac Toe Verifier.
;; Probably bad style to have internal functions
;; strewn in like this? I stuffed these in because
;; 4clojure likes everything in one function.
(defn get-winner [board]
  (letfn [(all-three [vec]
            (condp = (distinct vec)
              [:x] :x
              [:o] :o
              nil))
          (rows [b] b)
          (columns [b]
            (for [pos [first second last]]
              (map pos board)))
          (diagonals [b]
            [[(-> b first first) (-> b second second) (-> b last last)]
             [(-> b first last)  (-> b second second) (-> b last first)]])
          (solution-along [axis]
            (some->> (axis board)
                     (map all-three)
                     (reduce #(or %1 %2))))]
    (some solution-along [rows columns diagonals])))
