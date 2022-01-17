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

;; second attempt, trying to do with less stdlib
(defn merge-wiff [func & maps]
  (reduce (fn [m1 m2]
            (reduce (fn [m [k2 v2]]
                      (prn m :and k2 v2)
                      (if (m k2)
                        (assoc m k2 (func (m k2) v2))
                        (assoc m k2 v2)))
                    m1 m2))
          maps))

;; https://4clojure.oxal.org/#/problem/70
(defn splort [line]
  (sort-by clojure.string/lower-case
           (clojure.string/split line #"\W")))

;; https://4clojure.oxal.org/#/problem/73
;; Tic Tac Toe Verifier.
;; Probably bad style to have internal functions strewn in like this?
;; I stuffed these in because 4clojure likes everything in one function.
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

;; https://4clojure.oxal.org/#/problem/74
;; Nerd snipe territory. This solution won't work for large numbers becoz
;; floats. But I will spare myself from implementing this:
;; https://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer/18686659#18686659
;;
;; Also clojurescript interop is annoying me. Trust me (should you?!), this
;; works on REPL.
(defn str-squares-only [csvline]
  (letfn [(is-square? [num]
            (let [root (Math/sqrt num)]
              (== num (* root root))))]
    (as-> csvline x
      (clojure.string/split x #",")
      (map #(Integer/parseInt %) x)
      (filter is-square? x)
      (clojure.string/join "," x))))

;; https://4clojure.oxal.org/#/problem/77
(defn group-anagrams [words]
  (->> words
       (group-by frequencies)
       vals
       (filter #(> (count %) 1))
       (map set)
       set))

;; https://4clojure.oxal.org/#/problem/79
;; I couldn't solve this one without resorting to an imperative spaghetti. I
;; peeked into the solution and this was the only "stateless" one, ie, not a
;; loop-recur tangle or unreadable. Even though it's concise, nested maps in a
;; reduce is hard to reason about without trying it out in the REPL. Once I did
;; that, it made sense.
;;
;; Unlike tic-tac-toe, this hard-level problem is one that feels *easier* to
;; think about imperatively. Easy != Simple?
(defn shortest-path [triangle]
  (first
   (reduce (fn [cur nex]
             (map + (map min cur (rest cur)) nex))
           (reverse triangle))))

;; https://4clojure.oxal.org/#/problem/82
;;
;; No, I didn't come up with this without assistance.
;;
;; This is inspired by one of the solutions that more cleanly (and successfully)
;; did what I was trying. My initial solution was an unreadable mess of nested
;; maps and filters, which was easily replaceable by a for comprehension (I was
;; avoiding it because it "looked imperative"). Plus, destructuring exists,
;; which I should remember to use more often.
(defn transitive-closure [the-set]
  (letfn [(transitives [s]
            (for [[a b] s
                  [c d] s
                  :when (= c b)]
              [a d]))
          (merge-transitives [s]
            (let [new-s (into s (transitives s))]
              (when (not= s new-s) new-s)))]
    (->> the-set
         (iterate merge-transitives)
         (take-while identity)
         last)))

;; https://4clojure.oxal.org/#/problem/86
;; Also not cljs friendly, so won't run on 4clojure
(defn happy-number-meh [number]
  (letfn [(digits [n]
            (map #(Character/digit % 10) (str n)))
          (square [n] (* n n))
          (digit-squares-sum [n]
            (reduce + (map square (digits n))))]
    (true? (reduce (fn [t1 [i sum2]]
                     (if (= sum2 1) (reduced true) [i sum2]))
                   (map vector
                        (range 100)
                        ;; tolerance value: how many iterations till we give up trying?
                        (iterate digit-squares-sum number))))))

;; better?
(defn happy-number? [number]
  (let [digits (fn [n]
                 (map #(Character/digit % 10) (str n)))
        square (fn [n] (* n n))
        digit-squares-sum (fn [n]
                            (reduce + (map square (digits n))))
        exceeded-tolerance? (fn [tolerance n]
                              (when (not= (count n) tolerance)
                                true))
        tolerance 100]
    (->> (iterate digit-squares-sum number)
         (map vector (range tolerance))
         (take-while (fn [[i sum]] (not= 1 sum)))
         (exceeded-tolerance? tolerance))))

;; https://4clojure.oxal.org/#/problem/92
;; First hard problem I solved all on my own :D
(defn parse-roman-numeral [roman]
  (let [table {:M 1000, :D 500, :C 100, :L 50, :X 10, :V 5, :I 1}
        lookup (fn [key] (key table))
        char->keyword (fn [ch] (keyword (str ch)))
        resolve-pairs (fn [[n1 n2]]
                        (cond
                          (= [n1 n2] [n1 nil]) n1
                          (> n1 n2) (- n1 n2)
                          :else (+ n1 n2)))]
    (->> roman
         (map (comp lookup char->keyword))
         reverse
         (partition-all 2)
         (map resolve-pairs)
         (apply +))))

;; Same problem, refined a bit more
(defn parse-roman-numeral-v2 [roman]
  (let [table {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}
        lookup (fn [key] (get table key))
        split-into-pairs (fn [coll]
                           (cond
                             (even? (count coll)) (partition 2 coll)
                             :else (as-> coll c
                                     (rest c) (partition 2 c)
                                     (conj c (take 1 coll)))))
        resolve-pairs (fn [[n1 n2]]
                        (cond
                          (= [n1 n2] [n1 nil]) n1
                          (< n1 n2) (- n2 n1)
                          :else (+ n2 n1)))]
    (->> roman
         (map lookup)
         split-into-pairs
         (map resolve-pairs)
         (apply +))))

;; https://4clojure.oxal.org/#/problem/91
;;
;; FIXME: doesn't work.
;; A better solution than walking the graph and seeing if visited nodes
;; are the same as set of edges is to check Euler's Theorem instead, ie:
;; A connected graph has an euler cycle iff every vertex has even degree.
;;
;; maybe something like: (->> edges flatten frequencies vals filter-even even?)
(defn fully-connected-graph? [edges]
  (let [connected? (fn [[a b] [x y]]
                     (or (= a x) (= a y) (= b x) (= b y)))
        neighbours (fn [curr-edge visited]
                     #_(dbg curr-edge)
                     #_(dbg edges)
                     (filter (fn [edge]
                               (and (not= curr-edge edge)
                                    (not (visited edge))
                                    (connected? curr-edge edge)))
                             edges))
        visited #{}
        walk (fn walk [edge visited]
               (println "---")
               (print "for ") (dbg edge)
               (dbg visited)
               (dbg (neighbours edge visited))
               (cond
                 (empty? (neighbours edge visited)) (= visited (set edges))
                 :else (some true? (map (fn [next-edge]
                                          (walk next-edge (conj visited next-edge)))
                                        (neighbours edge visited)))))]
    (walk (first edges) (conj visited (ffirst edges)))))

(comment
  "test cases and scratch area"

  (defn dbg [exp] (prn exp) exp)

  [(= true (fully-connected-graph? [[:a :b]]))
   (= false (fully-connected-graph? [[:a :a] [:b :b]]))
   (= false (fully-connected-graph? [[:a :b] [:a :b] [:a :c] [:c :a]
                                     [:a :d] [:b :d] [:c :d]]))
   (= true (fully-connected-graph? [[1 2] [2 3] [3 4] [4 1]]))
   (= true (fully-connected-graph? [[2 3] [1 2] [4 3] [5 2] [5 4]]))
   (= true (fully-connected-graph? [[:a :b] [:a :c] [:c :b] [:a :e]
                                    [:b :e] [:a :d] [:b :d] [:c :e]
                                    [:d :e] [:c :f] [:d :f]]))
   (= false (fully-connected-graph? [[1 2] [2 3] [2 4] [2 5]]))]

  [(parse-roman-numeral "XIV")
   :is 14
   (parse-roman-numeral "XIX")
   :is 19
   (parse-roman-numeral "DCCCXXVII")
   :is 827
   (parse-roman-numeral "MMMCMXCIX")
   :is 3999
   (parse-roman-numeral "XLVIII")
   :is 48]

  [(parse-roman-numeral-v2 "XIV")
   :is 14
   (parse-roman-numeral-v2 "XIX")
   :is 19
   (parse-roman-numeral-v2 "DCCCXXVII")
   :is 827
   (parse-roman-numeral-v2 "MMMCMXCIX")
   :is 3999
   (parse-roman-numeral-v2 "XLVIII")
   :is 48]

  [(happy-number? 7)
   (happy-number? 986543210)
   (happy-number? 2)
   (happy-number? 3)])
