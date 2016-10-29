(ns diff-eq.data
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

(declare diff)
(declare num-differences)
(def ^:private third (comp first next next))

(defn diff-atom
  [a b {:keys [eq-marker ne-marker] :as options}]
  (if (= a b)
    [ne-marker ne-marker a]
    [a b eq-marker]))

(defn ^:private count-occurrences [needle-fn tree]
  (let [counter (atom 0)]
    (walk/prewalk (fn [x]
                 (when (needle-fn x)
                   (swap! counter inc))
                 x)
               tree)
    @counter))

(defn ^:private minimal-diff
  "Returns the smallest diff of x to one of ys."
  [x ys options]
  (let [number-of-differences last
        diff-of-x (comp first first first)]
    (or
     (diff-of-x
      (sort-by number-of-differences
               (map (juxt #(diff x % options) #(num-differences x %)) ys)))
     ;; otherwise we found no minimal diff - just show the entire value
     x)))

(defn ^:private diff-set [a b {:keys [eq-marker] :as options}]
  (let [a (set a)
        b (set b)
        a-only (not-empty (set/difference a b))
        b-only (not-empty (set/difference b a))

        use-marker-if-equal (fn use-marker-if-equal [value]
                              (let [value (set value)]
                                (if (and (or a-only b-only) (empty? value))
                                  eq-marker
                                  value)))]
    [(use-marker-if-equal (map #(minimal-diff % b-only options) a-only))
     (use-marker-if-equal (map #(minimal-diff % a-only options) b-only))
     (not-empty (set/intersection a b))]))

(defn diff-sequential [a b {:keys [eq-marker ne-marker] :as options}]
  (let [use-marker-if-equal (fn use-marker-if-equal [value marker]
                              (cond
                                (empty? value) marker
                                (every? #{marker} value) marker
                                :else value))

        remove-marker ::marker
        padding (- (max (count a) (count b))
                   (min (count a) (count b)))
        a (lazy-cat a (repeat padding remove-marker))
        b (lazy-cat b (repeat padding remove-marker))
        diffs (map #(diff %1 %2 options) a b)
        eq (map third diffs)
        equal-elements (map = a b)
        a' (->> (map #(if %2 eq-marker %1)
                     (map first diffs)
                     equal-elements)
                (filter (complement #{remove-marker})))
        b' (->> (map #(if %2 eq-marker %1)
                     (map second diffs)
                     equal-elements)
                (filter (complement #{remove-marker})))
        eq' (map #(if %2 %1 ne-marker)
                 eq
                 equal-elements)]
    [(use-marker-if-equal (not-empty (vec a')) ne-marker)
     (use-marker-if-equal (not-empty (vec b')) ne-marker)
     (not-empty (vec eq'))]))

(defn ^:private diff-associative-key [a b key options]
  (let [a-value (get a key)
        b-value (get b key)
        [a' b' eq] (diff a-value b-value options)
        in-a (contains? a key)
        in-b (contains? b key)
        same (and in-a in-b
                  (or (not (nil? eq))
                      (and (nil? a-value) (nil? b-value))))]
    [(when (and in-a (or (not (nil? a')) (not same))) {key a'})
     (when (and in-b (or (not (nil? b')) (not same))) {key b'})
     (when same {key eq})]))

(defn ^:private diff-associative [a b keys {:keys [eq-marker ne-marker] :as options}]
  (vec (reduce
        (fn [result key]
          (map merge
               result
               (diff-associative-key a b key options)))
        [nil nil nil] keys)))

(defn with-printer [[a b e] f]
  [(if (instance? clojure.lang.IObj a)
     (with-meta a {:diff-eq/printer f})
     a)
   (if (instance? clojure.lang.IObj b)
     (with-meta b {:diff-eq/printer f})
     b)
   (if (instance? clojure.lang.IObj e)
     (with-meta e {:diff-eq/printer f})
     e)])

(defn diff-strings-by-line-pretty-print [lines]
  (doseq [[line-offset line] (map-indexed vector lines)]
    (when (string? line)
      (println (format "Line %d: %s" (inc line-offset) (pr-str line))))))

(defn diff-strings-by-line [a b options]
  (let [a-lines (.split a "\n")
        b-lines (.split b "\n")]
    (with-printer
      (if (or (= 1 (count a-lines))
              (= 1 (count b-lines)))
        (diff-atom a b options)
        (diff-sequential a-lines b-lines options))
      diff-strings-by-line-pretty-print)))

(defprotocol Diffable
  (diff-partition-key [_ opt])
  (diff-similar [a b opt]))

(extend-protocol Diffable
  Object
  (diff-partition-key [_ _] :atom)
  (diff-similar [a b opt] (diff-atom a b opt))

  nil
  (diff-partition-key [_ _] :atom)
  (diff-similar [a b opt] (diff-atom a b opt))

  java.lang.String
  (diff-partition-key [_ {:keys [diff-strings?]}]
    (if diff-strings? :string :atom))
  (diff-similar [a b {:keys [diff-strings?] :as opt}]
    (if diff-strings?
      (diff-strings-by-line a b opt)
      (diff-atom a b opt)))

  java.util.Set
  (diff-partition-key [_ _] :set)
  (diff-similar [a b opt] (diff-set a b opt))

  java.util.List
  (diff-partition-key [_ _] :sequential)
  (diff-similar [a b opt] (diff-sequential a b opt))

  java.util.Map
  (diff-partition-key [_ _] :map)
  (diff-similar [a b opt] (diff-associative a b (set/union (keys a) (keys b)) opt)))

(defn diff
  "Similar to clojure.data/diff, but can accept optional arguments
  to annotate equal elements in a sequence.

  Like clojure.data/diff, this also returns a triplet value:

    [in-a in-b in-both]

  Where maps and sequences are the only diffable containers in this point in time.

  The third argument is a map of options:
    :eq-marker - The value to use for sequences when elements are equal.
    :ne-marker - The value to use for sequences when elements are not equal.
    :diff-strings? - Whether or not strings should be diffed by line. Defaults to false.

  Examples:

    (diff [1 nil nil] [nil nil 1] {:ne-marker '!= eq-marker '=})
    ;; => [[1 = nil] [nil = 1] [!= nil !=]]

  Diff for other types can be added by extending Diffable to it."
  ([a b] (diff a b {}))
  ([a b {:keys [eq-marker ne-marker diff-strings?]
         :or {diff-strings? false}
         :as options}]
   (let [options (assoc options :diff-strings? diff-strings?)]
     (if (= a b)
       [nil nil a]
       (if (= (diff-partition-key a options) (diff-partition-key b options))
         (diff-similar a b options)
         (diff-atom a b options))))))

(defn ^:private num-differences
  "Returns the number of modifications required to change a to b."
  [a b]
  (let [marker ::==
        [a-only b-only] (diff a b {:eq-marker marker})]
    (if (or a-only b-only)
      (+
       (count-occurrences (partial not= marker) a-only)
       (count-occurrences (partial not= marker) b-only))
      0)))
