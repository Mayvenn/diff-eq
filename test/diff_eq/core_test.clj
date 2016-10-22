(ns diff-eq.core-test
  (:require [diff-eq.core :refer :all]
            [clojure.test :refer :all])
  (:import java.util.ArrayList))

(defn text [& lines]
  (str (apply str (interpose "\n" lines))
       "\n"))

(deftest pretty-diff-atoms
  (is (= (pretty-print-diff [(diff 1 2) (diff 1 3)])
         (text "  - 1"
               "  + 2"
               ""
               "  - 1"
               "  + 3")))
  (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
    :a :b
    (text "  - :a"
          "  + :b")

    \a "a"
    (text "  - \\a"
          "  + \"a\"")

    1M 2N
    (text "  - 1M"
          "  + 2N")

    "hello" 1
    (text "  - \"hello\""
          "  + 1")))

(deftest pretty-diff-strings-by-lines
  (are [a b expected] (= (pretty-print-diff [(diff a b {:diff-strings? true})]) expected)
    "a\nb\nc" "a\nb\nd"
    (text "  - Line 3: \"c\""
          "  + Line 3: \"d\"")

    "a\nb\nc\ne" "a\nb\nd\ne"
    (text "  - Line 3: \"c\""
          "  + Line 3: \"d\"")))

(deftest pretty-diff-maps
  (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
    {:a 1 :b 3} {:b 3 :c 4}
    (text "  - {:a 1}"
          "  + {:c 4}")

    {:a {:b 1 :c 2}} {:a {:b 2 :c 2}}
    (text "  - {:a {:b 1}}"
          "  + {:a {:b 2}}")))

(deftest pretty-diff-sets
  (testing "Emits diff of sets to the closest-matching elements recursively"
    (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
      #{1 2 3} #{4 2 1}
      (text "  - #{3}"
            "  + #{4}")

      #{[1 2 3]} #{[1 2 4]}
      (text "  - #{[_ _ 3]}"
            "  + #{[_ _ 4]}")

      #{{:a 1} {:b 2 :c 4}} #{{:a 1} {:b 3 :c 4}}
      (text "  - #{{:b 2}}"
            "  + #{{:b 3}}")

      #{#{1 2} #{2 3} #{3 4}} #{#{1 2} #{2 4} #{3 4}}
      (text "  - #{#{3}}"
            "  + #{#{4}}"))))

(deftest pretty-diff-sequences
  (testing "Emits underscores instead of nils for equal elements"
    (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
      [1 2 3] [3 2 1]
      (text "  - [1 _ 3]"
            "  + [3 _ 1]")

      (ArrayList. [1 2 3]) (ArrayList. [1 2 4])
      (text "  - [_ _ 3]"
            "  + [_ _ 4]")

      [:x :y :z] [:x :z]
      (text "  - [_ :y :z]"
            "  + [_ :z]")

      [:x :y :z] [:x :z :a]
      (text "  - [_ :y :z]"
            "  + [_ :z :a]")

      [[1 2] [3 4]] [[1 2] [3 5]]
      (text "  - [_ [_ 4]]"
            "  + [_ [_ 5]]")

      (map inc [1 2 3]) (map inc [3 2 1])
      (text "  - [2 _ 4]"
            "  + [4 _ 2]")

      {:a [1 2 3]} {:a [3 2 1]}
      (text "  - {:a [1 _ 3]}"
            "  + {:a [3 _ 1]}")

      [nil nil 1] [1 nil nil]
      (text "  - [nil _ 1]"
            "  + [1 _ nil]")

      (seq [nil nil 1]) (seq [1 nil nil])
      (text "  - [nil _ 1]"
            "  + [1 _ nil]")

      [{:a :b} {:c :d}]
      [{:a :b} {:c :e}]
      (text "  - [_ {:c :d}]"
            "  + [_ {:c :e}]"))))

(deftest pretty-hides-deletions-if-not-needed
  (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
    [{:amount 53.4}]
    [{:amount 53.4}
     {:amount 53.4}]
    (text "  + [_ {:amount 53.4}]")

    #{1} #{nil 1}
    (text "  + #{nil}")))

(deftest pretty-hides-additions-if-not-needed
  (are [a b expected] (= (pretty-print-diff [(diff a b)]) expected)
    [{:amount 53.4}
     {:amount 53.4}]
    [{:amount 53.4}]
    (text "  - [_ {:amount 53.4}]")

    #{nil 1} #{1}
    (text "  - #{nil}")))
