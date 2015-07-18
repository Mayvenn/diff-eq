(ns diff-eq.core
  (:require [clojure.test :refer :all]
            [clojure.data :as data]
            [clojure.pprint :as pprint]))

(defn prefix-space [count string]
  (clojure.string/join (apply str "\n" (repeatedly count (constantly " ")))
                       (clojure.string/split string #"\n")))

(defn pprint-str [value]
  (prefix-space
   4
   (with-out-str
     (pprint/pprint value))))

(defn pretty-print-diff [diffs]
  (apply str
         (map (fn [[a b equal]]
                (str "  - " (pprint-str a)
                     "\n"
                     "  + " (pprint-str b)
                     "\n"))
              diffs)))

(defonce override
  (delay
   (defmethod assert-expr '= [msg form]
     (let [args (rest form)
           pred (first form)]
       `(let [values# (list ~@args)
              result# (apply ~pred values#)]
          (if result#
            (do-report {:type :pass, :message ~msg,
                        :expected '~form, :actual (cons ~pred values#)})
            (binding [*testing-contexts* (conj *testing-contexts*
                                               (str "\n\nDiff:\n"
                                                    (pretty-print-diff (map #(data/diff (first values#) %)
                                                                            (rest values#)))))]
              (do-report {:type :fail, :message ~msg,
                          :expected '~form, :actual (list '~'not (cons '~pred values#))})))
          result#)))))

(defn diff! []
  @override)
