(ns diff-eq.core
    (:require [clojure.test :refer :all]
              [clojure.pprint :as pprint]
              [clojure.walk :as walk]
              [diff-eq.data :as data]))

(def global-options (atom {}))

(defn prefix-space [count string]
  (clojure.string/join (apply str "\n" (repeatedly count (constantly " ")))
                       (clojure.string/split string #"\n")))

(defn pprint-str [value]
  (prefix-space
   4 (with-out-str (if-let [printer (:diff-eq/printer (meta value))]
       (printer value)
       (pprint/pprint value)))))

(defn pretty-print-diff [diffs]
  (apply str
         (interpose "\n"
                    (map (fn [[a b eq]]
                           (str (when (not= a '_)
                                  (str "  - " (pprint-str a) "\n"))
                                (when (not= b '_)
                                  (str "  + " (pprint-str b) "\n"))))
                         diffs))))

(defn diff
  ([a b] (diff a b {}))
  ([a b {:keys [diff-strings?] :or {diff-strings? (and (string? a) (string? b))}}]
   (data/diff a b {:eq-marker '_ :ne-marker '_ :diff-strings? diff-strings?})))

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
                                                    (pretty-print-diff (map #(diff (first values#) % @global-options)
                                                                            (rest values#)))))]
              (do-report {:type :fail, :message ~msg,
                          :expected '~form, :actual (list '~'not (cons '~pred values#))})))
          result#)))))

(defn diff!
  ([] (diff! {}))
  ([options]
   (reset! global-options options)
   @override))
