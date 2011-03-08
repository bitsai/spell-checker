(ns spell-checker
  (:require [clojure.string :as str]))

(defn words [text]
  (re-seq #"[a-z]+" (str/lower-case text)))

(def *nwords* (frequencies (words (slurp "big.txt"))))
(def *alphabet* "abcdefghijklmnopqrstuvwxyz")

(defn edits1 [word]
  (let [splits (for [i (range (inc (count word)))]
		 [(subs word 0 i) (subs word i)])
	deletes (for [[a b] splits :when (pos? (count b))]
		  (str a (subs b 1)))
	transposes (for [[a b] splits :when (> (count b) 1)]
		     (str a (nth b 1) (nth b 0) (subs b 2)))
	replaces (for [[a b] splits, c *alphabet* :when (pos? (count b))]
		   (str a c (subs b 1)))
	inserts (for [[a b] splits, c *alphabet*]
		  (str a c b))]
    (reduce into #{} [deletes transposes replaces inserts])))

(defn edits2 [word]
  (seq (set (for [e1 (edits1 word), e2 (edits1 e1) :when (*nwords* e2)]
	      e2))))

(defn known [words]
  (seq (filter *nwords* words)))

(defn correct [word]
  (let [candidates (or (known [word]) (known (edits1 word))
		       (edits2 word) [word])]
    (apply max-key #(get *nwords* % 1) candidates)))
