(ns mr-challenjer.analyzer
  "Searches for the most likely words in Hangman by indexing the search space.
  Initial load time is slow, due to indexing the dictionary file."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.edn :as edn])
  (:import [java.io File PushbackReader]))

(def dictionary-file "dictionary.txt")
(def index-file "index.edn")
(def dictionary-words
  "Eagerly loaded dictionary file"
  (with-open [rdr (io/reader dictionary-file)]
    (doall (line-seq rdr))))

(defn map-word
  "Converts a word into a series of single-letter vectors for the word.
  e.g. => (map-word \"foo\")
  ([\\f \\_ \\_] [\\_ \\o \\o])"
  [word]
  (letfn [(to-key [l] (map #(if (= l %) l \_) word))]
    (map to-key (into #{} word))))

(defn vector-word-map
  "Creates a map of vectors for a word to the word itself.
  Uses loop/recur instead of zipmap as this is 10% faster."
  [word]
  (loop [m {} [k & ks] (map-word word)]
    (if-not k
      m
      (recur (assoc m k #{word}) ks))))

(defn map-words
  "Creates an index of words keyed by their vectors."
  [words]
  (apply merge-with into (map vector-word-map words)))

(defn get-words
  "Creates an index of words keyed by their vectors."
  []
  (if (.exists (File. index-file))
    (with-open [rdr (PushbackReader. (io/reader index-file))]
      (edn/read rdr))
    (let [m (map-words dictionary-words)]
      (do
        (spit index-file m)
        m))))

(def word-map "Index of the entire dictionary" (get-words))

(defn to-keys
  "Converts a word guess into a seq of key vectors"
  [wordv]
  (letfn [(to-key [l] (when-not (= \_ l) (map #(if (= % l) l \_) wordv)))]
    (keep to-key wordv)))

(defn letter-ordering
  "Creates a seq of letters based on how frequently they appear in words
  of a given length. Only counts a word once per letter, no matter how
  many times the letter appears in the word."
  [words len]
  (->> (filter #(= len (count %)) words)
       (mapcat set)
       frequencies
       (sort-by second)
       reverse
       (map first)))

(defn lengths
  "Creates a seq of the lengths of the list of words."
  [words]
  (sort (into #{} (map count words))))

(defn get-ordering-map
  "Creates a map of word length to the letters that appear in those words,
  from most frequent to least."
  [words]
  (let [word-lengths (lengths words)]
    (zipmap word-lengths (map (partial letter-ordering words) word-lengths))))

(def ordered-letters (get-ordering-map dictionary-words))

(defn naive-guess
  "Make a guess of a letter in a word without any knowledge of the word
  except length and a set of unsuccessful guesses."
  [guessed length]
  (first (remove guessed (get ordered-letters length))))

(defn guesser
  "Guesses at a letter in a word. If no letters are known, then calls naive-guess
  to choose the most likely letter based on the length of the word. If some
  letters are known, these are used to look up the word index for potential words.
  The possible words are then scanned for how many words contain each letter.
  The letter appearing in the most number of words is then returned."
  [{:keys [word guesses tries-left] :as arg}]
  (let [guessed (set guesses)]
    (if (every? #{\_} word)
      (naive-guess guessed (count word))
      (let [wmap (select-keys word-map (to-keys word))
            possible-words (reduce set/intersection (map second wmap))
            letter-freq (frequencies (remove guessed (mapcat set possible-words)))
            best-pair (reduce (fn [[_ ca :as a] [_ cb :as b]] (if (> cb ca) b a)) letter-freq)]
        (first best-pair)))))

