(ns mr-challenjer.guessers
  "Defines various guesser functions that implement different strategies. A guesser function will be
  passed a the current guess state containing the following keys:

  * word - A vector of the current word state. It contains characters of correctly guessed letters
  or _ (underscore) as placeholders.
  * guesses - a sequence of past guessed characters.
  * tries-left - the number of incorrect guesses left."
  (:require [mr-challenjer.core :as mr]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [criterium.core :as criterium]))


(def always-a
  "A guesser that will always guess the letter a"
  (constantly \a))

(defn a-z
  "Guesser of all letters A-Z in order"
  [guess-state]
  (let [alphabet (map #(char (+ % (int \a))) (range 26))
        past-guess-set (set (:guesses guess-state))]
    (first (filter (complement past-guess-set) alphabet))))

(defn vowels-then-a-z
  "Guesser that guesses vowels first then the rest of the alphabet"
  [guess-state]
  (let [vowels #{\a \e \i \o \u}
        past-guess-set (set (:guesses guess-state))]
    (if (set/subset? vowels past-guess-set)
      (a-z guess-state)
      (first (filter (complement past-guess-set) vowels)))))

(defn simple-english-freq
  [guess-state]
  (let [alphabet [\e \t \a \o \i \n \s \r \h \d \l \u
                  \c \m \f \y \w \g \p \b \v \k \x \q \j \z]
        past-guess-set (set (:guesses guess-state))]
    (first (filter (complement past-guess-set) alphabet))))

(def all-the-words
  (with-open [rdr (io/reader mr/dictionary-file)]
    (vec (line-seq rdr))))

(def all-the-words-vecs
  (mapv vec all-the-words))

(defn word-matches?
  [pattern word]
  (re-matches pattern word))

(defn choose-letter
  [pattern possibles guesses]
  (let [guesses (set guesses)
        pos-letter-freq (for [n (range (count pattern))]
                          (frequencies (filterv (complement guesses)
                                               (mapv #(nth % n) possibles))))
        underscore-pos (ffirst (filterv #(= \_ (second %))
                                               (map-indexed vector pattern)))
        letter-freq (nth pos-letter-freq underscore-pos)]
    (->> letter-freq
         set/map-invert
         (sort-by first)
         reverse
         first
         second)))

(defn possible-matches
  "Given a set of possible words and the word vector
  returns the subset of possible words that match the word vector"
  [pos-word-vec word-vec]
  (let [pattern (re-pattern
                  (str "^"
                       (str/replace (apply str word-vec) "_" ".")
                       "$"))]
    (filterv (partial word-matches? pattern) pos-word-vec)))

(defn dictionary-attack
  [{:keys [guesses word extra-state]}]
  (when-not extra-state
    (println "Extra state not set"))
  (let [possibles (possible-matches (or extra-state
                                        all-the-words)
                                    word)]
    {:guess (choose-letter word possibles guesses)
     :extra-state possibles}))



(comment
  (def test-guess-state
    {:word [\_ \_ \_ \_]
     :guesses [\a \b]
     :tries-left 6})

  ;; Test calling guesser function
  (always-a test-guess-state)
  (a-z test-guess-state)

  (dictionary-attack test-guess-state)

  ;; Test a guesser with a single word
  (mr/test-guesser always-a)
  (mr/test-guesser a-z)
  (mr/test-guesser vowels-then-a-z)
  (time (assoc-in (mr/test-guesser dictionary-attack)
                  [:guess-state :extra-state]
                  nil))

  (time
    (dotimes [n 10]
      (mr/test-guesser dictionary-attack "abracadabra")))

  (mr/measure-guesser (fn [_]
                        (println "called")
                        \e))


  ;; Get the percentages success rate of a guesser
  (mr/measure-guesser always-a)
  (mr/measure-guesser a-z 1000)
  (mr/measure-guesser vowels-then-a-z 1000)
  (mr/measure-guesser dictionary-attack 100)

  (mr/measure-guesser simple-english-freq)

  )