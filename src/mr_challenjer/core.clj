(ns mr-challenjer.core
  "Implements the main hangman algorithm (test-guesser function) and a function for measuring
  a guessers win rate. "
  (:require [clojure.java.io :as io]))

(def dictionary-file
  "A file containing a word per line."
  "dictionary.txt")

(defn count-lines [f]
  (with-open [r (io/reader f)]
    (count (line-seq r))))

(def num-words
  "The number of words in the dictionary file."
  (count-lines dictionary-file))
(println num-words)

(defn- random-word
  "Returns a random word from the dictionary file."
  []
  (let [line-num (rand-int num-words)]
    (with-open [rdr (io/reader dictionary-file)]
      (first (drop line-num (line-seq rdr))))))



(defn- random-words
  "Returns multiple random words from the dictionary file."
  [amount]
  (let [line-nums (first (drop-while #(< (count %) amount)
                                     (reductions (fn [uniq-nums num]
                                                   (conj uniq-nums num))
                                                 #{}
                                                 (repeatedly #(rand-int (inc num-words))))))]
    (with-open [rdr (io/reader dictionary-file)]
      (doall
        (take amount
              (map second
                   (filter (comp line-nums first)
                           (map-indexed vector (line-seq rdr)))))))))


(defn- apply-guess
  "Takes the word being guessed as a vector, the current word state, and the guess character.
  Returns the word state updated with letters from a correct guess."
  [word-vec word-state guess]
  (map (fn [word-char state-char]
         (if (= word-char guess)
           word-char
           state-char))
       word-vec
       word-state))

(defn test-guesser
  "Tests a guesser function to see if it can guess the mystery word. Takes a guesser function
  and optionally a test word. Returns the result of playing the guesser function."
  ([guess-fn]
   (test-guesser guess-fn (random-word)))
  ([guess-fn word]
   (let [word-vec (vec word)
         guess-state {:word (vec (repeat (count word) \_))
                      :guesses []
                      :tries-left 8}]
     (loop [guess-state guess-state]
       (let [guess (guess-fn guess-state)
             new-word-state (apply-guess word-vec (:word guess-state) guess)
             correct-guess? (not= new-word-state (:word guess-state))
             new-guess-state (-> guess-state
                                 (assoc :word new-word-state)
                                 (update-in [:guesses] conj guess)
                                 (#(if correct-guess?
                                     %
                                     (update-in % [:tries-left] dec))))]
         (cond
           (= word-vec (:word new-guess-state))
           {:result :win :word word :guess-state new-guess-state}

           (= 0 (:tries-left new-guess-state))
           {:result :lose :word word :guess-state new-guess-state}
           :else
           (recur new-guess-state)))))))


(defn measure-guesser
  "Tries a guess function across many words. Returns the percentages of wins."
  ([guess-fn]
   (measure-guesser guess-fn 100))
  ([guess-fn times]
   (let [words (random-words times)
         win-count (reduce (fn [win-count word]
                             (if (= :win (:result (test-guesser guess-fn word)))
                               (inc win-count)
                               win-count))
                           0
                           words)]
     (* 100.0 (/ win-count times)))))
