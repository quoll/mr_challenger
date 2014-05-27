(ns mr-challenjer.guessers
  "Defines various guesser functions that implement different strategies. A guesser function will be 
  passed a the current guess state containing the following keys:
  
  * word - A vector of the current word state. It contains characters of correctly guessed letters
  or _ (underscore) as placeholders.
  * guesses - a sequence of past guessed characters.
  * tries-left - the number of incorrect guesses left."
  (:require [mr-challenjer.core :as mr]
            [clojure.set :as set]))


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

(comment 
  (def test-guess-state
    {:word [\_ \_ \_ \_]
     :guesses [\a \b]
     :tries-left 6})

  ;; Test calling guesser function 
  (always-a test-guess-state)
  (a-z test-guess-state)
  
  ;; Test a guesser with a single word
  (mr/test-guesser always-a)
  (mr/test-guesser a-z)
  (mr/test-guesser vowels-then-a-z)
  
  ;; Get the percentages success rate of a guesser
  (mr/measure-guesser always-a)
  (mr/measure-guesser a-z 1000)
  (mr/measure-guesser vowels-then-a-z 1000)
  
  
  )