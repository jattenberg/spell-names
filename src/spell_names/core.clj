(ns spell-names.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(def letters (apply hash-set (map str (seq "abcdefghijklmnopqrstuvwxyz"))))

(defn file-to-set
  [file]
  (with-open [reader (clojure.java.io/reader file)]
    (apply hash-set (map clojure.string/trim (map clojure.string/lower-case (line-seq reader)))))
)

(defn read-dict
  ([] (read-dict "/usr/share/dict/words"))
  ([dict-file] (file-to-set dict-file))
)

(defn read-spells
  ([spells-file] (file-to-set spells-file)) 
)


(defn test-chars-at-position
  "takes a vector of characters in a word and a position to mutate and a valid word list"
  [word-vec pos]
  (if (contains? letters (get word-vec pos))
    (map #(clojure.string/join (assoc word-vec pos %)) letters)
    []
    )
)

(defn valid-spell
  "is every word in the new spell a valid word?"
  [spell words]
  (let [spell-words (clojure.string/split spell #"[^a-z]")]
    (every? #(contains? words %) spell-words)
    )
  )

(defn non-repetitive-spell
  [new-spell original-spell]
  (not (= new-spell original-spell))
)

(defn filter-spells
  [original-spell spells-list words]
  (filter (and #(valid-spell % words) #(non-repetitive-spell % original-spell)) spells-list)
)

(defn mutate-spell
  "takes a spell and finds all valid permutations"
  [spell words]
  (let [candidate-spells (mapcat #(test-chars-at-position spell %) (range (count spell)))]
    (filter-spells (clojure.string/join spell) candidate-spells words)
    )
)

(defn mutate-spells
  [spells words]
  (let [spell-vecs (map #(vec (map str (seq %))) spells)
        mutated-spells (map #(mutate-spell % words) spell-vecs)]
    (zipmap spells mutated-spells)
    )
)

(defn -main
  "changes one character of the specified spell list"
  ([spells-file] (-main spells-file "/usr/share/dict/words"))
  ([spells-file words-file]
   (def spells (read-spells spells-file))
   (def words (read-dict words-file))
   ))


(defn foo
[valid-words word-vec]
  (filter
      #(not 
        (or (contains? valid-words %) (= % (str word-vec)))
        )
      [1 2 3 4]
      ))
