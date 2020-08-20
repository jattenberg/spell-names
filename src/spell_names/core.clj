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

(defn mutate-spell
  "takes a spell and finds all valid permutations"
  [spell words]
  (let [candidate-spells (mapcat #(test-chars-at-position spell %) (range (count spell)))
        original-spell (clojure.string/join spell)]
    (apply hash-set 
           (filter #(non-repetitive-spell original-spell %)
            (filter #(valid-spell % words) candidate-spells))
           )
    )
)

(defn mutate-spells
  [spells words]
  (let [spell-vecs (map #(vec (map str (seq %))) spells)
        mutated-spells (map #(mutate-spell % words) spell-vecs)]
    (zipmap spells mutated-spells)
    )
)

(defn write-map-to-file
  [file spell-map]
  (with-open [writer (clojure.java.io/writer file :append true)]
    (doseq [[k v] (map vector (keys spell-map) (vals spell-map))]
      (.write writer (str "## " k "\n\n"))
      (doseq [item v] (.write writer (str "- " item "\n")))
      (.write writer "\n\n"))
    )
)

(defn -main
  "changes one character of the specified spell list"
  ([spells-file] (-main spells-file "/usr/share/dict/words"))
  ([spells-file words-file] (-main spells-file words-file "./new-spells.md"))
  ([spells-file words-file out-file]
   (let [spells (read-spells spells-file)
         words (read-dict words-file)
         spell-map (mutate-spells spells words)]
     (write-map-to-file out-file spell-map)
     )))

