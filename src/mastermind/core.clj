(ns mastermind.core)


;;; core logic

(def colors #{:b :g :o :p :r :y})

(def code-length 4)

(def random-color
  ;; rand-nth doesn't work on sets, but colors is logically a set so I didn't
  ;; want to give it an arbitrary order just for this.
  (let [color-vec (vec colors)]
    (fn []
      (rand-nth color-vec))))

(defn random-code []
  (repeatedly code-length random-color))

(defn strong-match-indices [xs ys]
  (set (for [[index x y] (map vector (range) xs ys)
             :when (= x y)]
         index)))

(defn strong-match-count [xs ys]
  (count (strong-match-indices xs ys)))

(defn remove-indices [xs ixs]
  (for [[ix x] (map-indexed vector xs)
        :when (not (contains? ixs ix))]
    x))

(defn weak-match-count [xs ys]
  (let [indices (strong-match-indices xs ys)
        remaining-xs-freqs (frequencies (remove-indices xs indices))
        remaining-ys-freqs (frequencies (remove-indices ys indices))]
    (apply + (for [[k v] remaining-xs-freqs]
               (min v (get remaining-ys-freqs k 0))))))

(defn score [xs ys]
  {:strong (strong-match-count xs ys)
   :weak (weak-match-count xs ys)})

(defn new-game []
  {:secret-code (random-code)
   :guesses []})

(defn add-guess [{:keys [secret-code] :as game} guess]
  ;; alternatively, I could calculate scores on the fly while printing them out.
  ;; I chose this because this way we can send guesses to dumb clients without
  ;; sending them the secret code
  (update game :guesses conj {:guess guess :score (score guess secret-code)}))


;;; display utils

(defn print-3-cols [& cols]
  (apply printf "%20s%20s%20s\n" cols))

(defn print-guess [{g :guess
                    {:keys [strong weak]} :score}]
  (print-3-cols g strong weak)
  (when (= strong code-length)
    (println)
    (print-3-cols "°º¤ø,¸¸,ø¤º°`°º¤ø,¸" "  *** YOU WON! ***  " "¤º°`°º¤ø,¸¸,ø¤º°`°º¤ø,¸")))

(defn print-game [{:keys [guesses]}]
  (println)
  (print-3-cols "Guess" "Strong Matches" "Weak Matches")
  (print-3-cols "-----" "--------------" "------------")
  (doseq [g guesses]
    (print-guess g)))

(defn print-last-guess [{:keys [guesses]}]
  (print-guess (last guesses)))


;;; PLAY HERE

;; start new game by evaluating this `do` block
(do
  (def g (new-game))
  (print-game g))

;; make a guess by editing the vector and evaluating the `do` block
(do
  (def g (add-guess g [:o :o :o :o]))
  (print-last-guess g))
