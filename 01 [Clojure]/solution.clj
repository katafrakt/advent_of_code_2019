(use 'clojure.java.io)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(def fname "input")
(def lines (get-lines fname))
(defn process [num]
  (- (quot num 3) 2))
(defn reducer [acc line]
  (+ acc (process (Integer/parseInt line))))
(def fuel (reduce reducer 0 lines))
(println fuel)

(defn additional-fuel [mass]
  (loop [fuel-left mass acc 0]
    (let [extra (process fuel-left)]
      (if (<= extra 0)
        acc
        (recur (process fuel-left) (+ acc extra))))))

(defn reducer2 [acc line]
  (let [module-weight (process (Integer/parseInt line))]
    (let [additional (additional-fuel module-weight) fuel-weight (if (>= additional 0) additional 0)]
      (+ acc (+ module-weight fuel-weight)))))
(println (reduce reducer2 0 lines))