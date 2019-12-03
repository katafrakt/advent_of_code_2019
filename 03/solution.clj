(use 'clojure.java.io)
(use 'clojure.set)

(defn get-lines [fname]
  (with-open [r (reader fname)]
    (doall (line-seq r))))

(defn dir-vec [dir]
  (case dir
    "R" [0,1]
    "L" [0, -1]
    "U" [-1, 0]
    "D" [1,0]))

(defn visit-times [i limit current dir]
  (lazy-seq
    (when (< i limit)
      (cons (mapv + current (mapv * (iterate * (inc i)) dir)) (visit-times (inc i) limit current dir)))))

(defn visited-nodes [nodes instruction]
  (let [[_ dir dist-string] (re-matches #"(\w)(\d+)" instruction)
        distance (Integer/parseInt dist-string)
        vector (dir-vec dir)]
    (def visited (visit-times 0 distance (last nodes) vector))
    (concat nodes visited)))

(defn parse-line [line]
  (def instructions (clojure.string/split line #","))
  (reduce visited-nodes [[0,0]] instructions))

(defn manhattan [pair]
  (+ (Math/abs (first pair)) (Math/abs (second pair))))

;(def line1 (parse-line "R8,U5,L5,D3"))
;(def line2 (parse-line "U7,R6,D4,L4"))
(def input (get-lines "input"))
(def line1 (parse-line (first input)))
(def line2 (parse-line (second input)))
(def crossings (into [] (clojure.set/intersection (set line1) (set line2))))
(def distances (map manhattan crossings))
(println (apply min (remove zero? distances)))

; part 2
(defn index-of [e coll] (first (keep-indexed #(if (= e %2) %1) coll)))

(defn sum-steps [crossing]
  (def steps1 (index-of crossing line1))
  (def steps2 (index-of crossing line2))
  (+ steps1 steps2))
(def distances (map sum-steps crossings))
(println (apply min (remove zero? distances)))