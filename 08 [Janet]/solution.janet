(def input (file/read (file/open "input") :all))
(def layer-height 6)
(def layer-width 25)
#(def input "0222112222120000")
#(def layer-height 2)
#(def layer-width 2)
(def layer-size (* layer-width layer-height))

(var zeros 160)
(var ones nil)
(var twos nil)

(defn set-things [tmp-zeros layer]
  (set zeros tmp-zeros)
  (set ones (get (frequencies layer) 49))
  (set twos (get (frequencies layer) 50)))

(var image (array/new layer-size))

(loop [layer-num :range [0 (/ (length input) layer-size)]]
  (def layer-begin (* layer-num layer-size))
  (def layer (slice input layer-begin (+ layer-size layer-begin)))
  (def tmp-zeros (get (frequencies layer) 48))
  (if (< tmp-zeros zeros) (set-things tmp-zeros layer))
  
  (loop [pixel :range [0 layer-size]]
    (if (and (not (get image pixel)) (not (= (get layer pixel) 50)))
      (put image pixel (get layer pixel)))))

(print (* ones twos))
(print "")
(loop [x :range [0 layer-height]]
  (loop [y :range [0 layer-width]]
    (def chr (get image (+ (* x layer-width) y)))
    (printf "%s" (if (= chr 48) " " "#")))
  (print ""))