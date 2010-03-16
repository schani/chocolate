(ns at.ac.tuwien.complang.chocolate
  (:use clojure.contrib.seq-utils)
  (:import (javax.swing JFrame JPanel)
	   (java.awt Color Dimension)))

(defn- chocolate-array [choc]
  (let [len (count choc)
	arr (make-array (. Integer TYPE) len)]
    (loop [i 0 c choc]
      (when (< i len)
	(aset arr i (first c))
	(recur (inc i) (rest c))))
    arr))

(defn normalize [choc]
  (chocolate-array (take-while (complement zero?) choc)))

(defn chocolate [rows cols]
  (chocolate-array (repeat rows cols)))

(defn eat [choc move]
  (let [[row col] move
	[begin end] (split-at row choc)]
    (normalize (concat begin (map #(min % col) end)))))

(defn all-moves [choc]
  (rest (mapcat (fn [cols i]
		  (map (fn [j] [i j]) (range 0 cols)))
		choc (range 0 (count choc)))))

(def winning-chocolate?
     (memoize (fn [choc]
		(some #(not (winning-chocolate? (eat choc %))) (all-moves choc)))))

(defn winning-up-to? [rows cols]
  (every? winning-chocolate?
	  (rest (for [i (range 1 rows)
		      j (range 1 cols)]
		  (chocolate i j)))))

(defn- all-chocolates-raw [rows cols]
  (if (zero? rows)
    '(())
    (mapcat (fn [i]
	      (doall (map #(chocolate-array (cons i %))
			  (all-chocolates-raw (dec rows) i))))
	    (range 0 (inc cols)))))

(defn all-chocolates [rows cols]
  (map normalize (rest (all-chocolates-raw rows cols))))

(defn chocolate-bits [rows cols choc]
  (doall (let [unnormalized-choc (concat choc (repeat (- rows (count choc)) 0))]
	   (map (fn [i]
		  (concat (repeat i 1) (repeat (- cols i) 0)))
		unnormalized-choc))))

(defn add-chocolate-bits [bits1 bits2]
  (doall (map #(doall (map + %1 %2)) bits1 bits2)))

(defn win-loss-bits [rows cols]
  (let [groups (group-by winning-chocolate? (all-chocolates rows cols))
	winners (groups true)
	losers (groups nil)]
    [(reduce add-chocolate-bits (map #(chocolate-bits rows cols %) winners))
     (reduce add-chocolate-bits (map #(chocolate-bits rows cols %) losers))]))

(defn win-loss-visualize [rows cols]
  (let [[winner-bits loser-bits] (win-loss-bits rows cols)
	all-bits (reduce add-chocolate-bits (map #(chocolate-bits rows cols %) (all-chocolates rows cols)))
	frame (JFrame. (str "Win-Loss " rows "x" cols))
	panel (proxy [JPanel] []
		(paintComponent [g]
				(proxy-super paintComponent g)
				(let [size (.getSize this)
				      width (.getWidth size)
				      height (.getHeight size)]
				  (doseq [y (range 0 rows)
					  x (range 0 cols)]
				    (let [winners (nth (nth winner-bits y) x)
					  total (nth (nth all-bits y) x)
					  gray (float (/ winners total))]
				      ;(println (str "painting " x " " y " with " gray))
				      (.setColor g (Color. gray gray gray))
				      (.fillRect g
						 (/ (* x width) cols)
						 (/ (* y height) rows)
						 (/ width cols)
						 (/ height rows)))))))]
    (doto frame
      (.add panel)
      (.setSize 300 300)
      (.setVisible true))))
