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

(def winning-chocolate-internal?
     (memoize (fn [choc]
		(some #(not (winning-chocolate-internal? (into [] (eat choc %))))
		      (all-moves choc)))))

(defn winning-chocolate? [choc]
  (winning-chocolate-internal? (into [] choc)))

(defn winning-up-to? [rows cols]
  (every? winning-chocolate?
	  (rest (for [i (range 1 rows)
		      j (range 1 cols)]
		  (chocolate i j)))))

(defn- all-chocolates-recur [rows cols y choc acc]
  (if (= y rows)
    (conj! acc (normalize choc))
    (loop [x 0
	   acc acc]
      (if (<= x cols)
	(let [more-choc (conj choc x)]
	  (recur (inc x)
		 (all-chocolates-recur rows x (inc y) more-choc acc)))
	acc))))

(defn all-chocolates [rows cols]
  (rest (persistent! (all-chocolates-recur rows cols 0 [] (transient [])))))

(defn chocolate-bits [rows cols choc]
  (doall (let [unnormalized-choc (concat choc (repeat (- rows (count choc)) 0))]
	   (map (fn [i]
		  (concat (repeat i 1) (repeat (- cols i) 0)))
		unnormalized-choc))))

(defn bits-map2 [f bits1 bits2]
  (doall (map #(doall (map f %1 %2)) bits1 bits2)))

(defn add-chocolate-bits [bits1 bits2]
  (bits-map2 + bits1 bits2))

(defn win-loss-bits [rows cols]
  (let [chocolates (all-chocolates rows cols)
	winners (filter winning-chocolate? chocolates)
	losers (remove winning-chocolate? chocolates)]
    [(reduce add-chocolate-bits (map #(chocolate-bits rows cols %) winners))
     (reduce add-chocolate-bits (map #(chocolate-bits rows cols %) losers))]))

(defn win-loss-visualize [rows cols]
  (let [[winner-bits loser-bits] (win-loss-bits rows cols)
	all-bits (reduce add-chocolate-bits (map #(chocolate-bits rows cols %) (all-chocolates rows cols)))
	fractions (bits-map2 / winner-bits all-bits)
	min-fraction (apply min (flatten fractions))
	max-fraction (apply max (flatten fractions))
	frame (JFrame. (str "Win-Loss " rows "x" cols))
	panel (proxy [JPanel] []
		(paintComponent [g]
				(proxy-super paintComponent g)
				(let [size (.getSize this)
				      width (.getWidth size)
				      height (.getHeight size)
				      xs (map #(int (/ (* % width) cols)) (range 0 (inc cols)))
				      ys (map #(int (/ (* % height) rows)) (range 0 (inc rows)))]
				  (doseq [y (range 0 rows)
					  x (range 0 cols)]
				    (let [fraction (nth (nth fractions y) x)
					  gray (float (/ (- fraction min-fraction) (- max-fraction min-fraction)))]
				      (.setColor g (Color. gray gray gray))
				      (.fillRect g
						 (nth xs x)
						 (nth ys y)
						 (- (nth xs (inc x)) (nth xs x))
						 (- (nth ys (inc y)) (nth ys y))))))))]
    (doto frame
      (.add panel)
      (.setSize 300 300)
      (.setVisible true))))
