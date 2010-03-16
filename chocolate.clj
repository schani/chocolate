(ns at.ac.tuwien.complang.chocolate)

(defn normalize [choc]
  (take-while (complement zero?) choc))

(defn chocolate [rows cols]
  (repeat rows cols))

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
	      (map #(cons i %)
		   (all-chocolates-raw (dec rows) i)))
	    (range 0 (inc cols)))))

(defn all-chocolates [rows cols]
  (map normalize (rest (all-chocolates-raw rows cols))))
