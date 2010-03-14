(ns at.ac.tuwien.complang.chocolate)

(defn- real-len [choc]
  (if (empty? choc)
    0
    (let [rest-len (real-len (rest choc))]
      (if (and (zero? rest-len) (zero? (nth choc 0)))
	0
	(inc rest-len)))))

(defn normalize [choc]
  (take (real-len choc) choc))

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

(defn winning-chocolate? [choc] false)

(defn unmemoized-winning-chocolate? [choc]
  (let [moves (all-moves choc)]
    (if (empty? moves)
      false
      (some #(not (winning-chocolate? (eat choc %))) moves))))

(def winning-chocolate? (memoize unmemoized-winning-chocolate?))

(defn winning-up-to? [rows cols]
  (every? (fn [x] x)
	  (pmap winning-chocolate? (rest (for [i (range 1 rows)
					       j (range 1 cols)]
					   (chocolate i j))))))
