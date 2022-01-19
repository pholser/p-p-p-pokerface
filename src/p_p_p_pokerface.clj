(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (get {\T 10 \J 11 \Q 12 \K 13 \A 14} fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(def ranks (partial map rank))
(def suits (partial map suit))

(defn pair? [hand]
  (boolean (some #(= % 2) (vec (vals (frequencies (ranks hand)))))))

(defn three-of-a-kind? [hand]
  (boolean (some #(= % 3) (vec (vals (frequencies (ranks hand)))))))

(defn four-of-a-kind? [hand]
  (boolean (some #(= % 4) (vec (vals (frequencies (ranks hand)))))))

(defn flush? [hand]
  (= 1 (count (frequencies (suits hand)))))

(defn full-house? [hand]
  (let [sets (frequencies (ranks hand))]
    (and (= 2 (count sets))
         (= #{3 2} (set (vals sets))))))

(defn two-pairs? [hand]
  (let [sets (frequencies (ranks hand))]
    (= 2 (count (filter (fn [[_ occur]] (= 2 occur)) sets)))))

(defn straight? [hand]
  (let [ranks (vec (sort (ranks hand)))
        least (apply min ranks)
        ranks-ace-low (sort (replace {14 1} ranks))
        least-ace-low (apply min ranks-ace-low)]
    (or (= ranks (range least (+ 5 least)))
        (= ranks-ace-low (range least-ace-low (+ 5 least-ace-low))))))

(defn straight-flush? [hand]
  ((every-pred straight? flush?) hand))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
