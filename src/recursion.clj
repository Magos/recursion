(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (-> coll rest product))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))


(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (recur (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (-> a-seq rest max-element))))

(defn seq-max [seq-1 seq-2]
  (max-key count seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq)a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-filter pred? (rest a-seq)))
   :else (recur pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem)true
   :else (recur elem (rest a-seq))
    ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()
   ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (recur pred? (rest a-seq))
   :else (seq a-seq)
   ))

(defn seq= [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq)) (= a-seq b-seq)
   (= (first a-seq) (first b-seq)) (recur (rest a-seq) (rest b-seq))
   :else false
   ))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) ()
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   (= 1 k) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (-> n dec dec)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ()))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (map #(drop % a-seq) (-> a-seq count inc range)))

(defn inits [a-seq]
  (map #(take % a-seq) (-> a-seq count inc range)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (drop-last (map concat (tails a-seq) (inits  a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let[head (first a-seq)
         entry (get freqs head)]
      (if entry
        (recur (update-in freqs [head] inc) (rest a-seq))
        (recur (assoc freqs head 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map #(my-repeat (second %)(first %) ) a-map)))

(defn my-take [n coll]
  (if (or (-> n pos? not) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (-> n pos? not) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let[q (quot (count a-seq) 2)]
    (vector (my-take q a-seq) (my-drop q a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq))
    (concat a-seq b-seq)
    (let[[a-head & a-tail] a-seq
         [b-head & b-tail] b-seq]
      (if (<= a-head b-head)
        (cons a-head (seq-merge a-tail b-seq))
        (cons b-head (seq-merge a-seq b-tail))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (->> a-seq
         halve
         (map merge-sort)
         (apply seq-merge))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let[a (drop 1 (inits a-seq))
         rising (last (take-while (partial apply <=) a))
         falling (last (take-while (partial apply >) a))
         longest (longest-sequence [rising falling])]
      (cons longest (split-into-monotonics (drop (count longest) a-seq))))))

(defn intersperse [val a-seq]
  (map #(concat (take % a-seq) [val] (drop % a-seq))
       (-> a-seq count inc range)))

(intersperse :a [1 3 5 ])

(defn permutations [a-set]
  (cond
   (empty? a-set) '(())
   (singleton? a-set) (list a-set)
   :else (let[[head & tail] (seq a-set)
              tailperms (permutations tail)]
           (mapcat intersperse head tailperms))))



;(permutations #{}) ;=> (())
(permutations #{1 5 3}) ;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))


(defn powerset [a-set]
  [:-])

