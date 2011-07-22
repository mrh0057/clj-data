(ns clj-data.seq-utils)

(defn reduce-by-2 [func start-value sequence]
  (if (mod (count sequence) 2)
    (loop [sequence sequence
           val start-value]
      (if (empty? sequence)
        val
        (let [next-sequence (drop 2 sequence)]
          (recur next-sequence (func val (first sequence) (second sequence))))))
    (throw (new Exception "reduce-by-2 requires an even number of forms"))))
