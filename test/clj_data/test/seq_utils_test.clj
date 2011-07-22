(ns clj-data.test.seq-utils-test
  (:use clojure.test
        clj-data.seq-utils
        midje.sweet))

(deftest reduce-by-2-test
  (reduce-by-2 (fn [val first-value second-value]
                 (is (= first-value "one"))
                 (is (= second-value "two"))) nil '("one" "two"))
  (= 10 (reduce-by-2
         (fn [val first-value second-value]
           (+ first-value second-value val)) 0 '(1 2 3 4))))
