(ns clj-data.reductions-test
  (:use clojure.test
        clj-data.core
        clj-data.reductions))

(def *test-matrix* [[20 1 2 6 9 10 11 12 1000]
                    [3 4 5 8 30 50 9 9 10]
                    [100 123 135 233 143 152 142 123 210]])

(deftest PCAReductionTakeTest
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)
        pca-reduce-num (pca-by-number data-set 2)
        pca-reduce (pca-by-percentage data-set 0.5)]))

(deftest DataReductionTestPCA
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)
        pca-reduce (pca-by-number data-set 3)
        new-data-set (reduce-data-set pca-reduce data-set)]
    (println (:reduction-matrix pca-reduce))
    (println new-data-set)))
