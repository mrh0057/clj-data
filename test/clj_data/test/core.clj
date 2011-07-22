(ns clj-data.test.core
  (:use [clj-data.core])
  (:use [clojure.test]))

(set! *warn-on-reflection* true)

(defn- matrix-get-value [matrix x y]
  (. matrix getAsDouble y x))

(defn- verify-matrix [matrix]
  (is (= (. matrix getAsDouble 0 0) 0))
  (is (= (. matrix getAsDouble 0 1) 1))
  (is (= (. matrix getAsDouble 0 2) 2))
  (is (= (. matrix getAsDouble 1 0) 3))
  (is (= (. matrix getAsDouble 1 1) 4))
  (is (= (. matrix getAsDouble 1 2) 5)))

(def *test-matrix* [[0 1 2] [3 4 5]])

(deftest make-data-set-test
  (let [matrix *test-matrix*]
    (let [data-set (make-data-set "name" "attributes" matrix)]
      (is (= "name" (:name data-set)))
      (is (= "attributes" (:attributes data-set)))
      (is (= (get-value data-set 0 0) 0))
      (is (= (get-value data-set 1 0) 1))
      (verify-matrix (:data data-set)))
    (let [data-set (make-data-set "name" "attributes" matrix :row-labels "labels")]
      (is (= "name" (:name data-set)))
      (is (= "attributes" (:attributes data-set)))
      (is (= "labels" (:row-labels data-set))))))

(deftest create-select-string-test
  (is (= "1,2,3" (create-select-string [1 2 3])))
  (is (= "1-3" (create-select-string [1 :to 3])))
  (is (= "*" (create-select-string [:any])))
  (is (= "1,2,3-10,13,14" (create-select-string [1 2 3 :to 10 13 14]))))

(deftest select-test
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)
        new-data-set (select data-set [0] [0 :to 2])
        matrix (:data new-data-set)]
    (println new-data-set)
    (is (= (matrix-get-value matrix 0 0) 0))
    (is (= (matrix-get-value matrix 1 0) 1))
    (is (= (matrix-get-value matrix 2 0) 2))))

(deftest select-attributes-test
  (let [attributes ["one" "two" "three" "four"]]
    (is (= (select-attributes attributes 1) ["two"]))
    (is (= (select-attributes attributes [0 2]) ["one" "three"]))
    (is (= (select-attributes attributes [0 :to 3]) ["one" "two" "three" "four"]))
    (is (= (select-attributes attributes [0 :to 1 3]) ["one" "two" "four"]))
    (is (= (select-attributes attributes [:all]) attributes))
    (is (= (select-attributes nil [0 :to 3]) nil))))

(deftest test-svd
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)]
    (println (svd (:data data-set)))))

(deftest count-row-data-set-test
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)]
    (is (= (count-rows data-set) 2))))

(deftest count-column-data-set-test
  (let [data-set (make-data-set "name" "attributes" *test-matrix*)]
    (is (= (count-cols data-set) 3))))
