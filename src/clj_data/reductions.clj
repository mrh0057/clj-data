(ns clj-data.reductions
  (:use clj-data.core
        [clojure.contrib.math :only [round]])
  (:import [clj_data.core DataSet]))

(defrecord PCAReduction [reduction-matrix starting-number])

(defn make-pca-reduction [reduction-matrix starting-number]
  (PCAReduction. reduction-matrix starting-number))

(defprotocol PCA
  (pca-by-percentage [this percent] "Create a PCAReduction that contains information on how to reduce the data.")
  (pca-by-number [this take] "Create a PCAReduction that contains the information on how to reduce the data."))

(defprotocol DataReduction
  (reduce-data-set [this data-set] "Used to reduce a data set.  
data-set the data set to reduce."))

(extend-type DataSet
  PCA
  (pca-by-number [this take]
    (make-pca-reduction
     (select (:v (svd (:data this))) [:any] [0 :to (dec take)])
     (count-cols this)))
  (pca-by-percentage [this percent]
    (pca-by-number this (round (* percent (count-cols this))))))

(defn- dot-product [pca document]
  (reduce (fn [val pos]
            (+ val (* (get-value-matrix pca 0 pos)
                      (get-value-matrix document pos 0))))
          0 (range 0 (count-rows pca))))

(extend-type PCAReduction
  DataReduction
  (reduce-data-set [this data-set]
    (println (count-cols (:reduction-matrix this)))
    (let [new-matrix (make-dense-double-matrix (count-rows data-set) (count-cols (:reduction-matrix this)))]
      (doseq [d (range 0 (count-rows data-set))]
        (let [document (select (:data data-set) [d] [:any])]
          (println "The document is: " document)
          (doseq [i (range 0 (count-cols (:reduction-matrix this)))]
            (set-value-matrix! new-matrix i d (dot-product (select (:reduction-matrix this) [:any] [i]) document)))))
      (assoc data-set :data new-matrix :attributes (vec (range 0 (count-cols (:reduction-matrix this))))))))
