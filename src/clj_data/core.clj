(ns clj-data.core
  (:use clj-data.seq-utils)
  (:import [org.ujmp.core Matrix]
           [org.ujmp.core.matrix AbstractMatrix]
           [org.ujmp.core.doublematrix.impl DefaultDenseDoubleMatrix2D]
           [org.ujmp.core.doublematrix.stub AbstractDenseDoubleMatrix2D]))

(defrecord SVD [u
                s
                v])

(defprotocol MatrixProtocol
  (copy [matrix])
  (svd [matrix])
  (set-value-matrix! [matrix x y value])
  (get-value-matrix [matrix x y])
  (transpose [matrix]))

(defprotocol RowColumnProtocol
  (get-value [this x y])
  (count-rows [this])
  (count-cols [this])
  (select [data-set rows cols]
            "Used to select the contents of the matrix

rows - What rows to select.  To select all the rows use :any.  To select a range use 1 :to 2.  The values are expected to
be in a sequence.
cols - What cols to select. To select all the cols use :any.  To select a range use 1 : to 3. The values are expected to be
in a sequence.

returns A new data set with the selected values."))

(defn create-select-string [selection]
  (if (number? selection)
    (str selection)
    (loop [select-string ""
           previous-val nil
           selection selection]
      (if (empty? selection)
        select-string
        (let [current-val (first selection)]
          (cond (= current-val :to)
                (recur (str select-string "-")
                       current-val
                       (rest selection))
                (= current-val :any)
                (recur (str select-string "*")
                       current-val
                       (rest selection))
                (number? previous-val)
                (recur (str select-string "," current-val)
                       current-val
                       (rest selection)) 
                :else
                (recur (str select-string current-val)
                       current-val
                       (rest selection))))))))

(extend-type AbstractDenseDoubleMatrix2D
  MatrixProtocol
  (copy [matrix]
    (. matrix copy))
  (set-value-matrix! [#^DefaultDenseDoubleMatrix2D matrix #^Long x #^Long y #^Double value]
    (. matrix setAsDouble value y x))
  (svd [matrix]
    (let [svd-matrix (. matrix svd)]
      (SVD. (aget svd-matrix 0)
            (aget svd-matrix 1)
            (aget svd-matrix 2))))
  (get-value-matrix [#^DefaultDenseDoubleMatrix2D matrix #^Long x #^Long y]
    (. matrix getAsDouble y x))
  (transpose [matrix]
    (. matrix transpose))
  RowColumnProtocol
  (get-value [matrix x y]
    (get-value-matrix matrix x y))
  (count-rows [this]
    (. this getRowCount))
  (count-cols [this]
    (. this getColumnCount))
  (select [matrix rows cols]
    (. matrix select org.ujmp.core.calculation.Calculation$Ret/NEW
                 (str (create-select-string rows) ";"
                      (create-select-string cols)))))

(defn make-dense-double-matrix 
  "Creates a dense double matrix.

rows - The number of rows
cols - The number of cols

returns The matrix representing thoses deminisions"
  [rows cols]
  (new DefaultDenseDoubleMatrix2D (long rows) (long cols)))

(defrecord DataSet
    [name
     attributes
     row-labels
     classifications
     #^AbstractMatrix data])

(defn- create-vector [value]
  (if (or (nil? value) (vector? value))
    value
    (vec value)))

(defn make-data-set-my-matrix 
  "Used to create a dataset with your on specified matrix.

name - Is the name of the data set
attributes - The list of attributes for the data set
row-labels - The row labels for the data set.
classifications - The classifications for the data set
data - The matrix for the data set.  It must be a DefaultDenseDoubleMatrix2d for now."
  [name attributes row-labels classifications data]
 (DataSet. name
           (create-vector attributes)
           (create-vector row-labels)
           (create-vector classifications) data))

(defn- get-value-from-vec [x y data]
  (get (get data y) x))

(defn select-attributes [attributes selection]
  (cond (not attributes)
        nil
        (number? selection)
        [(nth attributes selection)]
        (= (first selection) :all)
        attributes
        :else
        (loop [vals '()
               previous-val nil
               selection selection]
          (if (empty? selection)
            (vec (reverse (flatten vals)))
            (let [current-val (first selection)]
              (cond (= :to current-val)
                    (recur (cons (reverse (take (second selection) (drop (inc previous-val) attributes))) vals)
                           (second vals)
                           (drop 2 selection))
                    :else
                    (recur (cons (nth attributes current-val) vals)
                           current-val
                           (rest selection))))))))

(extend-type DataSet
  RowColumnProtocol
  (get-value [data-set x y]
    (get-value-matrix (:data data-set) x y))
  (count-cols [data-set]
    (count-cols (:data data-set)))
  (count-rows [data-set]
    (count-rows (:data data-set)))
  (select [data-set rows cols]
    (DataSet. (:name data-set)
              (select-attributes (:attributes data-set) cols)
              (select-attributes (:row-labels data-set) rows)
              (select-attributes (:classifications data-set) rows)
              (select (:data data-set) rows cols))))

(defn- create-matrix [data options]
  (let [rows (count data)
        cols (count (first data))
        matrix (make-dense-double-matrix rows cols)
        get-value (fn [x y]
                    (double (get-value-from-vec x y data)))
        x-check (fn [x] (= x (- cols 1)))
        y-check (fn [y] (= y (- rows 1)))
        set-value (fn [#^Double value #^Long x #^Long y]
                    (set-value-matrix! matrix x y value))]
    (loop [x (long 0)
           y (long 0)]
      (if (and (y-check y) (x-check x))
        (do
          (set-value (get-value x y) x y)
          matrix)
        (do
          (set-value (get-value x y) x y)
          (recur
           (if (x-check x)
             (long 0)
             (inc x))
           (if (x-check x)
             (inc y)
             y)))))))

(defn make-data-set 
  "Used to createa a data set.  Data sets are expected to be numbers.  If you have strings in the
dataset they need to be converted to numbers.

name - The name for the dataset
attributes - The attributes for the dataset
data - A multidemisional vector contains the data for the dataset.
options - The options 

:row-labels - Is the row-labels for the data set.
:matrix-type - :sparse for a sparse matrix. :dense for a dense matrix.  Defaults to dense matrix.
:data-type - :double, :int, :long, :boolean, :biginter, bigdemical. Defaults to :double
:matrix - The matrix that stores your data. Current only DefaultDenseDoubleMatrix2D is supported"
  [name attributes data & options]
  (let [opts (reduce-by-2 (fn [opts name value]
                            (assoc opts name value)) {} options)]
    (DataSet. name attributes (:row-labels opts) (:classifications opts) (if (contains? opts :matrix)
                                                                           (:matrix opts)
                                                                           (create-matrix data opts)))))
