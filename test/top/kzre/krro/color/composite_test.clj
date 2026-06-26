;; composite_test.clj
(ns top.kzre.krro.color.composite-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.composite :as comp]))

(deftest over-opaque
  (is (= [1.0 0.0 0.0 1.0] (comp/over [0.0 0.0 0.0 1.0] [1.0 0.0 0.0 1.0]))))

(deftest over-semi-transparent
  (let [result (comp/over [0.0 0.0 0.0 1.0] [0.5 0.5 0.5 0.5])]
    ;; 预乘 over: 0.5*0.5 / 1.0 = 0.25, alpha 1.0
    (is (= [0.25 0.25 0.25 1.0] result))))

(deftest in-test
  (is (= [0.0 0.0 0.0 0.0] (comp/in [0.0 0.0 0.0 0.0] [1.0 0.0 0.0 1.0])))
  (is (= [1.0 0.0 0.0 0.5] (comp/in [0.0 0.0 0.0 0.5] [1.0 0.0 0.0 1.0]))))

(deftest all-operations
  (doseq [op [:clear :source :dest :over :in :out :atop :dest-over :dest-in :dest-out :dest-atop :xor :lighter]]
    (let [result (comp/composite [0.2 0.3 0.4 0.5] [0.6 0.7 0.8 0.9] op)]
      (is (vector? result))
      (is (= 4 (count result))))))

(deftest batch-composite
  (let [pixels (double-array [0.2 0.3 0.4 1.0  0.1 0.2 0.3 0.5])
        source [0.5 0.5 0.5 0.5]
        _ (comp/composite-batch pixels source :over)]
    ;; 第一个像素：[0.2 0.3 0.4 1.0] over [0.5 0.5 0.5 0.5] => [0.35 0.35 0.35 1.0]
    (is (== 0.35 (aget pixels 0)))))

(deftest blend-group-test
  (let [bg [0.0 0.0 0.0 1.0]
        layers [[1.0 0.0 0.0 0.5] [0.0 1.0 0.0 0.5]]
        result (comp/blend-group bg layers :mode :over)]
    (is (= 4 (count result)))
    (is (every? #(<= 0.0 % 1.0) result))))