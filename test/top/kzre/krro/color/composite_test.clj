(ns top.kzre.krro.color.composite-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.composite :as comp]))

(deftest over-opaque
  (let [bg [0.0 0.0 0.0 1.0]
        fg [1.0 0.0 0.0 1.0]
        result (comp/over bg fg)]
    (is (= [1.0 0.0 0.0 1.0] result))))

(deftest over-transparent
  (let [bg [0.0 0.0 0.0 0.0]
        fg [0.5 0.5 0.5 0.5]
        result (comp/over bg fg)]
    (is (= [0.5 0.5 0.5 0.5] result))))

(deftest in-operation
  (let [bg [0.0 0.0 0.0 0.5]
        fg [1.0 0.0 0.0 1.0]
        result (comp/in bg fg)]
    (is (< (Math/abs (- 0.5 (nth result 3))) 0.01))))

(deftest composite-batch
  (let [pixels (double-array [0.2 0.3 0.4 1.0  0.1 0.2 0.3 0.5])
        source [0.5 0.5 0.5 0.5]
        _ (comp/composite-batch pixels source comp/over)
        ;; 检查第一个像素被正确混合
        ]
    (is (== 0.5 (aget pixels 0)))  ;; 背景 0.2，前景 0.5，alpha 0.5 -> 期望混合结果
    ))