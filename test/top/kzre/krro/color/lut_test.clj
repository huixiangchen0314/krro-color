(ns top.kzre.krro.color.lut-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.lut :as lut]))

(def sample-data
  "2x2x2 LUT 的数据向量。"
  [[0.0 0.0 0.0]
   [1.0 0.0 0.0]
   [0.0 1.0 0.0]
   [1.0 1.0 0.0]
   [0.0 0.0 1.0]
   [1.0 0.0 1.0]
   [0.0 1.0 1.0]
   [1.0 1.0 1.0]])

(def sample-lut
  "一个完整的 2x2x2 恒等 LUT。"
  {:size 2
   :data sample-data})

(deftest generate-lut-test
  (let [generated (lut/generate-lut 2 (fn [r g b] [r g b]))]
    (is (= (:size generated) 2))
    (is (= (:data generated) sample-data))))

(deftest trilinear-interpolation-test
  ;; 使用完整的 LUT map
  (is (= [0.0 0.0 0.0] (lut/apply-lut sample-lut [0.0 0.0 0.0])))
  (is (= [1.0 1.0 1.0] (lut/apply-lut sample-lut [1.0 1.0 1.0])))
  (let [mid (lut/apply-lut sample-lut [0.5 0.5 0.5])]
    (is (every? true? (map #(< (Math/abs (- % 0.5)) 0.01) mid)))))

(deftest load-cube-test
  (is (fn? lut/load-cube)))