(ns top.kzre.krro.color.icc-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.icc :as icc]
            [top.kzre.krro.color.converter :as conv]))

(deftest parse-builtin-srgb
  (let [data icc/srgb-icc-data]
    (is (:white-point data))
    (is (:rXYZ data))
    (is (:gXYZ data))
    (is (:bXYZ data))
    (is (:rTRC data))))

(deftest matrix-transform
  (let [transform (icc/make-icc-transform icc/srgb-icc-data :a2b)
        xyz (transform 0.5 0.5 0.5)]
    (is (every? number? xyz))
    ;; 与 converter 结果对比（允许少量误差）
    (let [expected (conv/rgb->xyz [0.5 0.5 0.5])]
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-4) xyz expected))))))

(deftest lut-transform-smoke
  ;; 构造一个简单的 LUT 数据用于烟雾测试
  (let [icc-data {:a2b0 {:input-channels 3
                         :output-channels 3
                         :clut-size 2
                         :input-tables [[] [] []]
                         :output-tables [[] [] []]
                         :clut-values [0 0 0   0 0 1   0 1 0   0 1 1
                                       1 0 0   1 0 1   1 1 0   1 1 1]
                         :matrix nil}}
        transform (icc/make-icc-transform icc-data :a2b)]
    ;; 输入 0,0,0 -> 期望输出 0,0,0
    (is (= [0 0 0] (transform 0 0 0)))
    ;; 输入 1,1,1 -> 期望输出 1,1,1
    (is (= [1 1 1] (transform 1 1 1)))
    ;; 输入 0.5,0.5,0.5 -> 应在 0.5 左右
    (let [mid (transform 0.5 0.5 0.5)]
      (is (every? #(< (Math/abs (- % 0.5)) 0.1) mid)))))