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
  (let [transform (icc/make-icc-transform icc/srgb-icc-data :a2b)]
    (is transform)
    (let [xyz (transform 0.5 0.5 0.5)]
      ;; 期望的 XYZ 值（可由 converter 生成对比）
      (is (every? number? xyz)))))

;; 可增加加载真实 ICC 文件的测试，如果有测试资源的话。