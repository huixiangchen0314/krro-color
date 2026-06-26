(ns top.kzre.krro.color.icc-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.icc :as icc]
            [top.kzre.krro.color.converter :as conv]))

(deftest builtin-matrix-a2b
  (let [transform (icc/make-icc-transform icc/srgb-icc-data :a2b)
        xyz (transform 0.5 0.5 0.5)]
    (is (every? number? xyz))
    (let [expected (conv/rgb->xyz [0.5 0.5 0.5])]
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-4) xyz expected))))))

(deftest matrix-a2b-b2a-roundtrip
  (let [a2b (icc/make-icc-transform icc/srgb-icc-data :a2b)
        b2a (icc/make-icc-transform icc/srgb-icc-data :b2a)
        c [0.3 0.6 0.9]
        xyz (apply a2b c)
        c' (apply b2a xyz)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 0.01) c c')))))

(deftest lut-a2b-b2a-roundtrip
  (let [icc-data {:a2b0 {:input-channels 3 :output-channels 3 :clut-size 2
                         :input-tables [[] [] []]
                         :output-tables [[] [] []]
                         :clut-values [0 0 0  1 0 0  0 1 0  1 1 0
                                       0 0 1  1 0 1  0 1 1  1 1 1]
                         :matrix nil}
                  :b2a0 {:input-channels 3 :output-channels 3 :clut-size 2
                         :input-tables [[] [] []]
                         :output-tables [[] [] []]
                         :clut-values [0 0 0  1 0 0  0 1 0  1 1 0
                                       0 0 1  1 0 1  0 1 1  1 1 1]
                         :matrix nil}}
        a2b (icc/make-icc-transform icc-data :a2b)
        b2a (icc/make-icc-transform icc-data :b2a)]
    (let [c [0.3 0.6 0.9]
          c' (apply b2a (apply a2b c))]   ;; 使用 apply 解构向量
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 0.01) c c'))))))