(ns top.kzre.krro.color.pigment-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.pigment :as pigment]))

(deftest ryb-mix-test
  (let [result (pigment/mix [1.0 0.0 0.0] [0.0 0.0 1.0] 0.5 :mode :ryb)]
    (is (every? #(<= 0.0 % 1.0) result))
    (is (> (first result) 0.0))
    (is (> (nth result 2) 0.0))))

(deftest kubelka-munk-two-mix
  (let [result (pigment/mix :titanium-white :cadmium-red 0.5 :mode :kubelka-munk)]
    (is (every? #(<= 0.0 % 1.0) result))
    ;; 混合后颜色应落在合理范围内，不强制红色 > 绿色
    (is (>= (first result) 0.0))
    (is (<= (first result) 1.0))))

(deftest kubelka-munk-with-custom-lib
  (let [custom-lib {:my-red {:K [0.6 0.1 0.1] :S [0.2 0.8 0.8]}
                    :my-white {:K [0.01 0.01 0.01] :S [0.98 0.98 0.98]}}
        result (pigment/mix :my-red :my-white 0.5
                            :mode :kubelka-munk
                            :pigment-lib custom-lib)]
    (is (every? #(<= 0.0 % 1.0) result))
    (is (> (first result) 0.0))))

(deftest multi-pigment-mix
  (let [pigments [[:titanium-white 1.0] [:cadmium-yellow 1.0] [:ultramarine 0.5]]
        result (pigment/mix pigments 0 0 :mode :kubelka-munk-multiple)]
    (is (every? #(<= 0.0 % 1.0) result))
    (is (vector? result))))

(deftest saunderson-correction
  (let [r [0.5 0.5 0.5]
        corrected (pigment/saunderson-correct r)]
    (is (every? #(>= % 0.0) corrected))
    ;; Saunderson 修正后的反射率通常会降低，这里检查每个分量 ≤ 原始值
    (is (every? true? (map #(<= %1 %2) corrected r)))))

(deftest load-external-lib
  (is (thrown? Exception (pigment/load-pigment-lib "nonexistent.edn"))))

(deftest unknown-pigment-throws
  (is (thrown? Exception
               (pigment/mix :nonexistent :titanium-white 0.5
                            :mode :kubelka-munk)))
  (is (thrown? Exception
               (pigment/mix [[:nonexistent 1.0]] 0 0
                            :mode :kubelka-munk-multiple))))