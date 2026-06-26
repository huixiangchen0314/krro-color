(ns top.kzre.krro.color.pigment-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.pigment :as pigment]))

(deftest ryb-mix-test
  (let [result (pigment/mix [1.0 0.0 0.0] [0.0 0.0 1.0] 0.5 :mode :ryb)]
    (is (every? #(<= 0.0 % 1.0) result))
    (is (> (first result) 0.0))))

(deftest kubelka-munk-two-mix
  (let [result (pigment/mix :titanium-white :cadmium-red 0.5 :mode :kubelka-munk)]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest custom-lib
  (let [lib {:a {:K [0.5 0.5 0.5] :S [0.5 0.5 0.5]}
             :b {:K [0.1 0.1 0.1] :S [0.9 0.9 0.9]}}
        result (pigment/mix :a :b 0.3 :mode :kubelka-munk :pigment-lib lib)]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest multi-mix
  (let [result (pigment/mix [[:titanium-white 2] [:ultramarine 1]] 0 0 :mode :kubelka-munk-multiple)]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest saunderson
  (let [corrected (pigment/saunderson-correct [0.5 0.5 0.5])]
    (is (every? #(>= % 0.0) corrected))))

(deftest unknown-pigment-throws
  (is (thrown? Exception (pigment/mix :foo :bar 0.5 :mode :kubelka-munk))))