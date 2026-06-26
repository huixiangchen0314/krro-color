(ns top.kzre.krro.color.blend-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.blend :as blend]))

(deftest multiply-test
  (is (= [0.25 0.5 0.75] (blend/multiply [0.5 1.0 1.0] [0.5 0.5 0.75]))))

(deftest screen-test
  (let [result (blend/screen [0.2 0.8 0.3] [0.4 0.1 0.6])]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest overlay-test
  (let [result (blend/overlay [0.3 0.6 0.9] [0.2 0.7 0.4])]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest hard-light-test
  (is (= (blend/hard-light [0.2 0.5 0.8] [0.6 0.3 0.1])
         (blend/overlay [0.6 0.3 0.1] [0.2 0.5 0.8]))))

(deftest color-dodge-test
  (is (= [1.0 1.0 1.0] (blend/color-dodge [1.0 1.0 1.0] [1.0 1.0 1.0])))
  (is (every? #(<= 0.0 % 1.0) (blend/color-dodge [0.5 0.5 0.5] [0.0 0.0 0.0]))))

(deftest color-burn-test
  (is (= [0.0 0.0 0.0] (blend/color-burn [0.0 0.0 0.0] [1.0 1.0 1.0])))
  (is (every? #(<= 0.0 % 1.0) (blend/color-burn [0.2 0.6 0.3] [0.8 0.9 0.4]))))

(deftest darken-lighten-test
  (is (= [0.2 0.5 0.1] (blend/darken [0.2 0.5 0.3] [0.8 0.7 0.1])))
  (is (= [0.8 0.7 0.3] (blend/lighten [0.2 0.5 0.3] [0.8 0.7 0.1]))))

(deftest difference-test
  (let [result (blend/difference [0.5 0.5 0.5] [0.3 0.7 0.3])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-15) result [0.2 0.2 0.2])))))

(deftest exclusion-test
  (let [result (blend/exclusion [0.5 0.5 0.5] [0.5 0.5 0.5])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-15) result [0.5 0.5 0.5])))))

(deftest linear-burn-test
  (is (= [0.0 0.0 0.0] (blend/linear-burn [0.5 0.5 0.5] [0.5 0.5 0.5]))))

(deftest linear-dodge-test
  (is (= [1.0 0.8 0.6] (blend/linear-dodge [0.5 0.5 0.5] [0.5 0.3 0.1]))))

(deftest blend-dispatch-test
  (is (= (blend/multiply [0.5 0.6 0.7] [0.8 0.3 0.2])
         (blend/blend [0.5 0.6 0.7] [0.8 0.3 0.2] :multiply)))
  (is (thrown? IllegalArgumentException (blend/blend [0 0 0] [0 0 0] :invalid-mode))))

(deftest blend-with-alpha-test
  (let [bg [0.2 0.3 0.4 0.5]
        fg [0.8 0.1 0.2 0.6]
        result (blend/blend-with-alpha bg fg :normal)]
    (is (= 4 (count result)))
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest blend-with-alpha-multiply-test
  (let [bg [0.5 0.5 0.5 1.0]
        fg [0.5 0.5 0.5 0.5]
        result (blend/blend-with-alpha bg fg :multiply)]
    (is (every? #(<= 0.0 % 1.0) result))))

(deftest edge-cases
  (doseq [mode [:normal :multiply :screen :overlay :hard-light :soft-light
                :color-dodge :color-burn :darken :lighten :difference :exclusion
                :linear-light :vivid-light :pin-light :hard-mix :subtract :divide]]
    (let [r (blend/blend [0.0 0.0 0.0] [1.0 1.0 1.0] mode)]
      (is (vector? r)))))

(deftest blend-extreme-values
  (is (vector? (blend/screen [0.5 0.5 0.5] [-0.1 1.2 0.5])))
  (is (vector? (blend/multiply [0.5 0.5 0.5] [0.0 0.0 0.0])))
  (is (vector? (blend/overlay [0.5 0.5 0.5] [1.5 -0.2 0.8])))
  (is (vector? (blend/linear-light [0.5 0.5 0.5] [2.0 -0.5 0.7])))
  (is (vector? (blend/vivid-light [0.8 0.8 0.8] [-0.3 1.2 0.2])))
  (is (vector? (blend/pin-light [0.3 0.3 0.3] [2.0 -1.0 0.9]))))