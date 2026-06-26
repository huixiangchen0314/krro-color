(ns top.kzre.krro.color.format-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.format :as format]))

(deftest hex-formatting
  (testing "Opaque color (no alpha)"
    (is (= (format/hex [1.0 0.0 0.0]) "#ff0000"))
    (is (= (format/hex [0.0 1.0 0.0]) "#00ff00"))
    (is (= (format/hex [0.0 0.0 1.0]) "#0000ff"))
    (is (= (format/hex [0.5 0.5 0.5]) "#808080")))      ;; 127.5 -> 128
  (testing "With alpha"
    (is (= (format/hex [1.0 0.0 0.0 0.5] true) "#ff000080"))
    (is (= (format/hex [0.2 0.4 0.6 0.3] true) "#3366994d"))) ;; 0.3*255=76.5 -> 77
  (testing "Rounding"
    (is (= (format/hex [0.299 0.0 0.0]) "#4c0000"))))

(deftest rgba-string-test
  (testing "Standard colors"
    (is (= (format/rgba-string [1.0 0.0 0.0 1.0]) "rgba(255,0,0,1.00)"))
    (is (= (format/rgba-string [0.0 0.5 1.0 0.5]) "rgba(0,128,255,0.50)"))) ;; 0.5*255=127.5 -> 128
  (testing "Clamping"
    (is (= (format/rgba-string [1.2 -0.1 0.5 1.0]) "rgba(255,0,128,1.00)")))) ;; 钳制后 [1.0 0.0 0.5 1.0] -> 0.5*255=127.5 -> 128