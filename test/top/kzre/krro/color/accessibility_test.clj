(ns top.kzre.krro.color.accessibility-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.accessibility :as acc]))

(deftest color-blind-simulation
  (is (vector? (acc/simulate-color-blindness [1.0 0.0 0.0] :protanopia)))
  (is (vector? (acc/simulate-color-blindness [0.5 0.5 0.5] :deuteranopia)))
  (is (vector? (acc/simulate-color-blindness [0.2 0.8 0.3] :tritanopia))))

(deftest wcag-contrast
  (is (> (acc/contrast-ratio [0.0 0.0 0.0] [1.0 1.0 1.0]) 4.5)))