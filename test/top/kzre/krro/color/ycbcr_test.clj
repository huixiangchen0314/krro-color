
(ns top.kzre.krro.color.ycbcr-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.ycbcr :as ycbcr]
            [top.kzre.krro.color.converter :as conv]))

(deftest bt601-roundtrip
  (let [rgb [0.5 0.5 0.5]
        ycc (ycbcr/rgb->ycbcr rgb :bt601)
        back (ycbcr/ycbcr->rgb ycc :bt601)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest bt709-roundtrip
  (let [rgb [0.2 0.7 0.3]
        ycc (ycbcr/rgb->ycbcr rgb :bt709)
        back (ycbcr/ycbcr->rgb ycc :bt709)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest default-standard
  (let [rgb [0.8 0.1 0.4]
        ycc (ycbcr/rgb->ycbcr rgb)
        back (ycbcr/ycbcr->rgb ycc)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest compare-standards
  (let [rgb [0.3 0.6 0.9]
        ycc601 (ycbcr/rgb->ycbcr rgb :bt601)
        ycc709 (ycbcr/rgb->ycbcr rgb :bt709)]
    (is (not= ycc601 ycc709))))  ;; 不同标准应产生不同值