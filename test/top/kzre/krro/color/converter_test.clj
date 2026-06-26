(ns top.kzre.krro.color.converter-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.converter :as conv]))

(deftest rgb-hsl-roundtrip
  (let [rgb [0.2 0.6 0.8]
        hsl (conv/rgb->hsl rgb)
        back (conv/hsl->rgb hsl)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest rgb-hsv-roundtrip
  (let [rgb [0.3 0.1 0.7]
        hsv (conv/rgb->hsv rgb)
        back (conv/hsv->rgb hsv)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest rgb-cmyk-roundtrip
  (let [rgb [0.5 0.5 0.5]
        cmyk (conv/rgb->cmyk rgb)
        back (conv/cmyk->rgb cmyk)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) rgb back)))))

(deftest rgb-xyz-rgb-roundtrip
  (let [rgb [0.1 0.9 0.3]
        xyz (conv/rgb->xyz rgb)
        back (conv/xyz->rgb xyz)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-4) rgb back)))))

(deftest rgb-lab-rgb-roundtrip
  (let [rgb [0.8 0.2 0.5]
        lab (conv/rgb->lab rgb)
        back (conv/lab->rgb lab)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 2e-3) rgb back)))))

(deftest rgb-oklab-rgb-roundtrip
  (let [rgb [0.7 0.3 0.2]
        oklab (conv/rgb->oklab rgb)
        back (conv/oklab->rgb oklab)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-3) rgb back)))))

(deftest rgb-oklch-rgb-roundtrip
  (let [rgb [0.4 0.5 0.6]
        oklch (conv/rgb->oklch rgb)
        back (conv/oklch->rgb oklch)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-3) rgb back)))))

(deftest hsl-hsv-conversions
  (let [hsl [120.0 0.5 0.7]
        hsv (conv/hsl->hsv hsl)
        back (conv/hsv->hsl hsv)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) hsl back)))))

(deftest chromatic-adaptation-roundtrip
  (let [xyz [0.5 0.5 0.5]
        d65->d50->d65 (-> xyz conv/xyz-d65->d50 conv/xyz-d50->d65)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-5) xyz d65->d50->d65)))))

(deftest lab-d50-conversion
  (let [rgb [0.3 0.7 0.2]
        lab-d65 (-> rgb conv/rgb->xyz conv/xyz->lab)
        lab-d50 (conv/rgb->lab-d50 rgb)]
    (is (not= lab-d65 lab-d50))))

(deftest distance-functions-non-negative
  (let [c1 [0.0 0.0 0.0]
        c2 [1.0 1.0 1.0]]
    (is (>= (conv/delta-e76-rgb c1 c2) 0))
    (is (>= (conv/delta-e94-rgb c1 c2) 0))
    (is (>= (conv/delta-e2000-rgb c1 c2) 0))
    (is (> (conv/contrast-ratio-rgb c1 c2) 20.0))))

(deftest edge-cases
  ;; 灰色、黑色、白色等边界
  (is (every? true? (map #(< (Math/abs (- % 0.5)) 1e-6) (conv/hsl->rgb [0 0 0.5]))))
  (is (every? true? (map #(< (Math/abs (- % 0.0)) 1e-6) (conv/hsv->rgb [0 0 0]))))
  ;; 黑色 CMYK 转换应不报错，且结果为合理值
  (let [cmyk-black (conv/rgb->cmyk [0 0 0])]
    (is (every? true? (map #(<= 0.0 % 1.0) cmyk-black))))
  (is (every? true? (map #(< (Math/abs (- % 0.0)) 1e-6) (conv/cmyk->rgb [0 0 0 1]))))
  (let [white-xyz (conv/rgb->xyz [1.0 1.0 1.0])]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-3) white-xyz [0.9505 1.0 1.0890])))))

(deftest p3-conversions
  (let [rgb [0.8 0.2 0.3]
        p3 (-> rgb conv/rgb->xyz conv/xyz->p3)
        back (-> p3 conv/p3->xyz conv/xyz->rgb)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-2) rgb back)))))

(deftest adobe-rgb-conversions
  (let [rgb [0.5 0.5 0.5]
        aRGB (-> rgb conv/rgb->xyz conv/xyz->adobe-rgb)
        back (-> aRGB conv/adobe-rgb->xyz conv/xyz->rgb)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-2) rgb back)))))

(deftest prophoto-conversions
  (let [rgb [0.6 0.4 0.2]
        pro (-> rgb conv/rgb->xyz conv/xyz->prophoto)
        back (-> pro conv/prophoto->xyz conv/xyz->rgb)]
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-2) rgb back)))))

(deftest gray-xyz-roundtrip
  (let [y 0.7
        xyz (conv/gray->xyz y)
        back (conv/xyz->gray xyz)]
    (is (< (Math/abs (- y back)) 1e-10))))

(deftest gray-to-xyz-values
  (let [xyz (conv/gray->xyz 1.0)]
    ;; 白点应等于 D65 白点
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-5) xyz [0.95047 1.0 1.08883])))))