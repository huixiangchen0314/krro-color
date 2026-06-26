(ns top.kzre.krro.color.distance
  "颜色距离算法：ΔE76, ΔE94, ΔE2000 及 WCAG 对比度。"
  (:require [top.kzre.krro.color.util :as util]
            [top.kzre.krro.color.rgb :as rgb])
  (:import (java.lang Math)))

;; ── ΔE76 (CIE76) ────────────────────────────────────────
(defn delta-e76
  "计算两个 CIELAB 颜色之间的欧氏距离。"
  [[L1 a1 b1] [L2 a2 b2]]
  (Math/sqrt (+ (Math/pow (- L1 L2) 2)
                (Math/pow (- a1 a2) 2)
                (Math/pow (- b1 b2) 2))))

;; ── ΔE94 ────────────────────────────────────────────────
(def ^:private kL 1.0)
(def ^:private kC 1.0)
(def ^:private kH 1.0)
(def ^:private k1 0.045)
(def ^:private k2 0.015)

(defn delta-e94
  "计算两个 CIELAB 颜色之间的 ΔE94 距离。"
  [[L1 a1 b1] [L2 a2 b2]]
  (let [dL (- L1 L2)
        C1 (Math/sqrt (+ (* a1 a1) (* b1 b1)))
        C2 (Math/sqrt (+ (* a2 a2) (* b2 b2)))
        dC (- C1 C2)
        da (- a1 a2)
        db (- b1 b2)
        dH2 (- (+ (* da da) (* db db)) (* dC dC))
        dH (if (>= dH2 0) (Math/sqrt dH2) 0.0)
        SL 1.0
        SC (+ 1 (* k1 C1))
        SH (+ 1 (* k2 C1))]
    (Math/sqrt (+ (Math/pow (/ dL (* kL SL)) 2)
                  (Math/pow (/ dC (* kC SC)) 2)
                  (Math/pow (/ dH (* kH SH)) 2)))))

;; ── ΔE2000 ──────────────────────────────────────────────
(defn- deg->rad [deg] (* deg (/ Math/PI 180.0)))
(defn- rad->deg [rad] (* rad (/ 180.0 Math/PI)))

(defn delta-e2000
  "计算两个 CIELAB 颜色之间的 ΔE2000 距离。"
  [[L1 a1 b1] [L2 a2 b2]]
  (let [L' (/ (+ L1 L2) 2.0)
        C1 (Math/sqrt (+ (* a1 a1) (* b1 b1)))
        C2 (Math/sqrt (+ (* a2 a2) (* b2 b2)))
        C' (/ (+ C1 C2) 2.0)
        G (let [t (* C' 7.0)]
            (* 0.5 (- 1.0 (Math/sqrt (/ (* t t t) (+ (* t t t) 6103515625.0))))))
        a1' (* a1 (+ 1.0 G))
        a2' (* a2 (+ 1.0 G))
        C1' (Math/sqrt (+ (* a1' a1') (* b1 b1)))
        C2' (Math/sqrt (+ (* a2' a2') (* b2 b2)))   ;; 此处补全括号
        h1' (if (and (zero? a1') (zero? b1))
              0.0
              (rad->deg (Math/atan2 b1 a1')))
        h2' (if (and (zero? a2') (zero? b2))
              0.0
              (rad->deg (Math/atan2 b2 a2')))
        dL' (- L2 L1)
        dC' (- C2' C1')
        dh' (cond
              (and (zero? C1') (zero? C2')) 0.0
              (<= (Math/abs (- h2' h1')) 180.0) (- h2' h1')
              (> h2' h1') (- h2' h1' 360.0)
              :else (- h2' h1' 360.0))
        dH' (* 2.0 (Math/sqrt (* C1' C2')) (Math/sin (/ (deg->rad dh') 2.0)))
        L'' (/ (+ L1 L2) 2.0)
        C'' (/ (+ C1' C2') 2.0)
        h'' (let [h (if (<= (Math/abs (- h1' h2')) 180.0)
                      (/ (+ h1' h2') 2.0)
                      (if (< (+ h1' h2') 360.0)
                        (/ (+ h1' h2' 360.0) 2.0)
                        (/ (+ h1' h2' -360.0) 2.0)))]
              h)
        T (- 1.0
             (* 0.17 (Math/cos (deg->rad (- h'' 30.0))))
             (* 0.24 (Math/cos (deg->rad (* 2.0 h''))))
             (* 0.32 (Math/cos (deg->rad (+ (* 3.0 h'') 6.0))))
             (* 0.20 (Math/cos (deg->rad (- (* 4.0 h'') 63.0)))))
        SL (+ 1.0 (/ (* 0.015 (Math/pow (- L'' 50.0) 2))
                     (Math/sqrt (+ 20.0 (Math/pow (- L'' 50.0) 2)))))
        SC (+ 1.0 (* 0.045 C''))
        SH (+ 1.0 (* 0.015 C'' T))
        RT (let [theta (- 30.0 (Math/exp (* -1.0 (Math/pow (/ (- h'' 275.0) 25.0) 2.0))))]
             (* -2.0 (Math/sqrt (/ (Math/pow C'' 7.0) (+ (Math/pow C'' 7.0) (Math/pow 25.0 7.0))))
                (Math/sin (deg->rad (* 60.0 theta)))))
        kL 1.0 kC 1.0 kH 1.0]
    (Math/sqrt (+ (Math/pow (/ dL' (* kL SL)) 2.0)
                  (Math/pow (/ dC' (* kC SC)) 2.0)
                  (Math/pow (/ dH' (* kH SH)) 2.0)
                  (* RT (/ dC' (* kC SC)) (/ dH' (* kH SH)))))))

;; ── WCAG 对比度 ─────────────────────────────────────────
(defn relative-luminance
  "计算 sRGB 颜色的相对亮度 (WCAG 2.1)。"
  [c]
  (let [f (fn [x] (if (<= x 0.03928)
                    (/ x 12.92)
                    (Math/pow (/ (+ x 0.055) 1.055) 2.4)))]
    (+ (* 0.2126 (f (rgb/red c)))
       (* 0.7152 (f (rgb/green c)))
       (* 0.0722 (f (rgb/blue c))))))

(defn contrast-ratio
  "计算两个 sRGB 颜色之间的 WCAG 对比度。"
  [c1 c2]
  (let [l1 (relative-luminance c1)
        l2 (relative-luminance c2)
        lighter (max l1 l2)
        darker (min l1 l2)]
    (/ (+ lighter 0.05) (+ darker 0.05))))