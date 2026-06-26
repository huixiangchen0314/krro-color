(ns top.kzre.krro.color.oklab
  "OKLab 颜色空间 (Björn Ottosson, 2020)。"
  (:require [top.kzre.krro.color.util :as util])
  (:import (java.lang Math)))

;; 线性 sRGB -> LMS 矩阵
(def ^:private m1
  [[0.4122214708 0.5363325363 0.0514459929]
   [0.2119034982 0.6806995451 0.1073969566]
   [0.0883024619 0.2817188376 0.6299787005]])

;; LMS' -> OKLab 矩阵
(def ^:private m2
  [[ 0.2104542553  0.7936177850 -0.0040720468]
   [ 1.9779984951 -2.4285922050  0.4505937099]
   [ 0.0259040371  0.7827717662 -0.8086757660]])

;; OKLab -> LMS' 矩阵 (逆矩阵)
(def ^:private m2-inv
  [[ 1.0         1.0         1.0       ]
   [ 0.3963377774 -0.1055613458 -0.0894841775]
   [ 0.2158037573 -0.0638541728 -1.2914855480]])

;; LMS' -> 线性 sRGB 矩阵 (逆矩阵)
(def ^:private m1-inv
  [[ 4.0767416621 -3.3077115913  0.2309699292]
   [-1.2684380046  2.6097574011 -0.3413193965]
   [-0.0041960863 -0.7034186147  1.7076147010]])

(defn- matrix-mult
  [m v]
  (mapv #(apply + (map * % v)) m))

(defn- cubic-root [t]
  (let [t   (double t)
        abs (Math/abs t)]
    (double (* (Math/signum t) (Math/cbrt abs)))))

(defn- linear->oklab
  "将线性 sRGB 转换为 OKLab。"
  [[r g b]]
  (let [lms (matrix-mult m1 [r g b])
        lms' (mapv cubic-root lms)
        [L a b] (matrix-mult m2 lms')]
    [L a b]))

(defn- oklab->linear
  "将 OKLab 转换为线性 sRGB。"
  [[L a b]]
  (let [lms' (matrix-mult m2-inv [L a b])
        lms (mapv #(* % % %) lms')   ; 立方
        [r g b] (matrix-mult m1-inv lms)]
    [r g b]))

(defn linear-rgb->oklab
  "从线性 RGB 向量转换到 OKLab。"
  [c]
  (linear->oklab c))

(defn oklab->linear-rgb
  "从 OKLab 转换回线性 RGB。"
  [c]
  (oklab->linear c))