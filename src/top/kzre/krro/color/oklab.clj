(ns top.kzre.krro.color.oklab
  "OKLab 颜色空间 (Björn Ottosson, 2020)。"
  (:require [top.kzre.krro.color.util :as util])
  (:import (java.lang Math)))

;; ── 前向矩阵 ────────────────────────────────────────────
(def ^:private m1
  [[0.4122214708 0.5363325363 0.0514459929]
   [0.2119034982 0.6806995451 0.1073969566]
   [0.0883024619 0.2817188376 0.6299787005]])

(def ^:private m2
  [[ 0.2104542553  0.7936177850 -0.0040720468]
   [ 1.9779984951 -2.4285922050  0.4505937099]
   [ 0.0259040371  0.7827717662 -0.8086757660]])

;; ── 逆矩阵（官方确认）────────────────────────────────────
(def ^:private m2-inv
  [[ 1.0,          0.3963377774,  0.2158037573]
   [ 1.0,         -0.1055613458, -0.0638541728]
   [ 1.0,         -0.0894841775, -1.2914855480]])

(def ^:private m1-inv
  [[ 4.0767416621 -3.3077115913  0.2309699292]
   [-1.2684380046  2.6097574011 -0.3413193965]
   [-0.0041960863 -0.7034186147  1.7076147010]])

;; ── 矩阵运算 ────────────────────────────────────────────
(defn- matrix-mult
  [m v]
  (mapv #(apply + (map * % v)) m))

(defn- cubic-root [t]
  (let [t (double t)]
    (if (neg? t)
      (- (Math/cbrt (- t)))       ; 正确处理负数立方根
      (Math/cbrt t))))

(defn- linear->oklab
  [[r g b]]
  (let [lms (matrix-mult m1 [r g b])
        lms' (mapv cubic-root lms)
        [L a b] (matrix-mult m2 lms')]
    [L a b]))

(defn- oklab->linear
  [[L a b]]
  (let [lms' (matrix-mult m2-inv [L a b])
        lms (mapv #(* % % %) lms')
        [r g b] (matrix-mult m1-inv lms)]
    ;; 钳制微小的浮点负值，避免后续 gamma 校正异常
    (mapv #(max 0.0 %) [r g b])))

;; ── 公开 API ────────────────────────────────────────────
(defn linear-rgb->oklab
  "从线性 RGB 向量转换到 OKLab。"
  [c]
  (linear->oklab c))

(defn oklab->linear-rgb
  "从 OKLab 转换回线性 RGB。"
  [c]
  (oklab->linear c))