(ns top.kzre.krro.color.rgb
  "RGB 颜色空间操作。"
  (:require [top.kzre.krro.color.util :as util]))

(defn rgb
  "创建 RGB 颜色向量 [r g b]，分量 0-1。"
  ([r g b]
   [r g b])
  ([r g b a]
   [r g b a]))

(defn red
  "获取红色分量。"
  [c]
  (first c))

(defn green
  "获取绿色分量。"
  [c]
  (second c))

(defn blue
  "获取蓝色分量。"
  [c]
  (nth c 2))

(defn alpha
  "获取透明度分量（如果存在，否则返回 1.0）。"
  [c]
  (if (> (count c) 3)
    (nth c 3)
    1.0))

(defn add
  "两个 RGB 颜色逐分量相加（不 clamp）。"
  [c1 c2]
  (mapv + c1 c2))

(defn subtract
  "两个 RGB 颜色逐分量相减（不 clamp）。"
  [c1 c2]
  (mapv - c1 c2))

(defn scale
  "将颜色各分量乘以标量 s。"
  [c s]
  (mapv #(* % s) c))

(defn mix
  "线性混合两个 RGB 颜色，t 在 0-1 之间。"
  [c1 c2 t]
  (let [u (- 1 t)]
    (mapv #(+ (* u %1) (* t %2)) c1 c2)))

(defn invert
  "反转颜色。"
  [c]
  (mapv #(- 1 %) c))

(defn clamp
  "将颜色各分量限制在 [0,1] 内。"
  [c]
  (mapv #(util/clamp 0.0 1.0 %) c))

(defn gamma
  "对颜色应用伽马校正。"
  [c gamma]
  (mapv #(Math/pow % gamma) c))

(defn luminance
  "计算相对亮度 (ITU-R BT.709)。"
  [c]
  (+ (* 0.2126 (red c))
     (* 0.7152 (green c))
     (* 0.0722 (blue c))))

(defn grayscale
  "转换为灰度（使用亮度权重）。"
  [c]
  (let [y (luminance c)]
    [y y y (alpha c)]))