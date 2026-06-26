(ns top.kzre.krro.color.hsl
  "HSL 颜色空间操作。"
  (:require [top.kzre.krro.color.util :as util]))

(defn hsl
  "创建 HSL 颜色向量 [h s l]，h 为度数 [0,360)，s,l 在 0-1 之间。"
  [h s l]
  [h s l])

(defn hue
  "获取色相。"
  [c]
  (first c))

(defn saturation
  "获取饱和度。"
  [c]
  (second c))

(defn lightness
  "获取亮度。"
  [c]
  (nth c 2))

(defn mix
  "线性混合两个 HSL 颜色（色相通过短路径旋转）。"
  [c1 c2 t]
  (let [h1 (hue c1) h2 (hue c2)
        ;; 短路径色相插值
        dh (- h2 h1)
        dh (if (> (Math/abs dh) 180.0) (- dh (* 360.0 (Math/signum dh))) dh)
        h (+ h1 (* t dh))
        h (if (< h 0) (+ h 360) (if (>= h 360) (- h 360) h))]
    [h
     (+ (* (- 1 t) (saturation c1)) (* t (saturation c2)))
     (+ (* (- 1 t) (lightness c1)) (* t (lightness c2)))]))