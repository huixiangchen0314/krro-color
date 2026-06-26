(ns top.kzre.krro.color.interpolate
  "颜色插值算法。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.hsl :as hsl]
            [top.kzre.krro.color.converter :as conv]))

(defn lerp
  "线性插值两个颜色（在 RGB 空间）。"
  [c1 c2 t]
  (rgb/mix c1 c2 t))

(defn lerp-hsl
  "在 HSL 空间线性插值（色相走短路径）。"
  [c1 c2 t]
  (let [hsl1 (conv/rgb->hsl c1)
        hsl2 (conv/rgb->hsl c2)
        blended (hsl/mix hsl1 hsl2 t)]
    (conv/hsl->rgb blended)))

(defn gradient
  "生成从 c1 到 c2 的 n 个均匀插值颜色（含两端）。"
  [c1 c2 n]
  (if (<= n 2)
    [c1 c2]
    (let [step (/ 1.0 (dec n))]
      (mapv #(lerp c1 c2 (* step %)) (range n)))))