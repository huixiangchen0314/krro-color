(ns top.kzre.krro.color.oklch
  "OKLCH 颜色空间 (基于 OKLab 的极坐标表示)。"
  (:require [top.kzre.krro.color.oklab :as oklab]
            [top.kzre.krro.color.util :as util]))

(defn oklch
  "创建 OKLCH 颜色向量 [L C h]。L 和 C 范围类似 OKLab，h 为度数 [0,360)。"
  [L C h]
  [L C h])

(defn oklab->oklch
  "将 OKLab 转换为 OKLCH。"
  [[L a b]]
  (let [C (Math/sqrt (+ (* a a) (* b b)))
        h (if (< C 1e-10)
            0.0
            (let [h-rad (Math/atan2 b a)]
              (-> h-rad Math/toDegrees (mod 360))))]
    [L C h]))

(defn oklch->oklab
  "将 OKLCH 转换为 OKLab。"
  [[L C h]]
  (let [h-rad (Math/toRadians h)
        a (* C (Math/cos h-rad))
        b (* C (Math/sin h-rad))]
    [L a b]))