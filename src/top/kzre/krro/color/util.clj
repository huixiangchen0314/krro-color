(ns top.kzre.krro.color.util
  "内部工具函数，提供数学辅助运算。")

(defn clamp
  "将 x 限制在 [lo, hi] 范围内。"
  [lo hi x]
  (max lo (min hi x)))

(defn deg->rad
  "角度转弧度。"
  [deg]
  (* deg (/ Math/PI 180.0)))

(defn rad->deg
  "弧度转角度。"
  [rad]
  (* rad (/ 180.0 Math/PI)))

(defn linear->srgb
  "将线性 RGB 分量转换为 sRGB 伽马校正分量。"
  [c]
  (if (<= c 0.0031308)
    (* 12.92 c)
    (- (* 1.055 (Math/pow c (/ 1.0 2.4))) 0.055)))

(defn srgb->linear
  "将 sRGB 伽马校正分量转换为线性 RGB 分量。"
  [c]
  (if (<= c 0.04045)
    (/ c 12.92)
    (Math/pow (/ (+ c 0.055) 1.055) 2.4)))