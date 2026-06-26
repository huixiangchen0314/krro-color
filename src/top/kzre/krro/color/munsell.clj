(ns top.kzre.krro.color.munsell
  "基于 OKLCh 的简化 Munsell 颜色名称查找。
   利用感知均匀的色相角映射到 Munsell 40 色相环，并根据明度和彩度生成描述性名称。"
  (:require [top.kzre.krro.color.converter :as conv]))

;; ── Munsell 40 色相环名称（标准顺序）───────────────
(def ^:private munsell-hues
  ["10RP" "5R" "10R" "5YR" "10YR" "5Y" "10Y" "5GY" "10GY"
   "5G" "10G" "5BG" "10BG" "5B" "10B" "5PB" "10PB" "5P" "10P" "5RP"
   ;; 由于 40 个色相，重复前半部分以覆盖 0-360 度完整映射
   "10RP" "5R" "10R" "5YR" "10YR" "5Y" "10Y" "5GY" "10GY"
   "5G" "10G" "5BG" "10BG" "5B" "10B" "5PB" "10PB" "5P" "10P" "5RP"])

(defn rgb->munsell-name
  "根据 sRGB 颜色返回最接近的 Munsell 色相和近似描述（基于 OKLCh）。"
  [rgb]
  (let [[L C h] (conv/rgb->oklch rgb)
        ;; 提前构造中性色字符串
        neutral (str "Neutral "
                     (cond (< L 0.25) "Black"
                           (< L 0.5)  "Dark Gray"
                           (< L 0.75) "Light Gray"
                           :else       "White"))]
    (if (< C 0.03)                       ;; 彩度极低 → 直接返回中性色
      neutral
      ;; 有彩色部分
      (let [hue-idx (mod (long (Math/floor (/ (+ h (/ 360.0 80.0)) (/ 360.0 40.0)))) 40)
            hue-name (nth munsell-hues hue-idx "5R")
            value-desc (cond (< L 0.25) "Dark"
                             (< L 0.5)  "Medium"
                             (< L 0.75) "Light"
                             :else       "Pale")
            chroma-desc (cond (< C 0.1) "Greyish"
                              (< C 0.2) "Moderate"
                              (< C 0.3) "Strong"
                              :else       "Vivid")]
        (str value-desc " " chroma-desc " " hue-name)))))