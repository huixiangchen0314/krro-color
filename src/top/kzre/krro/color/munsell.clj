(ns top.kzre.krro.color.munsell
  "简化的 Munsell 颜色名称查找。"
  (:require [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.distance :as dist]))

(def ^:private munsell-hues
  ;; 40 个 Munsell 标准色相
  ["10RP" "5R" "10R" "5YR" "10YR" "5Y" "10Y" "5GY" "10GY"
   "5G" "10G" "5BG" "10BG" "5B" "10B" "5PB" "10PB" "5P" "10P" "5RP"])

(defn rgb->munsell-name
  "根据 sRGB 颜色返回最接近的 Munsell 色相和近似名称（简化）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl)
        l (nth hsl 2)
        s (second hsl)
        ;; 将 HSL 色相映射到最近的 Munsell 色相（0-360 映射到 0-19）
        hue-idx (mod (int (/ (+ h 18) 36)) 20)
        hue-name (nth munsell-hues hue-idx "5R")
        ;; 简化亮度范围描述
        value-desc (cond (< l 0.25) "Dark"
                         (< l 0.5)  "Medium"
                         (< l 0.75) "Light"
                         :else "Pale")
        ;; 简化彩度描述
        chroma-desc (cond (< s 0.2) "Greyish"
                          (< s 0.5) "Moderate"
                          (< s 0.8) "Strong"
                          :else "Vivid")]
    (str value-desc " " chroma-desc " " hue-name)))