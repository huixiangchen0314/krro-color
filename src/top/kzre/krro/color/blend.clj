(ns top.kzre.krro.color.blend
  "混合模式实现，参考 PS 标准公式。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

(defn normal
  "正常混合（上覆盖下）。"
  [backdrop source]
  source)

(defn dissolve
  "溶解模式（需 alpha 支持，此处简化返回 source）。"
  [backdrop source]
  source) ;; 完整实现需要透明度随机

(defn multiply
  "正片叠底。"
  [backdrop source]
  (mapv * backdrop source))

(defn screen
  "滤色。"
  [backdrop source]
  (mapv #(- 1 (* (- 1 %1) (- 1 %2))) backdrop source))

(defn overlay
  "叠加。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= b 0.5)
            (* 2 b s)
            (- 1 (* 2 (- 1 b) (- 1 s)))))
        backdrop source))

(defn hard-light
  "强光。"
  [backdrop source]
  (overlay source backdrop))

(defn soft-light
  "柔光。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (- b (* b (- 1 (* 2 s)) (- 1 b)))
            (let [d (if (<= b 0.25)
                      (/ (- (* 16 b 12 b) 4) (- 4 b))
                      (Math/sqrt b))]
              (+ b (* (- d b) (- 1 (* 2 s)))))))
        backdrop source))

(defn color-dodge
  "颜色减淡。"
  [backdrop source]
  (mapv (fn [b s]
          (if (zero? s)
            b
            (util/clamp 0.0 1.0 (/ b (- 1 s)))))
        backdrop source))

(defn color-burn
  "颜色加深。"
  [backdrop source]
  (mapv (fn [b s]
          (if (= s 1.0)
            1.0
            (util/clamp 0.0 1.0 (- 1 (/ (- 1 b) s)))))
        backdrop source))

(defn darken
  "变暗。"
  [backdrop source]
  (mapv min backdrop source))

(defn lighten
  "变亮。"
  [backdrop source]
  (mapv max backdrop source))

(defn difference
  "差值。"
  [backdrop source]
  (mapv (fn [b s] (Math/abs (- b s))) backdrop source))

(defn exclusion
  "排除。"
  [backdrop source]
  (mapv #(+ %1 %2 (- (* 2 %1 %2))) backdrop source))



(defn linear-light
  "线性光： = linear-dodge + linear-burn（依 source 明暗）。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (color-burn b (* 2 s))
            (color-dodge b (* 2 (- s 0.5)))))
        backdrop source))

(defn vivid-light
  "亮光： = color-dodge + color-burn（依 source 明暗）。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (color-burn b (* 2 s))
            (color-dodge b (* 2 (- s 0.5)))))
        backdrop source))

(defn pin-light
  "点光： = darken + lighten 组合。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (darken b (* 2 s))
            (lighten b (* 2 (- s 0.5)))))
        backdrop source))

(defn hard-mix
  "实色混合：硬阈值混合，结果分量要么 0 要么 1。"
  [backdrop source]
  (mapv (fn [b s]
          (if (<= (+ b s) 1.0) 0.0 1.0))
        backdrop source))

(defn subtract
  "减去：从基色减去源色。"
  [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (- %1 %2)) backdrop source))

(defn divide
  "划分：基色除以源色（源色接近零时结果趋于 1）。"
  [backdrop source]
  (mapv (fn [b s]
          (if (zero? s)
            1.0
            (util/clamp 0.0 1.0 (/ b s))))
        backdrop source))


;; 线性加深
(defn linear-burn
  [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (+ %1 %2 -1.0)) backdrop source))

;; 线性减淡
(defn linear-dodge
  [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (+ %1 %2)) backdrop source))

;; 亮光 (Vivid Light) — 已存在但公式可能不标准，我们重新实现标准版本：
(defn vivid-light
  [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (color-burn b (* 2.0 s))
            (color-dodge b (* 2.0 (- s 0.5)))))
        backdrop source))
