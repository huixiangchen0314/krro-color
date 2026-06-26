(ns top.kzre.krro.color.accessibility
  "色盲模拟与 WCAG 可访问性评估。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.util :as util]))

;; ── 色盲模拟矩阵（基于 LMS 空间）───────────────────
(def ^:private protanopia-matrix
  [[0.56667 0.43333 0]
   [0.55833 0.44167 0]
   [0       0       1]])

(def ^:private deuteranopia-matrix
  [[0.625   0.375   0]
   [0.7     0.3     0]
   [0       0       1]])

(def ^:private tritanopia-matrix
  [[0.95    0.05    0]
   [0       0.43333 0.56667]
   [0       0.475   0.525]])

(defn- matrix-mult [m v] (mapv #(apply + (map * % v)) m))

(defn simulate-protanopia
  "模拟红绿色盲（红色盲）。"
  [rgb]
  (matrix-mult protanopia-matrix rgb))

(defn simulate-deuteranopia
  "模拟红绿色盲（绿色盲）。"
  [rgb]
  (matrix-mult deuteranopia-matrix rgb))

(defn simulate-tritanopia
  "模拟蓝黄色盲。"
  [rgb]
  (matrix-mult tritanopia-matrix rgb))

(defn simulate-color-blindness
  "根据类型返回模拟后的颜色。类型 :protanopia / :deuteranopia / :tritanopia。"
  [rgb type]
  (case type
    :protanopia   (simulate-protanopia rgb)
    :deuteranopia (simulate-deuteranopia rgb)
    :tritanopia   (simulate-tritanopia rgb)))

;; ── WCAG 对比度（已在 distance.clj 中，这里直接复用）─
;; 提供便捷函数
(defn contrast-ratio [rgb1 rgb2]
  (require 'top.kzre.krro.color.distance)
  ((resolve 'top.kzre.krro.color.distance/contrast-ratio) rgb1 rgb2))

(defn meets-wcag-aa? [rgb1 rgb2 & {:keys [large-text?] :or {large-text? false}}]
  (let [cr (contrast-ratio rgb1 rgb2)]
    (if large-text?
      (>= cr 3.0)
      (>= cr 4.5))))

(defn meets-wcag-aaa? [rgb1 rgb2 & {:keys [large-text?] :or {large-text? false}}]
  (let [cr (contrast-ratio rgb1 rgb2)]
    (if large-text?
      (>= cr 4.5)
      (>= cr 7.0))))