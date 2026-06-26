(ns top.kzre.krro.color.cmyk
  "CMYK 颜色空间操作。"
  (:require [top.kzre.krro.color.util :as util]))

(defn cmyk
  "创建 CMYK 颜色向量 [c m y k]，分量 0-1。"
  [c m y k]
  [c m y k])

(defn cyan
  "获取青色分量。"
  [c] (first c))

(defn magenta
  "获取品红分量。"
  [c] (second c))

(defn yellow
  "获取黄色分量。"
  [c] (nth c 2))

(defn black
  "获取黑色分量。"
  [c] (nth c 3))

(defn rgb->cmyk [[r g b]]
  (let [k (- 1 (max r g b))]
    (if (>= k 0.9999)
      [0.0 0.0 0.0 1.0]
      (let [c (/ (- 1 r k) (- 1 k))
            m (/ (- 1 g k) (- 1 k))
            y (/ (- 1 b k) (- 1 k))]
        [c m y k]))))

(defn cmyk->rgb
  "将 CMYK 颜色转换为 sRGB。"
  [[c m y k]]
  [(min 1.0 (* (- 1 c) (- 1 k)))
   (min 1.0 (* (- 1 m) (- 1 k)))
   (min 1.0 (* (- 1 y) (- 1 k)))])