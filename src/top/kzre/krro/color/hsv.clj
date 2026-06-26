(ns top.kzre.krro.color.hsv
  "HSV 颜色空间操作。"
  (:require [top.kzre.krro.color.util :as util]))

(defn hsv
  "创建 HSV 颜色向量 [h s v]，h 为度数 [0,360)，s,v 在 0-1 之间。"
  [h s v]
  [h s v])

(defn hue
  "获取色相。"
  [c]
  (first c))

(defn saturation
  "获取饱和度。"
  [c]
  (second c))

(defn value
  "获取明度。"
  [c]
  (nth c 2))