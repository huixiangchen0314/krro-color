(ns top.kzre.krro.color.gray
  "灰度颜色空间操作。"
  (:require [top.kzre.krro.color.rgb :as rgb]))

(defn gray
  "创建灰度颜色向量 [y]，范围0-1。"
  [y]
  [y])

(defn from-rgb
  "从sRGB颜色计算灰度值（ITU-R BT.709亮度）。"
  [rgb]
  (rgb/luminance rgb))

(defn to-rgb
  "将灰度值转换为sRGB颜色（三通道相同）。"
  [g]
  [g g g])

(defn gray->rgb [g] (to-rgb g))
(defn rgb->gray [rgb] (from-rgb rgb))