(ns top.kzre.krro.color.xyz
  "CIE XYZ 颜色空间操作。默认使用 D65 参考白点。"
  (:require [top.kzre.krro.color.util :as util]))

;; D65 参考白点
(def d65-white [0.95047 1.0 1.08883])

(defn xyz
  "创建 XYZ 颜色向量。"
  [x y z]
  [x y z])