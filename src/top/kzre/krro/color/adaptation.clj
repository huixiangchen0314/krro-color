(ns top.kzre.krro.color.adaptation
  "色适应变换：在不同参考白点之间转换 XYZ 颜色。
   使用 Bradford 变换矩阵。"
  (:require [top.kzre.krro.color.util :as util]))

;; Bradford 变换矩阵 (XYZ -> cone response)
(def ^:private bradford-matrix
  [[ 0.8951  0.2664 -0.1614]
   [-0.7502  1.7135  0.0367]
   [ 0.0389 -0.0685  1.0296]])

;; Bradford 逆矩阵 (cone response -> XYZ)
(def ^:private bradford-inv
  [[ 0.9869929 -0.1470543  0.1599627]
   [ 0.4323053  0.5183603  0.0492912]
   [-0.0085287  0.0400428  0.9684867]])

;; 常见参考白点 (XYZ)
(def d65 [0.95047 1.0 1.08883])
(def d50 [0.96422 1.0 0.82521])

(defn- matrix-mult
  "矩阵乘以向量。"
  [m v]
  (mapv #(apply + (map * % v)) m))

(defn- cone-response
  "计算锥响应。"
  [xyz]
  (matrix-mult bradford-matrix xyz))

(defn- inv-cone-response
  "从锥响应恢复 XYZ。"
  [cone]
  (matrix-mult bradford-inv cone))

(defn chromatic-adapt
  "将 XYZ 颜色从 source-white 参考白点转换到 target-white 参考白点。
   source-white 和 target-white 均为 XYZ 三元组。"
  [xyz source-white target-white]
  (let [src-cone (cone-response source-white)
        tgt-cone (cone-response target-white)
        ;; 每个锥响应通道的缩放因子
        scale (mapv / tgt-cone src-cone)
        xyz-cone (cone-response xyz)
        adapted-cone (mapv * scale xyz-cone)]
    (inv-cone-response adapted-cone)))