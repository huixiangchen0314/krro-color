(ns top.kzre.krro.color.palette
  "从图像提取主色调。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

(defn- rand-point [points]
  (nth points (rand-int (count points))))

(defn- closest-centroid [p centroids]
  (apply min-key
         (fn [c] (reduce + (mapv #(* (- %1 %2) (- %1 %2)) p c)))
         centroids))

(defn- mean [points]
  (let [n (count points)]
    (mapv #(/ % n) (reduce #(mapv + %1 %2) [0 0 0] points))))

(defn k-means
  "简单的 K-means 聚类，返回 k 个主色调。"
  [pixels k & {:keys [max-iters] :or {max-iters 20}}]
  (let [;; 随机初始化 k 个中心
        centroids (repeatedly k #(rand-point pixels))]
    (loop [iters 0 centroids centroids]
      (if (>= iters max-iters)
        centroids
        (let [clusters (group-by #(closest-centroid % centroids) pixels)
              new-centroids (mapv mean (vals clusters))]
          (recur (inc iters) new-centroids))))))