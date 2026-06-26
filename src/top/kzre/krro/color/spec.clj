(ns top.kzre.krro.color.spec
    "颜色数据规格定义。使用 clojure.spec.alpha 进行运行时验证（可选）。
     所有颜色空间均用简单向量表示，不带 alpha（透明度单独处理）。"
    (:require [clojure.spec.alpha :as s]))

;; ── 辅助：0..1 浮点数 ────────────────────────────────────
(s/def ::component (s/and number? #(<= 0.0 % 1.0)))

;; ── RGB / RGBA ──────────────────────────────────────────
(s/def ::r ::component)
(s/def ::g ::component)
(s/def ::b ::component)
(s/def ::a ::component)

(s/def ::rgb (s/and (s/coll-of ::component :kind vector? :count 3)))
(s/def ::rgba (s/and (s/coll-of ::component :kind vector? :count 4)))

;; ── HSL ──────────────────────────────────────────────────
;; Hue: 0..360（通常用度数），但内部可能存储为弧度？我们统一用度数，范围 [0,360)
(s/def ::hue-degree (s/and number? #(<= 0.0 % 360.0)))
(s/def ::saturation ::component)
(s/def ::lightness ::component)

(s/def ::hsl (s/and (s/coll-of number? :kind vector? :count 3)
                    #(<= 0.0 (first %) 360.0)
                    #(<= 0.0 (second %) 1.0)
                    #(<= 0.0 (nth % 2) 1.0)))

;; ── HSV ──────────────────────────────────────────────────
(s/def ::value ::component)
(s/def ::hsv (s/and (s/coll-of number? :kind vector? :count 3)
                    #(<= 0.0 (first %) 360.0)
                    #(<= 0.0 (second %) 1.0)
                    #(<= 0.0 (nth % 2) 1.0)))

;; ── CMYK ─────────────────────────────────────────────────
;; 每个分量 0..1
(s/def ::c ::component)
(s/def ::m ::component)
(s/def ::y ::component)
(s/def ::k ::component)

(s/def ::cmyk (s/and (s/coll-of ::component :kind vector? :count 4)))

;; ── CIELAB ───────────────────────────────────────────────
;; L* 范围 0..100，a* 和 b* 范围大致 -128..127，为简单不强制范围
(s/def ::lab (s/and (s/coll-of number? :kind vector? :count 3)
                    #(<= 0.0 (first %) 100.0)))

;; ── XYZ (D65) ────────────────────────────────────────────
;; 范围与照明体有关，通常 X,Y,Z > 0
(s/def ::xyz (s/and (s/coll-of number? :kind vector? :count 3)
                    #(every? (fn [v] (>= v 0.0)) %)))

;; ── 灰度 ─────────────────────────────────────────────────
(s/def ::gray ::component)          ;; 0..1

;; ── 颜色容器：可包含 alpha 或仅向量 ──────────────────────
;; 通用颜色可以是任何已定义的类型
(s/def ::color (s/or :rgb ::rgb
                     :rgba ::rgba
                     :hsl ::hsl
                     :hsv ::hsv
                     :cmyk ::cmyk
                     :lab ::lab
                     :xyz ::xyz
                     :gray ::gray))


;; ── 颜料规格 ───────────────────────────────────────────
;; 单个颜料：包含吸收系数 K 和散射系数 S，均为 RGB 三个分量
(s/def ::k (s/coll-of ::component :kind vector? :count 3))
(s/def ::s (s/coll-of ::component :kind vector? :count 3))
(s/def ::pigment
  (s/keys :req-un [::k ::s]))

;; 颜料库：keyword 到颜料定义的映射
(s/def ::pigment-library
  (s/map-of keyword? ::pigment))
