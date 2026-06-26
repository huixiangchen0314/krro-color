(ns top.kzre.krro.color.blend
  "混合模式实现，参考 PS 标准公式。包含调度函数 blend 以及带 alpha 的合成。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]
            [top.kzre.krro.color.composite :as comp]))

(defn normal [backdrop source] source)

(defn dissolve [backdrop source] source) ;; 简化

(defn multiply [backdrop source] (mapv * backdrop source))

(defn screen [backdrop source]
  (mapv #(- 1 (* (- 1 %1) (- 1 %2))) backdrop source))

(defn overlay [backdrop source]
  (mapv (fn [b s]
          (if (<= b 0.5)
            (* 2 b s)
            (- 1 (* 2 (- 1 b) (- 1 s)))))
        backdrop source))

(defn hard-light [backdrop source] (overlay source backdrop))

(defn soft-light [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (- b (* b (- 1 (* 2 s)) (- 1 b)))
            (let [d (if (<= b 0.25)
                      (/ (- (* 16 b 12 b) 4) (- 4 b))
                      (Math/sqrt b))]
              (+ b (* (- d b) (- 1 (* 2 s)))))))
        backdrop source))

(defn darken [backdrop source] (mapv min backdrop source))
(defn lighten [backdrop source] (mapv max backdrop source))

(defn difference [backdrop source]
  (mapv (fn [b s] (Math/abs (- b s))) backdrop source))

(defn exclusion [backdrop source]
  (mapv #(+ %1 %2 (- (* 2 %1 %2))) backdrop source))

(defn color-dodge [backdrop source]
  (mapv (fn [b s]
          (if (>= s 1.0)
            1.0
            (util/clamp 0.0 1.0 (/ b (- 1.0 s)))))
        backdrop source))

(defn color-burn [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.0)
            b
            (util/clamp 0.0 1.0 (- 1.0 (/ (- 1.0 b) s)))))
        backdrop source))

;; 组合模式：完全使用逐通道公式，不再调用其他混合函数

(defn linear-light [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (util/clamp 0.0 1.0 (+ b (* 2.0 s) -1.0))          ;; linear-burn
            (util/clamp 0.0 1.0 (+ b (* 2.0 (- s 0.5))))))     ;; linear-dodge
        backdrop source))

(defn vivid-light [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (let [s2 (* 2.0 s)]
              (if (<= s2 0.0)
                b
                (util/clamp 0.0 1.0 (- 1.0 (/ (- 1.0 b) s2)))))   ;; color-burn
            (let [s2 (* 2.0 (- s 0.5))]
              (if (>= s2 1.0)
                1.0
                (util/clamp 0.0 1.0 (/ b (- 1.0 s2)))))))          ;; color-dodge
        backdrop source))

(defn pin-light [backdrop source]
  (mapv (fn [b s]
          (if (<= s 0.5)
            (min b (* 2.0 s))                   ;; darken (逐通道)
            (max b (* 2.0 (- s 0.5)))))         ;; lighten (逐通道)
        backdrop source))

(defn hard-mix [backdrop source]
  (mapv (fn [b s]
          (if (<= (+ b s) 1.0) 0.0 1.0))
        backdrop source))

(defn subtract [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (- %1 %2)) backdrop source))

(defn divide [backdrop source]
  (mapv (fn [b s]
          (if (zero? s)
            1.0
            (util/clamp 0.0 1.0 (/ b s))))
        backdrop source))

(defn linear-burn [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (+ %1 %2 -1.0)) backdrop source))

(defn linear-dodge [backdrop source]
  (mapv #(util/clamp 0.0 1.0 (+ %1 %2)) backdrop source))

;; ── 混合模式调度 ─────────────────────────────────────────
(defn blend
  "根据关键字 mode 应用混合模式。"
  [backdrop source mode]
  (case mode
    :normal       (normal backdrop source)
    :multiply     (multiply backdrop source)
    :screen       (screen backdrop source)
    :overlay      (overlay backdrop source)
    :hard-light   (hard-light backdrop source)
    :soft-light   (soft-light backdrop source)
    :color-dodge  (color-dodge backdrop source)
    :color-burn   (color-burn backdrop source)
    :darken       (darken backdrop source)
    :lighten      (lighten backdrop source)
    :difference   (difference backdrop source)
    :exclusion    (exclusion backdrop source)
    :linear-light (linear-light backdrop source)
    :vivid-light  (vivid-light backdrop source)
    :pin-light    (pin-light backdrop source)
    :hard-mix     (hard-mix backdrop source)
    :subtract     (subtract backdrop source)
    :divide       (divide backdrop source)
    (throw (IllegalArgumentException. (str "Unknown blend mode: " mode)))))

;; ── 带 alpha 的混合 ──────────────────────────────────────
(defn blend-with-alpha
  "对带 alpha 的颜色进行混合。先用 mode 混合颜色（仅对 RGB），
   然后应用 Porter-Duff over 合成。"
  [backdrop source mode]
  (let [blended-rgb (blend (subvec backdrop 0 3) (subvec source 0 3) mode)
        blended-rgba (conj (vec blended-rgb) (rgb/alpha source))]
    (comp/over backdrop blended-rgba)))