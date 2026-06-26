(ns top.kzre.krro.color.pigment
  "颜料混合模拟（基于 RYB 减色模型与 Kubelka-Munk 理论的基础实现）。
   提供两种模式：
   1. :ryb - 快速艺术混合，适合数字绘画直观调色。
   2. :kubelka-munk - 基于简化的光谱吸收/散射数据，效果更真实。
      可通过 :pigment-lib 参数传入自定义颜料库。"
  (:require [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.util :as util]))

;; ── 内置默认颜料库 ─────────────────────────────────
(def ^:private default-pigment-lib
  {:titanium-white  {:K [0.01 0.01 0.01] :S [0.98 0.98 0.98]}
   :cadmium-yellow  {:K [0.05 0.05 0.6 ] :S [0.8  0.8  0.2 ]}
   :cadmium-red     {:K [0.6  0.1  0.1 ] :S [0.2  0.8  0.8 ]}
   :ultramarine     {:K [0.1  0.1  0.6 ] :S [0.8  0.8  0.2 ]}
   :burnt-sienna    {:K [0.4  0.2  0.1 ] :S [0.5  0.7  0.7 ]}
   :lamp-black      {:K [0.9  0.9  0.9 ] :S [0.1  0.1  0.1 ]}})

;; ── RYB 颜色空间转换 ──────────────────────────────
(defn- rgb->ryb [[r g b]]
  (let [i (min r g b)
        r' (- r i)
        g' (- g i)
        b' (- b i)
        y (min r' g')
        r'' (- r' y)
        g'' (- g' y)
        b'' (- b' (min b' y))
        n (max r'' g'' b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ (+ y g'') n) (/ b'' n)]))

(defn- ryb->rgb [[r y b]]
  (let [i (min r y b)
        r' (- r i)
        y' (- y i)
        b' (- b i)
        g (min y' b')
        y'' (- y' g)
        b'' (- b' g)
        r'' (+ r' y'')
        n (max r'' g b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ g n) (/ b'' n)]))

;; ── RYB 混合 ─────────────────────────────────────
(defn mix-ryb
  "在 RYB 空间混合两种 RGB 颜色，t 为 source 的权重 (0..1)。"
  [c1 c2 t]
  (let [ryb1 (rgb->ryb c1)
        ryb2 (rgb->ryb c2)
        blended (mapv #(+ (* (- 1 t) %1) (* t %2)) ryb1 ryb2)]
    (ryb->rgb blended)))

;; ── Kubelka-Munk 混合（支持外部颜料库） ───────────
(defn- load-pigment [pigment-key pigment-lib]
  (or (get pigment-lib pigment-key)
      (throw (ex-info "Unknown pigment" {:key pigment-key}))))

(defn kubelka-munk-mix
  "基于 Kubelka-Munk 理论的两种颜料混合。
   参数：
     pigment1, pigment2 - 颜料关键字
     t - pigment2 的浓度权重 (0..1)
   Options:
     :pigment-lib - 可选，颜料库 map，默认使用内置库。
   返回：反射率 RGB 向量 [R G B] (0..1)。"
  [pigment1 pigment2 t & {:keys [pigment-lib] :or {pigment-lib default-pigment-lib}}]
  (let [p1 (load-pigment pigment1 pigment-lib)
        p2 (load-pigment pigment2 pigment-lib)
        K (mapv (fn [k1 k2] (+ (* (- 1 t) k1) (* t k2))) (:K p1) (:K p2))
        S (mapv (fn [s1 s2] (+ (* (- 1 t) s1) (* t s2))) (:S p1) (:S p2))
        ;; 避免除零
        ks (mapv / K (mapv #(max % 0.001) S))
        ;; R = 1 + K/S - sqrt((K/S)^2 + 2*(K/S))
        R (mapv (fn [ks]
                  (let [ks2 (* ks ks)
                        sqrt-disc (Math/sqrt (+ ks2 (* 2 ks)))]
                    (+ 1 ks (- sqrt-disc))))
                ks)]
    (mapv #(util/clamp 0.0 1.0 %) R)))

;; ── 通用混合调度 ─────────────────────────────────
(defn mix
  "颜料混合主函数。
   模式 :ryb 直接使用 RGB 颜色；:kubelka-munk 使用颜料关键字。
   对于 :kubelka-munk 可传入 :pigment-lib 来自定义颜料库。"
  [c1 c2 t & {:keys [mode pigment-lib] :or {mode :ryb}}]
  (case mode
    :ryb (mix-ryb c1 c2 t)
    :kubelka-munk (kubelka-munk-mix c1 c2 t :pigment-lib pigment-lib)
    (throw (ex-info "Unknown pigment mix mode" {:mode mode}))))

;; ── 便捷函数：使用自定义颜料库的混合 ─────────────────
(defn predefined-mix
  "使用指定颜料库按名称混合，浓度 t (0..1) 为第二种颜料的占比。"
  [pigment-key1 pigment-key2 t & {:keys [pigment-lib]}]
  (kubelka-munk-mix pigment-key1 pigment-key2 t :pigment-lib pigment-lib))