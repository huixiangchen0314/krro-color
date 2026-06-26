(ns top.kzre.krro.color.pigment
  "颜料混合模拟（基于 RYB 减色模型与 Kubelka-Munk 理论的基础实现）。
   提供两种模式：
   1. `:ryb` - 快速艺术混合，适合数字绘画直观调色。
   2. `:kubelka-munk` - 基于简化的光谱吸收/散射数据，效果更真实（默认使用内置颜料库）。"
  (:require [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.util :as util]))

;; ── RYB 颜色空间转换 ─────────────────────────────────
;; 基于 Gossett & Chen (2004) 的简化 RGB <-> RYB 转换

(defn- rgb->ryb [[r g b]]
  ;; 去饱和 → 转换为三色激励值
  (let [i (min r g b)                   ;; 白色分量
        r' (- r i)
        g' (- g i)
        b' (- b i)
        y (min r' g')                   ;; 黄色
        r'' (- r' y)
        g'' (- g' y)
        b'' (- b' (min b' y))           ;; 剩余蓝色
        ;; 规范化
        n (max r'' g'' b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ (+ y g'') n) (/ b'' n)]))

(defn- ryb->rgb [[r y b]]
  (let [i (min r y b)                   ;; 白色分量
        r' (- r i)
        y' (- y i)
        b' (- b i)
        g (min y' b')                   ;; 绿色 = min(y,b)
        y'' (- y' g)
        b'' (- b' g)
        r'' (+ r' y'')                  ;; 剩余红色
        ;; 规范化
        n (max r'' g b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ g n) (/ b'' n)]))

;; ── RYB 混合 ─────────────────────────────────────────
(defn mix-ryb
  "在 RYB 空间混合两种 RGB 颜色，t 为 source 的权重 (0..1)。"
  [c1 c2 t]
  (let [ryb1 (rgb->ryb c1)
        ryb2 (rgb->ryb c2)
        blended (mapv #(+ (* (- 1 t) %1) (* t %2)) ryb1 ryb2)]
    (ryb->rgb blended)))

;; ── Kubelka-Munk 简化实现 ────────────────────────────
;; 内置颜料数据（吸收系数 K 和散射系数 S，归一化到 [0,1] 范围）
;; 实际数据应来自测量，此处为示意值
(def ^:private pigment-db
  {:titanium-white  {:K [0.01 0.01 0.01] :S [0.98 0.98 0.98]}
   :cadmium-yellow  {:K [0.05 0.05 0.6 ] :S [0.8  0.8  0.2 ]}
   :cadmium-red     {:K [0.6  0.1  0.1 ] :S [0.2  0.8  0.8 ]}
   :ultramarine     {:K [0.1  0.1  0.6 ] :S [0.8  0.8  0.2 ]}
   :burnt-sienna    {:K [0.4  0.2  0.1 ] :S [0.5  0.7  0.7 ]}
   :lamp-black      {:K [0.9  0.9  0.9 ] :S [0.1  0.1  0.1 ]}})

(defn- load-pigment [pigment-key]
  (or (get pigment-db pigment-key)
      (throw (ex-info "Unknown pigment" {:key pigment-key}))))

(defn kubelka-munk-mix
  "基于 Kubelka-Munk 理论的两种颜料混合。
   参数：pigment1, pigment2 为颜料关键字（如 :titanium-white）
         t 为 pigment2 的浓度权重 (0..1)。
   返回：反射率向量 [R G B] (0..1)。"
  [pigment1 pigment2 t]
  (let [p1 (load-pigment pigment1)
        p2 (load-pigment pigment2)
        ;; 混合 K 和 S：线性加权
        K (mapv (fn [k1 k2] (+ (* (- 1 t) k1) (* t k2))) (:K p1) (:K p2))
        S (mapv (fn [s1 s2] (+ (* (- 1 t) s1) (* t s2))) (:S p1) (:S p2))
        ;; 计算反射率：R = 1 + (K/S) - sqrt((K/S)^2 + 2*(K/S))
        ;; 其中 K/S 是向量
        ks (mapv / K (mapv #(max % 0.001) S))  ;; 避免除零
        ;; R = 1 + ks - sqrt(ks^2 + 2*ks)
        R (mapv (fn [ks]
                  (let [ks2 (* ks ks)
                        discriminant (+ ks2 (* 2 ks))
                        sqrt-disc (Math/sqrt discriminant)]
                    (+ 1 ks (- sqrt-disc))))
                ks)]
    (mapv #(util/clamp 0.0 1.0 %) R)))

;; ── 通用颜料混合调度 ─────────────────────────────────
(defn mix
  "颜料混合主函数。
   模式：
     :ryb - 快速 RYB 艺术混合，直接接受 RGB 颜色。
     :kubelka-munk - 基于颜料名的精确混合，返回反射率 RGB。
   对于 :kubelka-munk 模式，c1 和 c2 应为颜料关键字（keyword），而非颜色向量。
   对于 :ryb 模式，c1 和 c2 为 RGB 向量。"
  [c1 c2 t & {:keys [mode] :or {mode :ryb}}]
  (case mode
    :ryb (mix-ryb c1 c2 t)
    :kubelka-munk (kubelka-munk-mix c1 c2 t)
    (throw (ex-info "Unknown pigment mix mode" {:mode mode}))))

;; ── 预设颜料调色板便捷函数 ───────────────────────────
(defn predefined-mix
  "使用内置颜料库按名称混合，浓度 t (0..1) 为第二种颜料的占比。"
  [pigment-key1 pigment-key2 t]
  (kubelka-munk-mix pigment-key1 pigment-key2 t))