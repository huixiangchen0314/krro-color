(ns top.kzre.krro.color.pigment
  "颜料混合模拟（基于 RYB 减色模型与 Kubelka-Munk 理论）。
   纯函数设计，无副作用。所有混合函数接受 :pigment-lib 参数。"
  (:require [top.kzre.krro.color.util :as util]))

;; ── 内置颜料库 ─────────────────────────────────
(def default-pigment-lib
  "默认颜料库，包含常用艺术颜料。"
  {:titanium-white  {:K [0.01 0.01 0.01] :S [0.98 0.98 0.98]}
   :cadmium-yellow  {:K [0.05 0.05 0.6 ] :S [0.8  0.8  0.2 ]}
   :cadmium-red     {:K [0.6  0.1  0.1 ] :S [0.2  0.8  0.8 ]}
   :ultramarine     {:K [0.1  0.1  0.6 ] :S [0.8  0.8  0.2 ]}
   :burnt-sienna    {:K [0.4  0.2  0.1 ] :S [0.5  0.7  0.7 ]}
   :lamp-black      {:K [0.9  0.9  0.9 ] :S [0.1  0.1  0.1 ]}
   :alizarin-crimson {:K [0.7 0.1 0.1] :S [0.2 0.8 0.8]}
   :cerulean-blue    {:K [0.1 0.1 0.7] :S [0.8 0.8 0.2]}
   :yellow-ochre     {:K [0.2 0.2 0.6] :S [0.7 0.7 0.3]}
   :viridian-green   {:K [0.3 0.7 0.3] :S [0.5 0.5 0.5]}
   :zinc-white       {:K [0.02 0.02 0.02] :S [0.95 0.95 0.95]}})

;; ── 辅助函数 ─────────────────────────────────
(defn- get-pigment [lib k]
  (or (get lib k)
      (throw (ex-info "Unknown pigment" {:key k}))))

;; ── RYB 混合 ─────────────────────────────────
(defn- rgb->ryb [[r g b]]
  (let [i (min r g b)
        r' (- r i) g' (- g i) b' (- b i)
        y (min r' g')
        r'' (- r' y) g'' (- g' y) b'' (- b' (min b' y))
        n (max r'' g'' b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ (+ y g'') n) (/ b'' n)]))

(defn- ryb->rgb [[r y b]]
  (let [i (min r y b)
        r' (- r i) y' (- y i) b' (- b i)
        g (min y' b')
        y'' (- y' g) b'' (- b' g) r'' (+ r' y'')
        n (max r'' g b'')
        n (if (zero? n) 1 n)]
    [(/ r'' n) (/ g n) (/ b'' n)]))

(defn mix-ryb
  "在 RYB 空间混合两种 RGB 颜色。"
  [c1 c2 t]
  (let [ryb1 (rgb->ryb c1)
        ryb2 (rgb->ryb c2)
        blended (mapv #(+ (* (- 1 t) %1) (* t %2)) ryb1 ryb2)]
    (ryb->rgb blended)))

;; ── Kubelka-Munk 混合 ──────────────────────────
(defn kubelka-munk-mix
  "双颜料混合。"
  [pigment1 pigment2 t & {:keys [pigment-lib] :or {pigment-lib default-pigment-lib}}]
  (let [p1 (get-pigment pigment-lib pigment1)
        p2 (get-pigment pigment-lib pigment2)
        K (mapv (fn [k1 k2] (+ (* (- 1 t) k1) (* t k2))) (:K p1) (:K p2))
        S (mapv (fn [s1 s2] (+ (* (- 1 t) s1) (* t s2))) (:S p1) (:S p2))
        ks (mapv / K (mapv #(max % 0.001) S))
        R (mapv (fn [ks]
                  (let [ks2 (* ks ks)]
                    (+ 1 ks (- (Math/sqrt (+ ks2 (* 2 ks)))))))
                ks)]
    (mapv #(util/clamp 0.0 1.0 %) R)))

(defn kubelka-munk-mix-multiple
  "多颜料混合（权重自动归一化）。"
  [pigments & {:keys [pigment-lib] :or {pigment-lib default-pigment-lib}}]
  (let [total-weight (apply + (map second pigments))
        normalized (if (zero? total-weight)
                     (map (fn [[k _]] [k (/ 1.0 (count pigments))]) pigments)
                     (map (fn [[k w]] [k (/ w total-weight)]) pigments))
        K (reduce (fn [acc [k w]]
                    (let [p (get-pigment pigment-lib k)]
                      (mapv + acc (mapv #(* w %) (:K p)))))
                  [0.0 0.0 0.0] normalized)
        S (reduce (fn [acc [k w]]
                    (let [p (get-pigment pigment-lib k)]
                      (mapv + acc (mapv #(* w %) (:S p)))))
                  [0.0 0.0 0.0] normalized)
        ks (mapv / K (mapv #(max % 0.001) S))
        R (mapv (fn [ks]
                  (let [ks2 (* ks ks)]
                    (+ 1 ks (- (Math/sqrt (+ ks2 (* 2 ks)))))))
                ks)]
    (mapv #(util/clamp 0.0 1.0 %) R)))

;; ── Saunderson 修正 ────────────────────────────
(defn saunderson-correct
  ([r] (saunderson-correct r 0.04 0.4))
  ([r K1 K2]
   (mapv (fn [r]
           (let [r (max 0.0 (min 1.0 r))]
             (+ K1 (/ (* (- 1 K1) (- 1 K2) r) (- 1 (* K2 r))))))
         r)))

;; ── 通用混合调度 ──────────────────────────────
(defn mix
  "颜料混合主函数。
   模式：
     :ryb - RYB 艺术混合，c1/c2 为 RGB 颜色
     :kubelka-munk - 双颜料混合，c1/c2 为颜料关键字
     :kubelka-munk-multiple - 多颜料混合，c1 为 [[key weight] ...] 列表
   可通过 :pigment-lib 传入自定义颜料库。"
  [c1 c2 t & {:keys [mode pigment-lib] :or {mode :ryb pigment-lib default-pigment-lib}}]
  (case mode
    :ryb (mix-ryb c1 c2 t)
    :kubelka-munk (kubelka-munk-mix c1 c2 t :pigment-lib pigment-lib)
    :kubelka-munk-multiple (kubelka-munk-mix-multiple c1 :pigment-lib pigment-lib)
    (throw (ex-info "Unknown pigment mix mode" {:mode mode}))))