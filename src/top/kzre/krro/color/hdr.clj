(ns top.kzre.krro.color.hdr
  "HDR 传输函数：PQ (ST 2084)、HLG (BT.2100)，以及 Reinhard/ACES 色调映射。"
  (:require [top.kzre.krro.color.util :as util]))

;; ── PQ 常量 ────────────────────────────────────────────
(def ^:const m1 0.1593017578125)
(def ^:const m2 78.84375)
(def ^:const c1 0.8359375)
(def ^:const c2 18.8515625)
(def ^:const c3 18.6875)

(defn linear->pq [Y]
  (let [Y (max 0.0 (double Y))
        Y-pow (Math/pow Y m1)
        numerator (+ c1 (* c2 Y-pow))
        denominator (+ 1.0 (* c3 Y-pow))]
    (Math/pow (/ numerator denominator) m2)))

(defn pq->linear [N]
  (let [N (max 0.0 (double N))
        N-pow (Math/pow N (/ 1.0 m2))
        numerator (max 0.0 (- N-pow c1))
        denominator (max 1e-10 (- c2 (* c3 N-pow)))]
    (Math/pow (/ numerator denominator) (/ 1.0 m1))))

;; ── HLG ───────────────────────────────────────────────
(def ^:const hlga 0.17883277)
(def ^:const hlgb 0.28466892)
(def ^:const hlgc 0.55991073)

(defn linear->hlg
  "将线性场景光信号 L (0..1) 转换为 HLG 编码值。"
  [L]
  (let [L (max 0.0 (double L))]
    (if (<= L (/ 1.0 12.0))
      (Math/sqrt (* 3.0 L))
      (+ (* hlga (Math/log (- (* 12.0 L) hlgb))) hlgc))))

(defn hlg->linear
  "将 HLG 编码值 V (0..1) 解码为线性场景光信号。"
  [V]
  (let [V (max 0.0 (double V))]
    (if (<= V 0.5)    ;; V=0.5 对应 L=1/12
      (/ (* V V) 3.0)
      (/ (+ (Math/exp (/ (- V hlgc) hlga)) hlgb) 12.0))))

;; ── 色调映射 ──────────────────────────────────────────
(defn reinhard
  "Reinhard 色调映射：HDR -> SDR。"
  [rgb]
  (mapv #(/ % (+ 1.0 %)) rgb))

(defn aces
  "ACES 电影色调映射（近似拟合公式）。"
  [rgb]
  (mapv (fn [x]
          (let [x (max 0.0 x)
                numerator (* x (+ (* 2.51 x) 0.03))
                denominator (+ (* x (+ (* 2.43 x) 0.59)) 0.14)]
            (util/clamp 0.0 1.0 (/ numerator denominator))))
        rgb))