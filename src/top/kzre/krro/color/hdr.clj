(ns top.kzre.krro.color.hdr
  "HDR 传输函数：PQ (ST 2084) 和 HLG (Hybrid Log-Gamma)。")

;; ── PQ (SMPTE ST 2084) ──────────────────────────────────
(def ^:private pq-m1 0.1593017578125)
(def ^:private pq-m2 78.84375)
(def ^:private pq-c1 0.8359375)
(def ^:private pq-c2 18.8515625)
(def ^:private pq-c3 18.6875)

(defn linear->pq
  "将线性光亮度 L（0..1 映射到 0..10000 nits）转换为 PQ 编码值。"
  [L]
  (let [L (max 0.0 L)
        L-pow (Math/pow L pq-m1)
        numerator (+ pq-c1 (* pq-c2 L-pow))
        denominator (+ 1 (* pq-c3 L-pow))
        value (Math/pow (/ numerator denominator) pq-m2)]
    value))

(defn pq->linear
  "将 PQ 编码值解码为线性光亮度。"
  [pq-val]
  (let [pq-val (max 0.0 pq-val)
        v-pow (Math/pow pq-val (/ 1.0 pq-m2))
        numerator (max 0.0 (- v-pow pq-c1))
        denominator (max 1e-10 (- pq-c2 (* pq-c3 v-pow)))
        L (Math/pow (/ numerator denominator) (/ 1.0 pq-m1))]
    L))

;; ── HLG (Hybrid Log-Gamma) ──────────────────────────────
(defn- hlg-encode [L]
  (let [a 0.17883277
        b 0.28466892
        c 0.55991073]
    (if (<= L 1/12)
      (* (Math/sqrt 3) (Math/sqrt L))
      (+ (* a (Math/log (- L b))) c))))

(defn- hlg-decode [V]
  (let [a 0.17883277
        b 0.28466892
        c 0.55991073]
    (if (<= V 0.5)
      (/ (* V V) 3.0)
      (+ (Math/exp (/ (- V c) a)) b))))

(defn linear->hlg
  "将线性亮度转换为 HLG 信号。"
  [L]
  (hlg-encode L))

(defn hlg->linear
  "将 HLG 信号转换为线性亮度。"
  [V]
  (hlg-decode V))