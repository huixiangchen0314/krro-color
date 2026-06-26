(ns top.kzre.krro.color.composite
  "Porter-Duff 合成操作与图层组混合模式。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

;; ── Porter-Duff 操作（基于预乘 alpha） ────────────────
(defn over
  "标准 over 合成：source over backdrop。"
  [backdrop source]
  (let [[rb gb bb ab] (if (== 4 (count backdrop)) backdrop (conj backdrop 1.0))
        [rs gs bs as] (if (== 4 (count source)) source (conj source 1.0))
        a-out (+ as (* ab (- 1 as)))
        inv-a-out (if (zero? a-out) 0.0 (/ 1.0 a-out))]
    [(/ (+ (* rs as) (* rb ab (- 1 as))) a-out)
     (/ (+ (* gs as) (* gb ab (- 1 as))) a-out)
     (/ (+ (* bs as) (* bb ab (- 1 as))) a-out)
     a-out]))

(defn in
  "source in backdrop。"
  [backdrop source]
  (let [[_ _ _ ab] (if (== 4 (count backdrop)) backdrop (conj backdrop 1.0))
        [rs gs bs as] (if (== 4 (count source)) source (conj source 1.0))
        a-out (* as ab)]
    (if (zero? a-out)
      [0 0 0 0]
      [(* rs (/ as a-out)) (* gs (/ as a-out)) (* bs (/ as a-out)) a-out])))

(defn out
  "source out backdrop。"
  [backdrop source]
  (let [[_ _ _ ab] (if (== 4 (count backdrop)) backdrop (conj backdrop 1.0))
        [rs gs bs as] (if (== 4 (count source)) source (conj source 1.0))
        a-out (* as (- 1 ab))]
    (if (zero? a-out)
      [0 0 0 0]
      [(* rs (/ as a-out)) (* gs (/ as a-out)) (* bs (/ as a-out)) a-out])))

(defn atop
  "source atop backdrop。"
  [backdrop source]
  (let [[rb gb bb ab] (if (== 4 (count backdrop)) backdrop (conj backdrop 1.0))
        [rs gs bs as] (if (== 4 (count source)) source (conj source 1.0))
        a-out ab
        inv-a-out (if (zero? a-out) 0.0 (/ 1.0 a-out))]
    (if (zero? a-out)
      [0 0 0 0]
      [(/ (+ (* rs as) (* rb (- 1 as))) a-out)
       (/ (+ (* gs as) (* gb (- 1 as))) a-out)
       (/ (+ (* bs as) (* bb (- 1 as))) a-out)
       a-out])))

(defn dest-in
  "backdrop in source。"
  [backdrop source]
  (in source backdrop))

(defn dest-out
  "backdrop out source。"
  [backdrop source]
  (out source backdrop))

(defn dest-atop
  "backdrop atop source。"
  [backdrop source]
  (atop source backdrop))

(defn xor
  "Porter-Duff XOR。"
  [backdrop source]
  (let [[rb gb bb ab] (if (== 4 (count backdrop)) backdrop (conj backdrop 1.0))
        [rs gs bs as] (if (== 4 (count source)) source (conj source 1.0))
        a-out (+ (* as (- 1 ab)) (* ab (- 1 as)))]
    (if (zero? a-out)
      [0 0 0 0]
      (let [inv-a-out (/ 1.0 a-out)]
        [(/ (+ (* rs as (- 1 ab)) (* rb ab (- 1 as))) a-out)
         (/ (+ (* gs as (- 1 ab)) (* gb ab (- 1 as))) a-out)
         (/ (+ (* bs as (- 1 ab)) (* bb ab (- 1 as))) a-out)
         a-out]))))

;; ── 图层组混合模式（穿透） ──────────────────────────
(defn pass-through
  "穿透混合：将组内已混合的源直接放在背景上，使用 over 合成。"
  [backdrop group-result]
  (over backdrop group-result))

;; ── 批量合成（性能优化） ──────────────────────────────
(defn composite-batch
  "对 RGBA 像素数组批量应用 Porter-Duff 操作。pixels 为背景像素数组（会修改），
   source 为前景颜色 [r g b a]，op 为上述函数之一（如 over）。"
  [^doubles pixels source op]
  (let [n (alength pixels)
        sr (double (nth source 0))
        sg (double (nth source 1))
        sb (double (nth source 2))
        sa (if (== 4 (count source)) (double (nth source 3)) 1.0)]
    (dotimes [i (quot n 4)]
      (let [idx (* i 4)
            rb (aget pixels idx)
            gb (aget pixels (+ idx 1))
            bb (aget pixels (+ idx 2))
            ab (if (< (+ idx 3) n) (aget pixels (+ idx 3)) 1.0)
            result (op [rb gb bb ab] [sr sg sb sa])
            ;; 写回背景像素（如果结果为 RGBA，写入 alpha 分量）
            _ (aset pixels idx (double (nth result 0)))
            _ (aset pixels (+ idx 1) (double (nth result 1)))
            _ (aset pixels (+ idx 2) (double (nth result 2)))
            _ (when (and (> (count result) 3) (< (+ idx 3) n))
                (aset pixels (+ idx 3) (double (nth result 3))))])))
  pixels)