(ns top.kzre.krro.color.converter
  "颜色空间转换函数。所有矩阵与 gamma 参数集中由 profiles 管理。"
  (:require
    [top.kzre.krro.color.adaptation :as adapt]
    [top.kzre.krro.color.cmyk :as cmyk]
    [top.kzre.krro.color.distance :as dist]
    [top.kzre.krro.color.lab :as lab]
    [top.kzre.krro.color.oklab :as oklab]
    [top.kzre.krro.color.oklch :as oklch]
    [top.kzre.krro.color.util :as util]
    [top.kzre.krro.color.profiles :as profiles]))

;; ── 辅助工具 ──────────────────────────────────────────
(defn matrix-mult [m v] (profiles/matrix-mult m v))
(defn matrix-inv [m] (profiles/matrix-inv m))

;; ── RGB ↔ HSL / HSV ──────────────────────────────────
(defn rgb->hsl
  "将 RGB 颜色转换为 HSL。"
  [[r g b]]
  (let [mx (max r g b)
        mn (min r g b)
        l (/ (+ mx mn) 2)]
    (if (= mx mn)
      [0 0 l]
      (let [d (- mx mn)
            s (if (> l 0.5) (/ d (- 2 mx mn)) (/ d (+ mx mn)))
            h (cond
                (= mx r) (mod (/ (- g b) d) 6)
                (= mx g) (+ 2 (/ (- b r) d))
                :else   (+ 4 (/ (- r g) d)))
            h (* 60 h)]
        [h s l]))))

(defn hsl->rgb
  "将 HSL 颜色转换为 RGB。"
  [[h s l]]
  (if (zero? s)
    [l l l]
    (let [q (if (< l 0.5) (* l (+ 1 s)) (+ l s (- (* l s))))
          p (- (* 2 l) q)
          hk (/ h 360)
          tR (mod (+ hk 1/3) 1)
          tG (mod hk 1)
          tB (mod (- hk 1/3) 1)
          adjust (fn [tc]
                   (cond
                     (< tc 1/6) (+ p (* (- q p) 6 tc))
                     (< tc 1/2) q
                     (< tc 2/3) (+ p (* (- q p) 6 (- 2/3 tc)))
                     :else p))]
      [(adjust tR) (adjust tG) (adjust tB)])))

(defn rgb->hsv
  "将 RGB 颜色转换为 HSV。"
  [[r g b]]
  (let [mx (max r g b)
        mn (min r g b)
        d (- mx mn)]
    (if (zero? d)
      [0 0 mx]
      (let [h (cond
                (= mx r) (mod (/ (- g b) d) 6)
                (= mx g) (+ 2 (/ (- b r) d))
                :else    (+ 4 (/ (- r g) d)))
            h (* 60 h)]
        [h (/ d mx) mx]))))

(defn hsv->rgb
  "将 HSV 颜色转换为 RGB。"
  [[h s v]]
  (if (zero? s)
    [v v v]
    (let [hi (mod (Math/floor (/ h 60)) 6)
          f (- (/ h 60) (Math/floor (/ h 60)))
          p (* v (- 1 s))
          q (* v (- 1 (* s f)))
          t (* v (- 1 (* s (- 1 f))))]
      (case (int hi)
        0 [v t p]
        1 [q v p]
        2 [p v t]
        3 [p q v]
        4 [t p v]
        5 [v p q]))))

(defn hsl->hsv [c] (-> c hsl->rgb rgb->hsv))
(defn hsv->hsl [c] (-> c hsv->rgb rgb->hsl))

;; ── RGB ↔ CMYK ────────────────────────────────────────
(defn rgb->cmyk [c] (cmyk/rgb->cmyk c))
(defn cmyk->rgb [c] (cmyk/cmyk->rgb c))

;; ── 通用 RGB ↔ XYZ（使用 profiles）─────────────────
(defn- rgb->xyz-using [c space-kw]
  (let [{:keys [primaries gamma]} (profiles/get-space space-kw)
        linear (mapv (:decode gamma) c)]
    (profiles/matrix-mult primaries linear)))

(defn- xyz->rgb-using [xyz-vec space-kw]
  (let [{:keys [primaries-inv gamma]} (profiles/get-space space-kw)
        linear (profiles/matrix-mult primaries-inv xyz-vec)
        ;; 钳制微小负值
        linear (mapv #(max 0.0 %) linear)]
    (mapv (:encode gamma) linear)))

;; sRGB
(defn rgb->xyz [c] (rgb->xyz-using c :srgb))
(defn xyz->rgb [c] (xyz->rgb-using c :srgb))

;; Display P3
(defn p3->xyz [c] (rgb->xyz-using c :display-p3))
(defn xyz->p3 [c] (xyz->rgb-using c :display-p3))

;; Adobe RGB
(defn adobe-rgb->xyz [c] (rgb->xyz-using c :adobe-rgb))
(defn xyz->adobe-rgb [c] (xyz->rgb-using c :adobe-rgb))

;; ProPhoto RGB
(defn prophoto->xyz [c] (rgb->xyz-using c :prophoto))
(defn xyz->prophoto [c] (xyz->rgb-using c :prophoto))

;; ── RGB ↔ CIELAB ─────────────────────────────────────
(defn rgb->lab [c] (-> c rgb->xyz lab/xyz->lab))
(defn lab->rgb [c] (-> c lab/lab->xyz xyz->rgb))

;; ── 色适应 ───────────────────────────────────────────
(defn xyz-d65->d50 [c] (adapt/chromatic-adapt c adapt/d65 adapt/d50))
(defn xyz-d50->d65 [c] (adapt/chromatic-adapt c adapt/d50 adapt/d65))

(defn rgb->lab-d50 [c]
  (-> c rgb->xyz xyz-d65->d50 lab/xyz->lab))

;; ── RGB ↔ OKLab / OKLCH ──────────────────────────────
(defn rgb->oklab [c]
  (let [{:keys [decode]} (:gamma (profiles/get-space :srgb))]
    (oklab/linear-rgb->oklab (mapv decode c))))

(defn oklab->rgb [c]
  (let [{:keys [encode]} (:gamma (profiles/get-space :srgb))]
    (mapv encode (oklab/oklab->linear-rgb c))))
(defn rgb->oklch [c] (-> c rgb->oklab oklch/oklab->oklch))
(defn oklch->rgb [c] (-> c oklch/oklch->oklab oklab->rgb))


(defn xyz->lab [c] (lab/xyz->lab c))
(defn lab->xyz [c] (lab/lab->xyz c))




;; ── 颜色距离便捷函数 ─────────────────────────────────
(defn delta-e76-rgb [c1 c2]
  (dist/delta-e76 (rgb->lab c1) (rgb->lab c2)))
(defn delta-e94-rgb [c1 c2]
  (dist/delta-e94 (rgb->lab c1) (rgb->lab c2)))
(defn delta-e2000-rgb [c1 c2]
  (dist/delta-e2000 (rgb->lab c1) (rgb->lab c2)))
(defn contrast-ratio-rgb [c1 c2]
  (dist/contrast-ratio c1 c2))

