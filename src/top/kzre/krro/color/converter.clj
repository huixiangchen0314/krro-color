(ns top.kzre.krro.color.converter
  "颜色空间转换函数。"
  (:require
   [top.kzre.krro.color.adaptation :as adapt]
   [top.kzre.krro.color.cmyk :as cmyk]
   [top.kzre.krro.color.distance :as dist]
   [top.kzre.krro.color.lab :as lab]
   [top.kzre.krro.color.oklab :as oklab]
   [top.kzre.krro.color.oklch :as oklch]
   [top.kzre.krro.color.util :as util]))

;; ── RGB ↔ HSL ──────────────────────────────────────────
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

;; ── RGB ↔ HSV ──────────────────────────────────────────
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
                :else   (+ 4 (/ (- r g) d)))
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

;; ── HSL ↔ HSV ──────────────────────────────────────────
(defn hsl->hsv
  "将 HSL 颜色转换为 HSV。"
  [c]
  (-> c hsl->rgb rgb->hsv))

(defn hsv->hsl
  "将 HSV 颜色转换为 HSL。"
  [c]
  (-> c hsv->rgb rgb->hsl))

;; ── RGB ↔ CMYK ──────────────────────────────────────────
(defn rgb->cmyk
  [c] (cmyk/rgb->cmyk c))

(defn cmyk->rgb
  [c] (cmyk/cmyk->rgb c))

;; ── RGB ↔ XYZ (D65) ────────────────────────────────────
;; sRGB -> XYZ 矩阵 (D65)
(def ^:private srgb->xyz-matrix
  [[0.4124564 0.3575761 0.1804375]
   [0.2126729 0.7151522 0.0721750]
   [0.0193339 0.1191920 0.9503041]])

(defn rgb->xyz
  "将 sRGB 颜色转换为 XYZ (D65)。"
  [c]
  (let [linear (mapv util/srgb->linear c)
        x (+ (* (srgb->xyz-matrix 0 0) (linear 0))
             (* (srgb->xyz-matrix 0 1) (linear 1))
             (* (srgb->xyz-matrix 0 2) (linear 2)))
        y (+ (* (srgb->xyz-matrix 1 0) (linear 0))
             (* (srgb->xyz-matrix 1 1) (linear 1))
             (* (srgb->xyz-matrix 1 2) (linear 2)))
        z (+ (* (srgb->xyz-matrix 2 0) (linear 0))
             (* (srgb->xyz-matrix 2 1) (linear 1))
             (* (srgb->xyz-matrix 2 2) (linear 2)))]
    [x y z]))

(def ^:private xyz->srgb-matrix
  [[ 3.2404542 -1.5371385 -0.4985314]
   [-0.9692660  1.8760108  0.0415560]
   [ 0.0556434 -0.2040259  1.0572252]])

(defn xyz->rgb
  "将 XYZ (D65) 颜色转换为 sRGB。"
  [[x y z]]
  (let [r (+ (* (xyz->srgb-matrix 0 0) x)
             (* (xyz->srgb-matrix 0 1) y)
             (* (xyz->srgb-matrix 0 2) z))
        g (+ (* (xyz->srgb-matrix 1 0) x)
             (* (xyz->srgb-matrix 1 1) y)
             (* (xyz->srgb-matrix 1 2) z))
        b (+ (* (xyz->srgb-matrix 2 0) x)
             (* (xyz->srgb-matrix 2 1) y)
             (* (xyz->srgb-matrix 2 2) z))]
    (mapv util/linear->srgb [r g b])))

;; ── RGB ↔ LAB (通过 XYZ) ────────────────────────────────
(defn rgb->lab
  "sRGB -> CIELAB。"
  [c]
  (-> c rgb->xyz lab/xyz->lab))

(defn lab->rgb
  "CIELAB -> sRGB。"
  [c]
  (-> c lab/lab->xyz xyz->rgb))


;; ── XYZ 空间下的色适应 ─────────────────────────────────
(defn xyz-d65->d50
  "将 XYZ (D65) 转换为 XYZ (D50)。"
  [xyz]
  (adapt/chromatic-adapt xyz adapt/d65 adapt/d50))

(defn xyz-d50->d65
  "将 XYZ (D50) 转换为 XYZ (D65)。"
  [xyz]
  (adapt/chromatic-adapt xyz adapt/d50 adapt/d65))

;; ── LAB 默认使用 D50，如果需要从 RGB (D65) 转换为 LAB (D50) ──
(defn rgb->lab-d50
  "sRGB (D65) -> CIELAB (D50)，通常用于印刷。"
  [c]
  (-> c rgb->xyz xyz-d65->d50 lab/xyz->lab))


;; ── sRGB ↔ OKLab ────────────────────────────────────────
(defn rgb->oklab
  "将 sRGB 颜色转换为 OKLab。"
  [c]
  (oklab/linear-rgb->oklab (mapv util/srgb->linear c)))

(defn oklab->rgb
  "将 OKLab 颜色转换为 sRGB。"
  [c]
  (mapv util/linear->srgb (oklab/oklab->linear-rgb c)))

;; ── sRGB ↔ OKLCH ────────────────────────────────────────
(defn rgb->oklch
  "将 sRGB 颜色转换为 OKLCH。"
  [c]
  (-> c rgb->oklab oklch/oklab->oklch))

(defn oklch->rgb
  "将 OKLCH 颜色转换为 sRGB。"
  [c]
  (-> c oklch/oklch->oklab oklab->rgb))


(defn delta-e76-rgb
  "计算两个 sRGB 颜色之间的 ΔE76。"
  [c1 c2]
  (dist/delta-e76 (rgb->lab c1) (rgb->lab c2)))


(defn delta-e94-rgb
  "计算两个 sRGB 颜色之间的 ΔE94。"
  [c1 c2]
  (dist/delta-e94 (rgb->lab c1) (rgb->lab c2)))

(defn delta-e2000-rgb
  "计算两个 sRGB 颜色之间的 ΔE2000。"
  [c1 c2]
  (dist/delta-e2000 (rgb->lab c1) (rgb->lab c2)))

(defn contrast-ratio-rgb
  "计算两个 sRGB 颜色之间的 WCAG 对比度。"
  [c1 c2]
  (dist/contrast-ratio c1 c2))



;; ── 广色域空间：Display P3 ──────────────────────────────
(def ^:private p3->xyz-matrix
  [[0.4865709486482162 0.26566769316909306 0.1982172852343625]
   [0.2289745640697488 0.6917385218365064  0.079286914093745]
   [0.0                0.04511338185890264 1.043944368900976]])

(def ^:private xyz->p3-matrix
  [[ 2.493496911941425   -0.9313836179191239 -0.40271078445071684]
   [-0.8294889695615747   1.7626640603183463  0.023624685841943577]
   [ 0.03584583024378447 -0.07617238926804182 0.9568845240076872]])

(defn p3->xyz
  "将 Display P3 颜色（线性 RGB）转换为 XYZ (D65)。"
  [c]
  (let [linear (mapv util/srgb->linear c)]  ;; P3 使用相同的 gamma 函数
    [(+ (* (p3->xyz-matrix 0 0) (linear 0))
        (* (p3->xyz-matrix 0 1) (linear 1))
        (* (p3->xyz-matrix 0 2) (linear 2)))
     (+ (* (p3->xyz-matrix 1 0) (linear 0))
        (* (p3->xyz-matrix 1 1) (linear 1))
        (* (p3->xyz-matrix 1 2) (linear 2)))
     (+ (* (p3->xyz-matrix 2 0) (linear 0))
        (* (p3->xyz-matrix 2 1) (linear 1))
        (* (p3->xyz-matrix 2 2) (linear 2)))]))

(defn xyz->p3
  "将 XYZ (D65) 颜色转换为 Display P3（线性 RGB），然后 gamma 校正。"
  [[x y z]]
  (let [r (+ (* (xyz->p3-matrix 0 0) x)
             (* (xyz->p3-matrix 0 1) y)
             (* (xyz->p3-matrix 0 2) z))
        g (+ (* (xyz->p3-matrix 1 0) x)
             (* (xyz->p3-matrix 1 1) y)
             (* (xyz->p3-matrix 1 2) z))
        b (+ (* (xyz->p3-matrix 2 0) x)
             (* (xyz->p3-matrix 2 1) y)
             (* (xyz->p3-matrix 2 2) z))]
    (mapv util/linear->srgb [r g b])))  ;; 使用相同的 gamma 函数

;; ── Adobe RGB (1998) ─────────────────────────────────────
(def ^:private adobe-rgb->xyz-matrix
  [[0.5767309 0.1855540 0.1881852]
   [0.2973769 0.6273491 0.0752741]
   [0.0270343 0.0706872 0.9911085]])

(def ^:private xyz->adobe-rgb-matrix
  [[ 2.0413690 -0.5649464 -0.3446944]
   [-0.9692660  1.8760108  0.0415560]
   [ 0.0134474 -0.1183897  1.0154096]])

(defn adobe-rgb->xyz
  "将 Adobe RGB 颜色（线性）转换为 XYZ (D65)。"
  [c]
  (let [linear (mapv #(Math/pow % 2.19921875) c)]  ;; Adobe RGB gamma = 2.2
    [(+ (* (adobe-rgb->xyz-matrix 0 0) (linear 0))
        (* (adobe-rgb->xyz-matrix 0 1) (linear 1))
        (* (adobe-rgb->xyz-matrix 0 2) (linear 2)))
     (+ (* (adobe-rgb->xyz-matrix 1 0) (linear 0))
        (* (adobe-rgb->xyz-matrix 1 1) (linear 1))
        (* (adobe-rgb->xyz-matrix 1 2) (linear 2)))
     (+ (* (adobe-rgb->xyz-matrix 2 0) (linear 0))
        (* (adobe-rgb->xyz-matrix 2 1) (linear 1))
        (* (adobe-rgb->xyz-matrix 2 2) (linear 2)))]))

(defn xyz->adobe-rgb
  "将 XYZ (D65) 颜色转换为 Adobe RGB（线性），然后 gamma 校正。"
  [[x y z]]
  (let [r (+ (* (xyz->adobe-rgb-matrix 0 0) x)
             (* (xyz->adobe-rgb-matrix 0 1) y)
             (* (xyz->adobe-rgb-matrix 0 2) z))
        g (+ (* (xyz->adobe-rgb-matrix 1 0) x)
             (* (xyz->adobe-rgb-matrix 1 1) y)
             (* (xyz->adobe-rgb-matrix 1 2) z))
        b (+ (* (xyz->adobe-rgb-matrix 2 0) x)
             (* (xyz->adobe-rgb-matrix 2 1) y)
             (* (xyz->adobe-rgb-matrix 2 2) z))]
    (mapv #(Math/pow % (/ 1.0 2.19921875)) [r g b])))  ;; gamma 2.2

;; ── ProPhoto RGB ─────────────────────────────────────────
(def ^:private prophoto->xyz-matrix
  [[0.7976749 0.1351917 0.0313534]
   [0.2880402 0.7118741 0.0000857]
   [0.0        0.0        0.8252100]])

(def ^:private xyz->prophoto-matrix
  [[ 1.3459433 -0.2556075 -0.0511118]
   [-0.5445989  1.5081673  0.0205351]
   [ 0.0         0.0        1.2118128]])

(defn prophoto->xyz
  "将 ProPhoto RGB 颜色（线性）转换为 XYZ (D50)。注意：ProPhoto 使用 D50 白点。"
  [c]
  (let [linear (mapv #(Math/pow % 1.8) c)]  ;; ProPhoto gamma = 1.8
    [(+ (* (prophoto->xyz-matrix 0 0) (linear 0))
        (* (prophoto->xyz-matrix 0 1) (linear 1))
        (* (prophoto->xyz-matrix 0 2) (linear 2)))
     (+ (* (prophoto->xyz-matrix 1 0) (linear 0))
        (* (prophoto->xyz-matrix 1 1) (linear 1))
        (* (prophoto->xyz-matrix 1 2) (linear 2)))
     (+ (* (prophoto->xyz-matrix 2 0) (linear 0))
        (* (prophoto->xyz-matrix 2 1) (linear 1))
        (* (prophoto->xyz-matrix 2 2) (linear 2)))]))

(defn xyz->prophoto
  "将 XYZ (D50) 颜色转换为 ProPhoto RGB（线性），然后 gamma 校正。"
  [[x y z]]
  (let [r (+ (* (xyz->prophoto-matrix 0 0) x)
             (* (xyz->prophoto-matrix 0 1) y)
             (* (xyz->prophoto-matrix 0 2) z))
        g (+ (* (xyz->prophoto-matrix 1 0) x)
             (* (xyz->prophoto-matrix 1 1) y)
             (* (xyz->prophoto-matrix 1 2) z))
        b (+ (* (xyz->prophoto-matrix 2 0) x)
             (* (xyz->prophoto-matrix 2 1) y)
             (* (xyz->prophoto-matrix 2 2) z))]
    (mapv #(Math/pow % (/ 1.0 1.8)) [r g b])))  ;; gamma 1.8



;; 在 converter.clj 中添加：
(defn matrix-mult [m v]
  "矩阵乘以向量。"
  (mapv #(apply + (map * % v)) m))

(defn matrix-inv
  "计算 3x3 矩阵的逆（用于 RGB->XYZ 矩阵求逆）。"
  [[[a b c] [d e f] [g h i]]]
  (let [det (+ (* a (- (* e i) (* f h)))
               (* b (- (* f g) (* d i)))
               (* c (- (* d h) (* e g))))]
    (if (zero? det)
      (throw (ex-info "Matrix is singular, cannot invert." {}))
      (let [inv-det (/ 1.0 det)]
        [[(* inv-det (- (* e i) (* f h)))
          (* inv-det (- (* c h) (* b i)))
          (* inv-det (- (* b f) (* c e)))]
         [(* inv-det (- (* f g) (* d i)))
          (* inv-det (- (* a i) (* c g)))
          (* inv-det (- (* c d) (* a f)))]
         [(* inv-det (- (* d h) (* e g)))
          (* inv-det (- (* b g) (* a h)))
          (* inv-det (- (* a e) (* b d)))]]))))