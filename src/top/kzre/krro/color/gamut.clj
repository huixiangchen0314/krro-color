(ns top.kzre.krro.color.gamut
  "色域映射：相对色度、绝对色度、感知意图（含 SGCK）。"
  (:require
    [top.kzre.krro.color.adaptation :as adapt]
    [top.kzre.krro.color.converter :as conv]
    [top.kzre.krro.color.oklab :as oklab]
    [top.kzre.krro.color.profiles :as profiles]
    [top.kzre.krro.color.util :as util]))

;; ── 辅助 gamma 函数 ─────────────────────────────────
(defn- linear-rgb->srgb [c]
  (let [{:keys [encode]} (:gamma (profiles/get-space :srgb))]
    (mapv encode c)))

(defn- srgb->linear-rgb [c]
  (let [{:keys [decode]} (:gamma (profiles/get-space :srgb))]
    (mapv decode c)))

;; ── 空间参数辅助函数 ─────────────────────────────────
(defn- space-info [space]
  (case space
    :srgb       {:white-point adapt/d65
                 :rgb->xyz (fn [c] (-> c srgb->linear-rgb conv/rgb->xyz))
                 :xyz->rgb (fn [c] (-> c conv/xyz->rgb linear-rgb->srgb))}
    :p3         {:white-point adapt/d65
                 :rgb->xyz (fn [c] (-> c srgb->linear-rgb conv/p3->xyz))
                 :xyz->rgb (fn [c] (-> c conv/xyz->p3 linear-rgb->srgb))}
    :adobe-rgb  {:white-point adapt/d65
                 :rgb->xyz (fn [c]
                             (let [gamma (mapv (fn [x] (Math/pow x 2.19921875)) c)]
                               (conv/adobe-rgb->xyz gamma)))
                 :xyz->rgb (fn [c]
                             (let [linear (conv/xyz->adobe-rgb c)]
                               (mapv (fn [x] (Math/pow x (/ 1.0 2.19921875))) linear)))}
    :prophoto   {:white-point adapt/d50
                 :rgb->xyz (fn [c]
                             (let [gamma (mapv (fn [x] (Math/pow x 1.8)) c)]
                               (conv/prophoto->xyz gamma)))
                 :xyz->rgb (fn [c]
                             (let [linear (conv/xyz->prophoto c)]
                               (mapv (fn [x] (Math/pow x (/ 1.0 1.8))) linear)))}))

;; ── 相对色度意图 ───────────────────────────────────
(defn relative-colorimetric
  [xyz-color src-white target-white xyz->target-rgb]
  (let [adapted (adapt/chromatic-adapt xyz-color src-white target-white)
        linear-rgb (xyz->target-rgb adapted)]
    (mapv #(util/clamp 0.0 1.0 %) linear-rgb)))

;; ── 绝对色度意图 ───────────────────────────────────
(defn absolute-colorimetric
  [xyz-color xyz->target-rgb]
  (relative-colorimetric xyz-color adapt/d65 adapt/d65 xyz->target-rgb))

;; ── Sigmoid 函数 ──────────────────────────────────────
(defn- sigmoid [x]
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))

(defn- inverse-sigmoid [y]
  (- (Math/log (/ y (- 1.0 y)))))

;; ── 目标色域边界估计（基于 OKLab 空间） ─────────────
(defn- estimate-gamut-boundary [L a b target-space-kw]
  (let [hue (Math/atan2 b a)
        {:keys [encode]} (:gamma (profiles/get-space target-space-kw))
        search-max-c (fn [L hue]
                       (loop [low 0.0 high 0.5]
                         (if (< (- high low) 0.0001)
                           low
                           (let [mid (/ (+ low high) 2.0)
                                 a' (* mid (Math/cos hue))
                                 b' (* mid (Math/sin hue))
                                 oklab [L a' b']
                                 linear-rgb (oklab/oklab->linear-rgb oklab)
                                 rgb (mapv encode linear-rgb)]
                             (if (every? #(<= 0.0 % 1.0) rgb)
                               (recur mid high)
                               (recur low mid))))))]
    (search-max-c L hue)))

;; ── SGCK 感知映射 ─────────────────────────────────────
(defn sgck
  [xyz-color src-white target-white xyz->target-rgb target-space-kw]
  (let [adapted (adapt/chromatic-adapt xyz-color src-white target-white)
        ;; 转换到线性 sRGB
        srgb (conv/xyz->rgb adapted)
        linear-rgb (srgb->linear-rgb srgb)
        oklab (oklab/linear-rgb->oklab linear-rgb)
        [L a b] oklab
        C (Math/sqrt (+ (* a a) (* b b)))
        ;; 如果色度极低，直接返回中性灰
        _ (if (< C 1e-10) (xyz->target-rgb adapted))
        C-max (estimate-gamut-boundary L a b target-space-kw)
        C-prime (if (> C C-max)
                  (* C-max (sigmoid (/ (- C C-max) C-max)))
                  C)
        scale (/ C-prime C)
        a' (* a scale)
        b' (* b scale)
        linear-rgb' (oklab/oklab->linear-rgb [L a' b'])
        {:keys [encode]} (:gamma (profiles/get-space target-space-kw))
        rgb' (mapv encode linear-rgb')]
    rgb'))

;; ── 感知意图调度 ─────────────────────────────────
(defn perceptual
  "感知映射，支持 :oklab (原实现) 和 :sgck (新算法)。"
  [xyz-color src-white target-white xyz->target-rgb target-space-kw mode]
  (case mode
    :oklab (let [adapted (adapt/chromatic-adapt xyz-color src-white target-white)
                 srgb (conv/xyz->rgb adapted)
                 linear-rgb (srgb->linear-rgb srgb)
                 oklab-color (oklab/linear-rgb->oklab linear-rgb)
                 [L a b] oklab-color]
             (if (or (> L 1.0) (< L 0.0))
               (relative-colorimetric xyz-color src-white target-white xyz->target-rgb)
               (loop [factor 1.0]
                 (let [a' (* factor a)
                       b' (* factor b)
                       linear-rgb' (oklab/oklab->linear-rgb [L a' b'])]
                   (if (every? #(<= 0.0 % 1.0) linear-rgb')
                     linear-rgb'
                     (let [next-factor (* factor 0.95)]
                       (if (> next-factor 0.01)
                         (recur next-factor)
                         (relative-colorimetric xyz-color src-white target-white xyz->target-rgb))))))))
    :sgck (sgck xyz-color src-white target-white xyz->target-rgb target-space-kw)
    (throw (ex-info "Unknown perceptual mode" {:mode mode}))))

;; ── 通用映射调度 ─────────────────────────────────
(defn map-color
  [color src-space dst-space intent & {:keys [perceptual-mode] :or {perceptual-mode :sgck}}]
  (if (= src-space dst-space)
    color   ;; 同空间，无需转换
    (let [{:keys [white-point rgb->xyz]} (space-info src-space)
          dst-info (space-info dst-space)
          dst-white (:white-point dst-info)
          xyz->dst (:xyz->rgb dst-info)
          xyz (rgb->xyz color)
          linear (case intent
                   :relative-colorimetric (relative-colorimetric xyz white-point dst-white xyz->dst)
                   :absolute-colorimetric (absolute-colorimetric xyz xyz->dst)
                   :perceptual (perceptual xyz white-point dst-white xyz->dst dst-space perceptual-mode))]
      linear)))