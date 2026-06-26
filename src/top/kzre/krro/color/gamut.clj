(ns top.kzre.krro.color.gamut
  "色域映射：相对色度、绝对色度、感知意图。
   所有映射均在 XYZ 空间完成，使用公开的转换函数。"
  (:require
   [top.kzre.krro.color.adaptation :as adapt]
   [top.kzre.krro.color.converter :as conv]
   [top.kzre.krro.color.oklab :as oklab]
   [top.kzre.krro.color.util :as util]))

;; ── 辅助 gamma 函数 ─────────────────────────────────
(defn- linear-rgb->srgb [c] (mapv util/linear->srgb c))
(defn- srgb->linear-rgb [c] (mapv util/srgb->linear c))

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

;; ── 感知意图（基于 OKLab 的色度压缩） ──────────────
(defn perceptual
  [xyz-color src-white target-white xyz->target-rgb]
  (let [adapted (adapt/chromatic-adapt xyz-color src-white target-white)
        srgb (conv/xyz->rgb adapted)
        linear-rgb (mapv util/srgb->linear srgb)
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
                (relative-colorimetric xyz-color src-white target-white xyz->target-rgb)))))))))

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

;; ── 通用色域映射调度 ─────────────────────────────────
(defn map-color
  "将颜色从源空间映射到目标色域。"
  [color src-space dst-space intent]
  (let [{:keys [white-point rgb->xyz]} (space-info src-space)
        dst-info (space-info dst-space)
        dst-white (:white-point dst-info)
        xyz->dst (:xyz->rgb dst-info)
        xyz (rgb->xyz color)
        linear (case intent
                 :relative-colorimetric (relative-colorimetric xyz white-point dst-white xyz->dst)
                 :absolute-colorimetric (absolute-colorimetric xyz xyz->dst)
                 :perceptual (perceptual xyz white-point dst-white xyz->dst))]
    linear))