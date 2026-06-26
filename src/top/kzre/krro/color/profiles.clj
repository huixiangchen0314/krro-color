(ns top.kzre.krro.color.profiles
  "所有支持的色彩空间定义。")

(def ^:private d65-white [0.95047 1.0 1.08883])
(def ^:private d50-white [0.96422 1.0 0.82521])

(def color-spaces
  {:srgb
   {:primaries [[0.4124564 0.3575761 0.1804375]
                [0.2126729 0.7151522 0.0721750]
                [0.0193339 0.1191920 0.9503041]]
    :primaries-inv [[ 3.2404542 -1.5371385 -0.4985314]
                    [-0.9692660  1.8760108  0.0415560]
                    [ 0.0556434 -0.2040259  1.0572252]]
    :white-point d65-white
    :gamma {:decode (fn [x] (if (<= x 0.04045)
                              (/ x 12.92)
                              (Math/pow (/ (+ x 0.055) 1.055) 2.4)))
            :encode (fn [x] (if (<= x 0.0031308)
                              (* 12.92 x)
                              (- (* 1.055 (Math/pow x (/ 1.0 2.4))) 0.055)))}}

   :display-p3
   {:primaries [[0.4865709486482162 0.26566769316909306 0.1982172852343625]
                [0.2289745640697488 0.6917385218365064  0.079286914093745]
                [0.0                0.04511338185890264 1.043944368900976]]
    :primaries-inv [[ 2.493496911941425   -0.9313836179191239 -0.40271078445071684]
                    [-0.8294889695615747   1.7626640603183463  0.023624685841943577]
                    [ 0.03584583024378447 -0.07617238926804182 0.9568845240076872]]
    :white-point d65-white
    :gamma {:decode (fn [x] (if (<= x 0.04045)
                              (/ x 12.92)
                              (Math/pow (/ (+ x 0.055) 1.055) 2.4)))
            :encode (fn [x] (if (<= x 0.0031308)
                              (* 12.92 x)
                              (- (* 1.055 (Math/pow x (/ 1.0 2.4))) 0.055)))}}

   :adobe-rgb
   {:primaries [[0.5767309 0.1855540 0.1881852]
                [0.2973769 0.6273491 0.0752741]
                [0.0270343 0.0706872 0.9911085]]
    :primaries-inv [[ 2.0413690 -0.5649464 -0.3446944]
                    [-0.9692660  1.8760108  0.0415560]
                    [ 0.0134474 -0.1183897  1.0154096]]
    :white-point d65-white
    :gamma {:decode (fn [x] (Math/pow x 2.19921875))
            :encode (fn [x] (Math/pow x (/ 1.0 2.19921875)))}}

   :prophoto
   {:primaries [[0.7976749 0.1351917 0.0313534]
                [0.2880402 0.7118741 0.0000857]
                [0.0        0.0        0.8252100]]
    :primaries-inv [[ 1.3459433 -0.2556075 -0.0511118]
                    [-0.5445989  1.5081673  0.0205351]
                    [ 0.0         0.0        1.2118128]]
    :white-point d50-white
    :gamma {:decode (fn [x] (Math/pow x 1.8))
            :encode (fn [x] (Math/pow x (/ 1.0 1.8)))}}})

(defn get-space
  "根据关键字返回色彩空间定义，不存在则抛异常。"
  [k]
  (or (get color-spaces k)
      (throw (ex-info "Unknown color space" {:key k}))))

(defn matrix-mult
  "3x3 矩阵乘向量。"
  [m v]
  (mapv #(apply + (map * % v)) m))

(defn matrix-inv
  "计算 3x3 矩阵的逆（备用，当 profiles 中未提供预置逆矩阵时使用）。"
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