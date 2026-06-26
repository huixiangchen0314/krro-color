(ns top.kzre.krro.color.util)

(defn clamp [lo hi x] (max lo (min hi x)))

(defn deg->rad [d] (* d (/ Math/PI 180.0)))
(defn rad->deg [r] (* r (/ 180.0 Math/PI)))

(defn sigmoid
  "Sigmoid 函数，将 x 映射到 (0,1)。"
  [x]
  (/ 1.0 (+ 1.0 (Math/exp (- x)))))