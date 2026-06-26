(ns top.kzre.krro.color.util)

(defn clamp [lo hi x] (max lo (min hi x)))

(defn deg->rad [d] (* d (/ Math/PI 180.0)))
(defn rad->deg [r] (* r (/ 180.0 Math/PI)))
