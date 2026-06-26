(ns top.kzre.krro.color.util
  (:require [top.kzre.krro.color.profiles :as profiles]))

(defn clamp [lo hi x] (max lo (min hi x)))

(defn deg->rad [d] (* d (/ Math/PI 180.0)))
(defn rad->deg [r] (* r (/ 180.0 Math/PI)))

(defn srgb->linear [c]
  (let [decode (get-in (profiles/get-space :srgb) [:gamma :decode])]
    (mapv decode c)))

(defn linear->srgb [c]
  (let [encode (get-in (profiles/get-space :srgb) [:gamma :encode])]
    (mapv encode c)))