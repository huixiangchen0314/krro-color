(ns top.kzre.krro.color.lab
  "CIELAB 颜色空间操作。"
  (:require [top.kzre.krro.color.profiles :as profiles]))

(defn- get-white-point []
  (:white-point (profiles/get-space :srgb)))

(defn lab
  "创建 CIELAB 颜色向量 [l a b]。"
  [l a b]
  [l a b])

(defn- f [t]
  (if (> t 0.008856)
    (Math/cbrt t)                         ;; 替代 Math/pow t 1/3
    (+ (* 7.787 t) (/ 16.0 116.0))))

(defn- inv-f [t]
  (if (> t 0.206893)
    (* t t t)
    (/ (- t (/ 16.0 116.0)) 7.787)))

(defn xyz->lab
  "将 XYZ 颜色转换为 CIELAB。使用 sRGB 空间的白点。"
  [xyz]
  (let [[x y z] xyz
        wp (get-white-point)
        xn (first wp)
        yn (second wp)
        zn (nth wp 2)
        fx (f (/ x xn))
        fy (f (/ y yn))
        fz (f (/ z zn))]
    [(- (* 116 fy) 16)
     (* 500 (- fx fy))
     (* 200 (- fy fz))]))

(defn lab->xyz
  "将 CIELAB 颜色转换为 XYZ。使用 sRGB 空间的白点。"
  [lab]
  (let [[l a b] lab
        wp (get-white-point)
        yn (second wp)
        fy (/ (+ l 16) 116)
        fx (+ fy (/ a 500))
        fz (- fy (/ b 200))
        xn (first wp)
        zn (nth wp 2)]
    [(* xn (inv-f fx))
     (* yn (inv-f fy))
     (* zn (inv-f fz))]))