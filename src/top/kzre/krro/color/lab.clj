(ns top.kzre.krro.color.lab
  "CIELAB 颜色空间操作。"
  (:require [top.kzre.krro.color.xyz :as xyz]))

(defn lab
  "创建 CIELAB 颜色向量 [l a b]。"
  [l a b]
  [l a b])

(defn- f [t]
  (if (> t 0.008856)
    (Math/pow t (/ 1.0 3.0))
    (+ (* 7.787 t) (/ 16.0 116.0))))

(defn- inv-f [t]
  (if (> t 0.206893)
    (* t t t)
    (/ (- t (/ 16.0 116.0)) 7.787)))

(defn xyz->lab
  "将 XYZ 颜色转换为 CIELAB。使用 D65 参考白点。"
  [[x y z]]
  (let [xn (first xyz/d65-white)
        yn (second xyz/d65-white)
        zn (nth xyz/d65-white 2)
        fx (f (/ x xn))
        fy (f (/ y yn))
        fz (f (/ z zn))]
    [(dec (* 116 fy))
     (* 500 (- fx fy))
     (* 200 (- fy fz))]))

(defn lab->xyz
  "将 CIELAB 颜色转换为 XYZ。使用 D65 参考白点。"
  [[l a b]]
  (let [yn (second xyz/d65-white)
        fy (/ (+ l 16) 116)
        fx (+ fy (/ a 500))
        fz (- fy (/ b 200))
        xn (first xyz/d65-white)
        zn (nth xyz/d65-white 2)]
    [(* xn (inv-f fx))
     (* yn (inv-f fy))
     (* zn (inv-f fz))]))