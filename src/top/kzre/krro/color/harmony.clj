(ns top.kzre.krro.color.harmony
  "色彩调和：生成互补色、三角色、分裂互补、矩形等方案。"
  (:require [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.util :as util]))

(defn- wrap-hue [h]
  (mod h 360.0))

(defn complementary
  "返回基色的互补色（色相+180°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h' (wrap-hue (+ (first hsl) 180.0))]
    (conv/hsl->rgb [h' (second hsl) (nth hsl 2)])))

(defn triadic
  "返回基色的两个三角色（±120°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl)
        s (second hsl)
        l (nth hsl 2)]
    [(conv/hsl->rgb [(wrap-hue (+ h 120.0)) s l])
     (conv/hsl->rgb [(wrap-hue (- h 120.0)) s l])]))

(defn split-complementary
  "返回基色的两个分裂互补色（±150°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl)
        s (second hsl)
        l (nth hsl 2)]
    [(conv/hsl->rgb [(wrap-hue (+ h 150.0)) s l])
     (conv/hsl->rgb [(wrap-hue (- h 150.0)) s l])]))

(defn tetradic
  "返回基色的三个矩形色（±60°, 180°, 240°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl)
        s (second hsl)
        l (nth hsl 2)]
    [(conv/hsl->rgb [(wrap-hue (+ h 60.0)) s l])
     (conv/hsl->rgb [(wrap-hue (+ h 180.0)) s l])
     (conv/hsl->rgb [(wrap-hue (+ h 240.0)) s l])]))