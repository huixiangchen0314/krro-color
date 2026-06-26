(ns top.kzre.krro.color.harmony
  "色彩调和：生成多种调和方案。所有返回值均经过钳制，保证在 [0,1] 范围内。"
  (:require [top.kzre.krro.color.converter :as conv]
            [top.kzre.krro.color.util :as util]))

(defn complementary
  "返回基色的互补色（色相+180°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h' (mod (+ (first hsl) 180.0) 360.0)]
    (mapv (fn [x] (util/clamp 0.0 1.0 x))
          (conv/hsl->rgb [h' (second hsl) (nth hsl 2)]))))

(defn triadic
  "返回基色的两个三角色（±120°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    [(mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 120.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (- h 120.0) 360.0) s l]))]))

(defn split-complementary
  "返回基色的两个分裂互补色（±150°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    [(mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 150.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (- h 150.0) 360.0) s l]))]))

(defn tetradic
  "返回基色的三个矩形色（±60°, 180°, 240°）。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    [(mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 60.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 180.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 240.0) 360.0) s l]))]))

(defn tetradic-alt
  "交替矩形方案：主色 + 120°, 180°, 300°。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    [(mapv (fn [x] (util/clamp 0.0 1.0 x)) rgb)
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 120.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 180.0) 360.0) s l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x))
           (conv/hsl->rgb [(mod (+ h 300.0) 360.0) s l]))]))

(defn hexadic
  "六色调和：主色 + 60°, 120°, 180°, 240°, 300°。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    (mapv (fn [angle]
            (mapv (fn [x] (util/clamp 0.0 1.0 x))
                  (conv/hsl->rgb [(mod (+ h angle) 360.0) s l])))
          [0 60 120 180 240 300])))

(defn monochromatic
  "单色系：从主色出发，生成 5 个不同明度/饱和度的变体。"
  [rgb]
  (let [hsl (conv/rgb->hsl rgb)
        h (first hsl) s (second hsl) l (nth hsl 2)]
    [(mapv (fn [x] (util/clamp 0.0 1.0 x)) (conv/hsl->rgb [h (max 0.0 (- s 0.3)) l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x)) (conv/hsl->rgb [h (min 1.0 (+ s 0.2)) l]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x)) (conv/hsl->rgb [h s (max 0.0 (- l 0.2))]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x)) (conv/hsl->rgb [h s (min 1.0 (+ l 0.2))]))
     (mapv (fn [x] (util/clamp 0.0 1.0 x)) (conv/hsl->rgb [h (max 0.0 (- s 0.5)) (min 1.0 (+ l 0.1))]))]))