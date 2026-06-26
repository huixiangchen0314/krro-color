(ns top.kzre.krro.color.format
  "将颜色格式化为字符串。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

(defn- to-byte [c]
  (int (Math/round (* 255 (util/clamp 0.0 1.0 c)))))

(defn hex
  "输出 HEX 字符串，如 \"#ff0066\"。"
  ([c] (hex c false))
  ([c include-alpha]
   (let [r (to-byte (rgb/red c))
         g (to-byte (rgb/green c))
         b (to-byte (rgb/blue c))
         a (if include-alpha (to-byte (rgb/alpha c)) 255)]
     (if include-alpha
       (format "#%02x%02x%02x%02x" r g b a)
       (format "#%02x%02x%02x" r g b)))))

(defn rgba-string
  "输出 CSS rgba() 格式。"
  [c]
  (let [r (to-byte (rgb/red c))
        g (to-byte (rgb/green c))
        b (to-byte (rgb/blue c))
        a (rgb/alpha c)]
    (format "rgba(%d,%d,%d,%.2f)" r g b a)))