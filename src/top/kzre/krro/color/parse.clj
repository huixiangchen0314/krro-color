(ns top.kzre.krro.color.parse
  "解析颜色字符串为 RGBA 向量。"
  (:require [clojure.string :as str]
            [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

(defn hex->rgb
  "解析 HEX 字符串（#rrggbb 或 #rgb 或 #rrggbbaa）。"
  [s]
  (let [s (str/replace s #"#" "")
        len (count s)]
    (case len
      3 (let [[r g b] (map #(Integer/parseInt (str %) 16) (seq s))
              r (+ (* r 16) r)   ;; 展开短格式
              g (+ (* g 16) g)
              b (+ (* b 16) b)]
          [r g b 255])
      6 (let [r (Integer/parseInt (subs s 0 2) 16)
              g (Integer/parseInt (subs s 2 4) 16)
              b (Integer/parseInt (subs s 4 6) 16)]
          [r g b 255])
      8 (let [r (Integer/parseInt (subs s 0 2) 16)
              g (Integer/parseInt (subs s 2 4) 16)
              b (Integer/parseInt (subs s 4 6) 16)
              a (Integer/parseInt (subs s 6 8) 16)]
          [r g b a])
      (throw (IllegalArgumentException. (str "Invalid hex color: " s))))))

(defn normalize
  "将 0-255 整数向量转为 0-1 浮点。"
  [ints]
  (mapv #(/ % 255.0) ints))

(defn parse
  "解析任意颜色字符串，返回 [r g b a] 均 0-1。"
  [s]
  (cond
    (str/starts-with? s "#")
    (-> s hex->rgb normalize)
    :else
    (throw (IllegalArgumentException. (str "Unsupported format: " s)))))