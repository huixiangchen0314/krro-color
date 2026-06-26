(ns top.kzre.krro.color.parse
  "解析颜色字符串为 RGBA 向量。"
  (:require
   [clojure.string :as str]
   [top.kzre.krro.color.converter :as conv]))

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

;; 新增辅助函数
(defn- parse-rgb-fn [s]
  (let [s (str/replace s #"(rgb|rgba)\(|\)|%| " "")
        parts (str/split s #",")
        r (/ (Integer/parseInt (parts 0)) 255.0)
        g (/ (Integer/parseInt (parts 1)) 255.0)
        b (/ (Integer/parseInt (parts 2)) 255.0)
        a (if (= (count parts) 4)
            (Double/parseDouble (parts 3))
            1.0)]
    [r g b a]))

(defn- parse-hsl-fn [s]
  (let [s (str/replace s #"hsl\(|\)|%| " "")
        parts (str/split s #",")
        h (Integer/parseInt (parts 0))
        s (/ (Integer/parseInt (parts 1)) 100.0)
        l (/ (Integer/parseInt (parts 2)) 100.0)
        rgb (conv/hsl->rgb [h s l])
        a (if (= (count parts) 4)
            (Double/parseDouble (parts 3))
            1.0)]
    (conj rgb a)))

(defn parse-css
  "解析 CSS 颜色字符串，返回 RGBA 向量。"
  [s]
  (cond
    (str/starts-with? s "rgb(") (parse-rgb-fn s)
    (str/starts-with? s "rgba(") (parse-rgb-fn s)
    (str/starts-with? s "hsl(") (parse-hsl-fn s)
    :else (throw (ex-info "Unsupported CSS color format" {:input s}))))

(defn parse
  "解析任意颜色字符串，返回 [r g b a] 均 0-1。"
  [s]
  (cond
    (str/starts-with? s "#")
    (-> s hex->rgb normalize)
    :else
    (throw (IllegalArgumentException. (str "Unsupported format: " s)))))