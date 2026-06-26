(ns top.kzre.krro.color.lut
  "3D LUT 支持：解析 .cube 文件并应用颜色变换。"
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io BufferedReader FileReader]))

;; ── .cube 文件解析 ──────────────────────────────────────
(defn- parse-cube-line [line]
  (when (and (not (str/blank? line))
             (not (str/starts-with? line "#")))
    (let [parts (str/split (str/trim line) #"\s+")]
      (when (every? #(re-matches #"-?\d+\.?\d*" %) parts)
        (mapv #(Double/parseDouble %) parts)))))

(defn load-cube
  "加载 .cube 文件，返回解析后的 LUT 数据映射：
   {:title \"...\", :size N, :domain [[min1 max1] [min2 max2] [min3 max3]], :data [[r g b]...]}"
  [path]
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)
          header {:title nil :size 32 :domain [[0.0 1.0] [0.0 1.0] [0.0 1.0]]}
          ;; 处理头部
          header (reduce
                   (fn [h line]
                     (cond
                       (str/starts-with? line "TITLE") (assoc h :title (subs line 6))
                       (str/starts-with? line "LUT_3D_SIZE") (assoc h :size (Integer/parseInt (second (str/split line #"\s+"))))
                       (str/starts-with? line "DOMAIN_MIN") (assoc-in h [:domain 0 0] (Double/parseDouble (nth (str/split line #"\s+") 1)))
                       (str/starts-with? line "DOMAIN_MAX") (assoc-in h [:domain 0 1] (Double/parseDouble (nth (str/split line #"\s+") 1)))
                       :else h))
                   header
                   lines)
          ;; 过滤数据行
          data (keep parse-cube-line lines)]
      (assoc header :data (vec data)))))

;; ── 应用 3D LUT ────────────────────────────────────────
(defn- trilinear-interpolate
  "三线性插值。"
  [lut c0 c1 c2 [r g b]]
  (let [size (:size lut)
        max-idx (dec size)
        idx-r (* r max-idx)
        idx-g (* g max-idx)
        idx-b (* b max-idx)
        r0 (int (Math/floor idx-r))
        r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g))
        g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b))
        b1 (min (inc b0) max-idx)
        rd (- idx-r r0)
        gd (- idx-g g0)
        bd (- idx-b b0)
        get-val (fn [ri gi bi]
                  (get-in (:data lut) [(+ ri (* size gi) (* size size bi))]))
        c000 (get-val r0 g0 b0)
        c001 (get-val r0 g0 b1)
        c010 (get-val r0 g1 b0)
        c011 (get-val r0 g1 b1)
        c100 (get-val r1 g0 b0)
        c101 (get-val r1 g0 b1)
        c110 (get-val r1 g1 b0)
        c111 (get-val r1 g1 b1)
        lerp (fn [a b t] (+ a (* t (- b a))))
        c00 (mapv #(lerp %1 %2 rd) c000 c100)
        c01 (mapv #(lerp %1 %2 rd) c001 c101)
        c10 (mapv #(lerp %1 %2 rd) c010 c110)
        c11 (mapv #(lerp %1 %2 rd) c011 c111)
        c0 (mapv #(lerp %1 %2 gd) c00 c10)
        c1 (mapv #(lerp %1 %2 gd) c01 c11)
        result (mapv #(lerp %1 %2 bd) c0 c1)]
    result))

(defn apply-lut
  "将加载的 LUT 应用到 RGB 颜色（分量 0-1），返回转换后的颜色。"
  [lut [r g b]]
  (trilinear-interpolate lut r g b [r g b]))