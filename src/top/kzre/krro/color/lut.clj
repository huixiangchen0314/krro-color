(ns top.kzre.krro.color.lut
  "高级 3D LUT 支持：加载 .cube、.3dl、.blut 文件，生成 LUT，应用 (三线性/四面体)。"
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [top.kzre.krro.color.util :as util])
  (:import [java.io DataInputStream FileInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]))

;; ── LUT 数据结构 ──────────────────────────────────────
;; LUT 定义为 {:size int, :data [[[r g b] ...]] (三维向量数组，按索引公式排列)}
;; 索引公式: idx = r + size * (g + size * b)

;; ── 加载 .cube 文件 ───────────────────────────────────
(defn load-cube
  "读取 .cube 格式的 3D LUT 文件。返回 {:size size, :data [[r g b] ...]}。"
  [path]
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)
          size (->> lines
                    (filter #(str/starts-with? % "LUT_3D_SIZE"))
                    first
                    (re-find #"\d+")
                    Integer/parseInt)
          data-lines (->> lines
                          (drop-while #(not (str/starts-with? % "LUT_3D_SIZE")))
                          rest
                          (take-while #(not (str/blank? %)))
                          (remove #(str/starts-with? % "#")))]
      (when (not= (count data-lines) (* size size size))
        (throw (ex-info "Inconsistent LUT size" {:expected (* size size size) :actual (count data-lines)})))
      {:size size
       :data (vec (for [line data-lines]
                    (->> (str/split line #"\s+")
                         (mapv #(Double/parseDouble %)))))})))

;; ── 加载 .3dl 文件（类似 .cube，但可能包含 HEADER） ─────
(defn load-3dl
  "读取 .3dl 格式的 LUT 文件。假设格式与 .cube 相同。"
  [path]
  (load-cube path)) ;; 大部分情况下相同，可扩展

;; ── 加载 .blut 文件（二进制，简化处理） ──────────────
(defn load-blut
  "读取 .blut 二进制 LUT 文件。假设文件头为 32 字节（包含尺寸信息），后续为 float RGB 三元组。"
  [path]
  (with-open [in (-> path io/input-stream DataInputStream.)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [buf (ByteBuffer/wrap (.toByteArray out))
          _ (.order buf ByteOrder/LITTLE_ENDIAN)
          header-size 32
          size (.getInt buf 0) ;; 假设偏移 0 处为整数字段
          ;; 更健壮的方式是解析头部，此处简化
          _ (.position buf header-size)
          n (* size size size)
          data (vec (for [i (range n)]
                      [(.getFloat buf) (.getFloat buf) (.getFloat buf)]))]
      {:size size :data data})))

;; ── 从变换函数生成 LUT ──────────────────────────────
(defn generate-lut
  "根据颜色变换函数 f (fn [r g b] -> [r' g' b']) 生成大小为 size 的 3D LUT。"
  [size f]
  (let [n (* size size size)
        data (vec (for [b (range size)
                        g (range size)
                        r (range size)]
                    (f (/ r (dec size))
                       (/ g (dec size))
                       (/ b (dec size)))))]
    {:size size :data data}))

;; ── 插值辅助函数 ────────────────────────────────────
(defn- trilinear-interpolate
  [lut-size data r g b]
  (let [max-idx (dec lut-size)
        idx-r (* r max-idx)
        idx-g (* g max-idx)
        idx-b (* b max-idx)
        r0 (int (Math/floor idx-r)) r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g)) g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b)) b1 (min (inc b0) max-idx)
        rd (- idx-r r0) gd (- idx-g g0) bd (- idx-b b0)
        get-val (fn [ri gi bi]
                  (nth data (+ ri (* lut-size (+ gi (* lut-size bi)))) [0 0 0]))
        c000 (get-val r0 g0 b0) c001 (get-val r0 g0 b1)
        c010 (get-val r0 g1 b0) c011 (get-val r0 g1 b1)
        c100 (get-val r1 g0 b0) c101 (get-val r1 g0 b1)
        c110 (get-val r1 g1 b0) c111 (get-val r1 g1 b1)
        lerp (fn [a b t] (+ a (* t (- b a))))
        c00 (mapv #(lerp %1 %2 rd) c000 c100)
        c01 (mapv #(lerp %1 %2 rd) c001 c101)
        c10 (mapv #(lerp %1 %2 rd) c010 c110)
        c11 (mapv #(lerp %1 %2 rd) c011 c111)
        c0 (mapv #(lerp %1 %2 gd) c00 c10)
        c1 (mapv #(lerp %1 %2 gd) c01 c11)
        result (mapv #(lerp %1 %2 bd) c0 c1)]
    result))

;; ── 四面体插值 ────────────────────────────────────
(defn- tetrahedral-interpolate
  [lut-size data r g b]
  (let [max-idx (dec lut-size)
        idx-r (* r max-idx)
        idx-g (* g max-idx)
        idx-b (* b max-idx)
        r0 (int (Math/floor idx-r)) r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g)) g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b)) b1 (min (inc b0) max-idx)
        dr (- idx-r r0) dg (- idx-g g0) db (- idx-b b0)
        get-val (fn [ri gi bi]
                  (nth data (+ ri (* lut-size (+ gi (* lut-size bi)))) [0 0 0]))]
    (if (>= dr dg db)
      ;; dr >= dg >= db
      (let [c000 (get-val r0 g0 b0)
            c100 (get-val r1 g0 b0)
            c110 (get-val r1 g1 b0)
            c111 (get-val r1 g1 b1)]
        (mapv (fn [c000 c100 c110 c111]
                (+ c000
                   (* dr (- c100 c000))
                   (* dg (- c110 c100))
                   (* db (- c111 c110))))
              c000 c100 c110 c111))
      (if (>= dr db dg)
        ;; dr >= db >= dg
        (let [c000 (get-val r0 g0 b0)
              c100 (get-val r1 g0 b0)
              c101 (get-val r1 g0 b1)
              c111 (get-val r1 g1 b1)]
          (mapv (fn [c000 c100 c101 c111]
                  (+ c000
                     (* dr (- c100 c000))
                     (* db (- c101 c100))
                     (* dg (- c111 c101))))
                c000 c100 c101 c111))
        (if (>= db dr dg)
          ;; db >= dr >= dg
          (let [c000 (get-val r0 g0 b0)
                c001 (get-val r0 g0 b1)
                c101 (get-val r1 g0 b1)
                c111 (get-val r1 g1 b1)]
            (mapv (fn [c000 c001 c101 c111]
                    (+ c000
                       (* db (- c001 c000))
                       (* dr (- c101 c001))
                       (* dg (- c111 c101))))
                  c000 c001 c101 c111))
          (if (>= db dg dr)
            ;; db >= dg >= dr
            (let [c000 (get-val r0 g0 b0)
                  c001 (get-val r0 g0 b1)
                  c011 (get-val r0 g1 b1)
                  c111 (get-val r1 g1 b1)]
              (mapv (fn [c000 c001 c011 c111]
                      (+ c000
                         (* db (- c001 c000))
                         (* dg (- c011 c001))
                         (* dr (- c111 c011))))
                    c000 c001 c011 c111))
            (if (>= dg dr db)
              ;; dg >= dr >= db
              (let [c000 (get-val r0 g0 b0)
                    c010 (get-val r0 g1 b0)
                    c110 (get-val r1 g1 b0)
                    c111 (get-val r1 g1 b1)]
                (mapv (fn [c000 c010 c110 c111]
                        (+ c000
                           (* dg (- c010 c000))
                           (* dr (- c110 c010))
                           (* db (- c111 c110))))
                      c000 c010 c110 c111))
              ;; dg >= db >= dr
              (let [c000 (get-val r0 g0 b0)
                    c010 (get-val r0 g1 b0)
                    c011 (get-val r0 g1 b1)
                    c111 (get-val r1 g1 b1)]
                (mapv (fn [c000 c010 c011 c111]
                        (+ c000
                           (* dg (- c010 c000))
                           (* db (- c011 c010))
                           (* dr (- c111 c011))))
                      c000 c010 c011 c111)))))))))

;; ── 应用 LUT（默认三线性，可选四面体） ──────────────
(defn apply-lut
  "对单个颜色 [r g b] (0..1) 应用 LUT。mode 为 :trilinear (默认) 或 :tetrahedral。"
  [lut [r g b] & {:keys [mode] :or {mode :trilinear}}]
  (case mode
    :trilinear (trilinear-interpolate (:size lut) (:data lut) r g b)
    :tetrahedral (tetrahedral-interpolate (:size lut) (:data lut) r g b)
    (throw (ex-info "Unknown interpolation mode" {:mode mode}))))

;; ── 保存 LUT 为 .cube 文件 ──────────────────────────
(defn save-cube
  "将 LUT 保存为 .cube 文件。"
  [lut path]
  (let [{:keys [size data]} lut]
    (with-open [w (io/writer path)]
      (.write w (str "# Created by krro-color\n"))
      (.write w (str "LUT_3D_SIZE " size "\n"))
      (doseq [[r g b] data]
        (.write w (str r " " g " " b "\n"))))))