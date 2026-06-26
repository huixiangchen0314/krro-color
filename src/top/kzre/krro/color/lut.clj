(ns top.kzre.krro.color.lut
  "高级 3D LUT 支持：加载 .cube、.3dl、.blut，保存，生成，合并，缩放，
   应用至颜色和图像，支持三线性及四面体插值。"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [top.kzre.krro.color.util :as util])
  (:import [java.io DataInputStream ByteArrayOutputStream]
           [java.nio ByteBuffer ByteOrder]
           [java.awt.image BufferedImage]))

;; ── LUT 数据结构 ──────────────────────────────────────
;; LUT 定义为 {:size int, :data [[r g b] ...]}
;; 数据排列：r 变化最快，其次 g，最外层 b
;; 索引公式：idx = r + size * (g + size * b)

;; ── 加载 .cube 文件 ───────────────────────────────────
(defn load-cube
  "读取 .cube 格式的 3D LUT 文件。"
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
        (throw (ex-info "Inconsistent LUT size" {:expected (* size size size)
                                                 :actual   (count data-lines)})))
      {:size size
       :data (vec (for [line data-lines]
                    (->> (str/split line #"\s+")
                         (mapv #(Double/parseDouble %)))))})))

;; ── 加载 .3dl 文件（文本格式，第一行为 size，后续为数据）──
(defn load-3dl
  "读取 .3dl 格式 LUT 文件。第一行为三个相同尺寸值，后续每行为 RGB 数据。"
  [path]
  (with-open [rdr (io/reader path)]
    (let [lines (line-seq rdr)
          non-empty (remove str/blank? lines)
          size-line (first non-empty)
          size (-> (re-find #"\d+" size-line) Integer/parseInt)
          data-lines (rest non-empty)]
      (when (not= (count data-lines) (* size size size))
        (throw (ex-info "Inconsistent LUT size in .3dl" {:expected (* size size size)
                                                         :actual   (count data-lines)})))
      {:size size
       :data (vec (for [line data-lines]
                    (mapv #(Double/parseDouble %) (str/split line #"\s+"))))})))

;; ── 加载 .blut 文件（二进制，简化版）───────────────
(defn load-blut
  "读取 .blut 二进制 LUT 文件。假设头部 32 字节，偏移 0 为 int 尺寸，后面是 float RGB 三元组。"
  [path]
  (with-open [in  (-> path io/input-stream DataInputStream.)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [buf (ByteBuffer/wrap (.toByteArray out))
          _   (.order buf ByteOrder/LITTLE_ENDIAN)
          size (.getInt buf 0)          ;; 假设尺寸存储在偏移 0
          _   (.position buf 32)        ;; 跳过 32 字节头部
          n   (* size size size)
          data (vec (repeatedly n #(vector (.getFloat buf) (.getFloat buf) (.getFloat buf))))]
      {:size size :data data})))

;; ── 保存 .cube 文件 ────────────────────────────────────
(defn save-cube
  "将 LUT 保存为 .cube 文件。"
  [lut path]
  (let [{:keys [size data]} lut]
    (with-open [w (io/writer path)]
      (.write w (str "# Created by krro-color\n"))
      (.write w (str "LUT_3D_SIZE " size "\n"))
      (doseq [[r g b] data]
        (.write w (str r " " g " " b "\n"))))))

;; ── 保存 .3dl 文件（简单格式） ──────────────────────────
(defn save-3dl
  "将 LUT 保存为 .3dl 文件（第一行为尺寸，后续为 RGB 数据）。"
  [lut path]
  (let [{:keys [size data]} lut]
    (with-open [w (io/writer path)]
      (.write w (str size " " size " " size "\n"))
      (doseq [[r g b] data]
        (.write w (str r " " g " " b "\n"))))))

;; ── 从变换函数生成 LUT ─────────────────────────────────
(defn generate-lut
  "根据颜色变换函数 f (fn [r g b] -> [r' g' b']) 生成大小为 size 的 3D LUT。
   输入/输出颜色空间目前均为 sRGB，可后续扩展。"
  [size f]
  (let [max-val (double (dec size))
        data    (vec (for [b (range size)
                           g (range size)
                           r (range size)]
                       (let [r-in (/ (double r) max-val)
                             g-in (/ (double g) max-val)
                             b-in (/ (double b) max-val)
                             [r-out g-out b-out] (f r-in g-in b-in)]
                         [r-out g-out b-out])))]
    {:size size :data data}))

;; ── 三线性插值 ────────────────────────────────────────
(defn- trilinear-interpolate
  [lut-size data r g b]
  (let [max-idx (dec lut-size)
        idx-r   (* r max-idx)
        idx-g   (* g max-idx)
        idx-b   (* b max-idx)
        r0 (int (Math/floor idx-r))  r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g))  g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b))  b1 (min (inc b0) max-idx)
        rd (- idx-r r0)  gd (- idx-g g0)  bd (- idx-b b0)
        get-val (fn [ri gi bi]
                  (nth data (+ ri (* lut-size (+ gi (* lut-size bi)))) [0.0 0.0 0.0]))
        c000 (get-val r0 g0 b0) c001 (get-val r0 g0 b1)
        c010 (get-val r0 g1 b0) c011 (get-val r0 g1 b1)
        c100 (get-val r1 g0 b0) c101 (get-val r1 g0 b1)
        c110 (get-val r1 g1 b0) c111 (get-val r1 g1 b1)
        lerp (fn [a b t] (+ a (* t (- b a))))
        c00 (mapv #(lerp %1 %2 rd) c000 c100)
        c01 (mapv #(lerp %1 %2 rd) c001 c101)
        c10 (mapv #(lerp %1 %2 rd) c010 c110)
        c11 (mapv #(lerp %1 %2 rd) c011 c111)
        c0  (mapv #(lerp %1 %2 gd) c00 c10)
        c1  (mapv #(lerp %1 %2 gd) c01 c11)]
    (mapv #(lerp %1 %2 bd) c0 c1)))

;; ── 四面体插值 ────────────────────────────────────────
(defn- tetrahedral-interpolate
  [lut-size data r g b]
  (let [max-idx (dec lut-size)
        idx-r (* r max-idx)  idx-g (* g max-idx)  idx-b (* b max-idx)
        r0 (int (Math/floor idx-r))  r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g))  g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b))  b1 (min (inc b0) max-idx)
        dr (- idx-r r0)  dg (- idx-g g0)  db (- idx-b b0)
        get-val (fn [ri gi bi]
                  (nth data (+ ri (* lut-size (+ gi (* lut-size bi)))) [0.0 0.0 0.0]))]
    (if (>= dr dg db)
      (let [c000 (get-val r0 g0 b0) c100 (get-val r1 g0 b0) c110 (get-val r1 g1 b0) c111 (get-val r1 g1 b1)]
        (mapv #(+ %1 (* dr (- %2 %1)) (* dg (- %3 %2)) (* db (- %4 %3))) c000 c100 c110 c111))
      (if (>= dr db dg)
        (let [c000 (get-val r0 g0 b0) c100 (get-val r1 g0 b0) c101 (get-val r1 g0 b1) c111 (get-val r1 g1 b1)]
          (mapv #(+ %1 (* dr (- %2 %1)) (* db (- %3 %2)) (* dg (- %4 %3))) c000 c100 c101 c111))
        (if (>= db dr dg)
          (let [c000 (get-val r0 g0 b0) c001 (get-val r0 g0 b1) c101 (get-val r1 g0 b1) c111 (get-val r1 g1 b1)]
            (mapv #(+ %1 (* db (- %2 %1)) (* dr (- %3 %2)) (* dg (- %4 %3))) c000 c001 c101 c111))
          (if (>= db dg dr)
            (let [c000 (get-val r0 g0 b0) c001 (get-val r0 g0 b1) c011 (get-val r0 g1 b1) c111 (get-val r1 g1 b1)]
              (mapv #(+ %1 (* db (- %2 %1)) (* dg (- %3 %2)) (* dr (- %4 %3))) c000 c001 c011 c111))
            (if (>= dg dr db)
              (let [c000 (get-val r0 g0 b0) c010 (get-val r0 g1 b0) c110 (get-val r1 g1 b0) c111 (get-val r1 g1 b1)]
                (mapv #(+ %1 (* dg (- %2 %1)) (* dr (- %3 %2)) (* db (- %4 %3))) c000 c010 c110 c111))
              ;; dg >= db >= dr
              (let [c000 (get-val r0 g0 b0) c010 (get-val r0 g1 b0) c011 (get-val r0 g1 b1) c111 (get-val r1 g1 b1)]
                (mapv #(+ %1 (* dg (- %2 %1)) (* db (- %3 %2)) (* dr (- %4 %3))) c000 c010 c011 c111)))))))))

;; ── 应用 LUT 到单个颜色 ─────────────────────────────────
(defn apply-lut
  "对单个颜色 [r g b] (0..1) 应用 LUT。
   mode 为 :trilinear (默认) 或 :tetrahedral。
   输入颜色会被钳制到 [0,1]。"
  [lut [r g b] & {:keys [mode] :or {mode :trilinear}}]
  (let [r (util/clamp 0.0 1.0 r)
        g (util/clamp 0.0 1.0 g)
        b (util/clamp 0.0 1.0 b)]
    (case mode
      :trilinear   (trilinear-interpolate (:size lut) (:data lut) r g b)
      :tetrahedral (tetrahedral-interpolate (:size lut) (:data lut) r g b)
      (throw (ex-info "Unknown interpolation mode" {:mode mode})))))

;; ── 合并两个 LUT ────────────────────────────────────────
(defn merge-luts
  "将两个 LUT 合并为一个新的 LUT，效果相当于先应用 lut1 再应用 lut2。
   新 LUT 的尺寸取两者中的较大值。"
  [lut1 lut2]
  (let [size (max (:size lut1) (:size lut2))]
    (generate-lut size (fn [r g b]
                         (let [c1 (apply-lut lut1 [r g b])]
                           (apply-lut lut2 c1))))))

;; ── 缩放 LUT 到新的尺寸 ─────────────────────────────────
(defn scale-lut
  "将 LUT 缩放到新的尺寸 new-size，使用三线性插值采样。"
  [lut new-size]
  (generate-lut new-size (fn [r g b]
                           (apply-lut lut [r g b] :mode :trilinear))))

;; ── 应用 LUT 到 BufferedImage ────────────────────────────
(defn apply-lut-image
  "对 BufferedImage 应用 3D LUT，返回新的图像。图像应为 TYPE_INT_RGB 或兼容格式。"
  [lut image]
  (let [width  (.getWidth image)
        height (.getHeight image)
        out    (BufferedImage. width height BufferedImage/TYPE_INT_RGB)]
    (dotimes [y height]
      (dotimes [x width]
        (let [rgb (.getRGB image x y)
              r   (double (/ (bit-and (bit-shift-right rgb 16) 0xFF) 255.0))
              g   (double (/ (bit-and (bit-shift-right rgb 8) 0xFF) 255.0))
              b   (double (/ (bit-and rgb 0xFF) 255.0))
              [r' g' b'] (apply-lut lut [r g b])
              r'  (int (* 255.0 (util/clamp 0.0 1.0 r')))
              g'  (int (* 255.0 (util/clamp 0.0 1.0 g')))
              b'  (int (* 255.0 (util/clamp 0.0 1.0 b')))
              new-rgb (bit-or (bit-shift-left r' 16) (bit-shift-left g' 8) b')]
          (.setRGB out x y new-rgb))))
    out))