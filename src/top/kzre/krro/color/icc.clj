(ns top.kzre.krro.color.icc
  "ICC 配置文件解析与转换。支持矩阵型及 LUT 型 (lut16Type) 标签。"
  (:require
   [clojure.java.io :as io]
   [top.kzre.krro.color.profiles :as profiles])
  (:import
   (java.io ByteArrayOutputStream)
   (java.nio ByteBuffer ByteOrder)))

;; ── 基础读取（ByteBuffer） ─────────────────────────────
(defn- read-u8 [^ByteBuffer buf] (Byte/toUnsignedInt (.get buf)))
(defn- read-u16 [^ByteBuffer buf] (Short/toUnsignedInt (.getShort buf)))
(defn- read-u32 [^ByteBuffer buf] (.getInt buf))
(defn- read-s15Fixed16 [^ByteBuffer buf] (/ (read-u32 buf) 65536.0))
(defn- read-xyz [^ByteBuffer buf]
  [(read-s15Fixed16 buf) (read-s15Fixed16 buf) (read-s15Fixed16 buf)])

;; ── 标签表解析 ─────────────────────────────────────────
(defn- read-tag-table [^ByteBuffer buf header-size]
  (.position buf header-size)
  (let [count (read-u32 buf)]
    (into {}
          (for [i (range count)]
            (let [sig (apply str (repeatedly 4 #(char (.get buf))))
                  offset (read-u32 buf)
                  size   (read-u32 buf)]
              [sig {:offset offset :size size}])))))

;; ── 解析曲线 (curveType/paramType) ────────────────────
(defn- parse-curve [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (if (= type-sig 0x63757276) ;; 'curv'
      (let [n (read-u32 buf)]
        (vec (repeatedly n #(read-u16 buf))))
      nil)))  ;; 其他类型暂不支持

;; ── 解析 lut16Type ─────────────────────────────────────
(defn- parse-lut16 [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (when (= type-sig 0x6d667432) ;; 'mft2'
      (let [_version (read-u32 buf)
            input-channels (read-u8 buf)
            output-channels (read-u8 buf)
            clut-size (read-u8 buf)
            _reserved (read-u8 buf)
            ;; 矩阵（3x3， 12 个 s15Fixed16）
            matrix (vec (repeatedly 12 #(read-s15Fixed16 buf)))
            ;; 输入表数量
            input-table-entries (read-u16 buf)
            ;; 输出表数量
            output-table-entries (read-u16 buf)
            ;; 输入表（16-bit 量化）
            input-tables (vec (for [c (range input-channels)]
                                (vec (repeatedly input-table-entries #(read-u16 buf)))))
            ;; CLUT 值（三维查找表，大小 clut-size^3 * output-channels）
            clut-values (let [n (* clut-size clut-size clut-size output-channels)]
                          (vec (repeatedly n #(read-u16 buf))))
            ;; 输出表
            output-tables (vec (for [c (range output-channels)]
                                 (vec (repeatedly output-table-entries #(read-u16 buf)))))]
        {:input-channels input-channels
         :output-channels output-channels
         :clut-size clut-size
         :matrix matrix
         :input-tables input-tables
         :output-tables output-tables
         :clut-values clut-values}))))

;; ── 解析矩阵（用于矩阵型配置文件）───────────────────────
(defn- parse-matrix [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (when (= type-sig 0x6d617474) ;; 'matt'
      (vec (repeatedly 9 #(read-s15Fixed16 buf))))))

;; ── 解析 XYZ 标签（用于白点/原色）───────────────────────
(defn- parse-xyz-tag [^ByteBuffer buf offset]
  (.position buf (+ offset 8))
  (read-xyz buf))

;; ── 主解析函数 ─────────────────────────────────────────
(defn parse-icc-buffer
  "从 ByteBuffer 解析 ICC 配置文件，返回一个 map，包含 :header 和可能的 :transform 信息。"
  [^ByteBuffer buf]
  (when (= 0x61637370 (.getInt buf 36)) ;; 'acsp'
    (.order buf ByteOrder/BIG_ENDIAN)
    (let [tag-table-offset (.getInt buf 128)
          tag-table (read-tag-table buf tag-table-offset)]
      ;; 提取常用标签
      (let [white-point (when-let [tag (get tag-table "wtpt")]
                          (parse-xyz-tag buf (:offset tag)))
            rXYZ (when-let [tag (get tag-table "rXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            gXYZ (when-let [tag (get tag-table "gXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            bXYZ (when-let [tag (get tag-table "bXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            ;; 曲线（如果存在）
            rTRC (when-let [tag (get tag-table "rTRC")]
                   (parse-curve buf (:offset tag)))
            gTRC (when-let [tag (get tag-table "gTRC")]
                   (parse-curve buf (:offset tag)))
            bTRC (when-let [tag (get tag-table "bTRC")]
                   (parse-curve buf (:offset tag)))
            ;; A2B0 标签（LUT 型）
            a2b0 (when-let [tag (get tag-table "A2B0")]
                   (parse-lut16 buf (:offset tag)))
            ;; 其他可能的 LUT 标签可类似添加
            ]
        {:white-point white-point
         :rXYZ rXYZ :gXYZ gXYZ :bXYZ bXYZ
         :rTRC rTRC :gTRC gTRC :bTRC bTRC
         :a2b0 a2b0
         :raw-tags tag-table}))))

;; ── 构建 ICC 颜色转换函数 ──────────────────────────────
;; icc.clj 中关键的修正部分
(defn make-icc-transform [icc-data direction]
  (letfn [(apply-curve [val curve]
            (cond
              (fn? curve) (curve val)                      ;; 若是函数，直接调用
              (number? curve) (Math/pow val curve)         ;; 若是数字，视为 gamma
              (vector? curve)                               ;; 若是向量，视为查找表
              (let [idx (* val (dec (count curve)))
                    i (int idx)
                    t (- idx i)
                    v1 (nth curve i)
                    v2 (nth curve (min (inc i) (dec (count curve))))]
                (+ v1 (* t (- v2 v1))))
              :else val))]
    (cond
      (and (= direction :a2b) (:rXYZ icc-data) (:rTRC icc-data))
      (let [rXYZ (:rXYZ icc-data) gXYZ (:gXYZ icc-data) bXYZ (:bXYZ icc-data)
            m [[(first rXYZ) (first gXYZ) (first bXYZ)]
               [(second rXYZ) (second gXYZ) (second bXYZ)]
               [(nth rXYZ 2) (nth gXYZ 2) (nth bXYZ 2)]]
            r-curve (:rTRC icc-data)
            g-curve (:gTRC icc-data)
            b-curve (:bTRC icc-data)]
        (fn [r g b]
          (let [r' (apply-curve r r-curve)
                g' (apply-curve g g-curve)
                b' (apply-curve b b-curve)
                m00 (get-in m [0 0]) m01 (get-in m [0 1]) m02 (get-in m [0 2])
                m10 (get-in m [1 0]) m11 (get-in m [1 1]) m12 (get-in m [1 2])
                m20 (get-in m [2 0]) m21 (get-in m [2 1]) m22 (get-in m [2 2])]
            [(+ (* m00 r') (* m01 g') (* m02 b'))
             (+ (* m10 r') (* m11 g') (* m12 b'))
             (+ (* m20 r') (* m21 g') (* m22 b'))])))
      (and (= direction :a2b) (:a2b0 icc-data))
      (fn [_r _g _b] (throw (ex-info "LUT transform not yet implemented" {})))
      :else (throw (ex-info "Unsupported ICC transform" {:direction direction})))))

;; srgb-icc-data 使用 decode 函数作为曲线
(def srgb-icc-data
  (let [s (profiles/get-space :srgb)
        gamma-fn (:decode (:gamma s))]   ;; 解码为线性
    {:white-point (:white-point s)
     :rXYZ (mapv first (:primaries s))
     :gXYZ (mapv second (:primaries s))
     :bXYZ (mapv last (:primaries s))
     :rTRC gamma-fn
     :gTRC gamma-fn
     :bTRC gamma-fn}))

;; ── 便捷加载函数 ──────────────────────────────────────
(defn load-icc-file
  "从文件加载 ICC 配置，返回解析后的 map。"
  [path]
  (with-open [in  (io/input-stream path)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [bytes (.toByteArray out)
          buf   (ByteBuffer/wrap bytes)]
      (parse-icc-buffer buf))))