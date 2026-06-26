(ns top.kzre.krro.color.icc
  "ICC 配置文件解析与转换。支持矩阵型及 LUT 型 (lut16Type) 标签。"
  (:require [clojure.java.io :as io]
            [top.kzre.krro.color.util :as util]
            [top.kzre.krro.color.profiles :as profiles])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.nio ByteBuffer ByteOrder)))
;; TODO 输入/输出表的应用以及矩阵处理


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

;; ── 解析曲线 (curveType) ──────────────────────────────
(defn- parse-curve [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (when (= type-sig 0x63757276) ;; 'curv'
      (let [n (read-u32 buf)]
        (if (zero? n)
          ;; 零条曲线表示简单 gamma = 1.0
          (fn [x] x)
          ;; 否则第一条元素可能是 gamma，若为0则表示查找表
          (let [first-val (read-u16 buf)]
            (if (zero? first-val)
              ;; 查找表
              (vec (repeatedly (dec n) #(read-u16 buf)))
              ;; 单 gamma
              (double first-val))))))))

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
            ;; 矩阵（3x3, 12 个 s15Fixed16）
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

;; ── 解析矩阵 (matrixType) ──────────────────────────────
(defn- parse-matrix [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (when (= type-sig 0x6d617474) ;; 'matt'
      (vec (repeatedly 9 #(read-s15Fixed16 buf))))))

;; ── 解析 XYZ 标签 ──────────────────────────────────────
(defn- parse-xyz-tag [^ByteBuffer buf offset]
  (.position buf (+ offset 8))
  (read-xyz buf))

;; ── 主解析函数 ─────────────────────────────────────────
(defn parse-icc-buffer
  [^ByteBuffer buf]
  (when (= 0x61637370 (.getInt buf 36))
    (.order buf ByteOrder/BIG_ENDIAN)
    (let [tag-table-offset (.getInt buf 128)
          tag-table (read-tag-table buf tag-table-offset)]
      (let [white-point (when-let [tag (get tag-table "wtpt")]
                          (parse-xyz-tag buf (:offset tag)))
            rXYZ (when-let [tag (get tag-table "rXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            gXYZ (when-let [tag (get tag-table "gXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            bXYZ (when-let [tag (get tag-table "bXYZ")]
                   (parse-xyz-tag buf (:offset tag)))
            rTRC (when-let [tag (get tag-table "rTRC")]
                   (parse-curve buf (:offset tag)))
            gTRC (when-let [tag (get tag-table "gTRC")]
                   (parse-curve buf (:offset tag)))
            bTRC (when-let [tag (get tag-table "bTRC")]
                   (parse-curve buf (:offset tag)))
            a2b0 (when-let [tag (get tag-table "A2B0")]
                   (parse-lut16 buf (:offset tag)))
            ;; B2A0 等其他标签可类似添加
            ]
        {:white-point white-point
         :rXYZ rXYZ :gXYZ gXYZ :bXYZ bXYZ
         :rTRC rTRC :gTRC gTRC :bTRC bTRC
         :a2b0 a2b0
         :raw-tags tag-table}))))

;; ── 三线性插值 ─────────────────────────────────────────
(defn- trilinear-interpolate [clut-size output-channels clut-values r g b]
  (let [max-idx (dec clut-size)
        ;; 将颜色值映射到 CLUT 索引空间 [0, max-idx]
        idx-r (* r max-idx)
        idx-g (* g max-idx)
        idx-b (* b max-idx)
        r0 (int (Math/floor idx-r)) r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g)) g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b)) b1 (min (inc b0) max-idx)
        rd (- idx-r r0) gd (- idx-g g0) bd (- idx-b b0)
        ;; 根据 (r,g,b) 索引 CLUT 值，CLUT 存储顺序：r 变化最快，然后 g，最后 b
        ;; 通常 CLUT 存储顺序：b + clut-size*(g + clut-size*r) 或类似，这里使用标准 ICC 顺序：r + clut-size*(g + clut-size*b)
        get-val (fn [ri gi bi]
                  (let [idx (+ ri (* clut-size (+ gi (* clut-size bi))))  ;; 注意这里 bi 是 b 索引，gi 是 g，ri 是 r
                        base (* idx output-channels)]
                    (for [c (range output-channels)]
                      (nth clut-values (+ base c) 0))))
        c000 (get-val r0 g0 b0) c001 (get-val r0 g0 b1)
        c010 (get-val r0 g1 b0) c011 (get-val r0 g1 b1)
        c100 (get-val r1 g0 b0) c101 (get-val r1 g0 b1)
        c110 (get-val r1 g1 b0) c111 (get-val r1 g1 b1)
        ;; 插值
        lerp (fn [a b t] (+ a (* t (- b a))))
        ;; 在 r 方向插值
        c00 (mapv #(lerp %1 %2 rd) c000 c100)
        c01 (mapv #(lerp %1 %2 rd) c001 c101)
        c10 (mapv #(lerp %1 %2 rd) c010 c110)
        c11 (mapv #(lerp %1 %2 rd) c011 c111)
        ;; 在 g 方向插值
        c0 (mapv #(lerp %1 %2 gd) c00 c10)
        c1 (mapv #(lerp %1 %2 gd) c01 c11)
        ;; 在 b 方向插值
        result (mapv #(lerp %1 %2 bd) c0 c1)]
    result))

;; ── LUT 型转换实现 ────────────────────────────────────
(defn- apply-lut-transform [lut r g b]
  (let [{:keys [input-channels output-channels input-tables output-tables matrix clut-size clut-values]} lut
        ;; 假设输入通道为 3（RGB）
        ;; 第一步：应用输入表（每个通道独立的 1D LUT）
        apply-input-table (fn [val table]
                            (if (empty? table)
                              val
                              (let [idx (* val (dec (count table)))
                                    i (int idx)
                                    t (- idx i)
                                    v1 (nth table i)
                                    v2 (nth table (min (inc i) (dec (count table))))]
                                (+ v1 (* t (- v2 v1))))))
        r1 (apply-input-table r (first input-tables))
        g1 (apply-input-table g (second input-tables))
        b1 (apply-input-table b (nth input-tables 2))
        ;; 第二步：3D CLUT 插值
        clut-result (trilinear-interpolate clut-size output-channels clut-values r1 g1 b1)
        ;; 第三步：应用输出表（每个输出通道）
        out (for [c (range output-channels)]
              (let [val (nth clut-result c)
                    table (nth output-tables c)]
                (apply-input-table val table)))
        ;; 第四步：应用矩阵（如果有），通常矩阵用于调整 PCS 值
        ;; 这里矩阵是 3x4？实际上 ICC 中矩阵通常是 3x3 用于 XYZ 转换，如果是 12 个值则是 3x4
        ;; 简单起见，暂时忽略矩阵，直接返回 out 作为 XYZ 或 LAB 值
        ]
    (vec out)))

;; ── 构建 ICC 颜色转换函数 ──────────────────────────────
(defn make-icc-transform
  [icc-data direction]
  (letfn [(apply-curve [val curve]
            (cond
              (fn? curve) (curve val)
              (number? curve) (Math/pow val curve)
              (vector? curve)
              (let [idx (* val (dec (count curve)))
                    i (int idx)
                    t (- idx i)
                    v1 (nth curve i)
                    v2 (nth curve (min (inc i) (dec (count curve))))]
                (+ v1 (* t (- v2 v1))))
              :else val))]
    (cond
      ;; 矩阵 + 曲线（常见 RGB 工作空间）
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
      ;; LUT 型 (A2B0)
      (and (= direction :a2b) (:a2b0 icc-data))
      (let [lut (:a2b0 icc-data)]
        (fn [r g b]
          (apply-lut-transform lut r g b)))
      :else (throw (ex-info "Unsupported ICC transform" {:direction direction})))))

;; ── 内建 sRGB ICC 配置文件数据 ─────────────────────────
(def srgb-icc-data
  (let [s (profiles/get-space :srgb)
        gamma-fn (:decode (:gamma s))]
    {:white-point (:white-point s)
     :rXYZ (mapv first (:primaries s))
     :gXYZ (mapv second (:primaries s))
     :bXYZ (mapv last (:primaries s))
     :rTRC gamma-fn
     :gTRC gamma-fn
     :bTRC gamma-fn}))

;; ── 便捷加载函数 ──────────────────────────────────────
(defn load-icc-file
  [path]
  (with-open [in  (io/input-stream path)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [bytes (.toByteArray out)
          buf   (ByteBuffer/wrap bytes)]
      (parse-icc-buffer buf))))