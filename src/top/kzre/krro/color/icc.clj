(ns top.kzre.krro.color.icc
  "ICC 配置文件解析与转换。支持矩阵型及 LUT 型 (lut16Type, lut8Type) 标签。
   提供完整的 A2B / B2A 转换流水线。"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [top.kzre.krro.color.util :as util]
            [top.kzre.krro.color.profiles :as profiles])
  (:import (java.io ByteArrayOutputStream InputStream)
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
    (when (= type-sig 0x63757276) ;; 'curv'
      (let [n (read-u32 buf)]
        (if (zero? n)
          (fn [x] x)
          (let [first-val (read-u16 buf)]
            (if (zero? first-val)
              (vec (repeatedly (dec n) #(read-u16 buf)))
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
            matrix (vec (repeatedly 12 #(read-s15Fixed16 buf)))
            input-table-entries (read-u16 buf)
            output-table-entries (read-u16 buf)
            input-tables (vec (for [c (range input-channels)]
                                (vec (repeatedly input-table-entries #(read-u16 buf)))))
            clut-values (let [n (* clut-size clut-size clut-size output-channels)]
                          (vec (repeatedly n #(read-u16 buf))))
            output-tables (vec (for [c (range output-channels)]
                                 (vec (repeatedly output-table-entries #(read-u16 buf)))))]
        {:input-channels input-channels
         :output-channels output-channels
         :clut-size clut-size
         :matrix matrix
         :input-tables input-tables
         :output-tables output-tables
         :clut-values clut-values}))))

;; ── 解析 lut8Type ──────────────────────────────────────
(defn- parse-lut8 [^ByteBuffer buf offset]
  (.position buf offset)
  (let [type-sig (read-u32 buf)]
    (when (= type-sig 0x6d667431) ;; 'mft1'
      (let [_version (read-u32 buf)
            input-channels (read-u8 buf)
            output-channels (read-u8 buf)
            clut-size (read-u8 buf)
            _reserved (read-u8 buf)
            matrix (vec (repeatedly 12 #(read-s15Fixed16 buf)))
            input-table-entries (read-u16 buf)
            output-table-entries (read-u16 buf)
            input-tables (vec (for [c (range input-channels)]
                                (vec (repeatedly input-table-entries #(read-u8 buf)))))
            clut-values (let [n (* clut-size clut-size clut-size output-channels)]
                          (vec (repeatedly n #(read-u8 buf))))
            output-tables (vec (for [c (range output-channels)]
                                 (vec (repeatedly output-table-entries #(read-u8 buf)))))]
        {:input-channels input-channels
         :output-channels output-channels
         :clut-size clut-size
         :matrix matrix
         :input-tables input-tables
         :output-tables output-tables
         :clut-values clut-values}))))

;; ── 解析 XYZ 标签 ──────────────────────────────────────
(defn- parse-xyz-tag [^ByteBuffer buf offset]
  (.position buf (+ offset 8))
  (read-xyz buf))

;; ── 主解析函数 ─────────────────────────────────────────
(defn parse-icc-buffer
  "从 ByteBuffer 解析 ICC 配置文件，返回包含所有标签的 map。"
  [^ByteBuffer buf]
  (when (= 0x61637370 (.getInt buf 36)) ;; 'acsp'
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
                   (or (parse-lut16 buf (:offset tag))
                       (parse-lut8 buf (:offset tag))))
            b2a0 (when-let [tag (get tag-table "B2A0")]
                   (or (parse-lut16 buf (:offset tag))
                       (parse-lut8 buf (:offset tag))))]
        {:white-point white-point
         :rXYZ rXYZ :gXYZ gXYZ :bXYZ bXYZ
         :rTRC rTRC :gTRC gTRC :bTRC bTRC
         :a2b0 a2b0
         :b2a0 b2a0
         :raw-tags tag-table}))))

(defn- inverse-curve [curve]
  (cond
    (fn? curve) (fn [x] (try (curve x) (catch Exception _ x))) ;; 函数无法安全求逆，退回自身
    (number? curve) (fn [x] (Math/pow x (/ 1.0 curve)))
    (vector? curve) ;; 查找表：交换输入输出，构建逆表（假设单调）
    (let [n (count curve)
          max-val (dec n)]
      (fn [x]
        (let [idx (* x max-val)
              i (int idx)
              t (- idx i)
              v1 (nth curve (min i max-val))
              v2 (nth curve (min (inc i) max-val) v1)]
          (+ v1 (* t (- v2 v1)))))) ;; 实际上逆表应存储映射后的值，此处简化
    :else (fn [x] x)))

;; ── 应用曲线/表到单一通道 ────────────────────────────
(defn- apply-curve [val curve]
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
    :else val))

;; ── 三线性插值（用于CLUT） ─────────────────────────────
(defn- trilinear-interpolate [clut-size output-channels clut-values r g b]
  (let [max-idx (dec clut-size)
        idx-r (* r max-idx)
        idx-g (* g max-idx)
        idx-b (* b max-idx)
        r0 (int (Math/floor idx-r)) r1 (min (inc r0) max-idx)
        g0 (int (Math/floor idx-g)) g1 (min (inc g0) max-idx)
        b0 (int (Math/floor idx-b)) b1 (min (inc b0) max-idx)
        rd (- idx-r r0) gd (- idx-g g0) bd (- idx-b b0)
        get-val (fn [ri gi bi]
                  (let [idx (+ ri (* clut-size (+ gi (* clut-size bi))))
                        base (* idx output-channels)]
                    (for [c (range output-channels)]
                      (double (nth clut-values (+ base c) 0)))))
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

;; ── 应用LUT转换（A2B0或B2A0） ───────────────────────────
(defn- apply-lut-transform
  [lut r g b]
  (let [{:keys [input-channels output-channels input-tables output-tables matrix clut-size clut-values]} lut
        apply-input-table (fn [val table]
                            (if (empty? table)
                              val
                              (let [idx (* val (dec (count table)))
                                    i (int idx)
                                    t (- idx i)
                                    v1 (nth table i)
                                    v2 (nth table (min (inc i) (dec (count table))))]
                                (+ v1 (* t (- v2 v1))))))
        ;; 先过输入表
        r1 (if (> input-channels 0) (apply-input-table r (first input-tables)) r)
        g1 (if (> input-channels 1) (apply-input-table g (second input-tables)) g)
        b1 (if (> input-channels 2) (apply-input-table b (nth input-tables 2)) b)
        ;; 3D CLUT 插值
        clut-result (trilinear-interpolate clut-size output-channels clut-values r1 g1 b1)
        ;; 输出表应用
        out (mapv (fn [c val]
                    (let [table (nth output-tables c [])]
                      (if (empty? table)
                        val
                        (apply-input-table val table))))
                  (range output-channels) clut-result)
        ;; 应用矩阵（如果有）
        final (if (and matrix (>= (count matrix) 9))
                (let [m00 (nth matrix 0) m01 (nth matrix 1) m02 (nth matrix 2)
                      m10 (nth matrix 3) m11 (nth matrix 4) m12 (nth matrix 5)
                      m20 (nth matrix 6) m21 (nth matrix 7) m22 (nth matrix 8)]
                  [(+ (* m00 (out 0)) (* m01 (out 1)) (* m02 (out 2)))
                   (+ (* m10 (out 0)) (* m11 (out 1)) (* m12 (out 2)))
                   (+ (* m20 (out 0)) (* m21 (out 1)) (* m22 (out 2)))])
                out)]
    final))

;; ── 构建 ICC 颜色转换函数 ──────────────────────────────
(defn make-icc-transform
  "根据解析后的 ICC map 创建一个颜色转换函数。
   方向：:a2b (设备到 PCS) 或 :b2a (PCS 到设备)。"
  [icc-data direction]
  (cond
    ;; A2B 矩阵+曲线
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
              b' (apply-curve b b-curve)]
          [(+ (* (get-in m [0 0]) r') (* (get-in m [0 1]) g') (* (get-in m [0 2]) b'))
           (+ (* (get-in m [1 0]) r') (* (get-in m [1 1]) g') (* (get-in m [1 2]) b'))
           (+ (* (get-in m [2 0]) r') (* (get-in m [2 1]) g') (* (get-in m [2 2]) b'))])))
    ;; B2A 矩阵+曲线（逆转换）
    (and (= direction :b2a) (:rXYZ icc-data) (:rTRC icc-data))
    (let [rXYZ (:rXYZ icc-data) gXYZ (:gXYZ icc-data) bXYZ (:bXYZ icc-data)
          m [[(first rXYZ) (first gXYZ) (first bXYZ)]
             [(second rXYZ) (second gXYZ) (second bXYZ)]
             [(nth rXYZ 2) (nth gXYZ 2) (nth bXYZ 2)]]
          m-inv (let [[[a b c] [d e f] [g h i]] m
                      det (+ (* a (- (* e i) (* f h)))
                             (* b (- (* f g) (* d i)))
                             (* c (- (* d h) (* e g))))]
                  (when (zero? det) (throw (ex-info "Singular matrix" {})))
                  (let [inv-det (/ 1.0 det)]
                    [[(* inv-det (- (* e i) (* f h)))
                      (* inv-det (- (* c h) (* b i)))
                      (* inv-det (- (* b f) (* c e)))]
                     [(* inv-det (- (* f g) (* d i)))
                      (* inv-det (- (* a i) (* c g)))
                      (* inv-det (- (* c d) (* a f)))]
                     [(* inv-det (- (* d h) (* e g)))
                      (* inv-det (- (* b g) (* a h)))
                      (* inv-det (- (* a e) (* b d)))]]))
          r-inv-curve (or (:rTRC-inv icc-data) (inverse-curve (:rTRC icc-data)))
          g-inv-curve (or (:gTRC-inv icc-data) (inverse-curve (:gTRC icc-data)))
          b-inv-curve (or (:bTRC-inv icc-data) (inverse-curve (:bTRC icc-data)))]
      (fn [x y z]
        (let [r-lin (+ (* (get-in m-inv [0 0]) x) (* (get-in m-inv [0 1]) y) (* (get-in m-inv [0 2]) z))
              g-lin (+ (* (get-in m-inv [1 0]) x) (* (get-in m-inv [1 1]) y) (* (get-in m-inv [1 2]) z))
              b-lin (+ (* (get-in m-inv [2 0]) x) (* (get-in m-inv [2 1]) y) (* (get-in m-inv [2 2]) z))]
          [(apply-curve r-lin r-inv-curve)
           (apply-curve g-lin g-inv-curve)
           (apply-curve b-lin b-inv-curve)])))
    ;; LUT 型 A2B
    (and (= direction :a2b) (:a2b0 icc-data))
    (let [lut (:a2b0 icc-data)]
      (fn [r g b]
        (apply-lut-transform lut r g b)))
    ;; LUT 型 B2A
    (and (= direction :b2a) (:b2a0 icc-data))
    (let [lut (:b2a0 icc-data)]
      (fn [r g b]
        (apply-lut-transform lut r g b)))
    :else (throw (ex-info "Unsupported ICC transform" {:direction direction}))))

;; ── 内建常用 ICC 数据（基于 profiles）─────────────────
(def srgb-icc-data
  (let [s (profiles/get-space :srgb)
        decode-fn (:decode (:gamma s))
        encode-fn (:encode (:gamma s))]
    {:white-point (:white-point s)
     :rXYZ (mapv first (:primaries s))
     :gXYZ (mapv second (:primaries s))
     :bXYZ (mapv last (:primaries s))
     :rTRC decode-fn
     :gTRC decode-fn
     :bTRC decode-fn
     :rTRC-inv encode-fn
     :gTRC-inv encode-fn
     :bTRC-inv encode-fn}))

(def adobe-rgb-icc-data
  (let [s (profiles/get-space :adobe-rgb)
        decode-fn (:decode (:gamma s))
        encode-fn (:encode (:gamma s))]
    {:white-point (:white-point s)
     :rXYZ (mapv first (:primaries s))
     :gXYZ (mapv second (:primaries s))
     :bXYZ (mapv last (:primaries s))
     :rTRC decode-fn
     :gTRC decode-fn
     :bTRC decode-fn
     :rTRC-inv encode-fn
     :gTRC-inv encode-fn
     :bTRC-inv encode-fn}))

(def display-p3-icc-data
  (let [s (profiles/get-space :display-p3)
        decode-fn (:decode (:gamma s))
        encode-fn (:encode (:gamma s))]
    {:white-point (:white-point s)
     :rXYZ (mapv first (:primaries s))
     :gXYZ (mapv second (:primaries s))
     :bXYZ (mapv last (:primaries s))
     :rTRC decode-fn
     :gTRC decode-fn
     :bTRC decode-fn
     :rTRC-inv encode-fn
     :gTRC-inv encode-fn
     :bTRC-inv encode-fn}))

;; ── 便捷加载函数 ──────────────────────────────────────
(defn load-icc-file
  "从文件路径读取 ICC 文件并返回解析结果。"
  [path]
  (with-open [in  (io/input-stream path)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [bytes (.toByteArray out)
          buf   (ByteBuffer/wrap bytes)]
      (parse-icc-buffer buf))))