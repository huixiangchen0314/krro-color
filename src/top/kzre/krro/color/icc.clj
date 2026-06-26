(ns top.kzre.krro.color.icc
  "极简 ICC 配置文件解析器，纯函数设计。"
  (:require [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream)
           (java.nio ByteBuffer ByteOrder)))

;; ── 辅助读取函数（基于 ByteBuffer，纯函数）─────────────────
(defn- read-u32 [^ByteBuffer buf]
  (.getInt buf))

(defn- read-u16 [^ByteBuffer buf]
  (Short/toUnsignedInt (.getShort buf)))

(defn- read-s15Fixed16 [^ByteBuffer buf]
  (/ (read-u32 buf) 65536.0))

(defn- read-xyz [^ByteBuffer buf]
  [(read-s15Fixed16 buf) (read-s15Fixed16 buf) (read-s15Fixed16 buf)])

;; ── 解析标签表 ──────────────────────────────────────────
(defn- read-tag-table [^ByteBuffer buf header-size]
  (.position buf header-size)
  (let [count (read-u32 buf)]
    (into {}
          (for [i (range count)]
            (let [sig (apply str (repeatedly 4 #(char (.get buf))))
                  offset (read-u32 buf)
                  size   (read-u32 buf)]
              [sig {:offset offset :size size}])))))

(defn- parse-lut [^ByteBuffer buf offset size]
  ;; 简化：假设是 lut16Type，实际需根据类型（mft2）判断
  ;; 返回一个映射，包含 :size 和 :data（一维数组）
  (let [type (.getInt buf (+ offset 8))  ;; 类型字段偏移 8
        _ (when (not= type 0x6d667432)    ;; 'mft2'
            (throw (ex-info "Unsupported LUT type" {:type type})))
        ;; 读取输入输出通道数和网格点数
        num-input-channels (.get buf (+ offset 16))
        num-output-channels (.get buf (+ offset 17))
        num-grid-points (.getInt buf (+ offset 20))
        ;; 读取数据偏移（相对于 tag 起始）
        data-offset (.getInt buf (+ offset 24))
        ;; 读取数据
        _ (.position buf (+ offset data-offset))
        data (vec (repeatedly (* num-grid-points num-output-channels)
                              #(Short/toUnsignedInt (.getShort buf))))]
    {:size (int (Math/cbrt num-grid-points))  ;; 假设立方体网格
     :channels num-output-channels
     :data data}))

;; 在 parse-icc-buffer 中增加对 LUT 标签的解析
(defn parse-icc-buffer
  "从 ByteBuffer 解析 ICC 配置，返回简化映射。"
  [^ByteBuffer buf]
  (when (= 0x61637370 (.getInt buf 36))  ;; 'acsp'
    (.order buf ByteOrder/BIG_ENDIAN)
    (let [tag-table-offset (.getInt buf 128)
          tag-table (read-tag-table buf tag-table-offset)]
      ;; 首先检查是否有 A2B0（设备到设备无关空间）LUT
      (if-let [a2b0-tag (get tag-table "A2B0")]
        ;; 存在 LUT
        (let [lut (parse-lut buf (:offset a2b0-tag) (:size a2b0-tag))]
          {:type :lut
           :lut lut
           :white-point (when-let [wtpt (get tag-table "wtpt")]
                          (let [offset (:offset wtpt)]
                            (.position buf (+ (long offset) 8))
                            (read-xyz buf)))})
        ;; 否则回退到矩阵/曲线型
        (letfn [(read-xyz-tag [sig]
                  (when-let [tag (get tag-table sig)]
                    (let [offset (:offset tag)]
                      (.position buf (+ (long offset) 8))
                      (read-xyz buf))))]
          (let [white-point (read-xyz-tag "wtpt")
                rXYZ (read-xyz-tag "rXYZ")
                gXYZ (read-xyz-tag "gXYZ")
                bXYZ (read-xyz-tag "bXYZ")
                rgb-to-xyz (when (and rXYZ gXYZ bXYZ)
                             [[(first rXYZ) (first gXYZ) (first bXYZ)]
                              [(second rXYZ) (second gXYZ) (second bXYZ)]
                              [(nth rXYZ 2) (nth gXYZ 2) (nth bXYZ 2)]])]
            {:type :matrix
             :white-point white-point
             :rgb-to-xyz-matrix rgb-to-xyz}))))))

;; ── 便捷工具：从文件加载并解析（副作用仅限于读取文件）───
(defn load-icc-file
  "从文件路径读取 ICC 文件并返回解析结果。"
  [path]
  (with-open [in  (io/input-stream path)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (let [bytes (.toByteArray out)
          buf   (ByteBuffer/wrap bytes)]
      (parse-icc-buffer buf))))