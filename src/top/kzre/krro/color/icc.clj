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

;; ── 主解析函数（纯函数，接受 ByteBuffer）────────────────
(defn parse-icc-buffer
  "从 ByteBuffer 解析 ICC 配置，返回简化映射。"
  [^ByteBuffer buf]
  (when (= 0x61637370 (.getInt buf 36))  ;; 幻数 'acsp' 在偏移 36 处
    (.order buf ByteOrder/BIG_ENDIAN)
    (let [tag-table-offset (.getInt buf 128)
          tag-table (read-tag-table buf tag-table-offset)]
      (letfn [(read-xyz-tag [sig]
                (when-let [tag (get tag-table sig)]
                  (let [offset (:offset tag)]
                    (.position buf (+ (long offset) 8))
                    (read-xyz buf))))]
        (let [white-point (read-xyz-tag "wtpt")
              rXYZ        (read-xyz-tag "rXYZ")
              gXYZ        (read-xyz-tag "gXYZ")
              bXYZ        (read-xyz-tag "bXYZ")
              rgb-to-xyz  (when (and rXYZ gXYZ bXYZ)
                            [[(first rXYZ) (first gXYZ) (first bXYZ)]
                             [(second rXYZ) (second gXYZ) (second bXYZ)]
                             [(nth rXYZ 2) (nth gXYZ 2) (nth bXYZ 2)]])]
          {:white-point white-point
           :rgb-to-xyz-matrix rgb-to-xyz})))))

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