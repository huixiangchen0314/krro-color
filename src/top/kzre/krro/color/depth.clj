(ns top.kzre.krro.color.depth
  "色彩深度与格式转换：16-bit 整数、半浮点（half-float）与标准 0-1 浮点。")

(defn float->unorm16
  "将 0-1 浮点转换为 16-bit 无符号归一化整数 (0-65535)。"
  [f]
  (-> f
      (max 0.0) (min 1.0)
      (* 65535.0)
      (Math/round)
      (int)))

(defn unorm16->float
  "将 16-bit 无符号归一化整数 (0-65535) 转换为 0-1 浮点。"
  [i]
  (double (/ i 65535.0)))

(defn float->unorm8
  "将 0-1 浮点转换为 8-bit 无符号归一化整数 (0-255)。"
  [f]
  (-> f
      (max 0.0) (min 1.0)
      (* 255.0)
      (Math/round)
      (int)))

(defn unorm8->float
  "将 8-bit 无符号归一化整数 (0-255) 转换为 0-1 浮点。"
  [i]
  (double (/ i 255.0)))

;; ── 半浮点 (IEEE 754 half-precision) ─────────────────
(defn float->half
  "将 32-bit float 转换为 16-bit half-float，返回 int。"
  [f]
  (let [bits (Float/floatToIntBits (float f))
        sign (bit-shift-right (bit-and bits 0x80000000) 16)
        exponent (bit-and (bit-shift-right bits 23) 0xff)
        mantissa (bit-and bits 0x007fffff)]
    (cond
      (= exponent 0xff)
      (bit-or sign 0x7c00)
      (> exponent 142)
      (bit-or sign 0x7c00)
      (< exponent 113)
      sign
      :else
      (let [new-exp (- exponent 112)
            new-mant (bit-shift-right mantissa 13)]
        (bit-or sign (bit-shift-left new-exp 10) new-mant)))))

(defn half->float
  "将 16-bit half-float 转换为 32-bit float。h 为 int。"
  [h]
  (let [h (int h)
        s (bit-shift-right h 15)
        e (bit-and (bit-shift-right h 10) 0x1f)
        m (bit-and h 0x3ff)]
    (cond
      (zero? e)
      (if (zero? m)
        (if (zero? s) 0.0 -0.0)
        ;; 次正规数：构造指数 1 的浮点数，再减去 1.0
        (let [bits (unchecked-int (bit-or (bit-shift-left s 31)
                                          (bit-shift-left 1 23)
                                          (bit-shift-left m 13)))]
          (float (- (Float/intBitsToFloat bits) 1.0))))
      (= e 31)
      (if (zero? m)
        (if (zero? s) Float/POSITIVE_INFINITY Float/NEGATIVE_INFINITY)
        Float/NaN)
      :else
      (let [exp (+ e 112)
            bits (unchecked-int (bit-or (bit-shift-left s 31)
                                        (bit-shift-left exp 23)
                                        (bit-shift-left m 13)))]
        (Float/intBitsToFloat bits)))))