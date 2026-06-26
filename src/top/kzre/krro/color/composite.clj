;; composite.clj
(ns top.kzre.krro.color.composite
  "Porter-Duff 合成操作与批量处理。"
  (:require [top.kzre.krro.color.rgb :as rgb]
            [top.kzre.krro.color.util :as util]))

(defn- to-rgba [c]
  (if (== 4 (count c)) c (conj c 1.0)))

(defn clear [_ _] [0.0 0.0 0.0 0.0])
(defn source [_ source] (to-rgba source))
(defn dest [backdrop _] (to-rgba backdrop))

(defn over [backdrop source]
  (let [[rb gb bb ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out (+ as (* ab (- 1 as)))
        inv-a (if (zero? a-out) 0.0 (/ 1.0 a-out))]
    [(/ (+ (* rs as) (* rb ab (- 1 as))) a-out)
     (/ (+ (* gs as) (* gb ab (- 1 as))) a-out)
     (/ (+ (* bs as) (* bb ab (- 1 as))) a-out)
     a-out]))

(defn in [backdrop source]
  (let [[_ _ _ ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out (* as ab)]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      [rs gs bs a-out])))

(defn out [backdrop source]
  (let [[_ _ _ ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out (* as (- 1 ab))]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      [rs gs bs a-out])))

(defn atop [backdrop source]
  (let [[rb gb bb ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out ab]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      [(/ (+ (* rs as) (* rb (- 1 as))) a-out)
       (/ (+ (* gs as) (* gb (- 1 as))) a-out)
       (/ (+ (* bs as) (* bb (- 1 as))) a-out)
       a-out])))

(defn dest-over [backdrop source] (over source backdrop))
(defn dest-in [backdrop source] (in source backdrop))
(defn dest-out [backdrop source] (out source backdrop))
(defn dest-atop [backdrop source] (atop source backdrop))

(defn xor [backdrop source]
  (let [[rb gb bb ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out (+ (* as (- 1 ab)) (* ab (- 1 as)))]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      (let [inv-a (/ 1.0 a-out)]
        [(/ (+ (* rs as (- 1 ab)) (* rb ab (- 1 as))) a-out)
         (/ (+ (* gs as (- 1 ab)) (* gb ab (- 1 as))) a-out)
         (/ (+ (* bs as (- 1 ab)) (* bb ab (- 1 as))) a-out)
         a-out]))))

(defn lighter [backdrop source]
  (let [[rb gb bb ab] (to-rgba backdrop)
        [rs gs bs as] (to-rgba source)
        a-out (min 1.0 (+ as ab))]
    (if (zero? a-out)
      [0.0 0.0 0.0 0.0]
      [(/ (+ (* rs as) (* rb ab)) a-out)
       (/ (+ (* gs as) (* gb ab)) a-out)
       (/ (+ (* bs as) (* bb ab)) a-out)
       a-out])))

(defn composite [backdrop src op]   ;; 参数改名
  (case op
    :clear      (clear backdrop src)
    :source     (source backdrop src)
    :dest       (dest backdrop src)
    :over       (over backdrop src)
    :in         (in backdrop src)
    :out        (out backdrop src)
    :atop       (atop backdrop src)
    :dest-over  (dest-over backdrop src)
    :dest-in    (dest-in backdrop src)
    :dest-out   (dest-out backdrop src)
    :dest-atop  (dest-atop backdrop src)
    :xor        (xor backdrop src)
    :lighter    (lighter backdrop src)
    (throw (IllegalArgumentException. (str "Unknown Porter-Duff op: " op)))))

(defn composite-batch [^doubles pixels source op]
  (let [n (alength pixels)
        sr (double (nth source 0))
        sg (double (nth source 1))
        sb (double (nth source 2))
        sa (if (== 4 (count source)) (double (nth source 3)) 1.0)]
    (dotimes [i (quot n 4)]
      (let [idx (* i 4)
            rb (aget pixels idx)
            gb (aget pixels (+ idx 1))
            bb (aget pixels (+ idx 2))
            ab (if (< (+ idx 3) n) (aget pixels (+ idx 3)) 1.0)
            result (composite [rb gb bb ab] [sr sg sb sa] op)]
        (aset pixels idx (double (nth result 0)))
        (aset pixels (+ idx 1) (double (nth result 1)))
        (aset pixels (+ idx 2) (double (nth result 2)))
        (when (and (> (count result) 3) (< (+ idx 3) n))
          (aset pixels (+ idx 3) (double (nth result 3)))))))
  pixels)

(defn blend-group [backdrop layers & {:keys [mode] :or {mode :over}}]
  (if (= mode :pass-through)
    (let [group-result (reduce (fn [acc layer] (over acc layer)) [0 0 0 0] layers)]
      (over backdrop group-result))
    (reduce (fn [acc layer] (composite acc layer mode)) backdrop layers)))