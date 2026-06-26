(ns top.kzre.krro.color.ycbcr
  "YCbCr 颜色空间转换 (BT.601, BT.709, BT.2020)。")

(defn- matrix-mult [m v]
  (mapv #(apply + (map * % v)) m))

(def ^:private bt601-matrix
  [[0.299 0.587 0.114]
   [-0.168736 -0.331264 0.5]
   [0.5 -0.418688 -0.081312]])

(def ^:private bt601-inv
  [[1.0 0.0 1.402]
   [1.0 -0.344136 -0.714136]
   [1.0 1.772 0.0]])

(def ^:private bt709-matrix
  [[0.2126 0.7152 0.0722]
   [-0.114572 -0.385428 0.5]
   [0.5 -0.454153 -0.045847]])

(def ^:private bt709-inv
  [[1.0 0.0 1.5748]
   [1.0 -0.187324 -0.468124]
   [1.0 1.8556 0.0]])

(defn rgb->ycbcr
  "将 RGB 颜色转换为 YCbCr，默认 BT.601。"
  ([rgb] (rgb->ycbcr rgb :bt601))
  ([rgb standard]
   (let [m (case standard :bt601 bt601-matrix :bt709 bt709-matrix :bt2020 (throw (ex-info "NYI" {})))]
     (matrix-mult m rgb))))

(defn ycbcr->rgb
  "将 YCbCr 转换为 RGB，默认 BT.601。"
  ([ycbcr] (ycbcr->rgb ycbcr :bt601))
  ([ycbcr standard]
   (let [m (case standard :bt601 bt601-inv :bt709 bt709-inv :bt2020 (throw (ex-info "NYI" {})))]
     (matrix-mult m ycbcr))))