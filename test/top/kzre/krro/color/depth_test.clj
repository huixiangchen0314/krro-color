(ns top.kzre.krro.color.depth-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.depth :as depth]))

(deftest unorm8-roundtrip
  (let [f 0.5
        i (depth/float->unorm8 f)
        back (depth/unorm8->float i)]
    (is (<= (Math/abs (- f back)) 0.01))))   ;; 8-bit 量化误差

(deftest unorm16-roundtrip
  (let [f 0.25
        i (depth/float->unorm16 f)
        back (depth/unorm16->float i)]
    (is (< (Math/abs (- f back)) 1e-5))))  ;; 16-bit 精度足够

(deftest half-float-roundtrip
  (let [values [0.0 0.5 1.0 -1.0 0.333]]
    (doseq [v values]
      (let [h (depth/float->half v)
            back (depth/half->float (int h))]
        (when (not (Float/isNaN v))
          (is (< (Math/abs (- v (double back))) 0.001)))))))

(deftest edge-cases
  (is (= 0 (depth/float->unorm8 -0.2)))
  (is (= 255 (depth/float->unorm8 1.5)))
  (is (= 0.0 (depth/unorm8->float 0)))
  (is (= 1.0 (depth/unorm8->float 255)))
  (is (= 0 (depth/float->unorm16 -0.1)))
  (is (= 65535 (depth/float->unorm16 2.0)))
  (is (= 0.0 (depth/unorm16->float 0)))
  (is (= 1.0 (depth/unorm16->float 65535))))