(ns top.kzre.krro.color.parse-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.parse :as parse]))

(deftest hex-parsing
  (testing "Short format #rgb"
    (is (= (parse/hex->rgb "#f00") [255 0 0 255]))
    (is (= (parse/hex->rgb "#0f0") [0 255 0 255]))
    (is (= (parse/hex->rgb "#00f") [0 0 255 255]))
    (is (= (parse/hex->rgb "#abc") [170 187 204 255])))
  (testing "Long format #rrggbb"
    (is (= (parse/hex->rgb "#ff0000") [255 0 0 255]))
    (is (= (parse/hex->rgb "#00ff00") [0 255 0 255]))
    (is (= (parse/hex->rgb "#0000ff") [0 0 255 255]))
    (is (= (parse/hex->rgb "#aabbcc") [170 187 204 255])))
  (testing "With alpha #rrggbbaa"
    (is (= (parse/hex->rgb "#ff000080") [255 0 0 128]))
    (is (= (parse/hex->rgb "#00000000") [0 0 0 0])))
  (testing "Case insensitivity"
    (is (= (parse/hex->rgb "#FF0000") [255 0 0 255])))
  (testing "Invalid inputs should throw"
    (is (thrown? IllegalArgumentException (parse/hex->rgb "#ggg")))
    (is (thrown? IllegalArgumentException (parse/hex->rgb "red")))
    (is (thrown? IllegalArgumentException (parse/hex->rgb "12345")))
    (is (thrown? IllegalArgumentException (parse/hex->rgb "#12345")))))

(deftest normalize
  (testing "Convert 0-255 integer vector to 0-1"
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6)
                           (parse/normalize [255 0 0 255])
                           [1.0 0.0 0.0 1.0]))))
  (testing "Empty vector returns empty"
    (is (= (parse/normalize []) []))))

(deftest parse-full-string
  (testing "HEX strings are parsed correctly"
    (let [c (parse/parse "#ff0066")]
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) c
                             [1.0 0.0 0.4 1.0]))))
    (let [c (parse/parse "#0f0")]
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) c
                             [0.0 1.0 0.0 1.0]))))
    (let [c (parse/parse "#00000000")]
      (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-6) c
                             [0.0 0.0 0.0 0.0])))))
  (testing "Unsupported format throws exception"
    (is (thrown? IllegalArgumentException (parse/parse "rgba(255,0,0,1)")))))