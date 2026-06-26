(ns top.kzre.krro.color.harmony-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.harmony :as harmony]))

(deftest complementary-test
  (let [result (harmony/complementary [1.0 0.0 0.0])]
    (is (vector? result))
    (is (= 3 (count result)))
    (is (every? #(<= 0.0 % 1.0) result))
    (is (> (nth result 1) 0.5))
    (is (> (nth result 2) 0.5))))

(deftest triadic-test
  (let [result (harmony/triadic [1.0 0.0 0.0])]
    (is (vector? result))
    (is (= 2 (count result)))
    (is (every? (fn [c] (and (vector? c) (= 3 (count c)) (every? #(<= 0.0 % 1.0) c))) result))
    ;; 三角色应为绿色和蓝色
    (is (> (-> result first (nth 1)) 0.5)) ; 绿色分量
    (is (> (-> result second (nth 2)) 0.5)) ; 蓝色分量
    ))

(deftest split-complementary-test
  (let [result (harmony/split-complementary [1.0 0.0 0.0])]
    (is (= 2 (count result)))
    (is (every? #(<= 0.0 % 1.0) (flatten result)))))

(deftest tetradic-test
  (let [result (harmony/tetradic [1.0 0.0 0.0])]
    (is (= 3 (count result))) ; 三个额外颜色
    (is (every? #(and (vector? %) (= 3 (count %))) result))))

(deftest tetradic-alt-test
  (let [result (harmony/tetradic-alt [1.0 0.0 0.0])]
    (is (= 4 (count result)))
    (is (every? #(and (vector? %) (= 3 (count %))) result))))

(deftest hexadic-test
  (let [result (harmony/hexadic [1.0 0.0 0.0])]
    (is (= 6 (count result)))
    (is (every? #(and (vector? %) (= 3 (count %))) result))))

(deftest monochromatic-test
  (let [result (harmony/monochromatic [0.5 0.5 0.5])]
    (is (= 5 (count result)))
    (is (every? #(and (vector? %) (= 3 (count %))) result))))

(deftest invalid-input-handling
  ;; 极端值不崩溃
  (is (every? #(<= 0.0 % 1.0) (harmony/complementary [0.5 0.5 0.5])))
  (is (vector? (harmony/hexadic [-1 2 0.5])))
  (is (vector? (harmony/monochromatic [0.2 0.8 1.5]))))