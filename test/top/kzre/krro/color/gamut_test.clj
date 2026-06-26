(ns top.kzre.krro.color.gamut-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.gamut :as gamut]
            [top.kzre.krro.color.converter :as conv]))

(deftest relative-colorimetric-srgb
  (let [rgb [0.8 0.2 0.3]
        mapped (gamut/map-color rgb :srgb :srgb :relative-colorimetric)]
    ;; 应该在 sRGB 内
    (is (every? #(<= 0.0 % 1.0) mapped))
    ;; 对于 sRGB -> sRGB，应该接近原值
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-5) rgb mapped)))))

(deftest absolute-colorimetric-srgb
  (let [rgb [0.5 0.5 0.5]
        mapped (gamut/map-color rgb :srgb :srgb :absolute-colorimetric)]
    (is (every? #(<= 0.0 % 1.0) mapped))))

(deftest perceptual-oklab-srgb
  (let [rgb [0.2 0.8 0.3]
        mapped (gamut/map-color rgb :srgb :srgb :perceptual :perceptual-mode :oklab)]
    (is (every? #(<= 0.0 % 1.0) mapped))
    ;; 应基本保持原色
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 1e-3) rgb mapped)))))

(deftest perceptual-sgck-srgb
  (let [rgb [0.9 0.1 0.1]
        mapped (gamut/map-color rgb :srgb :srgb :perceptual :perceptual-mode :sgck)]
    (is (every? #(<= 0.0 % 1.0) mapped))
    ;; 对于 sRGB 到 sRGB，sgck 不应大幅改变颜色
    (is (every? true? (map #(< (Math/abs (- %1 %2)) 0.05) rgb mapped)))))

(deftest p3-to-srgb
  (let [p3-red [1.0 0.0 0.0]   ;; P3 红色，在 sRGB 可能超出
        mapped (gamut/map-color p3-red :p3 :srgb :relative-colorimetric)]
    (is (every? #(<= 0.0 % 1.0) mapped))
    ;; 相对色度会裁剪，但红色仍应占主导
    (is (> (first mapped) (second mapped)))))

(deftest adobe-to-srgb-perceptual
  (let [aRGB [0.9 0.5 0.2]
        mapped (gamut/map-color aRGB :adobe-rgb :srgb :perceptual :perceptual-mode :sgck)]
    (is (every? #(<= 0.0 % 1.0) mapped))))

(deftest prophoto-to-srgb
  (let [pro [0.5 0.6 0.7]
        mapped (gamut/map-color pro :prophoto :srgb :relative-colorimetric)]
    (is (every? #(<= 0.0 % 1.0) mapped))))

(deftest same-space-no-change
  (doseq [intent [:relative-colorimetric :absolute-colorimetric :perceptual]
          mode (if (= intent :perceptual) [:oklab :sgck] [nil])]
    (let [rgb [0.3 0.5 0.7]
          mapped (if mode
                   (gamut/map-color rgb :srgb :srgb intent :perceptual-mode mode)
                   (gamut/map-color rgb :srgb :srgb intent))]
      (is (every? #(<= 0.0 % 1.0) mapped)))))

(deftest out-of-gamut-handling
  ;; 用 P3 红色转换到 sRGB 应产生有效值
  (let [mapped (gamut/map-color [1.0 0.0 0.0] :p3 :srgb :perceptual :perceptual-mode :sgck)]
    (is (every? #(<= 0.0 % 1.0) mapped))
    ;; 不应该全部为 0 或 1，应保留一些色调信息
    (is (not= mapped [1.0 0.0 0.0]))  ;; 因为 P3 红色映射到 sRGB 通常会被裁剪或压缩
    ))