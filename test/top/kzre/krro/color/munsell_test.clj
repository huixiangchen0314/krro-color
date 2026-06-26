(ns top.kzre.krro.color.munsell-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [top.kzre.krro.color.munsell :as munsell]))

(deftest munsell-name-string-test
  (is (string? (munsell/rgb->munsell-name [1.0 0.0 0.0])))
  (is (string? (munsell/rgb->munsell-name [0.5 0.5 0.5])))
  (is (string? (munsell/rgb->munsell-name [0.0 0.0 0.0])))
  (is (string? (munsell/rgb->munsell-name [1.0 1.0 1.0]))))

(deftest neutral-colors
  ;; 中性色应返回包含 "Neutral" 的名称
  (is (str/includes? (munsell/rgb->munsell-name [0.0 0.0 0.0]) "Neutral"))
  (is (str/includes? (munsell/rgb->munsell-name [0.5 0.5 0.5]) "Neutral"))
  (is (str/includes? (munsell/rgb->munsell-name [1.0 1.0 1.0]) "Neutral")))

(deftest chromatic-colors
  ;; 有彩色不应包含 "Neutral"
  (is (not (str/includes? (munsell/rgb->munsell-name [1.0 0.0 0.0]) "Neutral")))
  (is (not (str/includes? (munsell/rgb->munsell-name [0.0 1.0 0.0]) "Neutral")))
  (is (not (str/includes? (munsell/rgb->munsell-name [0.0 0.0 1.0]) "Neutral"))))