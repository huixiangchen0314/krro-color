(ns top.kzre.krro.color.munsell-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.munsell :as munsell]))

(deftest munsell-name-test
  (is (string? (munsell/rgb->munsell-name [1.0 0.0 0.0])))
  (is (string? (munsell/rgb->munsell-name [0.5 0.5 0.5]))))