(ns top.kzre.krro.color.hdr-test
  (:require [clojure.test :refer :all]
            [top.kzre.krro.color.hdr :as hdr]))

(deftest pq-roundtrip
  (let [Y 0.5
        pq-val (hdr/linear->pq Y)
        linear-back (hdr/pq->linear pq-val)]
    (is (< (Math/abs (- Y linear-back)) 0.001))))

(deftest hlg-roundtrip
  (let [L 0.3
        hlg-val (hdr/linear->hlg L)
        linear-back (hdr/hlg->linear hlg-val)]
    (is (< (Math/abs (- L linear-back)) 0.001))))

(deftest tone-mapping
  (let [hdr [5.0 2.0 0.5]
        sdr-reinhard (hdr/reinhard hdr)
        sdr-aces (hdr/aces hdr)]
    (is (every? #(<= 0.0 % 1.0) sdr-reinhard))
    (is (every? #(<= 0.0 % 1.0) sdr-aces))))