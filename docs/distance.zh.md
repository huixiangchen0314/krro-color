
# 颜色距离与对比度 (Color Distance & Contrast)

提供工业标准的颜色距离算法和可访问性对比度计算。

## 函数

### ΔE 距离（`krro.color.distance`）

- `(delta-e76 lab1 lab2)` — CIE76 欧氏距离
- `(delta-e94 lab1 lab2)` — CIE94 距离（图形艺术参考）
- `(delta-e2000 lab1 lab2)` — CIEDE2000 最新标准

便捷函数（直接接受 sRGB 向量）：
- `(delta-e76-rgb rgb1 rgb2)`
- `(delta-e94-rgb rgb1 rgb2)`
- `(delta-e2000-rgb rgb1 rgb2)`

### 对比度（`krro.color.distance`）

- `(relative-luminance rgb)` — WCAG 2.1 相对亮度
- `(contrast-ratio rgb1 rgb2)` — 对比度（1~21）

## 示例

```clojure
(require '[krro.color.distance :as dist])

(dist/delta-e2000-rgb [1.0 0.0 0.0] [1.0 0.1 0.0])
;; => 约 1.5

(dist/contrast-ratio [0.0 0.0 0.0] [1.0 1.0 1.0])
;; => 21.0
```
