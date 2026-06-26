
# 颜色调整 (Color Adjustments)

提供常见的颜色校正操作，均为纯函数，不改变原数据。

## 函数（`krro.color.adjustments`）

- `(brightness c factor)` — 亮度调整，`factor` ∈ [-1, 1]
- `(contrast c factor)` — 对比度调整，`factor` ∈ [-1, 1]
- `(gamma c value)` — 伽马校正
- `(saturation c factor)` — 饱和度调整，`factor` ∈ [-1, 1]
- `(hue-rotate c angle)` — 色相旋转，`angle` 度数
- `(colorize c color)` — 着色，保持亮度

## 示例

```clojure
(require '[krro.color.adjustments :as adj])

(adj/saturation [0.8 0.5 0.3] -0.5)  ; 降低饱和度
;; => [0.55 0.45 0.35]  (近似)

(adj/hue-rotate [1.0 0.0 0.0] 120)   ; 红色旋转120度
;; => [0.0 1.0 0.0]
```

```

## 7. `README.md` (可放在项目根目录)

```markdown
# krro-color

纯 Clojure 编写的专业色彩引擎，提供颜色空间转换、混合模式、距离计算、调整操作等完整功能。

## 特性
- 支持 sRGB、HSL、HSV、CMYK、XYZ、CIELAB、OKLab、OKLCH 等颜色空间
- 27 种标准混合模式
- ΔE76/ΔE94/ΔE2000 颜色距离，WCAG 对比度
- 色适应（Bradford）
- 亮度、对比度、饱和度、色相调整
- 高性能批量像素操作
- HEX 解析与格式化

## 快速开始

```clojure
(require '[krro.color.converter :as conv]
         '[krro.color.blend :as blend])

(def c (conv/rgb->hsl [0.2 0.6 0.9]))
(blend/multiply [1 0 0] [0.5 0.5 0.5])
```

## 文档
- [颜色空间](docs/color-space.md)
- [混合模式](docs/blend.md)
- [颜色距离](docs/distance.md)
- [颜色调整](docs/adjustments.md)
- [批量操作](docs/batch.md)
- [解析与格式化](docs/parse-format.md)

## 许可
MIT
```