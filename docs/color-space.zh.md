
# 颜色空间 (Color Spaces)

krro-color 提供多种颜色空间的表示与转换，所有颜色均用简单的 Clojure 向量表示，数据透明，无副作用。

## 支持的颜色空间

| 空间 | 向量格式 | 范围 |
|------|----------|------|
| sRGB | `[r g b]` | `r,g,b` ∈ [0,1] |
| sRGBA | `[r g b a]` | `r,g,b,a` ∈ [0,1] |
| HSL | `[h s l]` | `h` ∈ [0,360), `s,l` ∈ [0,1] |
| HSV | `[h s v]` | `h` ∈ [0,360), `s,v` ∈ [0,1] |
| CMYK | `[c m y k]` | `c,m,y,k` ∈ [0,1] |
| XYZ (D65) | `[x y z]` | `x,y,z` ≥ 0 |
| CIELAB (L*a*b*) | `[L a b]` | `L` ∈ [0,100], `a,b` 约 [-128,127] |
| OKLab | `[L a b]` | 类似 LAB，但更均匀 |
| OKLCH | `[L C h]` | `L,C` 类似 OKLab，`h` ∈ [0,360) |

## 主要函数

### 基础操作（`krro.color.rgb`）

- `(rgb r g b)` / `(rgba r g b a)` — 创建 RGB/RGBA 颜色
- `(red c)`, `(green c)`, `(blue c)`, `(alpha c)` — 获取分量
- `(add c1 c2)`, `(subtract c1 c2)`, `(scale c s)`, `(mix c1 c2 t)` — 算术运算
- `(luminance c)` — 相对亮度 (ITU-R BT.709)
- `(grayscale c)` — 转灰度

### 转换函数（`krro.color.converter`）

- `(rgb->hsl c)` / `(hsl->rgb c)`
- `(rgb->hsv c)` / `(hsv->rgb c)`
- `(hsl->hsv c)` / `(hsv->hsl c)`
- `(rgb->xyz c)` / `(xyz->rgb c)` — sRGB ↔ XYZ (D65)
- `(rgb->lab c)` / `(lab->rgb c)` — sRGB ↔ CIELAB
- `(rgb->oklab c)` / `(oklab->rgb c)` — sRGB ↔ OKLab
- `(rgb->oklch c)` / `(oklch->rgb c)` — sRGB ↔ OKLCH
- `(xyz-d65->d50 c)` / `(xyz-d50->d65 c)` — 色适应

### 色适应（`krro.color.adaptation`）

- `(chromatic-adapt xyz source-white target-white)` — 通用 Bradford 变换

## 示例

```clojure
(require '[krro.color.converter :as conv])

(def red [1.0 0.0 0.0])
(conv/rgb->hsl red)  ; => [0.0 1.0 0.5]

(def lab-red (conv/rgb->lab red))
;; [53.23288 80.09245 67.2032]
(conv/lab->rgb lab-red) ; => [1.0 0.0 0.0] (近似)
```
