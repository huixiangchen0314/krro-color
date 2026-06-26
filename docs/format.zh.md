
# 解析与格式化 (Parse & Format)

颜色字符串与内部向量表示的相互转换。

## 函数

### `krro.color.parse`
- `(parse s)` — 解析 HEX 字符串（`#rrggbb`, `#rgb`, `#rrggbbaa`）为 RGBA 向量 `[r g b a]` 0-1。

### `krro.color.format`
- `(hex c)` / `(hex c include-alpha?)` — 格式化为 HEX 字符串。
- `(rgba-string c)` — 格式化为 CSS `rgba()` 字符串。

## 示例

```clojure
(require '[krro.color.parse :as p]
         '[krro.color.format :as f])

(def color (p/parse "#ff0066"))
;; => [1.0 0.0 0.4 1.0]

(f/hex color) ; => "#ff0066"
(f/rgba-string color) ; => "rgba(255,0,102,1.00)"
```