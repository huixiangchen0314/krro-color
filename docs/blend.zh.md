
# 混合模式 (Blend Modes)

实现 Photoshop/After Effects 标准混合模式，可用于图层合成、笔触混合等场景。

## 使用方法

所有混合函数位于 `krro.color.blend` 命名空间，接受两个 RGB 颜色向量（`[r g b]` 0-1），返回混合后的颜色。

通用调度函数：
```clojure
(blend backdrop source mode)
```
其中 `mode` 为以下关键字之一。

## 支持的模式

### 基础组
- `:normal` — 正常（直接返回 source）
- `:dissolve` — 溶解（目前为占位）

### 变暗组
- `:darken` — 变暗
- `:multiply` — 正片叠底
- `:color-burn` — 颜色加深
- `:linear-burn` — 线性加深

### 变亮组
- `:lighten` — 变亮
- `:screen` — 滤色
- `:color-dodge` — 颜色减淡
- `:linear-dodge` — 线性减淡（添加）

### 对比组
- `:overlay` — 叠加
- `:soft-light` — 柔光
- `:hard-light` — 强光
- `:vivid-light` — 亮光
- `:linear-light` — 线性光
- `:pin-light` — 点光
- `:hard-mix` — 实色混合

### 差值组
- `:difference` — 差值
- `:exclusion` — 排除
- `:subtract` — 减去
- `:divide` — 划分

## 示例

```clojure
(require '[krro.color.blend :as blend])

(def bg [0.8 0.2 0.2])
(def fg [0.2 0.8 0.2])

(blend/blend bg fg :multiply)
;; => [0.16 0.16 0.04]

(blend/screen bg fg)
;; => [0.84 0.84 0.36]
```
