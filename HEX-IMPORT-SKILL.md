# Hex-Studio 外部模组图案导入 Skill

## 目标

从 Hexcasting 附属模组的源码中提取新图案和 Iota 类型，将其添加到 Hex-Studio 的 Elm 项目中。

## 前置条件

- 已阅读 `扩展指南.md`，了解 Pattern 和 Iota 的添加方式
- Elm 0.19.1 编译器已配置（`elm_path.txt`）
- PowerShell 环境可用（`build.ps1`）

---

## 一、仓库结构识别

### 1.1 定位关键文件

进入仓库后按以下顺序探查：

| 优先级 | 文件 | 提取内容 |
|:------:|------|---------|
| 1 | `gradle.properties` / `fabric.mod.json` | MOD_ID (`archives_base_name` 或 `id`) |
| 2 | `src/main/java/**/<MOD_ID>Actions.kt` (Kotlin) 或 `.java` | 图案注册 (signature, startDir, internalName) |
| 3 | `src/main/resources/assets/<MOD_ID>/lang/en_us.json` | 图案展示名称 (displayName) |
| 4 | `src/main/java/**/iotas/*.kt` | 新 Iota 类型定义 |

### 1.2 MOD_ID 提取

```
gradle.properties  →  archives_base_name = hexpose
fabric.mod.json    →  "id": "hexpose"
```

### 1.3 HexDir → Direction 映射

Kotlin/Java 中的 `HexDir` 与 Elm 的 `Direction` 对应关系：

| Kotlin `HexDir` | Elm `Direction` |
|-----------------|-----------------|
| `HexDir.NORTH_EAST` | `Northeast` |
| `HexDir.EAST` | `East` |
| `HexDir.SOUTH_EAST` | `Southeast` |
| `HexDir.SOUTH_WEST` | `Southwest` |
| `HexDir.WEST` | `West` |
| `HexDir.NORTH_WEST` | `Northwest` |

---

## 二、从源码提取图案

### 2.1 识别注册调用

图案注册的典型形式：

```kotlin
// Kotlin (hexpose 风格)
register("am_enlightened", "awqaqqq", HexDir.SOUTH_EAST, OpGetPlayerData { ... })

// 参数含义:
//   "am_enlightened"  → internalName
//   "awqaqqq"          → signature (笔顺)
//   HexDir.SOUTH_EAST  → startDirection
//   OpGetPlayerData{}  → action 实现
```

### 2.2 提取 displayName

在 `en_us.json` 中查找 `hexcasting.action.<MOD_ID>:<name>` 或 `hexcasting.action.book.<MOD_ID>:<name>`：

```json
"hexcasting.action.hexpose:am_enlightened": "Epiphany Purification"
```

**优先级**：`hexcasting.action.<MOD_ID>:<name>` 优先，其次 `hexcasting.action.book.<MOD_ID>:<name>`。

如果两者都不存在，使用 `hexcasting.iota.<MOD_ID>:<name>` 或其他 pattern 相关的 key。

**fallback**：如果完全没有语言文件条目，按照 hexcasting 命名惯例自行拟定 displayName。

### 2.3 提取 internalName

`internalName` 直接使用注册时的第一个参数（name），保持原样：

```
"am_enlightened" → internalName = "am_enlightened"
"is_brainswept"  → internalName = "is_brainswept"
```

---

## 三、Action 函数的 Elm 实现策略

### 3.1 只实现计算逻辑，不实现世界影响

对于 pattern 的 action 函数，只实现**栈上的值计算和返回**，不要涉及：
- ❌ 修改 Minecraft 世界
- ❌ 修改实体状态
- ❌ 施放法术效果
- ❌ 消耗/产生 media
- ✅ 从栈上取值并类型检查
- ✅ 数据转换和计算
- ✅ 返回计算结果到栈上

参考已有代码中 `spell` 系列函数的实现：消耗输入但不产生实际世界效果。

### 3.2 识别 pattern 的输入输出语义

从源码中推断每个 pattern 的输入输出类型：

```
register("entity_width", "dwe", HexDir.NORTH_WEST, OpGetEntityData { entity -> entity.width.asActionResult })
                                                                          ^^^^^^                 ^^^^^^^^^^^
                                                                          输入: Entity           输出: Number
```

常见的 action wrapper 及其语义：

| Kotlin Wrapper | 输入类型 | 输出类型 | Elm 实现方式 |
|---------------|---------|---------|------------|
| `OpGetEntityData { entity -> ... }` | 1 Entity | 取决于lambda | `action1Input ... getEntity` |
| `OpGetLivingEntityData { ... }` | 1 Entity (living) | 取决于lambda | `action1Input ... getEntity` |
| `OpGetPlayerData { player -> ... }` | 1 Entity (player) | 取决于lambda | `action1Input ... getEntity` |
| `OpGetBlockStateData { state -> ... }` | 1 Vector | 取决于lambda | `action1Input ... getVector` |
| `OpGetBlockTypeData { ... }` | 1 Vector | 取决于lambda | `action1Input ... getVector` |
| `OpGetItemStackData { stack -> ... }` | 1 ItemStack | 取决于lambda | `action1Input ... getItemStack` |
| `OpGetItemTypeData { ... }` | 1 Identifier | 取决于lambda | `action1Input ... getIdentifier` |
| `OpGetEnchantmentTypeData { ... }` | 1 Identifier | 取决于lambda | `action1Input ... getIdentifier` |
| `OpGetFoodTypeData { ... }` | 1 Identifier | 取决于lambda | `action1Input ... getIdentifier` |
| `OpGetWorldData { world -> ... }` | 0 | 取决于lambda | `actionNoInput ...` |
| `OpGetPositionData { world, pos -> ... }` | 1 Vector | 取决于lambda | `action1Input ... getVector` |
| `OpGetEnvData { env -> ... }` | 0 | 取决于lambda | `actionNoInput ...` |
| `OpGetAmbit` | 1 (Entity或Vector) | Boolean | `action1Input ... getAny` |
| `OpCreateDisplay` | 1 Iota | Display | `action1Input ... getAny` |
| `OpIdentify` | 1 (Entity/Vector/Item) | Identifier | `action1Input ... getAny` |
| `OpClassify` | 1 Iota | Identifier | `action1Input ... getAny` |

### 3.3 .asActionResult 推断

Kotlin 中的 `.asActionResult` 是扩展属性，将 Kotlin 类型转换为 Iota：

| Kotlin 表达式 | 对应 Elm Iota |
|-------------|-------------|
| `number.asActionResult` | `Number number` |
| `boolean.asActionResult` | `Boolean bool` |
| `Vec3d(x,y,z).asActionResult` | `Vector (x, y, z)` |
| `entity.asActionResult` 或 `EntityIota(entity)` | `Entity "..."`  |
| `listOf(...).asActionResult` | `IotaList (Array.fromList [...])` |
| `listOf(NullIota())` | `Null` |
| `identifier.asActionResult` | `Identifier "namespace:path"` (如果是新Iota) |
| `display.asActionResult` | `Display "text"` (如果是新Iota) |
| `stack.asActionResult` | `ItemStack ...` (如果是新Iota) |

对于无法在 Elm 中真实获取的值（如世界状态、实体数据），在模拟中返回 `selectedOutput` 指定的默认值。

### 3.4 条件返回 Null 的模式

源码中常见的 "返回 X 或 Null" 模式：

```kotlin
// Kotlin
if (condition)
    return@OpGetBlockStateData extractor().asActionResult
return@OpGetBlockStateData listOf(NullIota())
```

Elm 中对应为设置 `outputOptions`：

```elm
{ ...
, outputOptions = [VectorType, NullType]
, selectedOutput = Just (VectorType, Vector (0, 0, 0))
}
```

---

## 四、新 Iota 类型的处理

### 4.1 是否需要新建 Iota

判断标准：
- 如果新 Iota 只是已有 Iota 的别名 → **不需要**新建，直接映射到已有类型
- 如果新 Iota 有独特的数据结构且被多个 pattern 使用 → **需要**新建

### 4.2 hexpose 仓库的具体情况

| Kotlin Iota | 数据结构 | 建议 Elm 处理 |
|------------|---------|--------------|
| `IdentifierIota` | `namespace:path` 字符串 | **新建** `Identifier String` |
| `DisplayIota` | Text 组件（文本+样式+子节点） | **新建** `Display { text, color, bold, italic, ... }` |
| `ItemStackIota` | 物品ID + 数量 + NBT | **新建** `ItemStack String Int` |

### 4.3 新建 Iota 的文件修改清单

按 `扩展指南.md` 第二章操作，必须修改 7 个文件。

---

## 五、文件组织规范

### 5.1 新 Pattern 文件命名

在 `src/Logic/App/Patterns/` 下新建文件，**以 MOD_ID 命名**：

```
src/Logic/App/Patterns/Hexpose.elm    ← 所有 hexpose 图案的 action 函数
```

文件头：
```elm
module Logic.App.Patterns.Hexpose exposing (..)

-- Patterns from hexpose (https://github.com/miyucomics/hexpose)
-- A Hexcasting addon about getting information about the world

import ...
```

### 5.2 PatternRegistry 注册位置

在 `PatternRegistry.elm` 的 `patternRegistry` 列表末尾添加，**用注释标注来源**：

```elm
    -- ==================== hexpose patterns ====================
    , { signature = "awqaqqq"
      , internalName = "am_enlightened"
      , action = amEnlightened
      , displayName = "Epiphany Purification"
      , outputOptions = []
      , selectedOutput = Nothing
      , startDirection = Southeast
      }
    -- ==================== end hexpose ====================
```

### 5.3 导入声明

在 `PatternRegistry.elm` 文件头部添加：
```elm
import Logic.App.Patterns.Hexpose exposing (..)
```

在 `Main.elm` 中**不需要**添加导入（PatternRegistry 已是中转层）。

---

## 六、冲突检测与处理

### 6.1 签名冲突

在添加前检查 `signature` 是否与已有 pattern 重复。

**检测方法**：在 `PatternRegistry.elm` 中搜索 signature 字符串。

**如果冲突**：
- **停止操作**
- 向用户报告冲突的两个 pattern 的 signature、displayName、来源
- 询问用户如何处理（保留哪个、是否改名等）

### 6.2 修改已有代码

**绝对不要**在未经用户确认的情况下：
- 删除已有的 pattern 注册条目
- 修改已有 pattern 的 signature 或 action
- 修改已有的 Iota 类型定义

如果新功能确实需要修改已有代码，先暂停并询问。

---

## 七、逐步操作流程

### Step 1: 阅读仓库

```
1. 读取 gradle.properties → 确定 MOD_ID
2. 读取 fabric.mod.json → 确认 MOD_ID
3. 读取 *Actions.kt → 提取所有 pattern 的 (name, signature, startDir, action)
4. 读取 en_us.json → 提取所有 displayName
5. 读取 iotas/*.kt → 识别新 Iota 类型
```

### Step 2: 盘点分析

制作清单，列出：
- 新 Iota 类型数量及名称
- 新 Pattern 数量
- 签字冲突检查结果
- 需要用户决策的模糊点

### Step 3: 征求确认

向用户展示分析结果，确认：
- 是否添加所有 pattern
- 是否新建所有 Iota 类型
- 冲突如何处理
- 是否有不需要添加的 pattern

### Step 4: 生成代码

按以下顺序：
1. 新建 Iota 类型（如有）→ 修改 7 个文件
2. 新建 Pattern 文件 → `src/Logic/App/Patterns/<MOD_ID>.elm`
3. 注册 Pattern → 修改 `PatternRegistry.elm`
4. 编译验证 → `.\build.ps1 -BuildOnly`

### Step 5: 验证

1. 编译通过 ✓
2. 浏览器打开 → 搜索新增的 displayName → 确认 pattern 出现在自动补全中
3. 在网格上绘制对应签名 → 确认 pattern 被正确识别

---

## 八、实战笔记（hexpose 经验）

### 8.1 hexpose 数据量

- ~95 个 pattern
- 3 个新 Iota 类型（IdentifierIota, DisplayIota, ItemStackIota）
- 4 个 moreiotas 互操作 pattern（仅在加载 moreiotas 时注册）
- 签名长度：最短 3 字符 ("dwe")，最长 ~33 字符

### 8.2 moreiotas 互操作的特殊处理

hexpose 有 4 个 pattern 依赖 `moreiotas` 模组的 Iota 类型。这些 pattern 仅在 `moreiotas` 加载时注册。

在 Elm 中处理方式：
- 如果 moreiotas 的 Iota 类型尚未在 Elm 中实现 → **跳过**这 4 个 pattern
- 标记为 TODO，等到 moreiotas 也被移植时再补

### 8.3 Pattern 分类参考

hexpose 的 pattern 按功能可分为：

| 类别 | 数量 | 示例 |
|------|:----:|------|
| Entity 数据查询 | ~25 | entity_width, get_health, is_sleeping |
| Block 数据查询 | ~12 | block_hardness, blockstate_rotation |
| Item/Stack 操作 | ~20 | get_mainhand, count_stack, item_name |
| Display/Text 操作 | ~12 | create_display, display_color, parse_display |
| World/环境查询 | ~10 | get_weather, get_light, get_day |
| Identifier 操作 | 2 | identify, classify |
| Villager 数据 | 4 | villager_level, villager_profession |
| Enchantment 数据 | 6 | get_enchantments, enchantment_weight |
| StatusEffect 数据 | 4 | get_effects_entity, get_effect_amplifier |
| Media 查询 | 3 | env_media, get_media, get_max_media |
| Food 查询 | 5 | get_player_hunger, is_meat |
| Tag 查询 | 3 | block_tags, entity_tags, item_tags |
| Misc | 5 | cat_variant, creeper_fuse, painting_variant |
| Moreiotas 互操作 | 4 | (暂缓) |

### 8.4 ItemStackIota 的处理策略

hexpose 大量使用 `ItemStackIota`。但由于它携带 NBT、耐久等实时数据，在 Elm 模拟中无法完整表示。

**简化策略**：`ItemStackIota` → 简化为 `(itemId: String, count: Number)` 的 record 或新的简化 Iota。

---

## 九、常用映射速查

### HexDir → Direction

```
NORTH_EAST → Northeast
EAST       → East
SOUTH_EAST → Southeast
SOUTH_WEST → Southwest
WEST       → West
NORTH_WEST → Northwest
```

### Kotlin 类型 → Elm Iota

```
Double/Float  → Number
Boolean       → Boolean
Vec3d         → Vector (x, y, z)
Entity        → Entity
String        → 可能转 Identifier 或保持 String
List<Iota>    → IotaList
null (NullIota) → Null
```

### Action Wrapper → Elm 辅助函数

```
OpGetEntityData            → action1Input ... getEntity
OpGetLivingEntityData      → action1Input ... getEntity
OpGetPlayerData            → action1Input ... getEntity
OpGetBlockStateData        → action1Input ... getVector
OpGetBlockTypeData         → action1Input ... getVector
OpGetItemStackData         → action1Input ... <新 getter>
OpGetItemTypeData          → action1Input ... <新 getter>
OpGetEnchantmentTypeData   → action1Input ... <新 getter>
OpGetFoodTypeData          → action1Input ... <新 getter>
OpGetWorldData             → actionNoInput
OpGetPositionData          → action1Input ... getVector
OpGetEnvData               → actionNoInput
OpGetAmbit                 → 特殊处理
OpCreateDisplay            → action1Input ... getAny
OpIdentify                 → action1Input ... getAny
OpClassify                 → action1Input ... getAny
```

---

## 十、多模块项目路径查找策略

### 10.1 常见项目结构类型

Hexcasting 附属模组有 3 种常见的项目结构：

| 类型 | 特征 | 示例 | Actions 文件位置 |
|------|------|------|----------------|
| **单模块 Fabric** | `src/main/java/...` 直接在根目录 | hexpose, hexcellular, hexical | `src/main/java/<pkg>/<ModId>Actions.kt` |
| **多模块 (Common+Fabric)** | `settings.gradle` 含 `include("Common", "Fabric")` | MoreIotas, Hexal | `Common/src/main/java/<pkg>/common/lib/hex/<ModId>Actions.kt` |
| **Architectury** | `settings.gradle` 含 `include 'common', 'fabric', 'forge'` | HexFlow | `common/src/main/java/<pkg>/actions/HexFlowPatterns.kt` |

### 10.2 定位 Actions 文件的步骤

1. **读 `settings.gradle`** — 判断是单模块还是多模块项目
2. **读 `fabric.mod.json`**（在 `Fabric/src/main/resources/` 或 `src/main/resources/`） — 获取入口点类名
3. **读入口点类** — 找到 `init()` 方法，追踪 Actions 注册的 import 语句
4. **读 Actions 文件** — 提取所有 pattern 的 (name, signature, startDir)

### 10.3 各仓库已验证路径

| 仓库 | MOD_ID | 项目类型 | Actions 文件路径 |
|------|--------|---------|----------------|
| hexpose | hexpose | 单模块Fabric | `src/main/java/miyucomics/hexpose/HexposeActions.kt` |
| hexcellular | hexcellular | 单模块Fabric | `src/main/java/miyucomics/hexcellular/HexcellularActions.kt` |
| hexical | hexical | 单模块Fabric | `src/main/java/miyucomics/hexical/inits/HexicalActions.kt` |
| MoreIotas | moreiotas | Common+Fabric | `Common/src/main/java/ram/talia/moreiotas/common/lib/hex/MoreIotasActions.kt` |
| Hexal | hexal | Common+Fabric | `Common/src/main/java/ram/talia/hexal/common/lib/hex/HexalActions.kt` |
| HexFlow | hexflow | Architectury | `common/src/main/java/io/yukkuric/hexflow/actions/HexFlowPatterns.kt` |

### 10.4 语言文件位置

| 仓库 | 有语言文件 | 路径 |
|------|:---:|------|
| hexpose | ✅ | `src/main/resources/assets/hexpose/lang/en_us.json` |
| hexcellular | ✅ | `src/main/resources/assets/hexcellular/lang/en_us.json` |
| hexical | ✅ | `src/main/resources/assets/hexical/lang/en_us.json` |
| MoreIotas | ❌ | 无 en_us.json（使用代码内名称） |
| Hexal | ❌ | 无 en_us.json（使用代码内名称） |
| HexFlow | ❌ | 无 en_us.json（使用代码内名称） |

> 无语言文件时，displayName 直接使用 `internalName`（将下划线转为首字母大写，如 `string/empty` → `"String Empty"`）。

### 10.5 注册方式差异

| 仓库 | 注册方式 | 特征 |
|------|---------|------|
| hexpose | `Registry.register(HexActions.REGISTRY, id(name), ...)` | 直接注册 |
| hexcellular | 同上 | 同上 |
| hexical | 同上 | 同上，但路径在 `inits/` 子目录 |
| MoreIotas | `make(name, fromAngles(...), action)` 存入 Map，后 `register()` 批量注册 | `import HexDir.*` 使用简短名称 |
| Hexal | 同上 | 同上 |
| HexFlow | `wrap(name, signature, dir, action)` 存入 Map，`registerActions()` 批量注册 | 伴生对象 `companion object`，Kotlin 风格 |

---

## 十一、仓库规模总览（实际数据）

| 仓库 | MOD_ID | 图案数 | 新 Iota 数 | 新 Iota 名称 | 难度 |
|------|--------|:------:|:---------:|------------|:----:|
| hexpose | hexpose | 91 | 3 | Identifier, Display, ItemStack | ★★☆ |
| hexcellular | hexcellular | 4 | 1 | Property | ★☆☆ |
| hexical | hexical | ~100 | ~2 | Dye, Pigment | ★★★ |
| MoreIotas | moreiotas | 28 | 6 | String, Matrix, IotaType, EntityType, ItemType, ItemStack | ★★★ |
| Hexal | hexal | ~45 | 2 | Gate, Mote | ★★☆ |
| HexFlow | hexflow | 11 | 0 | (无新Iota) | ★☆☆ |
| **总计** | — | **~279** | **~14** | — | — |

> 注意：MoreIotas 的 `ItemStackIota` 与 hexpose 的 `ItemStackIota` 是两个不同的类型。hexical 依赖 hexpose，复用其 Iota。Hexal 依赖 MoreIotas，复用其 Iota。```
