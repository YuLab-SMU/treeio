# AGENTS.md — 在 treeio 里干活前先读这个

treeio 是 ggtree 生态的输入输出层：把 BEAST / MrBayes / MEGA / PAML / HyPhy /
IQ-TREE / RAxML / MCMCTree / LSD2 / r8s / jplace / phyloxml / NHX / Nextstrain 等
格式解析成 `treedata`（`phylo` + 注释 `data`），也负责写回。
文档：<https://yulab-smu.top/treedata-book-2ed/>（**第二版**，不是 `treedata-book/`）。

---

## 一、修一个 issue 的标准流程

1. **先复现**。用 `inst/extdata/<FORMAT>/` 里已有的 fixture，没有就造一个最小的
   放进去（供测试长期复用）。
2. **建对照环境**（不要直接在仓库里试）：
   ```bash
   cp -r . /tmp/treeio-<新的目录名>      # rm 被 shim 拦住，且别复用旧目录名
   cd /tmp/treeio-xxx && git checkout -- R/<要对比的文件>.R   # 这份当 old
   ```
   另一份拷工作树当 new，两边都 `pkgload::load_all()`。
3. **改完必须验证输出一致**：逐个 fixture 比较 `@phylo` 和 `as.data.frame(@data)`
   （`@data` 是 tibble，直接 `expect_equal` 会失败）。
4. **反向验证**：把修复撤销，新加的测试必须失败——否则这个测试没锁住行为。
5. 跑全套：`testthat::test_dir("tests/testthat")`。基线是 **0 FAIL**，只有
   `test-spt.R` 一个既有的 warning（`Couldn't reach some vertices`）。
6. **一个 issue 一个 commit**，正文写清"原来怎样 / 现在怎样"，末尾带 `closes #N`。
7. `NEWS.md` 加一条（格式见文件里现有条目）。
8. 更新仓库外的跟踪文档 `../treeio-issue.md`。

---

## 二、环境事实（别踩）

| 事实 | 后果 |
|---|---|
| `NAMESPACE` 是**手写**维护的；装的是 roxygen2 8.1.0 而 `DESCRIPTION` 声明 7.3.2 | **不要跑 `roxygenise()`**，会整片重写 NAMESPACE。改了 roxygen 注释就手改对应的 `man/*.Rd`，新增 `#' @export` 要手加一行到 NAMESPACE |
| 开发在 `devel`；`origin` = YuLab-SMU/treeio，Bioconductor 是 upstream | 别往 master 提交 |
| `codemeta.json` 在 `.Rbuildignore` 里，不进 tarball | 改了元数据要 `make codemetar` 重新生成 |
| 改版本要同步 4 处 | `DESCRIPTION` → `NEWS.md` 小节 → 渲染 README（`make readme`）→ `make codemetar` |
| **push 之前先问余老师** | 默认只 commit |

---

## 三、解析器的坑（按症状查）

### 症状：R 会话静默死掉（exit 0、无报错，表现为"循环跑完但循环后的代码不执行"）

`ape::read.tree()` 拿到**不是 Newick** 的文本不会报错，它会把碎片解析成一棵
无意义的树，或者直接把会话搞死（`read.beast()` 读 MEGA 的 `*_tabular.txt` 就是
这样，#98 同款）。
→ 任何 `read.*()` 在把文本交给 `read.tree()` 之前，先用 `is_newick()` /
`find_newick()`（在 `R/iqtree.R`）把住，`find_newick()` 的 `hint=` 可以带一句
"该用哪个函数"的提示。

### 症状：凭空多出 `V1` `V2` `V3` 这种垃圾列

`xml2::read_xml()` 默认 `options = "NOBLANKS"`，**一旦传 `options=` 就把默认值
覆盖掉**，缩进的空白文本节点会被当成每个 clade 的注释。
→ 必须写 `options = c("NOBLANKS", "HUGE")`。

### 症状：`Excessive depth in document: 256` / `C stack usage ... too close to the limit`

- 前者：libxml2 默认拒绝 >256 层嵌套 → 加 `HUGE`。
- 后者：`xml2::as_list()` 本身是递归的，~500 层爆 C stack，光加 `HUGE` 没用
  （上限只从 256 提到 ~450）。
  → 深树**不要整篇 `as_list()`**：用 `xml_children()` + `xml_name()` 直接走节点，
  只对扁平的注释子树调 `as_list()`，`read.phyloxml()` 就是这么改的。

### 症状：ape 相关的结果不对

- `read.nexus()` **只认 `TREE`，不认 `UTREE`**（MCMCTree、IQ-TREE 的 LSD2 输出都是 UTREE）。
- `read.nexus()` 按 **TRANSLATE 的 key** 给 tip 编号；key 不是 `1:Ntip` 时树就是坏的（MEGA）。
- `trans()` 在长度不是 3 的倍数时会**丢掉末尾碱基**，所以拿它做长度比较不可靠。
- `drop.tip()` 剩不足 2 个 tip 时返回 `NULL`。

### 症状：改快了但输出不一样了

`res <- rep(NA, n)` 是 **logical**；`res[i] <- 0.5` 会把它变成 numeric，之后赋
字符才转 character。所以**一个节点的注释全是数字时 `res` 会保持 numeric**，这直接
决定最后 tibble 的列是不是数值型。
→ 向量化改写时必须保留：`all(num)` 走 `res[] <- as.numeric(val)`，否则退回逐元素循环。

---

## 四、R 语言级的坑（都踩过）

- **复杂赋值 `<<-` 会跳过局部绑定**，从外层环境一路找到 attached 包：
  `stack[[top]] <<- NULL` 打到的是 `utils::stack`（一个闭包），报
  `object of type 'closure' is not subsettable`。函数内的显式栈全程用 `<-`。
- `is.na(list(NULL))` 是 **FALSE**（`x["不存在的名字"]` 返回 `list(NULL)` 而不是 `NA`）。
- `lapply(X, FUN, extra = ...)` 的 `extra` 是 promise，**第一次 forced 后结果被缓存**，
  所有元素共用同一个值（老 `parser_clade()` 就是靠这个传 parent 的）。
- `%>%` 一次约 10µs，`use_perl()` 就是 `getOption()`——**别放在 per-node 的循环里**。
- 递归 + 每层 `bind_rows()` = 二次复杂度（512 tip 7.2s → 改显式栈后 0.40s）。
- 同一个 md 上**并行发多个 Edit 会互相覆盖**（后一个基于旧快照写回，吃掉前一个的改动）。
  批量改表格行用 python 一次读改写，逐条 assert 找不到就报错。

---

## 五、性能：现在的热点在哪

`read.beast()` 的瓶颈 100% 在 `read.stats_beast_internal()`：

- 已做（`d7ceca9`）：抽出 `parse_annotation()`，`use_perl()` 每棵树只调一次，
  `strsplit()` 提到 lapply 外，`is_numeric()` 换成一次向量化 `as.numeric()`，
  去掉"把刚写出的树再 `read.tree()` 一遍"。500 棵树 × 200 tip：33.3s → 24.8s。
- **还剩**：per-node 的 4 个 `gsub()` 占 66%。要再快就得**跨节点整体向量化**（把
  所有节点的注释摊平成一个大向量再切分），难点是 SETS（`height_95%_HPD={a,b}`
  这种值里带逗号、要跨多个片段拼接）的还原。余老师 2026-09-21 决定**暂不做**，
  风险高于收益。

`read.phyloxml()` 已经改完（`495de56`），见第三节。

---

## 六、测试约定

- fixture 放 `inst/extdata/<FORMAT>/`，用 `system.file("extdata", ...)` 引用。
- Suggests 里的包（phangorn 等）用 `skip_if_not_installed()`。
- 比较 `@data` 前先 `as.data.frame()`。
- 时序测试不要写（CI 上会 flaky），要锁的是**行为**：比如"深于 256 层的树能读进来"。
- 测 `read.*()` 的报错路径同样重要（`expect_error(..., "cannot find a Newick tree")`）。

---

## 七、提交前

```bash
git status --short      # 看清楚再 add
```
别用 `git add -A`：仓库里常有别人的未提交改动（比如 README 改链接），
卷进你的 commit 就得 `git reset --soft HEAD~1` 再挑文件重提。
