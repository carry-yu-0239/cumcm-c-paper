# 2022 CUMCM C题论文骨架

## 编译

```bash
latexmk -xelatex main.tex
```

清理辅助文件：

```bash
latexmk -c
```

## 结构

- `main.tex`：入口文件，电子版第一页直接为摘要页，不含承诺书、编号页和目录。
- `settings.tex`：A4、四边2.5 cm页边距、页脚居中页码等设置。
- `sections/`：摘要、问题重述、四问模型、检验、结论、参考文献和附录。
- `figures/`：图形输出。
- `code/`：完整可运行程序。
- `data/`：清洗后的中间数据，题目原始附件无需重复放入支撑材料。

## 数据预处理复现

在已配置 MATLAB R2025b、R 4.6.1 和 Python 的环境中，按以下顺序执行：

```bash
cd cumcm_c_skeleton
matlab -batch "run('code/preprocess.m')"
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" code/verify_preprocess.R
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" code/make_figures.R
python code/verify_outputs.py
```

上述命令只覆盖预处理：MATLAB 是预处理的唯一主计算源；R 从 `question/附件.xlsx` 独立读取并复算关键结果，图形脚本仅读取清洗CSV快照。流程图由 `sections/05_data.tex` 中的 TikZ 直接生成。

## 问题二复现

问题二的数值计算与图形输出分离：前者只读取已验收的预处理快照，后者只读取前者写出的稳定CSV；两者均不改写原始附件。

```bash
cd cumcm_c_skeleton
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/run_problem2.R
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/make_problem2_figures.R
python code/verify_outputs.py
```

数值结果写入 `data/problem2/`：其中包含分组交叉验证、决策树规则、PLS--DA得分与VIP、K-means亚类归属、二维展示坐标、CLR逆变换中心与敏感性记录。图形脚本生成 `figures/problem2_*.pdf/.png`，其中PDF为中文矢量图，PNG为300 dpi预览；`verify_outputs.py`会将PDF渲染后检查非空与尺寸。

## 交稿前检查

1. 删除所有“【待补：……】”提示。
2. 摘要（含标题和关键词）控制在一页内。
3. 不生成目录，正文尽量控制在20页以内。
4. 摘要、正文、附录及支撑材料中不得出现姓名、学校、学号、赛区等身份信息。
5. 正文中引用处标注文献，参考文献列表与引用一一对应。
6. 附录列出支撑材料文件；支撑材料中提供全部完整、可运行程序。
7. 电子论文单独提交PDF或Word，建议PDF，大小不超过20MB；承诺书和编号页不放入电子论文。
