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

## 问题三复现

问题三沿用问题二锁定的14维CLR--PLS--DA主模型（1个潜变量），并以PbO决策树和PbO、K$_2$O、BaO、SrO四变量PLS--DA作解释与对照。数值计算、表格排版、图形输出和结构验收彼此分离；图形脚本只读取稳定CSV，不重新估计模型。

```bash
cd cumcm_c_skeleton
matlab -batch "run('code/export_problem3_inputs.m')"
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/run_problem3.R
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/make_problem3_tables.R
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/make_problem3_figures.R
"C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/verify_problem3_outputs.R
python code/verify_outputs.py
```

`data/problem3/run_summary.txt`记录实际运行状态；`unknown_predictions.csv`给出A1--A8的最终大类、两种对照和得分间隔；`sensitivity_runs.csv`、`sensitivity_summary.csv`记录CLR局部扰动和近零参数敏感性。若本机MATLAB许可证服务不可用，不能把上述MATLAB导出步骤表述为已执行；可使用已核验的既有导出输入继续进行R计算，并在交付记录中注明该环境限制。
`code/make_problem3_figures.R`生成`figures/problem3_*.pdf/.png`：PDF为中文矢量图，PNG为300 dpi预览；问题三流程图直接由`sections/08_problem3.tex`中的TikZ生成，`verify_outputs.py`会渲染统计图PDF并检查非空与尺寸。

## 问题四复现

问题四以67个有效采样点为单位，直接读取三份预处理核验快照中的14维CLR原始值；高钾18点、铅钡49点。run_problem4.R是唯一数值计算入口，依次输出检出率、Pearson与Spearman相关矩阵、91组关联差异、一次配对Wilcoxon整体比较和按文物完整删除的LOAO敏感性。它不重跑预处理、不读取未知样品，也不产生单对显著性结论。make_problem4_figures.R仅读取稳定CSV，输出中文矢量PDF及300 dpi PNG预览。

运行顺序如下：

    cd cumcm_c_skeleton
    "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/run_problem4.R
    "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/make_problem4_tables.R
    "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/make_problem4_figures.R
    "C:/Program Files/R/R-4.6.1/bin/Rscript.exe" --encoding=UTF-8 code/verify_problem4_outputs.R
    python code/verify_outputs.py

data/problem4/保存稳定CSV、自动生成的LaTeX表格和实际运行/验证报告；figures/problem4_*.pdf/.png保存四幅问题四图形。verify_problem4_outputs.R仅检查CSV、表格和数值范围；verify_outputs.py渲染PDF并检查预览PNG的非空、尺寸及300 dpi，不重算模型。

## 交稿前检查

1. 删除所有“【待补：……】”提示。
2. 摘要（含标题和关键词）控制在一页内。
3. 不生成目录，正文尽量控制在20页以内。
4. 摘要、正文、附录及支撑材料中不得出现姓名、学校、学号、赛区等身份信息。
5. 正文中引用处标注文献，参考文献列表与引用一一对应。
6. 附录列出支撑材料文件；支撑材料中提供全部完整、可运行程序。
7. 电子论文单独提交PDF或Word，建议PDF，大小不超过20MB；承诺书和编号页不放入电子论文。
