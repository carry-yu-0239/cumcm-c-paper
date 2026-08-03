# Repository Guidelines

## Project Structure & Module Organization

This repository contains a CUMCM C-problem paper workspace and the supplied problem materials.

- `cumcm_c_skeleton/main.tex` is the paper entry point. Update `\PaperTitle` and `\ProblemNumber` here.
- `cumcm_c_skeleton/settings.tex` holds shared page layout, fonts, numbering, and helper macros. Keep presentation-wide changes here.
- `cumcm_c_skeleton/sections/` contains the paper in reading order: `00_abstract.tex` through `14_appendix.tex`. Put model-specific prose in the matching numbered file.
- Use `cumcm_c_skeleton/figures/`, `code/`, and `data/` for generated visuals, runnable supporting programs, and cleaned intermediate data when those directories are added.
- `question/` contains the official problem PDF, format specification, and source attachment; treat these as inputs and do not edit them.

## Cloud Build and Development

The canonical build runs in GitHub Actions at `.github/workflows/build-latex.yml`. Each push to `main`, pull request, or manual run compiles `cumcm_c_skeleton/main.tex` with XeLaTeX and publishes `paper-pdf` as a 30-day artifact. Do not install TeX Live or depend on local compilation for routine editing.

If a local TeX environment is already available, the optional check is:

```bash
cd cumcm_c_skeleton
latexmk -xelatex main.tex  # Build main.pdf and resolve references
latexmk -c                 # Remove LaTeX auxiliary files
```

## Writing, LaTeX Style, and Naming

Keep source files UTF-8 and use Chinese prose consistently with the template. Preserve the existing four-space indentation inside LaTeX environments and keep one logical sentence or command per line where practical. Use descriptive, lowercase filenames: `figures/error_distribution.pdf`, `data/cleaned_samples.csv`, and `code/problem1_fit.py`.

Prefer semantic LaTeX commands over manual formatting. Reuse `\todo{}` only for temporary placeholders; remove or replace every placeholder before submission. Put labels close to their figures, tables, and equations, and use stable prefixes such as `fig:`, `tab:`, and `eq:`.

## Validation and Submission Checks

There is no automated test framework. Before delivering, inspect the successful GitHub Actions result, download `paper-pdf`, and confirm that citations resolve. Ensure the abstract is the first electronic page, no table of contents is added, the appendix lists supporting materials, and the paper, appendix, and support files contain no team, school, student, or regional identity information.

## 数据预处理与图形交付经验

对题目附件实施预处理时，先保留原始 `question/` 工作簿不变，并把采样点、文物和未知样品三层数据分开处理。文物编号只能用于关联；普通点、未风化点和严重风化点必须由完整采样点名称逐行识别，不能仅按编号给同一文物的所有点赋同一状态。若规则要求文物级代表成分，必须逐条保留普通点均值、普通点与严重风化点的 (1:2) 加权、普通点优先于未风化点等口径。

每类主计算只保留一个权威可执行入口，禁止维护内容复制的双份主程序。将固定验收值写成断言，例如有效记录数、未检出单元格数、指定填补颜色和众数回退条件；敏感性输出应同时记录关键填补结果、供体集合变化和异常诊断，不能只报告新增异常点数。

“独立复核”必须从原始 XLSX 重新读取并自行完成清洗、状态判定和数值变换；MATLAB 导出的 CSV 只能作为最后的结果比对对象，不能作为复核输入。复核报告应明确它覆盖的计算范围，并把模型尚未实现的下游指标列为未验证，不能以核心变换一致性泛称全部复核完成。

把数值计算和论文图形拆分：主程序输出稳定的清洗 CSV 或工作簿，图形脚本只读取这些中间结果。统计图输出矢量 PDF 和预览 PNG；使用真实成分名称、中文标签、克制且可灰度区分的配色，并按最终插入论文的尺寸设计。流程图优先用 XeLaTeX TikZ 绘制，不使用宽而低的信息贫乏流程条。PDF 文件存在、大小和签名检查不等于图形验收；至少要渲染为 PNG 并检查尺寸和非空内容，最终仍需在论文 PDF 中人工核对字号、裁切和重叠。

运行记录必须区分“脚本已修改”“R/Python 已执行”“MATLAB 已执行”“XeLaTeX 已编译”和“最终 PDF 已人工审阅”。未实际运行的步骤不得在正文、日志或交付说明中表述为通过；代码、图形、附录清单和 README 的运行顺序须同步更新。

## Commit & Pull Request Guidelines

No Git history is available, so use concise imperative commits such as `docs: complete problem 2 model` or `fix: align sensitivity table`. Keep each commit focused. Pull requests should describe the affected sections, state the PDF build result, link any relevant issue, and include PDF screenshots when visual layout changes.
