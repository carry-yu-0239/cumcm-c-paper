# 数据预处理环境说明

- MATLAB 主程序为 `code/preprocess.m`，保留 ASCII 源码以兼容此前对非 ASCII 脚本源有限制的本地启动器；中文工作簿字段由 Unicode 码点生成。
- 当前复核在 Windows 上使用 `C:\Program Files\R\R-4.6.1\bin\Rscript.exe` 运行。为保持复核可移植且不依赖外部 R 包，`verify_preprocess.R` 使用 base R 解包并读取原始 XLSX。
- `make_figures.R` 使用 Cairo 设备输出矢量 PDF 和 300 dpi PNG；可通过环境变量 `CUMCM_PLOT_FONT` 指定本机中文字体，默认 `Microsoft YaHei`。
- `verify_outputs.py` 使用 Poppler 的 `pdftoppm` 渲染 PDF，并检查渲染图的尺寸及非空像素区域。该检查不能代替插入最终论文后的人工版面验收。
- 本次调整实际运行了 R 独立复核、R 作图和 Python PDF 渲染检查；未在本机调用 MATLAB 或编译 XeLaTeX，MATLAB 与最终论文 PDF 仍应在交付前按 README 复跑和审阅。
