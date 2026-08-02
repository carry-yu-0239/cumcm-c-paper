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

## Commit & Pull Request Guidelines

No Git history is available, so use concise imperative commits such as `docs: complete problem 2 model` or `fix: align sensitivity table`. Keep each commit focused. Pull requests should describe the affected sections, state the PDF build result, link any relevant issue, and include PDF screenshots when visual layout changes.
