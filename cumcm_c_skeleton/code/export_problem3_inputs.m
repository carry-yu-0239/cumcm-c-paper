% Export the already approved unknown-sample CLR inputs for Problem 3.
% This script intentionally does not re-run preprocessing or modify its workbook.

script_dir = fileparts(mfilename('fullpath'));
root_dir = fileparts(script_dir);
data_dir = fullfile(root_dir, 'data');
out_dir = fullfile(data_dir, 'problem3');
if ~exist(out_dir, 'dir')
    mkdir(out_dir);
end

workbooks = dir(fullfile(data_dir, '*.xlsx'));
assert(numel(workbooks) == 1, 'Expected exactly one preprocessing workbook in data/.');
workbook_path = fullfile(workbooks(1).folder, workbooks(1).name);

clr_table = readtable(workbook_path, 'Sheet', 'U3_CLR', 'VariableNamingRule', 'preserve');
zclr_table = readtable(workbook_path, 'Sheet', 'U3_ZCLR', 'VariableNamingRule', 'preserve');

assert(height(clr_table) == 8 && width(clr_table) == 15, 'U3_CLR must be 8 by 15.');
assert(height(zclr_table) == 8 && width(zclr_table) == 15, 'U3_ZCLR must be 8 by 15.');
assert(isequal(string(clr_table{:, 1}), "A" + string((1:8)')), 'U3_CLR IDs must be A1--A8.');
assert(isequal(string(zclr_table{:, 1}), "A" + string((1:8)')), 'U3_ZCLR IDs must be A1--A8.');

names = [{'artifact_id'}, strcat('CLR_', string(1:14))];
clr_table.Properties.VariableNames = names;
zclr_table.Properties.VariableNames = names;
assert(all(isfinite(clr_table{:, 2:end}), 'all'), 'U3_CLR contains a non-finite value.');
assert(all(isfinite(zclr_table{:, 2:end}), 'all'), 'U3_ZCLR contains a non-finite value.');
assert(all(abs(sum(clr_table{:, 2:end}, 2)) < 1e-10), 'U3_CLR rows must sum to zero.');

writetable(clr_table, fullfile(out_dir, 'unknown_clr_input.csv'), 'Encoding', 'UTF-8');
writetable(zclr_table, fullfile(out_dir, 'unknown_zclr_reference.csv'), 'Encoding', 'UTF-8');

summary = [ ...
    "Problem 3 input export: PASS"; ...
    "Source: existing U3_CLR and U3_ZCLR sheets in the preprocessing workbook."; ...
    "Rows=8; CLR variables=14; IDs=A1--A8; all values finite; every CLR row sum is below 1e-10."; ...
    "No preprocessing, classification, figure generation, or source-workbook modification was performed." ...
];
writelines(summary, fullfile(out_dir, 'input_export_summary.txt'), 'Encoding', 'UTF-8');
