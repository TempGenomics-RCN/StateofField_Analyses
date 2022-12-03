Contains source & raw data read into R scripts for cleaning.

Raw data (*.xlsx* files) are in the sub-folder **/Original_Excel**. These files are where data were directly recorded. Source data read into R for data cleaning are in *csv* files.

The only difference between the raw and source data files are as follows:
1. Source data contains only the first 43 columns.
2. Source data contains only the rows with empirical studies where the **subject_1** column is the same subject as name of the source data file (e.g. "adaptation" for `adaptation.csv`).

All files contain the following information in each column:
1. **dfdf** contains XYZ.
