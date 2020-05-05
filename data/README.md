# Project data

## Folder structure

This folder contains processed data.

1. [complete](complete) contains all the data collected as part of this online—minus identifiable information of each participant:

..* [df_seas_one_clean.csv](complete/df_seas_one_clean.csv) contains all collected data after [processing](../src/01-clean_data.R)
..* [seas_one_labels.rds](complete/seas_one_labels.rds) contains labels for the corresponding items/columns. This [script](../src/02-get_col_labels.R) generated these labels from raw data.

2. [main](main) contains only the data used in the [main analysis](../notebooks/linear_models.Rmd)
  