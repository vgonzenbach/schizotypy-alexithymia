# Get column labels that match columns in cleaned df

#Load raw and cleaned data
df_raw = readRDS("./raw/qualtrics_seas_one.rds")
df_clean = read.csv("./data/main/df_seas_one_clean.csv")

# Get label for each column from raw data
labels = vapply(df_raw, 
                function(x){attr(x, "label")},
                character(1))

# Subset labels to keep only the ones for columns in clean data
labels = labels[names(labels) %in% colnames(df_seas)]
saveRDS(labels, "data/labels/seas_one_labels.rds")
