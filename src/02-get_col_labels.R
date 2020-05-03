# Store labels

df_raw = readRDS("./raw/qualtrics_seas_one.rds")
df_clean = read.csv("./data/main/df_seas_one_clean.csv")

labels = vapply(df_raw, 
                function(x){attr(x, "label")},
                character(1))

#Keeponly labels in df_seas_one_clean.csv
labels = labels[names(labels) %in% colnames(df_seas)]
saveRDS(labels, "data/labels/seas_one_labels.rds")
