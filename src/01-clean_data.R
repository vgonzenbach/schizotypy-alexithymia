if(!file.exists("./raw/qualtrics_seas_one.rds")){
  source("./src/00-get_data.R")
}

# Read data from rds file----
df_seas = readRDS("./raw/qualtrics_seas_one.rds")

# Remove useless columns----
cols2remove = c(1:4, 9:17, 70, 86, 102, 118, 134, 137:141, 225)
df_seas[,cols2remove] = NULL

# Delete empty rows----
empty_rows = is.na(df_seas$email) | is.na(df_seas$NetID)
df_seas = df_seas[!empty_rows,]

# Filter incompletes----
## Cause: Exited survey
df_seas = df_seas[df_seas$Finished,]

## Cause: Skipped questions
nas_in_row = rowSums(apply(df_seas, 2, function(x){is.na(x)}))
nas_cut_off = mean(nas_in_row) + 3 * sd(nas_in_row)
df_seas = df_seas[!nas_in_row > nas_cut_off,]

# Define Function: get duplicated identifier----
get_duplicated_id = function(id){
  "Returns a vector with ids/emails that appear more than once in data set"
  id = tolower(id)
  is_dup = table(id) > 1
  dup_ids = names(which(is_dup)) #take names of is_dup where TRUE
  return(dup_ids)
}
emails = tolower(df_seas$email)
ids = tolower(df_seas$NetID)
dup_email = get_duplicated_id(emails)
dup_id = get_duplicated_id(ids)

# Save first observation per respondent----
index_first_dup = unique(c(match(dup_email, emails),
                           match(dup_id, ids)))
first_obs = df_seas[index_first_dup,]

# Delete all duplicated observations from main df----
bool_dup_obs = ids %in% dup_id | emails %in% dup_email
df_seas = df_seas[!bool_dup_obs,]

# Re-add only first observations for duplicated----
df_seas = rbind(df_seas, first_obs)

# Reorder by date----
df_seas = df_seas[order(df_seas$RecordedDate),]

# Remove variables----
rm(empty_rows, bool_dup_obs, dup_email, dup_id, emails, ids, 
   index_first_dup, first_obs, cols2remove, nas_cut_off, nas_in_row)

#### Save data ######### 
#with identifiers (for admin)
#write.csv(df_seas, "~/R/SEAS/admin/data/df_seas_one_identifiable.csv", row.names = FALSE)

##without identifiers
df_seas$email = NULL
df_seas$NetID = NULL
df_seas[,1:10] = NULL #delete useless columns
write.csv(df_seas, "./data/main/df_seas_one_clean.csv", row.names = FALSE) #exportable to python, SPSS, etc.
#saveRDS(df_seas, "data/df_seas_one_clean.rds") #backup

