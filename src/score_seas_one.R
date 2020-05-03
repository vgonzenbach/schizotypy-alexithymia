library(dplyr)
source("scoring_functions.R")

#read data
#df_seas = readRDS("data/df_seas_one.rds")
df_seas = read.csv("data/df_seas_one.csv")

# filter by Age according to previous psychometric validation studies of the MSS
df_seas = df_seas %>% filter(Age %in% (18:59))

#generalized NA filter
filter_NA = function(data, item_stem_list){
  "Function takes list of item stem strings as second argument"
  for(item_stem in item_stem_list){
    nas = data %>% 
      select(starts_with(item_stem)) %>% 
      is.na() %>% 
      rowSums() > 0
    data = data[!nas, ]
  }
  return(data)
}
df_seas = filter_NA(df_seas, list("MSS", "BVAQ", "PANAS"))
write.csv(df_seas, "data/df_seas_filtered.csv")

df_scores = data.frame(score_MSS(df_seas), score_BVAQ(df_seas), score_PANAS(df_seas))
df_scores = cbind(df_scores, 
                 cog_alex = df_scores$idnt_bvaq + df_scores$nlyz_bvaq + df_scores$verb_bvaq,
                 aff_alex = df_scores$emot_bvaq + df_scores$fant_bvaq)
write.csv(df_scores, "data/df_seas_scores.csv", row.names = FALSE)


