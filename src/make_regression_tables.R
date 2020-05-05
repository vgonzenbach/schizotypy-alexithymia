library(dplyr)
library(lmSupport)

reg.tables = vector(mode = "list", length = 3)
table = matrix(nrow = 4, ncol = 16)

# Save Cognitive Alexithymia Models
# Model 1

DVs = c("cog_alex", "emot_bvaq", "fant_bvaq")
IVs = c("pos_schz", "neg_schz", "dis_schz", "pa_PANAS", "na_PANAS")


for(k in 1:length(DVs)){
  m1 <- as.formula(paste(DVs[k], paste(IVs[1:3], collapse=" + "), sep=" ~ "))
  m2 <- as.formula(paste(DVs[k], paste(IVs[1:4], collapse=" + "), sep=" ~ "))
  m3 <- as.formula(paste(DVs[k], paste(IVs[c(1:3, 5)], collapse=" + "), sep=" ~ "))
  m4 <- as.formula(paste(DVs[k], paste(IVs[1:5], collapse=" + "), sep=" ~ "))
  
  formulas = c(m1, m2, m3, m4)
  
  for(j in 1:length(formulas)){
    
    reg = lm(formulas[[j]], data = main_df)
    betas = reg$coefficients[2:length(reg$coefficients)]
    R2 = summary(reg)$r.squared
    all.eff = modelEffectSizes(reg, Print = FALSE)$Effects
    dR2 = all.eff[2:nrow(all.eff),4]
    f2 = dR2/(1-R2)
    
    all_effects = numeric(0)
    sig = ifelse(summary(reg)$coefficients[2:(length(betas)+1),4] < .001, "*", " ")
    for(i in 1:length(betas)){
      
      effects = c(paste0(round(betas[i], 3), sig[i]),
                  paste0(round(dR2[i], 3), sig[i]),
                  paste0(round(f2[i], 3), sig[i])
                  )
      all_effects = c(all_effects, effects)
      
      if(betas)
      i = i + 1
    }
    
    all_effects
    
    
    
    table[j,] = c(all_effects, rep(NA, 16 - length(all_effects)))
    table[j, 16] = round(R2, 3)
    
    j = j + 1
  }
  
  reg.tables[[k]] = table 
  k = k + 1
}


#make flex table
library(flextable)
f.tables = vector(mode = "list", 3)
stats_header = c(rep(c("β","∆R2", "ƒ2"), 5), "R2")
reg.tables[[1]] = as.data.frame(reg.tables[[1]], stringsAsFactors = FALSE)
reg.tables[[2]] = as.data.frame(reg.tables[[2]], stringsAsFactors = FALSE)
reg.tables[[3]] = as.data.frame(reg.tables[[3]], stringsAsFactors = FALSE)

titles = c("Cognitive Alexithymia", "Affective: Emotionalizing", "Affective: Fantasizing")

for(t in 1:length(titles)){
  df4header = data.frame(col_keys = paste0(rep("V", 16), 1:16),
                         DV = rep(titles[t], 16),
                         IV = c(rep(c("Positive Schz", 
                                      "Negative Schz",
                                      "Disorganized Schz",
                                      "Positive Affect",
                                      "Negative Affect"), each = 3), ""),
                         stats = stats_header, stringsAsFactors = FALSE)
  
  
  f.tables[[t]] = flextable(reg.tables[[t]]) %>% 
    set_header_df(mapping = df4header) %>% 
    merge_h(part = "header") %>% 
    align(align = "center", part = "header") %>% 
    align(align = "center", part = "body") %>% 
    theme_booktabs() %>% border_inner(fp_border(color="grey", width = 1), part = "header")
  
  save_as_pptx(f.tables[[t]], path = paste0("../results/internal/reg_table_", t,".pptx"))
  t = t + 1
}

