library(dplyr)
library(stringr)

prep_MSS = function(SEAS, item_stem = "MSS"){
  "Dependency for score_MSS: Handles reverse-coded items prior to scoring totals"
  MSS = SEAS %>% 
    select(starts_with(item_stem))
  MSS = apply(MSS, 2, as.numeric) # change to numeric
  
  #reverse scores
  rev = c(4, 7, 10, 12, 19, 22, 28, 34, 46, 49, 58, 75, 76)
  MSS[,rev] = (MSS[,rev] - 1)^2 #turns 0 into 1 and 1 into 0
  return(MSS)
}

#computes three subscales
#returns dataframe
score_MSS = function(data, item_stem = "MSS", norm = FALSE){
  "Returns dataframe with 3 columns - one per subscale"
  "'norm' determines whether scores are scaled to population norms"
  MSS = prep_MSS(data, item_stem)
  
  #define subscale indices
  P = seq(2, 77, by = 3)
  N = seq(1, 77, by = 3) 
  D = seq(3, 77, by = 3)
  
  #compute subscale scores
  pos_schz = rowSums(MSS[,P]) # pos Schizotypy
  neg_schz = rowSums(MSS[,N]) # neg Schizotypy
  dis_schz = rowSums(MSS[,D]) # dis schizotypy
  
  if(norm){
    pos_schz_normed = scale(pos_schz, center = 3.587, scale = 4.384)
    neg_schz_normed = scale(neg_schz, center = 3.604, scale = 4.436)
    dis_schz_normed = scale(dis_schz,center = 4.008, scale = 5.751)
    
    MSS_scores = data.frame(pos_schz_normed, neg_schz_normed, dis_schz_normed)
    return(MSS_scores)
  }else{
    MSS_scores = data.frame(pos_schz, neg_schz, dis_schz)
    return(MSS_scores)
  }
}


get_hi_schz = function(data){
  "Returns vector of logicals to subset dataframes"
  "TRUE for observations where schizotypy is higher than one standard deviation"
  data$pos_schz_normed >= 1 | data$neg_schz_normed >= 1 | data$dis_schz_normed >= 1
}

get_lo_schz = function(data){
  "Returns vector of logicals to subset dataframes"
  "TRUE for observations where schizotypy is lower than one standard deviation"
  data$pos_schz_normed < 1 & data$neg_schz_normed < 1 & data$dis_schz_normed < 1
}

prep_BVAQ = function(data, item_stem = "BVAQ"){
  "Dependency for score_BVAQ: Extracts integer from choice text and handles reverse coded items"
  BVAQ = data %>% 
    select(starts_with(item_stem, ignore.case = FALSE))
  
  #extract number within choice text and convert to numeric
  BVAQ = as.data.frame(apply(BVAQ, 2, function(x){as.numeric(str_extract(x, "[[:digit:]]+"))}))
  
  #reverse scores
  rev = c(  2,  3,  6, 10,
            12, 13, 14, 16,
            19, 20, 26, 27,
            28, 29, 30, 31,
            37, 38, 39, 40)
  BVAQ[,-rev] = 6 - BVAQ[, -rev]
  return(BVAQ)
}

score_BVAQ = function(data){
  
  BVAQ = prep_BVAQ(data)
  
  #  Identifying emotions Items: 
  idnt = c(3,8,13,18,23,28,33,38)
  
  #  Analyzing emotions Items: 
  nlyz = c(5,10,15,20,25,30,35,40)
  
  #  Verbalizing emotions Items: 
  verb = c(1,6,11,16,21,26,31,36)
  
  #  Emotionalizing Items: 
  emot = c(4,9,14,19,24,29,34,39)
  
  #  Fantasizing Items: 
  fant = c(2,7,12,17,22,27,32,37)
  
  #higher scores equal more alexithymic traits
  
  #Score 
  idnt_bvaq = rowSums(BVAQ[,idnt])
  nlyz_bvaq = rowSums(BVAQ[,nlyz])
  verb_bvaq = rowSums(BVAQ[,verb])
  emot_bvaq = rowSums(BVAQ[,emot])
  fant_bvaq = rowSums(BVAQ[,fant])
  
  #for NAs, impute means of their repective
  
  BVAQ_scores = data.frame(idnt_bvaq,
                           nlyz_bvaq,
                           verb_bvaq,
                           emot_bvaq,
                           fant_bvaq
                           )
  return(BVAQ_scores)
}

prep_PANAS = function(data, item_stem = "PANAS"){
  "Dependency for score_PANAS: Extracts integer from choice text"
  PANAS = data %>% 
    select(starts_with(item_stem))
  
  #extract number in string and conver to numeric
  PANAS = as.data.frame(apply(PANAS, 2, function(x){as.numeric(str_extract(x, "[[:digit:]]+"))}))
  return(PANAS)
}

score_PANAS = function(data, item_stem = "PANAS"){
  "Returns dataframe with scores for 2 subscales according to psychometric model"
  PANAS = prep_PANAS(data, item_stem)
  
  pa_index = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)
  na_index = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)
  pa_PANAS = rowSums(PANAS[, pa_index])
  na_PANAS = rowSums(PANAS[, na_index])
  
  PANAS_scores = data.frame(pa_PANAS, na_PANAS)
  return(PANAS_scores)
}