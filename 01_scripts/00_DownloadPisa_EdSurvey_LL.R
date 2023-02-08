library(EdSurvey)
library(tidyverse)

######################### 
# Test du package EdSurvey
########################

## DONE 
# downloadPISA(
# 
#   root = "data/raw/",
#   years = c(2000,2003,2006),
#   database = "INT",
#   verbose = T
# )

downloadPISA(

  root = "00_raw_data_pisa/",
  years = c(2000, 2003, 2006, 2009,2012,2015,2018),
  database = "INT",
  verbose = T
)
#### Test des fonctions de lecture 