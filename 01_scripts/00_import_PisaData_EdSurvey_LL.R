library(EdSurvey)
library(tidyverse)

########## Test des fonctions de lecture 
### Chaque année est très longue donc on va procéder sans boucle 
# General Database 
# All countries 
int2000 <- readPISA(
  "00_raw_data_pisa/PISA/2000/",
  database="INT",
  countries="*"
)

# On extrait les données: liste avec un EdSurvey.data.frame par pays 
data2000 <- int2000[[1]]

# On extrait le nom des variables 
vars_2000 <- searchSDF(data = data2000[[1]], string = ".")
vars_2000 <- searchSDF(data = data2000[[1]], string = "stuwt")

# On va récolter les données de tous les pays pour chaque année 
# D'abord pour les plausible values: MATH et READ 

####### 2000 
data_pvs_2000 <- map(
  data2000,
  ~getData(.x,
           varnames = c("cnt","country","subnatio",
                        "schoolid","stidstd",
                        "st03q01",# Gender
                        "read",
                        "math",
                        "w_fstuwt_read",
                        "w_fstuwt_math")) %>%
    mutate(cnt=as.character(cnt),
           country=as.character(country),
           st03q01 = as.character(st03q01)) %>%
    rename(gender=st03q01)
)

############### On va écrire la donnée brute 
data_pvs_2000 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2000_PVs.csv")

# write_csv(data_pvs_2000,"00_semi_raw_data_pisa/PISA_2000_PVs.csv")

####### 2003 
int2003 <- readPISA(
  "00_raw_data_pisa/PISA/2003/", 
  database = "INT",
  countries = "*"
)

data2003 <- int2003[[1]]

data_pvs_2003 <- map(
  data2003,
  ~getData(.x,
           varnames = c("cnt","country","schoolid","stidstd",
                        "subnatio",
                        "st03q01",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio=as.character(subnatio),
           country=as.character(country),
           st03q01 = as.character(st03q01)) %>%
    rename(gender=st03q01)

)

data_pvs_2003 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2003_PVs.csv")

####### 2006 
int2006 <- readPISA(
  "00_raw_data_pisa/PISA/2006/", 
  database = "INT",
  countries = "*"
)

data2006 <- int2006[[1]]


vars2006 <- searchSDF(data = data2006[[1]],string = ".")
  
data_pvs_2006 <- map(
  data2006,
  ~getData(.x,
           varnames = c("cnt","country","schoolid","stidstd",
                        "subnatio",
                        "st04q01",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio=as.character(subnatio),
           country=as.character(country),
           st04q01 = as.character(st04q01)) %>%
    rename(gender=st04q01)
  
)

data_pvs_2006 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2006_PVs.csv")


####### 2009
int2009 <- readPISA(
  "00_raw_data_pisa/PISA/2009/", 
  database = "INT",
  countries = "*"
)

data2009 <- int2009[[1]]

vars2009 <- searchSDF(data = data2009[[1]],string = ".")


data_pvs_2009 <- map(
  data2009,
  ~getData(.x,
           varnames = c("cnt","country","schoolid","stidstd",
                        "subnatio",
                        "st04q01",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio=as.character(subnatio),
           country=as.character(country),
           st04q01 = as.character(st04q01)) %>%
    rename(gender=st04q01)
  
)

data_pvs_2009 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2009_PVs.csv")

######### 2012

int2012 <- readPISA(
  "00_raw_data_pisa/PISA/2012/", 
  database = "INT",
  countries = "*"
)

data2012 <- int2012[[1]]

vars2012 <- searchSDF(data = data2012[[1]],string = ".")


data_pvs_2012 <- map(
  data2012,
  ~getData(.x,
           varnames = c("cnt","schoolid","stidstd",
                        "subnatio",
                        "st04q01",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio = as.character(subnatio),
           st04q01 = as.character(st04q01)) %>%
    rename(gender=st04q01)
  
)

data_pvs_2012 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2012_PVs.csv")

####2015
int2015 <- readPISA(
  "00_raw_data_pisa/PISA/2015/", 
  database = "INT",
  countries = "*"
)

data2015 <- int2015[[1]]

vars2015 <- searchSDF(data = data2015[[1]],string = ".")


data_pvs_2015 <- map(
  data2015,
  ~getData(.x,
           varnames = c("cnt",
                        "cntschid",# school id 
                        "cntstuid", # student id
                        "subnatio",
                        "region",#region
                        "st004d01t",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio = as.character(subnatio),
           region = as.character(region),
           st004d01t = as.character(st004d01t)) %>%
    rename(gender=st004d01t)
  
)

data_pvs_2015 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2015_PVs.csv")

####2018
int2018 <- readPISA(
  "00_raw_data_pisa/PISA/2018/", 
  database = "INT",
  countries = "*"
)

data2018 <- int2018[[1]]

vars2018 <- searchSDF(data = data2018[[1]],string = ".")


data_pvs_2018 <- map(
  data2018,
  ~getData(.x,
           varnames = c("cnt",
                        "cntschid",# school id 
                        "cntstuid", # student id
                        "subnatio",
                        "region",#region
                        "st004d01t",# Gender
                        "read",
                        "math",
                        "w_fstuwt")) %>%
    mutate(cnt=as.character(cnt),
           subnatio = as.character(subnatio),
           region = as.character(region),
           st004d01t = as.character(st004d01t)) %>%
    rename(gender=st004d01t)
  
)

data_pvs_2018 %>% bind_rows() %>% write_csv("00_semi_raw_data_pisa/PISA_2018_PVs.csv")

