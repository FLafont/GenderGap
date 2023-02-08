library(tidyr)
library(Hmisc)

#very useful website: https://www.edmeasurementsurveys.com/using-plausible-values-to-analyse-pisa-data.html

#1. dataset 2000 ######

test <- read.csv("00_semi_raw_data_pisa/PISA_2000_PVs.csv") %>%
  select(cnt:gender, contains("math")) %>%
  mutate(across(pv1math:w_fstuwt_math, ~as.numeric(.))) %>%
  filter(gender %in% c("Male", "Female")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:pv5math, w_fstr_math1:w_fstuwt_math, gender, country)%>% group_by(gender, country) %>%
  pivot_longer(c(w_fstr_math1:w_fstuwt_math), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, country) %>%
  summarise(across(pv1math:pv5math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt_math

x <- test_2 %>% filter(replicate == "w_fstuwt_math") %>%  
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(country, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s <- test_2 %>% left_join(x) %>%
  group_by(country, gender) %>%
  summarise(across(pv1math:pv5math, ~0.05*(sum((. - .[replicate == "w_fstuwt_math"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(gender, country)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/5)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2000")


  #dataset 2003#########
test <- read.csv("00_semi_raw_data_pisa/PISA_2003_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("Male", "Female")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, country)%>% group_by(gender, country) %>%
  pivot_longer(c(w_fstr1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, country) %>%
  summarise(across(pv1math:pv5math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(country, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2003 <- test_2 %>% left_join(x) %>%
  group_by(country, gender) %>%
  summarise(across(pv1math:pv5math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(gender, country)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/5)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2003") %>% ungroup() %>%rbind(v_s)


#dataset 2006#########
test <- read.csv("00_semi_raw_data_pisa/PISA_2006_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("Male", "Female")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, country)%>% group_by(gender, country) %>%
  pivot_longer(c(w_fstr1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, country) %>%
  summarise(across(pv1math:pv5math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(country, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2006 <- test_2 %>% left_join(x) %>%
  group_by(country, gender) %>%
  summarise(across(pv1math:pv5math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(gender, country)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/5)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2006") %>% ungroup() %>%rbind(v_s_2003)


#dataset 2009#########
test <- read.csv("00_semi_raw_data_pisa/PISA_2009_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("Male", "Female")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, country)%>% group_by(gender, country) %>%
  pivot_longer(c(w_fstr1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, country) %>%
  summarise(across(pv1math:pv5math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(country, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2009 <- test_2 %>% left_join(x) %>%
  group_by(country, gender) %>%
  summarise(across(pv1math:pv5math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(gender, country)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/5)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2009") %>% ungroup() %>%rbind(v_s_2006)

#dataset 2012#########
test <- read.csv("00_semi_raw_data_pisa/PISA_2012_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("Male", "Female")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, cnt)%>% group_by(gender, cnt) %>%
  pivot_longer(c(w_fstr1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, cnt) %>%
  summarise(across(pv1math:pv5math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(cnt, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2012 <- test_2 %>% left_join(x) %>%
  group_by(cnt, gender) %>%
  summarise(across(pv1math:pv5math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv5math), names_to = "pv", values_to = "values") %>%
  group_by(gender, cnt)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/5)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2012", country = cnt) %>% ungroup() %>%
  select(-cnt) %>%
  rbind(v_s_2009)

#dataset 2015#########
test <- read.csv("00_semi_raw_data_pisa/PISA_2015_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("MALE", "FEMALE")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, cnt)%>% group_by(gender, cnt) %>%
  pivot_longer(c(w_fsturwt1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, cnt) %>%
  summarise(across(pv1math:pv10math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv10math), names_to = "pv", values_to = "values") %>%
  group_by(cnt, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2015 <- test_2 %>% left_join(x) %>%
  group_by(cnt, gender) %>%
  summarise(across(pv1math:pv10math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv10math), names_to = "pv", values_to = "values") %>%
  group_by(gender, cnt)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/10)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2015", country = cnt) %>% ungroup() %>%
  select(-cnt) %>%
  rbind(v_s_2012)


#dataset 2018#####
test <- read.csv("00_semi_raw_data_pisa/PISA_2018_PVs.csv") %>%
  select(cnt:gender, pv1math:w_fstuwt) %>%
  mutate(across(pv1math:w_fstuwt, ~as.numeric(.))) %>%
  filter(gender %in% c("MALE", "FEMALE")) #sorry pour l'attitude binocentrée

#on calcule la moyenne en math pour chaque PV, pondérée par les sampling weights w_fstuwt_math et les replicates fstr_math1:80

test_2 <- test %>% 
  select(pv1math:w_fstuwt, gender, cnt)%>% group_by(gender, cnt) %>%
  pivot_longer(c(w_fsturwt1:w_fstuwt), names_to = "replicate", values_to = "values") %>% 
  group_by(replicate, gender, cnt) %>%
  summarise(across(pv1math:pv10math, ~weighted.mean(., values))) 

#calcule de moyenne et variance uniquement avec les sampling weights w_fstuwt

x <- test_2 %>% filter(replicate == "w_fstuwt") %>%  
  pivot_longer(c(pv1math:pv10math), names_to = "pv", values_to = "values") %>%
  group_by(cnt, gender) %>%
  summarise(mean_math = mean(values), v_m = var(values)) 

#calcul sampling variance et moyenne des sampling variances de tous les PV selon la formule σ2T=120R∑r=1(Tr−T)2

v_s_2018 <- test_2 %>% left_join(x) %>%
  group_by(cnt, gender) %>%
  summarise(across(pv1math:pv10math, ~0.05*(sum((. - .[replicate == "w_fstuwt"])^2))), v_m = v_m, 
            mean_math = mean_math)%>% ungroup() %>%
  pivot_longer(c(pv1math:pv10math), names_to = "pv", values_to = "values") %>%
  group_by(gender, cnt)%>%
  mutate(v_s = mean(values), 
         se = sqrt(v_s+(1+1/10)*v_m)) %>%
  select(-pv, -values) %>% unique() %>% mutate(year = "2018", country = cnt) %>% ungroup() %>%
  select(-cnt) %>%
  rbind(v_s_2015)

write.csv(v_s_2018, "02_output/mean_se_gender_2000_2018.csv")


