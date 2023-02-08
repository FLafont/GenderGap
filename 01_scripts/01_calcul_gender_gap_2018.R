library(haven)
library(tidyverse)
library(readxl)
library(writexl)
###### READ DATA FROM PISA 

# Test avec les données des students 
# On garde que les PV, le genre, la classe et le pays 
# Tou.Tes es élèves interrogé.es ont entre 15 ans 3 mois et 16 ans


dat_student <- read_sas(data_file = "data/raw/PISA_2018/SAS_STU_QQQ/STU/cy07_msu_stu_qqq.sas7bdat",
                        #n_max = 1e5, 
                        col_select = contains(c("ST001",#grade 
                                                "ST002",#program
                                                "ST003", #AGE: DATE OF BIRTH 
                                                "ST004D01T",# GENDER FEMALE = 1, MALE = 2 
                                                "PV",# PLAUSIBLE VALUES OF GRADES
                                                "CNT",
                                                "CNTRYID",
                                                "CNTSCHID",#school ID 
                                                "CNTSTUID",#student ID 
                                                "OECD")))

dat_weights <- read_sas(data_file = "data/raw/PISA_2018/SAS_STU_QQQ/STU/cy07_msu_stu_qqq.sas7bdat",
                        col_select = contains(c("CNT","WT", "ST004D01T","CNTSCHID","CNTSTUID"))
)

## GET LABELS
# 
# labels_weights <- tibble(
#   col_name = dat_weights %>% names(),
#   labels = dat_weights %>% map_chr(attr_getter("label"))
# )

## GET PLAUSIBLE VALUES FOR MATH READING AND SCIENCE TO START-OFF AND GENDER 
## SELECT MATH related topics 
df_math <- dat_student %>%
  select(gender = ST004D01T,
         CNT,
         CNTSCHID,
         CNTSTUID,
         OECD,
         contains("MATH"))%>%
  inner_join(dat_weights) %>%
  rename( STUID=CNTSTUID)

labels_df <- tibble(
  col_name = df_math %>% names(),
  labels = df_math %>% map_chr(attr_getter("label"))
)

### COMPUTE MATH SCORES 
res_math <- df_math %>% 
  pivot_longer(c(contains("MATH")),
               names_to="SUBJECT",
               values_to ="SCORE")%>%
  group_by(STUID,W_FSTUWT,gender,CNT,OECD)%>%
  summarise(MEAN_MATH = weighted.mean(SCORE ,na.rm=T)) %>%
  mutate(gender=as.character(gender))

# Gender gap: 
gender_gap <- res_math %>%
  filter(!is.na(gender))%>%
  group_by(CNT,gender,OECD) %>%
  summarise(NAT_MATH = weighted.mean(MEAN_MATH,W_FSTUWT, na.rm=T)) %>%
  mutate(gender = ifelse(gender=="1","FEMALE","MALE"))%>%
  pivot_wider(names_from = gender,values_from = NAT_MATH)%>%
  mutate(GGAP = MALE-FEMALE,
         GGAP_QUAL= case_when(
           GGAP<0~"GIRL ADVANTAGE",
           GGAP ==0~"EQUALITY",
           GGAP>0~"MALE ADVANTAGE")) %>%
  mutate(OECD = ifelse(as.character(OECD) =="1","OECD","NON-OECD"))


#### Add continent 
continent <- read_csv("data/raw/country_continent.csv")


gender_gap <- left_join(gender_gap,continent)
# continent <- read_xlsx("data/raw/country_continent.xlsx") %>%
#   select(CNT=`3letter_code`,Country,Continent)
# 

write_csv(gender_gap,"data/processed/math_weighted_gender_gap_2018.csv") # Handmodif of strange OECD regions (MoscowRegion, Beijin Shanghai etc.)

#### VIZ GAP ###################################
gender_gap<- read_csv("data/processed/math_weighted_gender_gap_2018.csv") %>%
  mutate(GGAP = as.numeric(GGAP))

gender_gap %>%
  filter(!is.na(GGAP))%>%
  arrange(OECD)%>%
  ggplot(aes(x=CNT,y=GGAP,color=GGAP_QUAL))+
  geom_point()+
  geom_segment(aes(x=CNT,xend=CNT,y=0,yend=GGAP))+
  coord_flip()+
  facet_wrap(vars(OECD),scales="free")+
  labs(x="",y="Math Gender Gap",color="")+
  theme_light()

ggsave("math_weighted_gender_gap_2018.png")

