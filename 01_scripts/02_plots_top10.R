library(tidyverse)

top10files <- str_subset(list.files("00_semi_raw_data_pisa/", full.names = T),"TOP10",)

res_final <- map(top10files,~read_csv(.x))

check <- res_final[[7]] %>% 
  group_by(cnt)%>%
  mutate(share_top10 = nb_gender/sum(nb_gender),
         top_10_average = weighted.mean(mean_pv_math,nb_gender)) 

check_male <- check %>% filter(gender == "MALE")

data <- data.frame(
  x=check_male$cnt, 
  y=abs((check_male$share_top10))
)

# Change baseline
ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0.7, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")


check %>%
  filter(gender=="MALE")%>%
  ggplot(aes(x=share_top10,
             y=top_10_average))+
  geom_point(shape=10)+
  #geom_label(aes(label = cnt))+
  geom_smooth(method = "lm")+
  theme_bw()
#xlim(0.25,1)+
#ylim(300,700)+
#annotate("segment", x=-Inf, xend=Inf,y=-Inf, yend=Inf)