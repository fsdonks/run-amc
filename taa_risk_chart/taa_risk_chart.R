library("dplyr")
library("ggplot2")
library("tidyr")
library("janitor")

data <- read.csv("results_archive.csv", stringsAsFactors=FALSE) %>%
  mutate(fill=AC.fill+RC.fill, demand=total.quantity) %>%
  select(rep.seed, SRC, AC, RC, phase, fill, demand) %>%
  group_by(SRC, AC, RC, phase) %>%
  summarize(fill=sum(fill), demand=sum(demand)) %>%
  mutate(fill_rate=fill/demand) %>% 
  select(SRC, AC, RC, phase, fill_rate) %>%
  pivot_wider(names_from=phase, values_from=fill_rate) %>%
  clean_names() %>%
  mutate(score=0.25*comp+0.75*phase_1) %>%
  mutate("risk_comp"=case_when(comp>=0.90 ~ 4,
                               comp>=0.80 ~ 3,
                               comp>=0.70 ~ 2,
                               comp>=0 ~ 1),
         "risk_conflict"=case_when(phase_1>=0.90 ~ 4,
                                   phase_1>=0.80 ~ 3,
                                   phase_1>=0.70 ~ 2,
                                   phase_1>=0 ~ 1),
         "risk"=case_when(score>=0.90 ~ 4,
                          score>=0.80 ~ 3,
                          score>=0.70 ~ 2,
                          score>=0 ~ 1))

for(s in unique(data$src)){
  src_data <- filter(data, src==s)
  
  if(length(unique(src_data$risk))==4){
    g <- ggplot(src_data, aes(ac, rc, fill=risk)) + 
          geom_tile(color="black") +
          scale_fill_gradientn(colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91"),
                               labels=c("extreme", "major", "modest", "minor")) +
          theme_bw() +
          theme(text=element_text(size=20),
                axis.text=element_text(size=20),
                legend.text=element_text(size=20))
    print(g)
  } else {
    print(src)
  }
}

ggplot(filter(data, src=="77202K000"), aes(ac, rc, fill=risk)) + 
  geom_tile(color="black") +
  scale_fill_gradientn(colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91"),
                       labels=c("extreme", "major", "modest", "minor")) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20))
