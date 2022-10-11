library("dplyr")
library("ggplot2")
library("tidyr")

input <- read.csv("./workspace/run-amc/taa_risk_chart/results_archive.csv", stringsAsFactors=FALSE)
input$phase <- gsub("-", "", input$phase)

data <- input %>%
  rename(RA=AC) %>%
  mutate(fill=AC.fill+RC.fill, demand=total.quantity) %>%
  mutate(fill=ifelse(RA+RC==0, 0, fill), demand=ifelse(RA+RC==0, 1, demand)) %>%
  select(rep.seed, SRC, RA, RC, phase, fill, demand) %>%
  group_by(SRC, RA, RC, phase) %>%
  summarize(fill=sum(fill), demand=sum(demand)) %>%
  mutate(fill_rate=fill/demand) %>% 
  select(SRC, RA, RC, phase, fill_rate) %>%
  pivot_wider(names_from=phase, values_from=fill_rate) %>%
  mutate(score=0.25*comp+0.75*phase1) %>%
  mutate("risk"=case_when(score>=0.90 ~ 4,
                          score>=0.80 ~ 3,
                          score>=0.70 ~ 2,
                          score>=0 ~ 1))

for(src in unique(data$SRC)){
  src_data <- filter(data, SRC==src)
  
  if(length(unique(src_data$risk))==4){
    g <- ggplot(src_data, aes(RA, RC, fill=risk)) + 
          geom_tile(color="black") +
          ggtitle(src) +
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

#experimental
src_data <- filter(data, SRC=="44693K000")
print(length(unique(src_data$risk)))

a<-ggplot(filter(data, src=="44693K000"), aes(RA, RC, fill=risk)) + 
  geom_tile(color="black") +
  scale_fill_gradientn(colors=c("#F05454", "#FFC990"),
                       labels=c("extreme", "major"),
                       breaks=c(1, 2)) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20))
print(a)
