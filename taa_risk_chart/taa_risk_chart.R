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
  mutate("risk"=case_when(score==1 ~ 5,
                          score>=0.90 ~ 4,
                          score>=0.80 ~ 3,
                          score>=0.70 ~ 2,
                          score>=0 ~ 1))

risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#005000")
risk_labels=c("extreme", "major", "modest", "minor", "none")

for(src in unique(data$SRC)){
  src_data <- filter(data, SRC==src)
  risks = sort(unique(src_data$risk))
  #Did this so that something works but
  #todo: 
  #should have same legend for all charts (can I add a manual legend?)
  #should get rid of color gradient for discrete color legend
  colors_subset=risk_colors[risks]
  labels_subset=risk_labels[risks]
  g <- ggplot(src_data, aes(RA, RC, fill=risk)) + 
        geom_tile(color="black") +
        ggtitle(src) +
        scale_fill_gradientn(colors=colors_subset,
                             labels=labels_subset,
                             breaks=risks) +
        theme_bw() +
        theme(text=element_text(size=20),
              axis.text=element_text(size=20),
              legend.text=element_text(size=20))
    print(g)
}

#continuous color scale, but John doesn't like this as much as discrete colors
a<-ggplot(filter(data, SRC=="87312K000"), aes(RA, RC, fill=score)) + 
  geom_tile(color="black") +
  scale_fill_gradient(low = "red",
                        high = "green")+

  theme_bw() +
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20))
print(a)
