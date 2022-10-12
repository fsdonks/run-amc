library("dplyr")
library("ggplot2")
library(purrr)

out_location="./workspace/run-amc/outputs/"

ac_rc <- function(data_initial){
  data <- data_initial[-1, ]
  
  for(ra in 0:data_initial$RA){
    data_new <- data_initial
    for(rc in 0:data_initial$RC){
      data_new$RA <- ra
      data_new$RC <- rc
      if(ra+rc>=0){
        data <- rbind(data, data_new)
      }  
    }  
  }
  
  data
}

data_initial <- list(
  data.frame("SRC"="A", "RA"=6, "RC"=12, "RC_Available"=0.5, "T2"=30, "T3"=60),
  data.frame("SRC"="B", "RA"=6, "RC"=8, "RC_Available"=0, "T2"=30, "T3"=60),
  data.frame("SRC"="C", "RA"=14, "RC"=30, "RC_Available"=0.45, "T2"=30, "T3"=60))
  
data <- map_dfr(data_initial, ac_rc)

supply <- data %>% 
  mutate("T0A"=RA*1.0, "T2A"=ifelse(0.2>RC_Available, RA+RC*RC_Available, RA+RC*0.2), "T3A"=RA+RC*RC_Available)

demand <- read.csv("./workspace/run-amc/edta_risk_chart/early_demand.csv", stringsAsFactors=FALSE)

data_fill <- left_join(supply, demand, by="SRC") %>%
  mutate("available"=case_when(Day<T2 ~ T0A, Day<T3 ~ T2A, Day>=T3 ~ T3A)) %>%
  mutate("fill"=ifelse(available>Demand, Demand, available)) %>%
  filter(Day<=T3) %>%
  group_by(SRC, RA, RC) %>%
  summarize(fill=sum(fill), Demand=sum(Demand), fill_rate=fill/Demand) %>%
  mutate("Risk"=case_when(fill_rate==1 ~ 5,
                          fill_rate>=0.90 ~ 4,
                          fill_rate>=0.80 ~ 3,
                          fill_rate>=0.70 ~ 2,
                          fill_rate>=0 ~ 1))

write.table(data_fill, paste(out_location, "edta_data.txt"), sep="\t", row.names=FALSE)

risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#005000")
risk_labels=c("extreme", "major", "modest", "minor", "none")

for(src in unique(data_fill$SRC)){
  src_data <- filter(data_fill, SRC==src)
  
  g <- ggplot(src_data, aes(RA, RC, fill=Risk)) + 
  geom_tile(color="black") +
  scale_fill_gradientn(colors=risk_colors,
                       labels=risk_labels) +
  ggtitle(src) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20))
  print(g)
  ggsave(paste(out_location, "edta_", src, ".png", sep=""))
}
                       