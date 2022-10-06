library("dplyr")
library("ggplot2")

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

data_initial <- data.frame("SRC"="A", "RA"=6, "RC"=12, "RC_Available"=0.5, "T2"=30, "T3"=60)

supply <- data_initial %>% 
  ac_rc() %>%
  mutate("T0A"=RA*1.0, "T2A"=RA+RC*RC_Available*0.2, "T3A"=RA+RC*RC_Available)

demand <- read.csv("./workspace/run-amc/edta_risk_chart/early_demand.csv", stringsAsFactors=FALSE)

data_fill <- left_join(supply, demand, by="SRC") %>%
  mutate("available"=case_when(Day<T2 ~ T0A, Day<T3 ~ T2A, Day>=T3 ~ T3A)) %>%
  mutate("fill"=ifelse(available>Demand, Demand, available)) %>%
  filter(Day<=T3) %>%
  group_by(SRC, RA, RC) %>%
  summarize(fill=sum(fill), Demand=sum(Demand), fill_rate=fill/Demand) %>%
  mutate("Risk"=case_when(fill_rate>=0.90 ~ 4,
                          fill_rate>=0.80 ~ 3,
                          fill_rate>=0.70 ~ 2,
                          fill_rate>=0 ~ 1))


ggplot(data_fill, aes(RA, RC, fill=Risk)) + 
  geom_tile(color="black") +
  scale_fill_gradientn(colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91"),
                       labels=c("extreme", "major", "modest", "minor")) +
  scale_x_continuous(breaks=seq(0, 6, by=1)) +
  scale_y_continuous(breaks=seq(0, 12, by=1)) +
  theme_bw() +
  theme(text=element_text(size=20),
        axis.text=element_text(size=20),
        legend.text=element_text(size=20))                       
                       