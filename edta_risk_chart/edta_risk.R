library("dplyr")
library("ggplot2")
library(purrr)
library(readxl)
library(writexl)

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

make_edta_charts <- function(supply_path, demand_path){
  data_initial <-
    read_excel(supply_path, trim_ws = TRUE, col_names = TRUE)
  
  split_data <- 
    split(data_initial, f = sort(as.numeric(rownames(data_initial))))
  
  data <- 
    map_dfr(split_data, ac_rc)
  
  supply <- 
    data %>% 
    mutate("T0A"=RA*1.0, "T2A"=ifelse(0.2>RC_Available, round(RA+RC*RC_Available), round(RA+RC*0.2)), "T3A"=RA+RC*RC_Available)
  
  demand <- read.csv(demand_path, stringsAsFactors=FALSE)
  
  #computes the score and risk for the SRCs
  data_fill <- left_join(supply, demand, by="SRC") %>%
    mutate("available"=case_when(Day<=T2 ~ T0A, Day<=T3 ~ T2A, Day>=T3 ~ T3A)) %>%
    mutate("fill"=ifelse(available>Demand, Demand, available))%>%
    filter(Day<=T3) %>%
    group_by(SRC, RA, RC, Day) %>%
    summarize(fill=sum(fill), Demand=sum(Demand), fill_rate=fill/Demand) %>%
    mutate("Risk"=case_when(fill_rate>=0.999 ~ 5,
                            fill_rate>=0.90 ~ 4,
                            fill_rate>=0.80 ~ 3,
                            fill_rate>=0.70 ~ 2,
                            fill_rate>=0 ~ 1))
  
  # #writes the dataframe to an Excel file
  write_xlsx(data_fill, paste(out_location, "data_fill_by_day.xlsx"))
  
  #write.table(data_fill, paste(out_location, "edta_output.txt"), sep="\t", row.names=FALSE)
  
  risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#009E73")
  risk_labels=c("extreme", "major", "modest", "minor", "none")
  
  for(src in unique(data_fill$SRC)){
    src_data <- filter(data_fill, SRC==src)
    risks = sort(unique(src_data$Risk))
    colors_subset=risk_colors[risks]
    labels_subset=risk_labels[risks]
    
    g <- ggplot(src_data, aes(RA, RC, fill=Risk)) + 
      geom_tile(color="black", position = position_nudge(x = 0.5, y = 0.5)) +
      scale_fill_gradientn(colors=colors_subset,
                           labels=labels_subset, breaks=risks) +
      #ggtitle(src) +
      theme_bw() +
      theme(text=element_text(size=20),
            axis.text=element_text(size=20),
            legend.text=element_text(size=20),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(g)
    ggsave(paste(out_location, "edta_", src, ".png", sep=""))
  }
}
