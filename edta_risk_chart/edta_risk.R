library("dplyr")
library("ggplot2")
library(purrr)
library(readxl)
library(writexl)
#also depends on util.R

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

make_edta_charts <- function(supply_path, demand_path, out_location, supply_demand){
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
    group_by(SRC, RA, RC) %>%
    summarize(fill=sum(fill), Demand=sum(Demand), fill_rate=fill/Demand) %>%
    #covers the case where we have no demand<=day T3
    mutate(fill_rate=ifelse(is.na(fill_rate), 1, fill_rate)) %>%
    mutate("Risk"=case_when(fill_rate>=0.95 ~ 5,
                            fill_rate>=0.90 ~ 4,
                            fill_rate>=0.80 ~ 3,
                            fill_rate>=0.70 ~ 2,
                            fill_rate>=0 ~ 1)) %>%
    add_base_supply(supply_demand)
  
  #writes the dataframe to an Excel file
  write_xlsx(data_fill, paste(out_location, "edta_output.xlsx", sep = ''))
  
  #spit the risk charts
  #from util.R
  risk_charts(data_fill, paste(out_location, "edta_", sep=''))
  
}
