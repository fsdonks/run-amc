library("dplyr")
library("ggplot2")
library("tidyr")
library(writexl)
library(readxl)
library(purrr)
#also depends on util.R

compute_score <- function(weights, df){
  df["score"]<-0
  for(w in names(weights)){
    df["score"]<-df["score"]+df[w]*weights[w]
  }
  df
}
#This function computes the scores and risks for each SRC's AC-RC permutation 
functor <- 
  function(input, weights){
    compute_score_partial <- partial(compute_score, weights)
    input %>%
      rename(RA=AC) %>%
      mutate(fill=AC.fill+RC.fill, demand=total.quantity) %>%
      mutate(fill=ifelse(RA+RC==0, 0, fill), demand=ifelse(RA+RC==0, 1, demand)) %>%
      select(rep.seed, SRC, RA, RC, phase, fill, demand) %>%
      group_by(SRC, RA, RC, phase) %>%
      summarize(fill=sum(fill), demand=sum(demand)) %>%
      mutate(fill_rate=fill/demand) %>% 
      mutate(fill_rate=ifelse(is.na(fill_rate), 1, fill_rate)) %>%
      select(SRC, RA, RC, phase, fill_rate) %>%
      pivot_wider(names_from=phase, values_from=fill_rate) %>%
      #mutate(score=0.25*comp+0.75*phase1) 
      compute_score_partial %>%
      mutate("Risk"=case_when(score>=0.95 ~ "none",
                              score>=0.90 ~ "minor",
                              score>=0.80 ~ "modest",
                              score>=0.70 ~ "major",
                              score>=0 ~ "extreme"))}
    
make_taa_charts <- function(out_location, inputfiles, weights, supply_demand,
                            title_start, subtitle, caption_start){
  
  all_files <- lapply(inputfiles, read.csv, sep="\t")
  
  i <- as.numeric(1)
  n <- as.numeric(length(all_files))
  
  #cleanup phase cols
  while(i <= n) {
    
    all_files[[i]]$phase <- gsub("-", "", all_files[[i]]$phase)
    i <- i+1
    
  }

  #Loop through list of dataframes, applying function to each item
  i <- as.numeric(1)
  
  while(i <= n) {
    all_files[[i]] <- functor(all_files[[i]], weights)
    i <- i+1
  }
  
  #Reduce list of dataframes to single object
  data <- 
    bind_rows(all_files, .id = "column_label")
  
  #Determine the min risk per SRC-AC-RC group; save in score_min
  data <-
    data %>%
    group_by(SRC, RA, RC) %>%
    #Ensure that only the min scores are used for plotting, should only have one record.
    slice_min(order_by = score) %>%
    ungroup() %>%
    add_base_supply(supply_demand)

  # #write dataframe to Excel file
  # write_xlsx(data, "test_data.xlsx")
  
  write.table(data, paste(out_location, "taa-risk_output.txt"), sep="\t", row.names=FALSE)
  
  #spit the risk charts
  #util.R
  risk_charts(data, paste(out_location, "taa_", sep=''), title_start, subtitle, caption_start)
  
  #continuous color scale risk chart, but John doesn't like this as much as discrete colors
  a<-ggplot(filter(data, SRC=="87312K000"), aes(RA, RC, fill=score)) + 
    geom_tile(color="black") +
    scale_fill_gradient(low = "red",
                        high = "green")+
    
    theme_bw() +
    theme(text=element_text(size=20),
          axis.text=element_text(size=20),
          legend.text=element_text(size=20))
  print(a)
}