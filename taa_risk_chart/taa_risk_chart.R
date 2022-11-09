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
      mutate("Risk"=case_when(score>=0.95 ~ 5,
                              score>=0.90 ~ 4,
                              score>=0.80 ~ 3,
                              score>=0.70 ~ 2,
                              score>=0 ~ 1))}
    
    
make_taa_charts <- function(out_location, inputfiles, weights, supply_demand){
  
  #Prep input files for separation; capture filenames
  names <- substr(inputfiles, 9, 13)
  all_files <- lapply(inputfiles, read.csv, sep="\t")
  names(all_files) <- substr(inputfiles, 9, 13)
  
  i <- as.numeric(1)
  n <- as.numeric(length(names))
  
  #Create risk columns; cleanup phase cols
  while(i <= n) {
    
    Risk <- rep(i, nrow(all_files[[i]]))
    all_files[[i]][ , ncol(all_files[[i]]) + 1] <- Risk
    colnames(all_files[[i]])[ncol(all_files[[i]])] <- paste0("Risk")
    
    all_files[[i]]$phase <- gsub("-", "", all_files[[i]]$phase)
    
    i <- i+1
    
  }
  rm(Risk)
  
  #This function can split the list of dataframes, if needed
  #list2env(all_files, envir=.GlobalEnv)
  
  #Loop through list of dataframes, applying function to each item
  i <- as.numeric(1)
  
  while(i <= n) {
    all_files[[i]] <- functor(all_files[[i]], weights)
    i <- i+1
  }
  
  #Reduce list of dataframes to single object
  data <- 
    bind_rows(all_files, .id = "column_label")
  
  data$risk_min <- NA
  
  #Determine the min risk per SRC-AC-RC group; save in risk_min
  data <-
    data %>%
    group_by(SRC, RA, RC) %>%
    mutate(risk_min = min(Risk)) %>%
    ungroup() %>%
    #Ensure that only the min values are used for plotting
    subset(Risk == risk_min) %>%
    add_base_supply(supply_demand)

  # #write dataframe to Excel file
  # write_xlsx(data, "test_data.xlsx")
  
  write.table(data, paste(out_location, "taa-risk_output.txt"), sep="\t", row.names=FALSE)
  
  #spit the risk charts
  #util.R
  risk_charts(data, paste(out_location, "taa_", sep=''))
  
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