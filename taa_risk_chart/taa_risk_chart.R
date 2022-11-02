library("dplyr")
library("ggplot2")
library("tidyr")
library(writexl)
library(readxl)

make_taa_charts <- function(out_location, inputfiles){
  
  #Prep input files for separation; capture filenames
  names <- substr(inputfiles, 9, 13)
  all_files <- lapply(inputfiles, read.csv)
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
  
  #This function computes the scores and risks for each SRC's AC-RC permutation 
  functor <- 
    function(input){
      
    input %>%
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
    mutate("Risk"=case_when(score>=0.95 ~ 5,
                            score>=0.90 ~ 4,
                            score>=0.80 ~ 3,
                            score>=0.70 ~ 2,
                            score>=0 ~ 1))
  }
  
  #Loop through list of dataframes, applying function to each item
  i <- as.numeric(1)
  
  while(i <= n) {
    all_files[[i]] <- functor(all_files[[i]])
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
    ungroup()
  
  #Ensure that only the min values are used for plotting
  data <- 
    subset(data, Risk == risk_min)
  
  # #write dataframe to Excel file
  # write_xlsx(data, "test_data.xlsx")
  
  write.table(data, paste(out_location, "taa-risk_output.txt"), sep="\t", row.names=FALSE)
  
  risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#009E73")
  risk_labels=c("extreme", "major", "modest", "minor", "none")
  
  for(src in unique(data$SRC)){
    src_data <- filter(data, SRC==src)
    risks = sort(unique(src_data$Risk))
    #Did this so that something works but
    #todo: 
    #should have same legend for all charts (can I add a manual legend?)
    #should get rid of color gradient for discrete color legend
    colors_subset=risk_colors[risks]
    labels_subset=risk_labels[risks]
    g <- ggplot(src_data, aes(RA, RC, fill=Risk)) + 
      geom_tile(color="black", position = position_nudge(x = 0.5, y = 0.5)) +
      #ggtitle(src) +
      scale_fill_gradientn(colors=colors_subset,
                           labels=labels_subset,
                           breaks=risks) +
      theme_bw() +
      theme(text=element_text(size=20),
            axis.text=element_text(size=20),
            legend.text=element_text(size=20),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(g)
    ggsave(paste(out_location, "taa_", src, ".png", sep=""))
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
}