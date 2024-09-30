library(ggplot2)
library(tidyverse)
library(readr)
library(stringr)
library(data.table)
library(readxl)

post_process<-function(results1_name, results2_name, src_str_path,
                       out1_name, out2_name, root, phase3_length_1,
                       phase3_length_2, comp1_length) {
  out1<-paste(root, out1_name, sep='')
  out2<-paste(root, out2_name, sep='')
  results2<-paste(root, results2_name, sep='')
  results1<-paste(root, results1_name, sep='')
  out_max<-paste(root, "fatShaveDataMax.txt", sep='')
  out_comp<-paste(root, "fatShaveData_Comp.txt", sep='')
  barchart<-paste(root, "barChartData.txt", sep='')
  results_data2 <- read_tsv(results2, col_names=TRUE)
  results_data1 <-read_tsv(results1, col_names=TRUE)
  
  unitDetails <- read_excel(src_str, sheet=1)
  unitDetails <- unitDetails[!duplicated(unitDetails$SRC), ]
  #The branch should be in unitDetails...
  #SRCbranch <- read.csv("Branch.csv", stringsAsFactors = FALSE)
  
  results_data1 <- results_data1 %>%
    rename("ACfill" = 'AC-fill') %>%
    rename("NGfill" = 'NG-fill') %>%
    rename("RCfill" = 'RC-fill') %>%
    rename("ACoverlap" = 'AC-overlap') %>%
    rename("NGoverlap" = 'NG-overlap') %>%
    rename("RCoverlap" = 'RC-overlap') %>%
    rename("Demand" = 'total-quantity') %>%
    rename("ACTotal" = 'AC-total') %>%
    rename("NGTotal" = 'NG-total') %>%
    rename("RCTotal" = 'RC-total') %>%
    rename("ACnotready" = 'AC-not-ready') %>%
    rename("NGnotready" = 'NG-not-ready') %>%
    rename("RCnotready" = 'RC-not-ready') %>%
    rename("ACdeployable" = 'AC-deployable') %>%
    rename("NGdeployable" = 'NG-deployable') %>%
    rename("RCdeployable" = 'RC-deployable')
  
  results_data2 <- results_data2 %>%
    rename("ACfill" = 'AC-fill') %>%
    rename("NGfill" = 'NG-fill') %>%
    rename("RCfill" = 'RC-fill') %>%
    rename("ACoverlap" = 'AC-overlap') %>%
    rename("NGoverlap" = 'NG-overlap') %>%
    rename("RCoverlap" = 'RC-overlap') %>%
    rename("Demand" = 'total-quantity') %>%
    rename("ACTotal" = 'AC-total') %>%
    rename("NGTotal" = 'NG-total') %>%
    rename("RCTotal" = 'RC-total') %>%
    rename("ACnotready" = 'AC-not-ready') %>%
    rename("NGnotready" = 'NG-not-ready') %>%
    rename("RCnotready" = 'RC-not-ready') %>%
    rename("ACdeployable" = 'AC-deployable') %>%
    rename("NGdeployable" = 'NG-deployable') %>%
    rename("RCdeployable" = 'RC-deployable')
  
  
  results_data1 <-results_data1 %>%
    mutate("RAFill" = (ACfill)) %>%
    mutate("RCFill" = (NGfill + RCfill)) %>%
    mutate("RAExcess" = ACdeployable) %>%
    mutate("RCExcess" = (NGdeployable + RCdeployable))
  
  results_data2 <-results_data2 %>%
    mutate("RAFill" = (ACfill)) %>%
    mutate("RCFill" = (NGfill + RCfill)) %>%
    mutate("RAExcess" = ACdeployable) %>%
    mutate("RCExcess" = (NGdeployable + RCdeployable))
  
  
  
  
  
  ##average_fill <- results_data %>%
    ##group_by(SRC, AC, phase) %>%
    ##summarise(RAdmet = mean(RAFill)+ mean(RAExcess))
  
  
  
  
  phase3_fill_1 <- results_data1 %>%
    subset(phase=="phase3") %>%
    group_by(SRC, AC, NG, RC, phase) %>%
    #summarise(Demand = mean(Demand)/48, RAdmet = mean(RAFill) + mean(RAExcess), RCdmet = mean(RCFill) + mean(RCExcess))
    summarise(Demand =mean(Demand)/phase3_length_1, SupplyRA = (mean(RAFill) + mean(RAExcess))/phase3_length_1,
              SupplyRC = (mean(RCFill)+mean(RCExcess))/phase3_length_1, RCTotal = mean(RCTotal+NGTotal)/phase3_length_1)
  
  #shave chart data = (average daily supply ra + average daily supply rc) / average daily demand 
  #reduces to average supply ra + average supply rc / average demand reduces to 
  #sum(supply ra) + sum(supply rc) / sum(demand)
  #bar chart data mean((RCFill+RCExcess)/Demand)
  phase3_fill_2 <- results_data2 %>%
    subset(phase=="phase3") %>%
    group_by(SRC, AC, NG, RC, phase) %>%
    #summarise(Demand = mean(Demand)/48, RAdmet = mean(RAFill) + mean(RAExcess), RCdmet = mean(RCFill) + mean(RCExcess))
    summarise(Demand =mean(Demand)/phase3_length_2, SupplyRA = (mean(RAFill) + mean(RAExcess))/phase3_length_2,
              SupplyRC = (mean(RCFill)+mean(RCExcess))/phase3_length_2, RCTotal = mean(RCTotal+NGTotal)/phase3_length_2)
  
  phase3_fill_1 <- phase3_fill_1 %>%
    mutate("TotalSupply" = SupplyRA + SupplyRC)
  
  phase3_fill_2 <- phase3_fill_2 %>%
    mutate("TotalSupply" = SupplyRA + SupplyRC)
  
  
  phase3_fill_base_1 <- phase3_fill_1 %>%
    group_by(SRC,phase) %>%
    slice(which.max(AC))
  
  phase3_fill_base_2 <- phase3_fill_2 %>%
    group_by(SRC,phase) %>%
    slice(which.max(AC))
  
  phase3_fill_base_1 <- phase3_fill_base_1 %>%
    mutate("UnmetDemand" = max(c(Demand-TotalSupply,0))) %>%
    mutate("RCUnavailable" = max(c(NG + RC - SupplyRC,0))) %>%
    mutate("RApercent" = SupplyRA/Demand) %>%
    mutate("RCpercent" = (SupplyRC/Demand)) %>%
    mutate("Unmetpercent" = UnmetDemand/Demand) %>%
    mutate("RCunavailpercent" = RCUnavailable/Demand) %>%
    mutate("Totalpercent" = RApercent + RCpercent)
  
  
  phase3_fill_base_2 <- phase3_fill_base_2 %>%
    mutate("UnmetDemand" = max(c(Demand-TotalSupply,0))) %>%
    mutate("RCUnavailable" = max(c(NG + RC - SupplyRC,0))) %>%
    mutate("RApercent" = SupplyRA/Demand) %>%
    mutate("RCpercent" = (SupplyRC/Demand)) %>%
    mutate("Unmetpercent" = UnmetDemand/Demand) %>%
    mutate("RCunavailpercent" = RCUnavailable/Demand) %>%
    mutate("Totalpercent" = RApercent + RCpercent)
  
  
  #write.table(phase3_fill_base, "phase3fill_base.txt", sep="\t", row.names = FALSE, col.names = TRUE)
  
  
  fatShaveData1 <- inner_join(phase3_fill_base_1, unitDetails, by="SRC")
  fatShaveData2 <- inner_join(phase3_fill_base_2, unitDetails, by="SRC")
  
  fatShaveData1 <- fatShaveData1 %>%
    mutate("Demand" = round(Demand*STR)) %>%
    mutate("SupplyRA" = round(SupplyRA*STR)) %>%
    mutate("SupplyRC"= round(SupplyRC*STR)) %>%
    mutate("TotalSupply" = round(TotalSupply*STR)) %>%
    mutate("UnmetDemand" = round(UnmetDemand*STR)) %>%
    mutate("RCTotal" = round(RCTotal*STR)) %>%
    mutate("RCUnavailable" = round(RCUnavailable*STR))
    #mutate("RApercentForce"= SupplyRA/1024750) %>%
    #mutate("RCpercentForce" = SupplyRC/1024750) %>%
    #mutate("RCUApercent" = RCUnavailable/1024750) %>%
    #mutate("AggPercent" = RApercentForce + RCpercentForce + RCUApercent)
  
  
  fatShaveData2 <- fatShaveData2 %>%
    mutate("Demand" = round(Demand*STR)) %>%
    mutate("SupplyRA" = round(SupplyRA*STR)) %>%
    mutate("SupplyRC"= round(SupplyRC*STR)) %>%
    mutate("TotalSupply" = round(TotalSupply*STR)) %>%
    mutate("UnmetDemand" = round(UnmetDemand*STR)) %>%
    mutate("RCTotal" = round(RCTotal*STR)) %>%
    mutate("RCUnavailable" = round(RCUnavailable*STR))
  #mutate("RApercentForce"= SupplyRA/1024750) %>%
  #mutate("RCpercentForce" = SupplyRC/1024750) %>%
  #mutate("RCUApercent" = RCUnavailable/1024750) %>%
  #mutate("AggPercent" = RApercentForce + RCpercentForce + RCUApercent)
  
  
  #fatShaveData1 <- left_join(fatShaveData1, SRCbranch, by = "SRC")
  #fatShaveData2 <- left_join(fatShaveData2, SRCbranch, by = "SRC")
  
  fatShaveData1 <- fatShaveData1 %>%
    select(Branch, SRC,TITLE,Demand,SupplyRA, SupplyRC, TotalSupply,UnmetDemand,RCUnavailable, 
             RApercent, RCpercent,Unmetpercent,RCunavailpercent,Totalpercent, RCTotal)
  
  fatShaveData2 <- fatShaveData2 %>%
    select(Branch, SRC,TITLE,Demand,SupplyRA, SupplyRC, TotalSupply,UnmetDemand,RCUnavailable, 
           RApercent, RCpercent,Unmetpercent,RCunavailpercent,Totalpercent, RCTotal)
  
  
  
  
  # Competition Data ======================================================================================================================
  comp1_fill <- results_data2%>%
    subset(phase=="comp1") %>%
    group_by(SRC, AC, NG, RC, phase) %>%
    summarise(Demand =mean(Demand)/comp1_length, SupplyRA = (mean(RAFill) + mean(RAExcess))/comp1_length,
              SupplyRC = (mean(RCFill)+mean(RCExcess))/comp1_length, RCTotal = mean(RCTotal+NGTotal)/comp1_length)
  
  comp1_fill <- comp1_fill %>%
    mutate("TotalSupply" = SupplyRA + SupplyRC)
  
  
  comp1_fill_base <- comp1_fill %>%
    group_by(SRC,phase) %>%
    slice(which.max(AC))
  
  comp1_fill_base <- comp1_fill_base %>%
    mutate("UnmetDemand" = max(c(Demand-TotalSupply,0))) %>%
    mutate("RCUnavailable" = max(c(NG + RC - SupplyRC,0))) %>%
    mutate("RApercent" = SupplyRA/Demand) %>%
    mutate("RCpercent" = (SupplyRC/Demand)) %>%
    mutate("Unmetpercent" = UnmetDemand/Demand) %>%
    mutate("RCunavailpercent" = RCUnavailable/(Demand)) %>%
    mutate("Totalpercent" = RApercent + RCpercent)
  
  
  fatShaveData_Comp <- inner_join(comp1_fill_base, unitDetails, by="SRC")
  
  fatShaveData_Comp <- fatShaveData_Comp %>%
    mutate("Demand" = round(Demand*STR)) %>%
    mutate("SupplyRA" = round(SupplyRA*STR)) %>%
    mutate("SupplyRC"= round(SupplyRC*STR)) %>%
    mutate("TotalSupply" = round(TotalSupply*STR)) %>%
    mutate("UnmetDemand" = round(UnmetDemand*STR)) %>%
    mutate("RCTotal" = round(RCTotal*STR)) %>%
    mutate("RCUnavailable" = round(RCUnavailable*STR))
  #mutate("RApercentForce"= SupplyRA/1024750) %>%
  #mutate("RCpercentForce" = SupplyRC/1024750) %>%
  #mutate("RCUApercent" = RCUnavailable/1024750) %>%
  #mutate("AggPercent" = RApercentForce + RCpercentForce + RCUApercent)
  
  #fatShaveData_Comp <- inner_join(fatShaveData_Comp, SRCbranch, by = "SRC")
  
  fatShaveData_Comp <- fatShaveData_Comp %>%
    select(Branch, SRC,TITLE,Demand,SupplyRA, SupplyRC, TotalSupply,UnmetDemand,RCUnavailable, 
           RApercent, RCpercent,Unmetpercent,RCunavailpercent,Totalpercent, RCTotal)
  
  
  
  
  fatShaveData1 <- fatShaveData1 %>%
    select(-RCTotal) %>%
    ungroup(phase) %>%
    select(-phase)
  
  
  fatShaveData2 <- fatShaveData2 %>%
    select(-RCTotal) %>%
    ungroup(phase) %>%
    select(-phase)
  
  
  fatShaveData_Comp <- fatShaveData_Comp %>%
    select(-RCTotal) %>%
    ungroup(phase) %>%
    select(-phase)
  
  
  write.table(fatShaveData1, out1, sep="\t", row.names = FALSE, col.names = TRUE)
  write.table(fatShaveData2, out2, sep="\t", row.names = FALSE, col.names = TRUE)
  write.table(fatShaveData_Comp, out_comp, sep="\t", row.names = FALSE, col.names = TRUE)
  
  
  
  
  
  
  #========= max demand calculations
  
  
  fatShaveDataMax <- bind_rows(fatShaveData1, fatShaveData2)
  
  fatShaveDataMax <- fatShaveDataMax %>%
    group_by(SRC) %>%
    slice(which.min(Totalpercent))
               
  
  
  
  write.table(fatShaveDataMax, out_max, sep="\t", row.names = FALSE, col.names = TRUE)
  
  #BarChart Output
  #===================================================================================================================
  
  # This routine generates one output file, "barChartData.txt" used by FM to build OI Bar Charts
  
  average_fill <- results_data2 %>%
    group_by(SRC, AC, NG, RC, phase) %>%
    #summarise_all(mean)
    summarise(dmetRA=mean((RAFill+RAExcess)/Demand), dmetRC=mean((RCFill+RCExcess)/Demand),
              ACunavailable = mean(ACnotready/Demand), RCunavailable = mean((NGnotready+RCnotready)/Demand)) %>%
    mutate("Scenario" = "A")
  
  average_fill2 <- results_data1 %>%
    group_by(SRC, AC, NG, RC, phase) %>%
    #summarise_all(mean)
    summarise(dmetRA=mean((RAFill+RAExcess)/Demand), dmetRC=mean((RCFill+RCExcess)/Demand),
              ACunavailable = mean(ACnotready/Demand), RCunavailable = mean((NGnotready+RCnotready)/Demand)) %>%
    mutate("Scenario" = "B")
  
  average_fill_combined <- bind_rows(average_fill, average_fill2)
    
  
  barChartdata <- average_fill_combined %>%
    #select(-`rep-seed`) %>%
    subset(phase=="comp1" |phase== "phase3") %>%
    arrange(Scenario,SRC, -AC)
  
  barChartdata <- barChartdata %>%
    mutate("key" = paste(SRC,AC,NG,RC,substr(phase,0,1),Scenario, sep=""))
  
  barChartdata <- barChartdata %>%
    select(SRC, key, AC, NG, RC, phase, Scenario, dmetRA, dmetRC, ACunavailable, RCunavailable)
  
  write.table(barChartdata, file=barchart, sep="\t", row.names=FALSE, col.names = TRUE)
  }