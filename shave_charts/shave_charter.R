##### BUILD DETAIL BY BRANCH SHAVE CHART - CONFLICT ##################
# Original script written by Dusty
# Script modified by Heather
# Script further modified by Sarah
######################################################################

# To produce shave charts you must run Sections 2, 3, 4, and 7
# To run a specific plot you must run Sections 2, 3, 4, and 5 or 6

#Load libraries
library(tidyverse)
library(scales)
library(ggrepel)
library(tidyr)
library(formattable)
library(dplyr)
library(ggpattern)
library(readxl)

#################################################################################
#Initialize Plot Data
###############################################################################
#Use this to split RC unavailable between the top of the bar and as an overlap
#to unmet demand in order to show how much of the umet demand could be met by
#rc unavailable if there were no more rc unavailable.
add_overlap <- function(det_data){
  # Create overlap variable to account for overlap.This variable accounts for the overlap between unmet demand and rc unavailable
  det_data$Overlap <- ifelse(det_data$Unmet_Demand==det_data$Unavailable,det_data$Unmet_Demand,
                             ifelse(det_data$Unmet_Demand<det_data$Unavailable,det_data$Unmet_Demand,
                                    ifelse(det_data$Unmet_Demand> det_data$Unavailable,det_data$Unavailable,-999)))
  
  ## The number -999 does not mean anything, it was chosen as the "else" value because overlap is NOT negative. 
  
  ### Recalculate the unmet demand and rc unavailable
  det_data$unmet_demand<-det_data$Unmet_Demand - det_data$Overlap
  det_data$unavailable <-det_data$Unavailable -det_data$Overlap
  
  det_data<- subset(det_data,select=-c(Unmet_Demand,Unavailable))
  return(det_data)
}

#Stop the script from running if there are duplicate values in the column of
#the dataframe.
stop_if_duplicates<-function(data, column) {
  if(!anyDuplicated(data[column])==0) {
    msg<-paste("The column", column, "in \'data\' has duplicates.", sep=" ")
    stop(msg)
  }
    
}

make_plot_data <- function(fat_shave_path, src_str, supply_demand, use_overlap, 
                           by_branch=FALSE){

  data <- 
    read_tsv(fat_shave_path, col_names =  TRUE)%>%
    janitor::clean_names()  %>% 
    filter(!is.na(demand))

  det_data <- data %>%
    mutate(unit_type=title) %>%
    rename(RA_Supply = supply_ra,
           RC_Supply = supply_rc,
           Unmet_Demand = unmet_demand,
           Unavailable = rc_unavailable) %>%
    select(src,
           unit_type,
           branch,
           title,
           demand,
           RA_Supply,
           RC_Supply,
           Unmet_Demand,
           Unavailable)
  
  # filter to prep for mapping to modeldata
  size_labels <- read_excel(src_str,sheet=1)
  size_labels <- size_labels[order(size_labels$Branch),]
  #If there are duplicates in size_labels, the bars for results by SRC
  #may not start at y=0.
  stop_if_duplicates(size_labels, "SRC")
  supply_demands<-read_excel(supply_demand,sheet=1) %>%
    subset(select=c(SRC,RA, USAR, ARNG))

  test_data <- 
    merge(size_labels,det_data,by.x='SRC',by.y='src') %>%
    merge(supply_demands, by='SRC') %>%
    #In case we updated SRC STR Branch but didn't rerun the post processor.
    mutate(branch=Branch) %>%
    mutate(RA_STR=RA*STR, USAR_STR=USAR*STR, ARNG_STR = ARNG*STR)

  test_data$RA_STR <- round(test_data$RA_STR/1000,1)
  test_data$ARNG_STR <- round(test_data$ARNG_STR/1000,1)
  test_data$USAR_STR <- round(test_data$USAR_STR/1000,1)

  det_data <- test_data
  
  if(use_overlap){
    det_data <- add_overlap(test_data)
  } else {
    det_data<-rename(det_data, unmet_demand=Unmet_Demand, unavailable=Unavailable)
  }
  
  
  
  if(by_branch) {
    summarises<-c("demand", "RA_Supply", "RC_Supply", "unmet_demand", "unavailable")
    if(use_overlap){
      summarises<-append(summarises, c("Overlap"))
    }
    # Aggregate demand, ra supply, rc supply, rc unavailable, unmet demand
    det_data <- det_data %>%  group_by(branch) %>%
      summarise_at(summarises, sum)
  }
  
  return(det_data)
}

##### Section 4: Prepare Branch Plot Details #####
#Calculate percents for labels
add_percents<- function(data_for_analysis, use_overlap){
  if(use_overlap) {
    data_for_analysis<-
      mutate(data_for_analysis, 
           percent_structure =(RA_Supply + RC_Supply  +Overlap
                               + unmet_demand + unavailable)/demand )
  } else {
    data_for_analysis<-
      mutate(data_for_analysis,
           percent_structure =(RA_Supply + RC_Supply+ unmet_demand)/demand )
  }
  acrosses<-c("RA_Supply","RC_Supply","unmet_demand")
  if(use_overlap) {
    acrosses<-append(acrosses, c("Overlap", "unavailable"))
  }
  # prepare data to be plotted in stacked bars
  data_for_analysis <- 
    data_for_analysis %>%
    mutate(across(all_of(acrosses),
                  .fns = ~round(./demand,3),.names = "perc_{col}")) %>%
    unite("RA_SupplySPACEperc_RA_Supply",c(RA_Supply,perc_RA_Supply),sep = "SPACE") %>% 
    unite("RC_SupplySPACEperc_RC_Supply",c(RC_Supply,perc_RC_Supply),sep = "SPACE") %>% 
    unite("unmet_demandSPACEperc_unmet_demand",c(unmet_demand,perc_unmet_demand),sep = "SPACE")
  #This will determine the order of the bars.
  pivot_longers=c("RA_SupplySPACEperc_RA_Supply", 
                  "RC_SupplySPACEperc_RC_Supply")
  if(use_overlap) {
    data_for_analysis <- 
      data_for_analysis %>%
      unite("OverlapSPACEperc_Overlap",c(Overlap,perc_Overlap),sep = "SPACE") %>%
      unite("unavailableSPACEperc_unavailable",c(unavailable,perc_unavailable),sep = "SPACE") 
    pivot_longers<-append(pivot_longers, c("OverlapSPACEperc_Overlap"))
  }
  pivot_longers<-append(pivot_longers, "unmet_demandSPACEperc_unmet_demand")
  #Only show unavailable if we are in the phase where we also show overlap.
  if(use_overlap) {
    pivot_longers<-append(pivot_longers, "unavailableSPACEperc_unavailable")
}
  data_for_analysis <- 
    data_for_analysis %>%
    pivot_longer(all_of(pivot_longers))
  return(data_for_analysis)
}

arrange_and_group <- function(data, by_branch) {
  if(by_branch){
    column="branch"
  } else {
    column="unit_type"
  }
  xs<-arrange(data, !!sym(column)) %>%  #comment this line if using fat shave bars
    #arrange(-widths) %>% #uncomment this line if using fat shave bars so that bars are arranged by decreasing width
    # Calculate ymin and ymax for bar
    group_by_at(column) 
  return(xs)
}

scale_x <- function(by_branch, breaks_helper, det_final) {
  if(by_branch) {
    
      scale_x_continuous(breaks = unique(det_final$label_x_break), 
                         labels = unique(det_final$branch) ) 
  } else {
    
      scale_x_continuous(breaks = breaks_helper$label_x_break, 
                         labels = breaks_helper$label_x_break ) 
  }
}

bar_labels <- function(by_branch) {
  if(by_branch) {
    return(aes(x = xmax+10, y = .01, label = str_c(branch)))
  } else {
    return(aes(x = xmax+10, y = .01, label = str_c(unit_type)))
  }
}

add_strength <- function(by_branch) {
  if(by_branch){
    return(NULL)
  } else {
    return(geom_text(aes(x = xmax+30, y = .01, 
                               label = str_c( '  (', RA_STR, 
                                              'K, ', 
                                              ARNG_STR, 'K, ', 
                                              USAR_STR, 'K )    ') ), 
              angle = 90, hjust = 0, size = 3.0,
              check_overlap = TRUE, fontface='bold'))
  }
}

make_title <- function(by_branch, chart_title, branch_to_filter) {
  if(by_branch) {
    return(chart_title)
  } else {
    return(paste(str_c(branch_to_filter), 
                 "-Aggregated Modeling Results as Percentages of Demand",
                 sep=''))
  }
}

plot_branch <- function(data_name, chart_subtitle, chart_file_prefix, 
                        data_for_analysis,
                        use_overlap,
                        branch_to_filter = "Engineer", 
                        ##Only needed if we need to load the data.
                        src_str=NULL,
                        supply_demand=NULL,
                        by_branch=FALSE,
                        chart_title=""){
  bar_width <- 50
  ##Check to see if data_for_analysis is a path and need to load data or
  ##if it's already data.
  if(is.character(data_for_analysis)){
    data_for_analysis<-make_plot_data(data_for_analysis, src_str, 
                                      supply_demand, use_overlap, 
                                      by_branch=by_branch)
  }
  
  if(!by_branch) {
    data_for_analysis<-filter(data_for_analysis,
                              branch == branch_to_filter)
  }
  det_final <-
    data_for_analysis %>%
    ### If standard widths are desired comment out the following three lines. 
    #mutate(widths = round(demand/1000)) %>% 
    # mutate(widths = if_else(widths<5,5,widths)) %>%
    #mutate(widths = if_else(widths>900,750,widths)) %>%
    ## If a fat shave style is desired, comment out the next line. 
    mutate(widths=50) %>%
    mutate(percent_demand_met = round((RA_Supply + RC_Supply)/ demand)) %>% 
    mutate(percent_RA = RA_Supply/demand) %>%
    mutate(percent_RC = RC_Supply/demand) %>%
    add_percents(use_overlap) %>%
    separate(name, into = c("name", "perc"), "SPACE", convert = TRUE) %>% 
    separate(value, into = c("name_value", "perc_value"), "SPACE", convert = TRUE) %>%
    mutate(name = str_replace(string = name, pattern = "_", replacement = " ")) %>% 
    arrange_and_group(by_branch) %>%
    mutate(cummulative=cumsum(perc_value))%>%
    mutate(ymin = cumsum(perc_value) - perc_value) %>% 
    mutate(ymax = ifelse(cummulative>2.5,2.5,cummulative))%>%
    # mutate(ymax=cummulative) %>% 
    ungroup() %>% 
    #Calculate space between bars
    mutate(width_and_space = widths + bar_width) %>% 
    #Calculate xmin and xmax for bars
    group_by(name) %>% 
    mutate(xmax = cumsum(width_and_space)) %>% 
    mutate(xmin = lag(xmax)) %>% 
    mutate(xmin = replace_na(xmin, 0)) %>% 
    mutate(xmax = xmax - bar_width) %>% 
    ungroup() %>% 
    ##Calculate space between x-axis labels
    mutate(label_x_break = (xmax + xmin)/2)
  
  
  if(!by_branch){
    breaks_helper <-
      det_final %>% 
      select(label_x_break, unit_type) %>% 
      distinct(label_x_break,unit_type)
  } else {
    breaks_helper <- FALSE
  }
  
  det_final$over_y_axis <- ifelse(det_final$percent_structure > 2.5, str_c('Total Structure: ',round(det_final$percent_structure *100),'%'),' ')
  
  #This is for ordering the legend entries.
  det_final$name<-factor(det_final$name, 
                         levels = c('RA Supply', 'RC Supply', 'Overlap', 
                                    'unmet demand',"unavailable" ))
  #Determine legend entries according to if we are plotting conflict or 
  #campaigning.
  if("unavailable" %in% det_final$name){
    legend_entries=c('none', "none",
                     "crosshatch","none",
                     "none")
    legend_labels = c("RA Supply", "RC Supply", 
                      "RC Unavailable As Portion of Unmet",
                      "Unmet Demand",
                      "RC Unavailable Leftover from Unmet")
  } else {
    legend_entries=c('none', "none", "none")
    legend_labels = c("RA Supply", "RC Supply", "Unmet Demand")
  }
  out <-
    det_final %>%
    ggplot() +
    geom_rect_pattern(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill=name, pattern=name),color='black',pattern_fill='white',pattern_angle=45,pattern_spacing=.025)+
    geom_hline(aes(yintercept = 1), color = "red", size = 1) +
    scale_pattern_manual(values=c('RA Supply'='none', 'RC Supply' = 'none', 'Overlap'='crosshatch', 'unmet demand'='none',"unavailable"='none' ), guide="none")+
    scale_fill_manual(labels=legend_labels, 
                      values = c('RA Supply' = '#bdd7ee',
                                 'RC Supply' = '#c6e0b4',"Overlap"='#ffffb2', 
                                 'unmet demand' = '#ffffb2', 
                                 'unavailable' = "white"), 
                      #Modify the legend to only show the pattern were we want
                      #it instead of a pattern for all keys.
                      guide=guide_legend(override.aes=
                                           list(pattern=legend_entries))) +
    geom_text(bar_labels(by_branch), angle = 90, hjust = 0, size = 4,check_overlap = TRUE, fontface='bold') + ## dark labels
    add_strength(by_branch) +
    geom_text(aes(x = xmax+10, y = 1.47, label = str_c('Demand Met : ', round(percent_RA,2)*100+ round(percent_RC,2)*100,'%' )), angle = 90, hjust = 0, size = 3,check_overlap = TRUE, fontface = "bold") + ##  big number for demand
    geom_text(aes(x = xmax+10, y = 2.0, label = str_c(over_y_axis)), angle = 90, hjust = 0, size = 3.0,check_overlap = TRUE, fontface='bold') +
    scale_x(by_branch, breaks_helper, det_final) +
    scale_y_continuous(labels = percent_format(accuracy = 5L), breaks = seq(0,2.5,.25),expand = c(0,0), limits = c(0,2.5)) +
    labs(title = make_title(by_branch, chart_title, branch_to_filter),
         subtitle = chart_subtitle, fill="",
         x = "", y = "",
         caption=paste(branch_to_filter, '/',data_name,'/', "Built on ",
                       Sys.Date(),sep='')) +
    theme(legend.position = "bottom", title=element_text(size=20), axis.text.x = element_blank(),axis.text.y=element_text(size=15,color='black',face='bold'), axis.ticks = element_blank(), line = element_blank(), 
          axis.line = element_line(color = "black"))
  return(out)
  
}

save_shave_chart <- function(data_name, chart_subtitle, chart_file_prefix, 
                           data_for_analysis, root, plot_folder,
                           use_overlap,
                           ##Only needed if we need to load the data.
                           src_str=NULL,
                           supply_demand=NULL,
                           branch_to_filter = "",
                           by_branch=FALSE,
                           chart_title="")
  {
  if(!dir.exists(paths = paste(root, plot_folder, sep=''))){
    dir.create(paste(root, plot_folder, sep=''))
  }
  plot <- plot_branch(data_name, chart_subtitle, chart_file_prefix, 
                      data_for_analysis, 
                      use_overlap,
                      branch_to_filter = branch_to_filter,
                      src_str, supply_demand, by_branch=by_branch,
                      chart_title=chart_title)
  ggsave(filename = str_c(root, plot_folder, "/", chart_file_prefix, "_", 
                          branch_to_filter, ".jpeg"), device="jpeg",
         plot = plot, width = 15, height = 8, units = "in", dpi = 1000)
}

#####Plot details for all branches as listed in Branch Overview and save each 
#plot as a png file for placement in PowerPoint charts #####
save_all_shave_charts<- function(data_name, chart_subtitle, chart_file_prefix, 
                           root, plot_folder, data_for_analysis,
                           src_str,
                           supply_demand, use_overlap){
  data<-make_plot_data(data_for_analysis,
                    src_str,
                    supply_demand, use_overlap)
  unique(data$branch) %>%
    purrr::walk(.f = ~save_shave_chart(data_name, chart_subtitle, 
                                       chart_file_prefix, 
                                       data, root, plot_folder, use_overlap,
                                       ##Only needed if we need to load the data.
                                       src_str=NULL,
                                       supply_demand=NULL,
                                       branch_to_filter = .x))
}

#Plot these branches only #####
plot_branches <- function(data_name, chart_file_prefix, data){
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "Aviation", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "HR_FN_PA", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "Military Police", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "SOF", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "Other_1of3", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "Sustainment", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "MFE_1of2", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = "Medical_1of 2", TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = 'Military Intelligence', TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = 'JAG', TRUE)
  plot_branch(data_name, chart_file_prefix, data,
              branch_to_filter = 'Bands_CHAP_HIST', TRUE)
}



