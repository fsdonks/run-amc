#Used to plot the Programmed Force/Baseline Supply per SRC

#Utility functions that are shared between the taa and R risk chart scripts

risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#009E73")
risk_labels=c("extreme", "major", "modest", "minor", "none")

integer_breaks <- function(n=5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

#Add the baseline supply numbers to the dataframe
add_base_supply<-function(data, supply_demand) {
  x_mark <- read_excel(supply_demand, trim_ws = TRUE, col_names = TRUE)
  x_mark$RC <- as.numeric(x_mark$Total - x_mark$RA)
  x_mark <- subset(x_mark, select = c(SRC, RA, RC))
  x_mark <- rename(x_mark, prog_RA = RA, prog_RC = RC)
  data_fill <- left_join(data, x_mark, by="SRC")
  return(data_fill)
}

#Make all of the risk charts and spit them to the out_location
#Mark the baseline supply/programmed force with an x using the
#SupplyDemand file located at the supply_demand path.
risk_charts <- function(data, out_location) {
  
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
      geom_tile(color="black") +
      ggtitle(src) +
      scale_fill_gradientn(colors=colors_subset,
                           labels=labels_subset,
                           breaks=risks) +
      scale_x_continuous(breaks=integer_breaks(5))+
      scale_y_continuous(breaks=integer_breaks(5))+
      geom_text(x=src_data$prog_RA, y=src_data$prog_RC, label="X")+
      theme_bw() +
      theme(text=element_text(size=20),
            axis.text=element_text(size=20),
            legend.text=element_text(size=20),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    print(g)
    ggsave(paste(out_location, src, ".png", sep=""))
  }
}