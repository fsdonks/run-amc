#As of ggplot2 version 3.5, we had to make changes so that all factor levels
#appear in the legend and are sorted according to increasing risk.
if(packageVersion("ggplot2")<"3.5") {
  stop("Need ggplot2 version >= 3.5 for intended behavior.")
}

#Used to plot the Programmed Force/Baseline Supply per SRC
#Utility functions that are shared between the taa and R risk chart scripts

risk_colors<-c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#009E73")
risk_labels<-c("extreme", "major", "modest", "minor", "none")

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
  x_mark <- read_excel(supply_demand, trim_ws = TRUE, col_names = TRUE, sheet="SupplyDemand")
  x_mark$RC <- as.numeric(x_mark$Total - x_mark$RA)
  x_mark <- subset(x_mark, select = c(SRC, RA, RC, UNTDS))
  x_mark <- rename(x_mark, prog_RA = RA, prog_RC = RC)
  data_fill <- left_join(data, x_mark, by="SRC")
  return(data_fill)
}

#Make all of the risk charts and spit them to the out_location
#Mark the baseline supply/programmed force with an x using the
#SupplyDemand file located at the supply_demand path.
risk_charts <- function(data, out_location, title_start, subtitle, caption_start) {
  
  for(src in unique(data$SRC)){
    src_data <- filter(data, SRC==src)
    src_title=src_data['UNTDS'][[1]]
    g <- 
      ggplot(src_data, aes(x=RA, y=RC)) + 
      geom_tile(aes(fill=factor(Risk, levels=risk_labels)), colour="black", show.legend=TRUE)+
      labs(title=title_start,
           subtitle=src_title,
           caption=paste("SRC: ", src, "/", caption_start,"/", subtitle, 
                         "/Built on ", 
                         Sys.Date(),
                         "          X indicates programmed force", sep='')) +
      xlab("# of RA Units")+
      ylab("# of RC Units")+
      scale_fill_manual(name = "Risk", values=setNames(risk_colors, risk_labels), drop=FALSE)+
      scale_x_continuous(breaks=integer_breaks(5))+
      scale_y_continuous(breaks=integer_breaks(5))+
      geom_text(x=src_data$prog_RA, y=src_data$prog_RC, label="X")+
      theme_bw() +
      theme(text=element_text(size=20),
            axis.text=element_text(size=20),
            legend.text=element_text(size=20),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            plot.caption = element_text(hjust = 0, size=9),
            plot.caption.position =  "plot")
    print(g)
    ggsave(paste(out_location, src, ".jpeg", sep=""), device="jpeg")
  }
}