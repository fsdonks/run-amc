#Utility functions that are shared between the taa and R risk chart scripts

risk_colors=c("#F05454", "#FFC990", "#FFF990", "#3DCC91", "#009E73")
risk_labels=c("extreme", "major", "modest", "minor", "none")

#Make all of the risk charts and spit them to the out_location
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
}