#assume that this usage file is in the top of the run-amc/ folder, so we
#check if we are running from Rstudio or sourcing this file and set the
#location of the runamc_folder accordingly.
if ( if(!is.na(Sys.getenv("RSTUDIO", unset = NA))) sys.nframe() == 4L 
     else sys.nframe() == 0L) 
{ 
  my_runamc_folder <- paste(getSrcDirectory(function(dummy) {dummy}), "/", sep='')
} else {
  my_runamc_folder <- paste(getwd(), "/", sep='')
}

#I think you'll need R version 4 or later for this.

source(paste(my_runamc_folder, "util.R", sep=''))
#load code from the two runamc scripts
#these will both depend on utils.R as well.
source(paste(my_runamc_folder, "edta_risk_chart/edta_risk.R", sep=''))
source(paste(my_runamc_folder, "taa_risk_chart/taa_risk_chart.R", sep=''))
source(paste(my_runamc_folder, "shave_charts/shave_charter_preprocessor.R", sep=''))
source(paste(my_runamc_folder, "shave_charts/shave_charter.R", sep=''))

supply_demand=paste(my_runamc_folder, "SupplyDemand_Original.xlsx", sep='')

#What appears in the title before -SRC
edta_title="Early Deploying Unit Risk Matrix"
edta_subtitle="UPLAN based 94 day future"
caption_start="Data from BAA86-91"
#make the edta risk charts
edta_supply=paste(my_runamc_folder, "edta_risk_chart/Data Initial List.xlsx", sep='')
edta_demand=paste(my_runamc_folder, "edta_risk_chart/early_demand.csv", sep='')
output_path=paste(my_runamc_folder, "outputs/", sep='')
make_edta_charts(edta_supply,
                 edta_demand,
                 output_path,
                 supply_demand,
                 edta_title,
                 edta_subtitle,
                 caption_start
                 )

#make the taa risk charts
weights<- c("comp"=0.25, "phase1"=0.75)
m4_results_1=paste(my_runamc_folder, "taa_risk_chart/results_test.txt", sep='')
m4_results_2=paste(my_runamc_folder, "taa_risk_chart/results_archive.txt", sep='')
results_files=list(m4_results_1,
                   m4_results_2)
#What appears in the title before -SRC
taa_title="Force Generation Risk Matrix"
taa_subtitle="OPS based 36 year future"

make_taa_charts(output_path, results_files, weights, supply_demand,
                taa_title, taa_subtitle, caption_start)

##Building shave charts
##First, do this preprocessing to feed the shave chart script.
results1_out_name<-"fatShaveData1.txt"
results2_out_name<-"fatShaveData2.txt"
src_str<-supply_demand

##ended off here after making phases variable.