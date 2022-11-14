
my_runamc_folder=paste(getwd(), "/workspace/run-amc/", sep='')
#my_runamc_folder=paste(getwd(), "/", sep='')

source(paste(my_runamc_folder, "util.R", sep=''))
#load code from the two runamc scripts
#these will both depend on utils.R as well.
source(paste(my_runamc_folder, "edta_risk_chart/edta_risk.R", sep=''))
source(paste(my_runamc_folder, "taa_risk_chart/taa_risk_chart.R", sep=''))
supply_demand=paste(my_runamc_folder, "SupplyDemand_Original.xlsx", sep='')

#make the edta risk charts
edta_supply=paste(my_runamc_folder, "edta_risk_chart/Data Initial List.xlsx", sep='')
edta_demand=paste(my_runamc_folder, "edta_risk_chart/early_demand.csv", sep='')
output_path=paste(my_runamc_folder, "outputs/", sep='')
make_edta_charts(edta_supply,
                 edta_demand,
                 output_path,
                 supply_demand,
                 TRUE)

#make the taa risk charts
weights<- c("comp"=0.25, "phase1"=0.75)
m4_results_1=paste(my_runamc_folder, "taa_risk_chart/results_test.txt", sep='')
m4_results_2=paste(my_runamc_folder, "taa_risk_chart/results_archive.txt", sep='')
results_files=list(m4_results_1,
                   m4_results_2)
make_taa_charts(output_path, results_files, weights, supply_demand, TRUE)