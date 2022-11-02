source(paste(getwd(), "/workspace/run-amc/edta_risk_chart/edta_risk.R", sep=''))
source(paste(getwd(), "/workspace/run-amc/taa_risk_chart/taa_risk_chart.R", sep=''))

make_edta_charts("./workspace/run-amc/edta_risk_chart/Data Initial List.xlsx",
                 "./workspace/run-amc/edta_risk_chart/early_demand.csv")

results_files=list("./workspace/run-amc/taa_risk_chart/results_test.csv",
                   "./workspace/run-amc/taa_risk_chart/results_archive.csv")
make_taa_charts("./workspace/run-amc/outputs/", results_files)                 
