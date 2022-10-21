(load-file "/home/craig/workspace/run-amc/ac-rc_random-runs/runamc.clj")
(ns marathon.analysis.runamc)
;;example usage
;;m4 workbook should have all NG and RC supply coded as being RC
(def path "/home/craig/workspace/run-amc/ac-rc_random-runs/testdata.xlsx")
(def proj (a/load-project path))
(def phases [["comp" 1 821] ["phase-1" 822 967]])

(def results (rand-runs-ac-rc proj :reps 2 :phases phases
                              :lower 0 :upper 0.2
                              :compo-lengths r/default-compo-lengths)) 

(r/write-output "results.txt" results) 
