(load-file "/home/craig/workspace/run-amc/ac-rc_random-runs/runamc.clj")
(ns marathon.analysis.runamc)
;;example usage
;;m4 workbook should have all NG and RC supply coded as being RC
(def path "/home/craig/workspace/run-amc/ac-rc_random-runs/testdata.xlsx")
(def proj (a/load-project path))
(def phases [["comp" 1 821] ["phase-1" 822 967]])

(def results (binding [r/*project->experiments*
                       ;;min-distance
                       (partial project->experiments-ac-rc 5)]
               (r/rand-runs proj :reps 2 :phases phases
                              :lower 0 :upper 0.2
                              :compo-lengths r/default-compo-lengths))) 

(r/write-output "results.txt" results) 
