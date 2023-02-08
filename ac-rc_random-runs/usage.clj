(ns marathon.analysis.random)
;;example usage
;;m4 workbook should have all NG and RC supply coded as being RC
(def path "/home/craig/workspace/run-amc/ac-rc_random-runs/testdata.xlsx")
(def proj (a/load-project path))
(def phases [["comp" 1 821] ["phase-1" 822 967]])

(def results (rand-runs-ac-rc 5 ;;min-distance
                                0.5 ;;lower-rc
                                0.7 ;;upper-rc
                              (add-transform proj adjust-cannibals
                                []) :reps 2 :phases phases
                              :lower 0 :upper 0.1
                              :compo-lengths default-compo-lengths
                              ))

(write-output "results.txt" results) 
