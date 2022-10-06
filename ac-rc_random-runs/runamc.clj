;This namespace builds on the functionality in marathon.analysis.random.
;It modifies several functions in that namesapce to do AC and RC supply variations with random initial conditions. 
(ns marathon.analysis.runamc
  (:require [marathon.analysis.random :as r]
            [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.analysis.experiment :as e]
            [marathon.analysis.nolh :as nolh]
            [marathon.ces.core :as c]
            [spork.util.table :as tbl]
            [spork.util.general :as gen]
            [clojure.spec.alpha :as s]))

;;default compo lengths
(def default-compo-lengths {"AC" 1095 "RC" 1825 "NG" 1825})

;;number of threads to use for pmap
(def ^:dynamic *threads* 4)

;;how many times a rep can fail before we toss the whole thing
(def ^:dynamic *retries* 2)

;;probability of printing a message.
(def ^:dynamic *noisy* 0.5)

;;default seed for reproducibility
(def +default-seed+ 42)

(defn adjust-rc
  [rc-demand rec]
  (if (= (:DemandGroup rec) "RC_NonBOG-War")
    (assoc rec :Quantity rc-demand)
    rec))	

(defn rc-proj
  [proj]
  (let [supply (-> proj :tables :SupplyRecords tbl/table-records)
        src (-> proj :tables :SupplyRecords tbl/table-records first :SRC)
        percent (:rc-unavailable (:Tags (first supply)))
        rc (:Quantity (first (filter #(= "RC" (:Component %)) supply)))
        rc-demand (int (* rc percent))]
    (->> proj 
         :tables 
         :DemandRecords 
         tbl/table-records
         (map #(adjust-rc rc-demand %))
         tbl/records->table     
         (assoc-in proj [:tables :DemandRecords]))))    

;;it probably makes sense to fence out a general scheme to apply this pipeline
;;to demand records, policy records, etc.  There are probably many types of
;;transformations (or compiler passes) we'd like to apply.
;;this is really a random supply project.
(defn rand-proj
  "Takes a project and makes a new project with random unit initial cycle times.
   If the project supplies a:supply-record-randomizer key associated to a
   function of supply-record -> supply-record, that function will be supplied
   as a transformation when generating random records.  Otherwise, no transformation
   will occur beyond the normal expansion of a batch supply record into multiple records
   with custom names."
  [proj]
  (let [supply-record-randomizer (get proj :supply-record-randomizer identity)
        proj2 (rc-proj proj)]
    (->> proj2
         :tables
         :SupplyRecords
         tbl/table-records
         (mapcat #(r/rand-recs % supply-record-randomizer))
         tbl/records->table
         (assoc-in proj2 [:tables :SupplyRecords]))))

(defn ac-rc-supply-reduction-experiments
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  Upper and lower bounds have been modified so we can look at AC supply levels
  above the current inventory."
  [tables lower upper & {:keys [levels step] :or {step 1}}]
  (let [init         (-> tables :SupplyRecords)
        groups       (-> init e/grouped-supply)
        [lowAC highAC]   (r/bound->bounds (-> "AC" groups :Quantity) [lower upper])
        [lowRC highRC]   (r/bound->bounds (-> "RC" groups :Quantity) [lower upper])]
    (cond 
      (and (not= lowAC highAC) (not= lowRC highRC))
        (for [n (r/compute-spread-descending (or levels (inc highAC)) lowAC highAC)
              m (r/compute-spread-descending (or levels (inc highRC)) lowRC highRC)
              :let [groups2 (-> (assoc-in groups ["AC" :Quantity] n)
                                (assoc-in ["RC" :Quantity] m))]]
          (->> groups2
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      (not= lowAC highAC)
        (for [n (r/compute-spread-descending (or levels (inc highAC)) lowAC highAC)]
          (->> (assoc-in groups ["AC" :Quantity] n)
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      (not= lowRC highRC)
        (for [n (r/compute-spread-descending (or levels (inc highRC)) lowRC highRC)]
          (->> (assoc-in groups ["RC" :Quantity] n)
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      :else     
        [tables])))

(defn project->experiments-ac-rc
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  This function is modified so we can look at AC supply levels above the
  current inventory."
  [prj lower upper & {:keys [levels step] :or {step 1}}]
  (for [tbls (ac-rc-supply-reduction-experiments (:tables prj) lower upper
                  :step step :levels (:levels prj))]
    (assoc prj :tables tbls)))

;;if we can't copmute a fill, we should log it somewhere.
(defn try-fill
  ([proj src idx phases]
   (loop [n *retries*]
     (let [_   (when (and *noisy*
                          (not (zero? *noisy*))
                          (or (= *noisy* 1.0)
                              (< (rand) *noisy*)))
                 (util/log [:trying src :level idx]))
           res (try (let [seed (:rep-seed proj)
                          fill (r/project->phase-fill (rand-proj proj) phases)]
                      (mapv #(assoc % :rep-seed seed)
                            (e/summary-availability-records src proj fill)))
                    (catch Exception e #_:error (throw e)))]
       (case res
         :error (if (pos? n)
                  (do (util/log {:retrying n :src src :idx idx})
                      (recur (dec n)))
                  (let [err {:error (str "unable to compute fill " src)
                             :src   src
                             :idx   idx}
                        _    (util/log err)]
                    err))
         res)))))

(def ^:dynamic *project->experiments-ac-rc* marathon.analysis.runamc/project->experiments-ac-rc)

(defn rand-target-model-ac-rc
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj & {:keys [phases lower upper levels gen seed->randomizer]
           :or   {lower 0 upper 1 gen util/default-gen
                  seed->randomizer (fn [_] identity)}}]
   (let [project->experiments *project->experiments-ac-rc*]
     (->> (assoc proj :phases phases :lower lower :upper upper :levels levels
                 :gen gen  :seed->randomizer :seed->randomizer)
          (e/split-project)
          (reduce
           (fn [acc [src proj]]
             (let [experiments (project->experiments proj lower upper)]
               (into acc
                     (filter (fn blah [x] (not (:error x))))
                     (util/pmap! *threads*
                                 (fn [[idx proj]]
                                   (let [rep-seed   (util/next-long gen)]
                                     (-> proj
                                         (assoc :rep-seed rep-seed
                                                :supply-record-randomizer
                                                (seed->randomizer rep-seed))
                                         (try-fill src idx phases))))
                                 (map-indexed vector experiments))))) [])
          (apply concat)
          vec)))

(defn rand-runs-ac-rc
  "Runs replications of the rand-target-model function.
   Mid-level function meant to be invoked from higher-level APIs.
   Caller may supply
   :reps - int, number of random replications
   :phases - optional, sequence of [phase from to] :: [string int int],
             derived from PeriodRecords if nil
   :lower - lower bound for the supply variation multiplier, defaut 0.
   :upper - upper bound for the supply variation multipler, default 1.
   :seed - integer, random seed to use for all the replications, default +default-seed+.
   :compo-lengths optional, map of {compo cyclelength} used for distribution
                  random initial cycletimes, default default-compo-lengths ."
  [proj & {:keys [reps phases lower upper seed levels compo-lengths seed->randomizer]
           :or   {lower 0 upper 1 seed +default-seed+
                  compo-lengths default-compo-lengths}}]
  (let [seed->randomizer (or seed->randomizer #(r/default-randomizer % compo-lengths))
        gen              (util/->gen seed)
        phases           (or phases (util/derive-phases proj))]    
    (apply concat
           (map (fn [n] (rand-target-model-ac-rc proj
                            :phases phases :lower lower :upper upper
                            :gen   gen     :seed->randomizer seed->randomizer
                            :levels levels))
                 (range reps)))))

(comment
  ;example usage
  ;m4 workbook should have all NG and RC supply coded as being RC
  (def path "C:\\Users\\Keith.A.MacFarlane\\Desktop\\run amc dev\\testdata.xlsx")
  (def proj (a/load-project path))
  (def phases [["comp" 1 821] ["phase-1" 822 967]])
    
  (def results (rand-runs-ac-rc proj :reps 30 :phases phases
                                     :lower 0 :upper 1.5
                                     :compo-lengths default-compo-lengths)) 

  (r/write-output "results.csv" results) 
)
