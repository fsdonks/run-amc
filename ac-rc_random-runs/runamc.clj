;This namespace builds on the functionality in marathon.analysis.random.
;It modifies several functions in that namesapce to do AC and RC supply variations with random initial conditions. 
(ns marathon.analysis.runamc
  (:require
   [taa.capacity :as capacity]
   [marathon.analysis.random :as r]
   [marathon.analysis :as a]
   [marathon.analysis.util :as util]
   [marathon.analysis.experiment :as e]
   [marathon.analysis.nolh :as nolh]
   [marathon.ces.core :as c]
   [spork.util.table :as tbl]
   [spork.util.general :as gen]
   [clojure.spec.alpha :as s]))

(defn adjust-rc ;;new
  [rc-demand rec]
  (if (= (:DemandGroup rec) "RC_NonBOG-War")
    (assoc rec :Quantity rc-demand)
    rec))	

(defn rc-proj  ;;new
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
(defn rand-proj  ;;there is a proj2 here now.
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

(defn ac-rc-supply-reduction-experiments  ;;new
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

(defn project->experiments-ac-rc  ;;new
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  This function is modified so we can look at AC supply levels above the
  current inventory."
  [prj lower upper & {:keys [levels step] :or {step 1}}]
  (for [tbls (ac-rc-supply-reduction-experiments (:tables prj) lower upper
                  :step step :levels (:levels prj))]
    (assoc prj :tables tbls)))
