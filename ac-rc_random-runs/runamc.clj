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

;;hack in a patch for now
(ns marathon.analysis.random)
(defn adjust-rc ;;new
  [rc-demand rec]
  (if (= (:DemandGroup rec) "RC_NonBOG-War")
    (assoc rec :Quantity rc-demand)
    rec))	

(defn rc-record
  "Returns the rc-record in the project.  Throw an error if we don't
  find an rc record because we need the rc record for
  rc-unavailability so we should have 1 RC record for every SRC, even
  if the quantity is 0.  The project has been filtered down to one SRC
  only at this point."
  [proj]
  (let [rec (->> proj
                 :tables
                 :SupplyRecords
                 tbl/table-records
                 (filter #(= "RC" (:Component %)))
                 first)]
    (if rec
      rec
      (throw (Exception. (str "There was no RC record."))))))

(defn get-rc-unavailable
  "Returns the :rc-unavailable from the SupplyRecord Tags.  If
  :rc-unavailable does not exist, throw an exception because we should
  have set that beforehand during preprocessing when calling this."
  [{:keys [rc-unavailable]}]
  (if rc-unavailable
    rc-unavailable
    ;;Careful!! This won't throw 
       (throw (Exception. (str ":rc-unavailable missing from
  SupplyRecord Tags.")))))

(defn rc-proj  ;;new
  [proj]
  (let [{:keys [Quantity Tags]} (rc-record proj) 
        percent (get-rc-unavailable Tags)
        rc-demand (int (* Quantity percent))]
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
         (mapcat #(rand-recs % supply-record-randomizer))
         tbl/records->table
         (assoc-in proj2 [:tables :SupplyRecords]))))

