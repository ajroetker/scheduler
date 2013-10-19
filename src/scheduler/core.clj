(ns scheduler.core
  (:require [clojure.pprint :refer :all]
            [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.math.combinatorics :refer (combinations permutations)]
            [clojure.set :refer :all])
  (:gen-class))

(comment

  "These functions assume a collection of mappings
  and a list of proffesors.
  The mappings are a 'primary orals board' that
  looks like the following..."

  {:student "John Doe" :first "Some Prof" :second "Other Prof" :department "Major Dept."}

  )

(def prof-dept-map
  {"Keith Karoly" "Economics" "Jim Fix" "Mathematics"
   "Suzy Renn" "Economics" "Angelina Jolie" "Biology"
   "Paul Hovda" "Economics" "Jamie Pommershiem" "Biology"
   "Bruce Wayne" "Physics" "Clark Kent" "Sociology"
   "Diana Wayne" "Physics" "Lois Kent" "Sociology"
   "Wally West" "Anthropology" "Sonia Sabnis" "Classics"
   "Ellen Millender" "Anthropology" "Wally Englert" "Classics"
   "Tom Weiting" "Biology"  "Jeff Parker" "Mathematics"})

(defn pcross
  [& seqs]
  (when (every? (complement empty?) seqs)
    (let [s1 (first seqs)]
      (if-let [other-seqs (next seqs)]
        (let [l1 (count s1)
              [half1 half2] (split-at (quot l1 2) s1)
              xrest (apply pcross other-seqs)
              f1 (future (for [x half1 yz xrest] (cons x yz)))
              f2 (future (for [x half2 yz xrest] (cons x yz)))]
          (concat @f1 @f2))
        (map list s1)))))

(defn inc-cnt-in-map
  [m k]
  (assoc m k ((fnil inc 0) (m k))))

(defn remove-nil-keys
  [m]
  (into {} (filter (complement #((comp nil? key) %)) m)))

(defn num-third-readers
  [orals]
  (reduce (fn [accum oral] (if (:third oral) (inc accum) accum)) 0 orals))

(defn equal-sizes
  [c1 c2]
  (= (count c1) (count c2)))

(defn reducer-counter-prof-orals
  "Given a map of professors to integer values and an orals board
  increment the values in the map of the profs in that orals board"
  [professor-orals-count {:keys [first second third] :as orals-board}]
  (-> professor-orals-count
      (inc-cnt-in-map first)
      (inc-cnt-in-map second)
      (inc-cnt-in-map third)))

(defn profs-count
  "Return a map of the profs to their number of occrences
  in a list of orals boards"
  [orals-boards]
  ;; Filter out nil values which come from the destructuring in the reducer
  (remove-nil-keys (reduce reducer-counter-prof-orals {} orals-boards)))

(defn profs-count-day
  "Returns a map of the number of orals a prof has left
  according to a current prof count and a day of orals"
  [day profs-count-so-far]
  (let [prof-count-day (profs-count day)
        add-prof (fn [pool [prof-name avail-before]]
                   (let [avail-today (- avail-before (or (prof-count-day prof-name) 0))]
                     ;; BROKENESSSS := if the number of slots is less than three, fucked up shit
                     ;; ......annnnd profs get assigned more than limit atm
                     (assoc pool prof-name (if (> 3 avail-today) avail-today 3))))]
    (reduce add-prof {} profs-count-so-far)))

(defn profs-count-week
  "Returns a map of the number of orals a prof has left"
  [week profs limit]
  (let [taken-pfs (profs-count (apply concat week))
        add-prof (fn [pool [prof dept]] (assoc pool prof (- limit (or (taken-pfs prof) 0))))]
    (reduce add-prof {} profs)))

(defn students-that-week
  "The set of students that week"
  [week]
  (set (apply concat week)))

(defn filter-period
  "Return all combinations which don't have a prof
  going more than n times a day"
  [period n]
  (filter (complement #(some (fn [m] (< n (val m))) (profs-count %))) period))

(defn make-add-day
  [students rooms time-slots]
  (fn [week]
    (let [left-over-students (difference (set students) (students-that-week week))
          new-days (filter-period (combinations left-over-students (* rooms time-slots)) 3)]
      (if (empty? new-days)
        (list (conj week (into [] left-over-students)))
        (for [day new-days] (conj week day))))))

(defn week-builder-helper
  [students week-accum rooms time-slots days]
  (if (or (= days 0) (empty? students))
    (into #{} (map #(filter (complement empty?) %) (set week-accum)))
    (let [add-day (make-add-day students rooms time-slots)]
      (week-builder-helper students (mapcat add-day week-accum) rooms time-slots (dec days)))))

(defn week-builder
  "Returns a set of all the possible weeks of orals
  where each week is a set of days and each day is a set of students orals"
  [students rooms time-slots days]
  (week-builder-helper students [#{}] rooms time-slots days))

(defn pf-map-to-pool
  [prof-map]
  (mapcat #(for [_ (range (val %))] (key %)) prof-map))

(defn pf-pool-to-map
  [prof-list]
  (reduce #(assoc %1 %2 ((fnil inc 0) (%1 %2))) {} prof-list))

(defn update-profs
  "Given a prof map and a pool of professors for a day
  update the map the reflect the new day pool"
  [prof-map profs-pool]
  (if-let [prof-name (first profs-pool)]
    (let [after-update (update-profs prof-map (rest profs-pool))]
     (assoc after-update prof-name (dec (after-update prof-name))))
    prof-map))

(defn reducer-counter-dept
  "Counts the number of orals a dept is on
  for a reducer function"
  ([dept-orals-count {:keys [dept] :as orals-board}]
   (inc-cnt-in-map dept-orals-count dept))
  ([dept-map dept-orals-count prof]
   (inc-cnt-in-map dept-orals-count (dept-map prof))))

(defn dept-count
  "When we have a list of profs and need their dept"
  ([orals-boards]
   (remove-nil-keys (reduce reducer-counter-dept {} orals-boards)))
  ([period-pool dept-map]
   (remove-nil-keys (reduce (partial reducer-counter-dept dept-map) {} period-pool))))

(defn dept-filter-period
  [period period-pf-pools]
  (let [dept-day-count (dept-count period)
        make-dept-pool-count (fn [period-pool] (dept-count period-pool prof-dept-map))
        make-merged-pf-maps (fn [period-pool] (merge-with + dept-day-count (make-dept-pool-count period-pool)))]
    (filter (complement #(some (fn [[k v]] (< (count period) v)) (make-merged-pf-maps %))) period-pf-pools)))

(defn third-reader-accum
  [week prof-map full-week-accum]
  (if-let [day (first week)]
    (let [day-pool-map (profs-count-day day prof-map)
          day-pool-list (pf-map-to-pool day-pool-map)
          orals-wo-third (- (count day) (num-third-readers day))
          day-pools (combinations day-pool-list orals-wo-third)
          ;;Here's where we would filter for dept
          day-pools-filtered (dept-filter-period day day-pools)
          random-day-pool (if (empty? day-pools-filtered) '() (rand-nth day-pools-filtered))
          updated-prof-map (update-profs prof-map random-day-pool)
          updated-accum (conj full-week-accum [day random-day-pool])]
      (third-reader-accum (rest week) updated-prof-map updated-accum))
    full-week-accum))

(defn add-third-reader-pools
  "Week is a set of sets, where each set is a day of orals"
  [week profs limit rooms time-slots days]
  (let [total-pool-map (profs-count-week week profs limit)]
    (filter (fn [[s p]] (and (not (empty? s)) (not (empty? p))))
            (third-reader-accum week total-pool-map #{}))))

;;;;;;;;;; The rest of the code assumes we have a
;;;;;;;;;; set of days with an associated pool of third readers for that day

(defn make-add-slots
  [students rooms time-slots]
  (fn [day]
    (let [left-over-students (difference (set students) (apply concat day))
          new-slots (filter-period (combinations left-over-students rooms) 1)]
      (if (empty? new-slots)
        (if (empty? left-over-students)
          (list day)
          (for [slot (filter-period [left-over-students] 1)] (conj day slot)))
        (for [slot new-slots] (conj day slot))))))

(defn full-days
  [days students-that-day]
  (filter (fn [period] (equal-sizes students-that-day (apply concat period))) days))


(defn day-builder-helper
  [students day-accum rooms time-slots]
  (if (or (= time-slots 0) (empty? students))
    day-accum
    (let [add-slots (make-add-slots students rooms time-slots)]
      (day-builder-helper students (mapcat add-slots day-accum) rooms (dec time-slots)))))

(defn day-builder
  [day rooms time-slots]
  (day-builder-helper day #{#{}} rooms time-slots))

(defn make-add-profs
  [slot profs-map rooms]
  (fn [day]
    (let [left-over-profs
          (pf-map-to-pool
            (if (empty? day)
              profs-map
              (update-profs profs-map (mapcat (comp first rest) day))))
          needed-thirds (- rooms (num-third-readers slot))
          valid-profs (filter #(= needed-thirds (count %)) (map set (combinations left-over-profs needed-thirds)))
          in-slot-already? (fn [s] (some (fn [v] (= s v)) (mapcat vals slot)))
          more-valid-profs (dept-filter-period slot valid-profs)
          new-profs (into #{} (filter (complement #(some true? (map in-slot-already? %))) more-valid-profs))]
      (if (empty? new-profs)
        (list day)
        (for [pool new-profs] (conj day [slot pool]))))))

(defn slots-with-pools-helper
  [day profs-map slot-accum]
  (if-let [slot (first day)]
    (let [orals-num (count slot)
          add-profs (make-add-profs slot profs-map orals-num)]
      (slots-with-pools-helper (rest day) profs-map (mapcat add-profs slot-accum)))
    slot-accum))

(defn slots-with-pools
  [day profs]
  (slots-with-pools-helper day (pf-pool-to-map profs) #{#{}}))

(defn schedule-week
  [week-profs-pools rooms time-slots]
  (for [[students profs] week-profs-pools]
    (let [full-slotted-day (rand-nth (into [] (full-days (day-builder students rooms time-slots) students)))]
      (first (filter (fn [day] (equal-sizes students (mapcat first day))) (slots-with-pools full-slotted-day profs))))))

(defn add-third-reader
  "Given a readers map of a student and the
  first and second readers, add the third"
  [readers faculty-member]
  (assoc readers :third faculty-member))

(defn grab-third-reader
  [orals prof]
  (if-let [oral (first orals)]
    (if (or (= (prof-dept-map prof) (:department oral)) (:third oral))
      (grab-third-reader (rest orals) prof)
      (add-third-reader oral prof)
      )))

(defn merge-third-readers-helper
  [orals profs accum]
  (if-let [pf (first profs)]
    (let [works (grab-third-reader orals pf)
          after-orals (filter #(not (= (:student works) (:student %))) orals)
          new-accum (conj accum works)]
      (merge-third-readers-helper after-orals (rest profs) new-accum))
    accum))

(defn merge-third-readers
  [orals-boards profs]
  (merge-third-readers-helper orals-boards profs []))

(defn assemble-week
  [week]
  (for [day week]
    (for [slot day]
      (merge-third-readers (first slot) (second slot)))))

(defn -main
  [& args]
  (if-let [config (first args)]
    (do
      (with-open [in-file (io/reader config)]
      (prn
        (csv/read-csv in-file))))
    (do
      (println "Need a data file!")
      (System/exit 1))))
