(ns scheduler.core-test
  (:require [clojure.pprint :refer :all]
            [clojure.test :refer :all]
            [scheduler.core :refer :all]))

(def professors
  {"Keith Karoly"
   "Jim Fix"
   "Suzy Renn"
   "Angelina Jolie"
   "Paul Hovda"
   "Jamie Pommershiem"
   "Bruce Wayne"
   "Clark Kent"
   "Wally West"
   "Sonia Sabnis"
   "Tom Weiting"
   "Jeff Parker"})

(def professors
  {"Keith Karoly" "Economics" "Jim Fix" "Mathematics"
   "Suzy Renn" "Economics" "Angelina Jolie" "Biology"
   "Paul Hovda" "Economics" "Jamie Pommershiem" "Biology"
   "Bruce Wayne" "Physics" "Clark Kent" "Sociology"
   "Wally West" "Anthropology" "Sonia Sabnis" "Classics"
   "Tom Weiting" "History"  "Jeff Parker" "Mathematics"})

(def primary-orals-boards
  [{:student "Toni Scarcello" :first "Keith Karoly" :second "Suzy Renn" :dept "Economics"}
   {:student "Joseph Rennie" :first "Paul Hovda" :second "Angelina Jolie" :dept "Economics"}
   {:student "Robert Redford" :first "Paul Hovda" :second "Suzy Renn" :dept "Economics"}
   {:student "Matthew Olsen" :first "Angelina Jolie" :second "Jim Fix" :dept "Economics"}
   {:student "Dean Schmeltz" :first "Paul Hovda" :second "Angelina Jolie" :dept "Psychology"}
   {:student "Heather Hambley" :first "Sonia Sabnis" :second "Wally West" :dept "Psychology"}
   {:student "Alex Ledger" :first "Bruce Wayne" :second "Clark Kent" :dept "Psychology"}
   {:student "Jacob Canter" :first "Clark Kent" :second "Jamie Pommershiem" :dept "Sociology"}
   {:student "Emma Furth" :first "Jim Fix" :second "Bruce Wayne" :dept "Sociology"}
   {:student "Marni Cohen" :first "Jim Fix" :second "Tom Weiting" :dept "Sociology"}
   {:student "Torrey Payne" :first "Jeff Parker" :second "Jamie Pommershiem" :dept "Mathematics"}
   {:student "Andrew Roetker" :first "Jim Fix" :second "Tom Weiting" :dept "Mathematics"}])

(deftest overall-functionality-test
  (let [week (rand-nth (into [] (week-builder primary-orals-boards 2 3 4)))
        week-with-pools (add-third-reader-pools week professors 4 2 3 4)]
  (pprint (schedule-week week-with-pools 2 3))))
