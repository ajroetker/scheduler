(ns scheduler.core-test
  (:require [clojure.pprint :refer :all]
            [clojure.test :refer :all]
            [scheduler.core :refer :all]))

(def professors
  ["Keith Karoly"
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
   "Ellen Millender"
   "Wally Englert"
   "Lois Kent"
   "Diana Wayne"
   "Jeff Parker"])

(def prof-dept-map-reference
  {"Keith Karoly" "Economics" "Jim Fix" "Mathematics"
   "Suzy Renn" "Economics" "Angelina Jolie" "Biology"
   "Paul Hovda" "Economics" "Jamie Pommershiem" "Biology"
   "Bruce Wayne" "Physics" "Clark Kent" "Sociology"
   "Diana Wayne" "Physics" "Lois Kent" "Sociology"
   "Wally West" "Anthropology" "Sonia Sabnis" "Classics"
   "Ellen Millender" "Anthropology" "Wally Englert" "Classics"
   "Tom Weiting" "Biology"  "Jeff Parker" "Mathematics"})

(def primary-orals-boards
  [{:student "Toni Scarcello" :first "Keith Karoly" :second "Suzy Renn" :third "Jim Fix" :dept "Economics"}
   {:student "Joseph Rennie" :first "Paul Hovda" :second "Keith Karoly" :dept "Economics"}
   {:student "Robert Redford" :first "Paul Hovda" :second "Suzy Renn" :dept "Economics"}
   {:student "Matthew Olsen" :first "Angelina Jolie" :second "Jamie Pommershiem" :dept "Biology"}
   {:student "Dean Schmeltz" :first "Paul Hovda" :second "Keith Karoly" :dept "Economics"}
   {:student "Heather Hambley" :first "Sonia Sabnis" :second "Wally Englert" :dept "Classics"}
   {:student "Alex Ledger" :first "Bruce Wayne" :second "Diana Wayne" :dept "Physics"}
   {:student "Jacob Canter" :first "Clark Kent" :second "Lois Kent" :dept "Sociology"}
   {:student "Emma Furth" :first "Diana Wayne" :second "Bruce Wayne" :dept "Physics"}
   {:student "Marni Cohen" :first "Angelina Jolie" :second "Tom Weiting" :dept "Biology"}
   {:student "Torrey Payne" :first "Tom Weiting" :second "Jamie Pommershiem" :dept "Mathematics"}
   {:student "Andrew Roetker" :first "Jim Fix" :second "Jeff Parker" :dept "Mathematics"}])

(deftest overall-functionality-test
  (-> (into [] (week-builder primary-orals-boards 2 3 4))
      rand-nth
      (add-third-reader-pools prof-dept-map-reference 3 2 3 4)
      (schedule-week 2 3)
      assemble-week
      pprint))
