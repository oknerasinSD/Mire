(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *money*)
(def ^:dynamic *loot*)

(def prompt "> ")
(def streams (ref {}))
(def health (ref {}))
(def scores (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))
