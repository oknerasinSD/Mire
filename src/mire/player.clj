(ns mire.player)

(def ^:dynamic *current-room*)
(def ^:dynamic *inventory*)
(def ^:dynamic *name*)
(def ^:dynamic *money*)
(def ^:dynamic *loot*)
(def ^:dynamic *arrows* (ThreadLocal.))
(def ^:dynamic *bullet* (ThreadLocal.))

(def damage 25)
(def prompt "> ")
(def streams (ref {}))
(def health (ref {}))
(def score (ref {}))
(def lives (ref {}))

(defn carrying? [thing]
  (some #{(keyword thing)} @*inventory*))

(defn change-points [points]
  "Add points to current player"
  (dosync
    (commute score assoc *name* (+ (@score *name*) points))))

(defn set-health-value [target value]
  "Set player's health value"
  (dosync
    (if (contains? @health target)
      (do
         (commute health assoc target value)
          true
      )
      false
    )
  )
)

(defn overhealed []
  "Check if player's health is over 100"
   (dosync
     (if (> (@health *name*) 100)
       (set-health-value *name* 100)
     )
   )
)
