(ns dataorigin.core
  (:gen-class))

(defrecord Data [value origin])

(defn make-data [value origin]
  (Data. value origin))

(defn data? [object]
  (isa? (class object) Data))

(defn unwrap [data]
  (if (data? data)
    (:value data)
    data))

(defn origin
  ([data]
   (origin data {:origin-kind ::unknown-origin}))
  ([data default]
   (if (data? data)
     (:origin data)
     default)))

(defn combined-origin [& origins]
  {:origin-kind ::combined-origin :origins origins ::modified true})
(defn modify-origin [origin]
  (assoc origin ::modified true))

(defn apply-data [f data & args]
  (if (data? data)
    (make-data (apply f (unwrap data) args) (modify-origin (origin data)))
    (apply f data args)))
