(ns sketch.core
  (:require [quil.core :as q]
            [sketch.dynamic :as dynamic])
  (:gen-class))

(def sketch)

(q/defsketch sketch
  :title "sketch"
  :setup dynamic/initialise
  :draw dynamic/draw
  :size [1458 901])

(defn refresh
  []
  (use :reload 'sketch.dynamic)
  (.loop sketch))
