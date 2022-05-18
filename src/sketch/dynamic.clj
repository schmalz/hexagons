(ns sketch.dynamic
  (:require [clojure.pprint :as pp]
            [quil.core :as q]))

(def colours
  
  ; Use saturation and brightness = 90.
  {:yellow-green-brown {:bg-hue-low 60
                        :bg-hue-high 90
                        :shape-fill 30}
   
  ; Use saturation = 6, brightness = 100.
   :floral-white {:bg-hue-low 160
                  :bg-hue-high 280
                  :shape-fill 40}
   
   ; Use saturation = 32, brightness = 100.
   :navajo-white {:bg-hue-low 276
                  :bg-hue-high 156
                  :shape-fill 36}
   
   ; Use saturation = brightness = 99.
   :purples {:bg-hue-low 227
             :bg-hue-high 287
             :shape-fill 54}})

(defonce ^:private cos-0 (q/cos (q/radians 0.0)))

(defonce ^:private sin-0 (q/sin (q/radians 0.0)))

(defonce ^:private cos-60 (q/cos (q/radians 60.0)))

(defonce ^:private sin-60 (q/sin (q/radians 60.0)))

(defonce ^:private cos-120 (q/cos (q/radians 120.0)))

(defonce ^:private sin-120 (q/sin (q/radians 120.0)))

(defonce ^:private cos-180 (q/cos (q/radians 180.0)))

(defonce ^:private sin-180 (q/sin (q/radians 180.0)))

(defonce ^:private cos-240 (q/cos (q/radians 240.0)))

(defonce ^:private sin-240 (q/sin (q/radians 240.0)))

(defonce ^:private cos-300 (q/cos (q/radians 300.0)))

(defonce ^:private sin-300 (q/sin (q/radians 300.0)))

(defn initialise
  []
  (q/smooth)
  (q/color-mode :hsb 360 100 100 1.0))

(defn paint-background
  [hue-low hue-high]
  (q/no-stroke)
  (doseq [y (range 0 1000 3)]
    (q/fill (q/map-range y 0 1000 hue-low hue-high)
            90
            99)
    (q/rect 0 y 1600 3)))

(defn paint-hexagon
  [hue]
  (q/stroke hue 15 100)
  (q/no-fill)
  (q/begin-shape)
  (q/vertex (* 10 cos-0)
            (* 10 sin-0))
  (q/vertex (* 10 cos-60)
            (* 10 sin-60))
  (q/vertex (* 10 cos-120)
            (* 10 sin-120))
  (q/vertex (* 10 cos-180)
            (* 10 sin-180))
  (q/vertex (* 10 cos-240)
            (* 10 sin-240))
  (q/vertex (* 10 cos-300)
            (* 10 sin-300))
  (q/end-shape :close))

(defn paint-hexagons
  [hue]
  (dotimes [_ 99]
    (q/with-translation [(q/random 0 (q/width)) (q/random (q/height))]
      (paint-hexagon hue))))

(defn save-frame-to-disk
  ([]
   (q/save-frame (pp/cl-format nil
                               "frames/~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-####.jpeg"
                               (q/year) (q/month) (q/day)
                               (q/hour) (q/minute) (q/seconds))))
  ([state _]
   (save-frame-to-disk)
   state))

(defn draw
  []
  (q/no-loop)
  (let [colour :purples]
    (paint-background (get-in colours [colour :bg-hue-low])
                    (get-in colours [colour :bg-hue-high]))
    (paint-hexagons (get-in colours [colour :shape-fill])))
  (save-frame-to-disk))
