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

(defn save-frame-to-disk
  ([]
   (q/save-frame (pp/cl-format nil
                               "frames/~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-####.jpeg"
                               (q/year) (q/month) (q/day)
                               (q/hour) (q/minute) (q/seconds))))
  ([state _]
   (save-frame-to-disk)
   state))

(defn paint-background
  "Paint the background; a gradient from HUE-LOW to HUE-HIGH."
  [hue-low hue-high]
  (q/no-stroke)
  (doseq [y (range 0 1000 3)]
    (q/fill (q/map-range y 0 1000 hue-low hue-high) 90 99)
    (q/rect 0 y 1600 3))
  (q/display-filter :blur))

(defn paint-hexagon
  "Paint a hexagon centered on X, Y."
  [[x y]]
  (q/with-translation [x y]
    (q/begin-shape)
    (q/vertex (* 10 cos-0) (* 10 sin-0))
    (q/vertex (* 10 cos-60) (* 10 sin-60))
    (q/vertex (* 10 cos-120) (* 10 sin-120))
    (q/vertex (* 10 cos-180) (* 10 sin-180))
    (q/vertex (* 10 cos-240) (* 10 sin-240))
    (q/vertex (* 10 cos-300) (* 10 sin-300))
    (q/end-shape :close)))

(defn random-inside-margin
  "A random point within MARGIN inside DIMENSION."
  [dimension margin]
  (q/random margin (- dimension margin)))

(defn random-walk
  "An infinte, random sequence of x, y pairs, beginning at X, Y."
  [x y]
  (lazy-seq
   (let [r     (* 11 (q/random-gaussian))
         theta (q/random (* 2 q/PI))
         new-x (+ x (* r (q/cos theta)))
         new-y (+ y (* r (q/sin theta)))]
     (cons [x y] (random-walk new-x new-y)))))


(defn paint-hexagons
  "Paint M clusters of N hexagons using HUE for the stroke."
  [m n hue]
  (q/stroke hue 15 100)
  (q/no-fill)
  (dotimes [_ m]
    (dorun (map paint-hexagon
                (take n (random-walk (random-inside-margin (q/width) 10)
                                     (random-inside-margin (q/height) 10)))))))

(defn dots
  "Paint a string of N dots along a bezier curve defined by X1, Y1, CX1, CY1, CX2, CY2, X2, Y2"
  [n x1 y1 cx1 cy1 cx2 cy2 x2 y2]
  (dotimes [i n]
    (let [t (/ i n)
          x (+ (* 3 (q/random-gaussian)) (q/bezier-point x1 cx1 cx2 x2 t))
          y (+ (* 3 (q/random-gaussian)) (q/bezier-point y1 cy1 cy2 y2 t))]
      (q/ellipse x y 3 3))))

(defn braids
  "Paint P strings of N dots."
  [p n]
  (let [x1  (random-inside-margin (q/width) 10)
        y1  (random-inside-margin (q/height) 10)
        cx1 (random-inside-margin (q/width) 10)
        cy1 (random-inside-margin (q/height) 10)
        cx2 (random-inside-margin (q/width) 10)
        cy2 (random-inside-margin (q/height) 10)
        x2  (random-inside-margin (q/width) 10)
        y2  (random-inside-margin (q/height) 10)
        hue (q/random 0 21)]
    (q/stroke hue 65 80)
    (q/fill hue 65 80)
    (dotimes [_ p]
      (dots n x1 y1
            (+ cx1 (* 5 (q/random-gaussian))) (+ cy1 (* 5 (q/random-gaussian)))
            (+ cx2 (* 5 (q/random-gaussian))) (+ cy2 (* 5 (q/random-gaussian)))
            x2 y2))))

(defn paint-dots
  "Paint M braids of P strings of N blurred dots using HUE for the stroke and fill."
  [m p n]
  (dotimes [_ m]
    (braids p n))
  (q/display-filter :blur))

(defn draw
  []
  (q/no-loop)
  (let [colour :purples]
    (paint-background (get-in colours [colour :bg-hue-low]) (get-in colours [colour :bg-hue-high]))
    (paint-dots 3 5 99 #_(get-in colours [colour :shape-fill]))
    (paint-hexagons 2 999 (get-in colours [colour :shape-fill])))
  (save-frame-to-disk))
