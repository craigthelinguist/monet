(ns cols)


(defn- hex->dec [hex]
  (Integer/parseInt hex 16))


(defn- dec->hex [dec]
  (.toUpperCase (Integer/toHexString dec)))


(defn- abs [x]
  (if (< x 0) (- x) x))


(defn hex->rgb [hex]
  "Convert a colour from a hex literal to the corresponding RGB value. E.g. \"#FFFFFF\" -> [255 255 255]"
  (let [r (hex->dec (subs hex 1 3))
        g (hex->dec (subs hex 3 5))
        b (hex->dec (subs hex 5 7))]
    [r g b]))


(defn rgb->hex [rgb]
  "Convert an RGB colour value into the corresponding hex literal. E.g. [255 255 255] -> \"#FFFFFF\""
  (apply str (conj (map cols/dec->hex rgb) "#")))


(defn rgb->hsv [rgb]
  "Convert an RGB colour value into the corresponding HSV colour value."
  (let [[r' g' b']  (map #(/ % 255) rgb)
        cmax        (max r' g' b')
        cmin        (min r' g' b')
        delta       (- cmax cmin)
        value       cmax
        saturation  (if (zero? cmax) 0 (/ delta cmax))
        hue         (cond
                      (= delta 0) 0
                      (= cmax r') (* 60 (rem (/ (- g' b') delta) 6))
                      (= cmax g') (* 60 (+ (/ (- b' r') delta) 2))
                      (= cmax b') (* 60 (+ (/ (- r' g') delta) 4)))]
    [hue saturation value]))


(defn hsv->rgb [[hue saturation value]]
  "Convert an HSV colour value into the corresponding RGB colour value."
  (let [c (* value saturation)
        x (* c (- 1
                  (abs (-
                         (rem (/ hue 60) 2)
                         1))))
        m (- value c)
        [r' g' b'] (cond
                     (<= 0   hue  59) [c x 0]
                     (<= 60  hue 119) [x c 0]
                     (<= 120 hue 179) [0 c x]
                     (<= 180 hue 239) [0 x c]
                     (<= 240 hue 299) [x 0 c]
                     (<= 300 hue 359) [c 0 x])]

    [(* 255 (+ r' m))
     (* 255 (+ g' m))
     (* 255 (+ b' m))]))

