(ns goals.app
  (:require-macros [hiccups.core :as hiccups :refer [html]])
  (:require [oops.core :refer [oget oset! ocall ocall!]]
            [hiccups.runtime :as hiccupsrt]
            [clojure.string :refer [join split split-lines trim]]))

(defn text->input [t]
  (let [associations (->> (split-lines t)
                          (filter #(-> % trim count (> 0)))
                          (map #(->> (split % "=>")
                                     (map trim))))]
    (->> (rest associations)
         (reduce (fn [m [par child]] (if child (assoc m child par)
                                         (assoc m par child))) {})
         (#(reduce (fn [m [k v]] (if (m v) m (if v (assoc m v nil) (assoc m k nil)))) % %))
         (reduce (fn [coll [k v]] (conj coll {:id (hash k) :parent (if v (hash v) nil) :text k})) [])
         (cons {:text (-> associations first first)}))))

(defn add-on [collfn addfn coll data]
  (let [par-ids (mapv :id (collfn coll))]
    (->> par-ids
         (into #{})
         (#(filter (fn [{par :parent}] (% par)) data))
         (sort-by :parent (fn [p1 p2] (compare (.indexOf par-ids p1) (.indexOf par-ids p2))))
         (#(if (empty? %) coll (add-on collfn addfn (addfn coll %) data))))))

(defn addLeft [coll data]
  (add-on first (fn [c els] (into [els] c)) coll data))

(defn addRight [coll data]
  (add-on last (fn [c els] (conj c els)) coll data))

(defn ->mindmap [data]
  (let [root (->> data (filter #(nil? (:id %))) first)
        top-vertices (->> data (filter #(and (:id %) (nil? (:parent %)))))]
    (-> (conj [] (sort :id (take-nth 2 (rest top-vertices))))
        (conj [root])
        (conj (sort :id (take-nth 2 top-vertices)))
        (addLeft data)
        (addRight data))))

(defn column->div [column num]
  (apply conj
         [:div {:id (str "div" num) :class "f-col"}]
         (map (fn [{id :id t :text}] [:p {:id (str "p" id)} t]) column)))

(-> (count (range 6)) ((fn [x] (- x 1 (mod x 2)))) (/ 2) ((partial - 0)))

(hiccups/defhtml create-mindmap-html [data]
  (->> (->mindmap data)
       ((juxt identity
              #(mapv (fn [x] (-> (count %) dec ((fn [x] (- x (mod x 2)))) (/ 2) ((partial - x)))) (range (count %)))))
       ((fn [[coll idx]] (map-indexed #(column->div %2 (idx %1)) coll)))))

(defn gelid [id]
  (ocall js/document "getElementById" id))

(defn add-mindmap [data]
  (oset! (gelid "container") "innerHTML" (create-mindmap-html data)))

(defn after! [parent element]
  (ocall! parent "after" element))

(defprotocol DomRect
  (top [r])
  (height [r])
  (left [r])
  (width [r]))

(defn dom-rect [elid]
  (let [rect (ocall (gelid elid) "getBoundingClientRect")]
    (reify DomRect
      (top [_] (oget rect "top"))
      (height [_] (oget rect "height"))
      (left [_] (oget rect "left"))
      (width [_] (oget rect "width")))))

(defn +width [x r]
  (+ x (width r)))

(defn pathx [rect left? shift]
  (-> (left rect) (- shift) (#(if left? (+width % rect) %))))

(defn pathy [rect shift]
  (-> rect top (- shift) (+ (-> rect height (/ 2)))))

(defn mid [x y] (-> (+ x y) (/ 2)))

(defn curved [sx sy ex ey]
  (str "M" sx "," sy
       " C " (mid sx ex) "," sy " " (mid sx ex) "," ey " " ex "," ey))

(defn path-curve [eid1 eid2 {shift-left :left shift-top :top}]
  (let [rect1 (dom-rect eid1) rect2 (dom-rect eid2)
        first-to-the-left-of (< (left rect1) (left rect2))
        startx (pathx rect1 first-to-the-left-of shift-left)
        endx (pathx rect2 (not first-to-the-left-of) shift-left)
        starty (pathy rect1 shift-top)
        endy (pathy rect2 shift-top)]
    (curved startx starty endx endy)))

(defn sattr! [el k v]
  (ocall! el "setAttribute" k v))

(defn draw-svg [[attrs children]]
  (println "drawing")
  (let [svg (ocall js/document "createElementNS" "http://www.w3.org/2000/svg" "svg")]
    (ocall! svg "setAttributeNS" "http://www.w3.org/2000/xmlns/" "xmlns:xlink" "http://www.w3.org/1999/xlink")
    (doseq [[k v] attrs]
      (case k
        :style (sattr! svg "style" (reduce (fn [s [k v]] (join [s (name k) \: v ";"])) "" v))
        :viewBox (sattr! svg "viewBox" (let [{:keys [x y width height]} v] (join " " [x y width height])))
        :parent nil
        (sattr! svg (name k) v)))
    (after! (first (ocall js/document "getElementsByTagName" "body")) svg)
    (doseq [child children]
      (let [path (ocall js/document "createElementNS" "http://www.w3.org/2000/svg" "path")]
        (doseq [[k v] child] (sattr! path (name k) v))
        (ocall! svg "appendChild" path)))
    svg))

(defn px [x] (str x "px"))

(defn edges [data]
  (let [mC (dom-rect "container")]
    [{:id "mySVG"
      :style (reduce (fn [m [k v]] (assoc m k (px v))) {}
                     {:top (top mC) :left (left mC) :width (width mC) :height (height mC)})
      :viewBox {:x 0 :y 0 :width (width mC) :height (height mC)}
      :parent :body}
     (->> data
          (filter :id)
          (map (fn [{:keys [id parent]}]
                 {:id (str id "-" (if parent parent "root"))
                  :class "path"
                  :d (path-curve (str "p" id)
                                 (if parent (str "p" parent) "p")
                                 {:left (left mC) :top (top mC)})})))]))

(def text-input
  (str "Goals\n"
       "Dream goal 1 => Long term goal 1\n"
       "Long term goal 1 => Short term goal 11\n"
       "Long term goal 1 => Short term goal 12\n"
       "Short term goal 11 => Immediate goal 11\n"
       "Short term goal 12 => Immediate goal 12\n"

       "Dream goal 2 => Long term goal 2\n"
       "Long term goal 2 => Short term goal 2\n"
       "Short term goal 2 => Immediate goal 2\n"

       "Dream goal 3 => Long term goal 3\n"
       "Long term goal 3 => Short term goal 3\n"
       "Short term goal 3 => Immediate goal 31\n"
       "Short term goal 3 => Immediate goal 32\n"

       "Dream goal 4 => Long term goal 41\n"
       "Dream goal 4 => Long term goal 42\n"
       "Long term goal 41 => Short term goal 41\n"
       "Long term goal 42 => Short term goal 42\n"
       "Short term goal 41 => Immediate goal 41\n"
       "Short term goal 42 => Immediate goal 42\n"))

(defn add-textarea [id]
  (let [ta (ocall js/document "createElement" "textarea")]
    (sattr! ta "id" id)
    (sattr! ta "style" "width:80%;height:200px;margin-left:10%")
    (set! (.-value ta) text-input)
    (after! (first (ocall js/document "getElementsByTagName" "body")) ta)
    ta))

(defn get-input [id]
  (oget (gelid id) "value"))

(defn on-resize [id f]
  (-> (js/ResizeObserver. (fn [_] (f)))
      (ocall! "observe" (gelid id))))

(def rObs (atom nil))

(defn redraw
  ([] (redraw (text->input (get-input "input-field"))))
  ([input]
   (when-let [mySVG (gelid "mySVG")] (ocall! mySVG "remove"))
   (draw-svg (edges input))))

(defn on-input-change [e]
  (let [input (text->input (.-value (.-target e)))]
    (add-mindmap input)
    (redraw input)))

(defn init []
  (let [ta (add-textarea "input-field")]
    (reset! rObs (on-resize "container" redraw))
    (add-mindmap (text->input (get-input "input-field")))
    (ocall! ta "addEventListener" "input" on-input-change)))

(init)
