(ns advent-code.problem.day20-2020
  (:require [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [clojure.set :as set]))

(defn parse-tile [raw-tile]
  (let [[tile-header & raw-tiles] (dh/split-lines raw-tile)
        tile-id (Integer/parseInt (re-find #"\d+" tile-header))]
    {:id tile-id :tiles (mapv vec raw-tiles) :transform :id}))

(defn parse-data [raw]
  (let [split-tiles (dh/split-paragraphs raw)]
    (map parse-tile split-tiles)))

(defn edges-for-tile [tile]
  {:top (first (:tiles tile))
   :bottom (last (:tiles tile))
   :left (map first (:tiles tile))
   :right (map last (:tiles tile))})

(defn populate-edges-for-tiles [tiles]
  (map (fn [tile] (into (dissoc tile :tiles) (edges-for-tile tile))) tiles))

; top 1-> 4
; left: 1->2
; right: 4->3
; bottom: 2->3

(defn rotate-90 [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :rotate-90
     :top right
     :right (reverse bottom)
     :bottom left
     :left (reverse top)}))

(defn rotate-180 [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :rotate-180
     :top (reverse bottom)
     :right (reverse left)
     :bottom (reverse top)
     :left (reverse right)}))

(defn rotate-270 [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :rotate-270
     :top (reverse left)
     :right top
     :bottom (reverse right)
     :left bottom}))

(defn flip-horizontal [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :horizontal
     :top bottom
     :right (reverse right)
     :bottom top
     :left (reverse left)}))

(defn flip-vertical [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :vertical
     :top (reverse top)
     :right left
     :bottom (reverse bottom)
     :left right}))

(defn flip-diagonal-1 [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :diag1
     :top left
     :right bottom
     :bottom right
     :left top}))

(defn flip-diagonal-2 [tile]
  (let [{:keys [id top bottom left right]} tile]
    {:id id
     :transform :diag2
     :top (reverse right)
     :right (reverse top)
     :bottom (reverse left)
     :left (reverse bottom)}))

(defn transformation-set-tile [tile]
  [tile
   (rotate-90 tile)
   (rotate-180 tile)
   (rotate-270 tile)
   (flip-horizontal tile)
   (flip-vertical tile)
   (flip-diagonal-1 tile)
   (flip-diagonal-2 tile)])

(defn edge-to-tile-map [populated-tiles]
  (apply (partial merge-with set/union)
         (for [tile populated-tiles
               dir [:top :right :bottom :left]]
           {[dir (dir tile)] #{[(:id tile) (:transform tile)]}})))

(defn create-id-to-tile-map [tiles]
  (reduce (fn [m t] (assoc m [(:id t) (:transform t)] t))
          {}
          tiles))

(def match-sides
 {:left :right
  :right :left
  :top :bottom
  :bottom :top})

; Know this won't work if there are multiple options :-)
(defn walk-tiles [parsed-tiles path start-tile-id]
  (let [tiles (populate-edges-for-tiles parsed-tiles)
        tile-map (create-id-to-tile-map tiles)
        edge-map (edge-to-tile-map tiles)]
    (loop [rem-path path cur-path [1951] cur-tile-id start-tile-id]
      (println "Looking for next candidate" rem-path cur-path cur-tile-id)
      (cond (nil? cur-tile-id) nil
            (empty? rem-path) cur-path
            :else
            (let [tile (tile-map cur-tile-id)
                  [cur-dir & rest-path] rem-path
                  target-edge ((tile :edges) cur-dir)
                  candidate-tiles (edge-map [(match-sides cur-dir) target-edge])]
              (println "Hopefull found something here" (match-sides cur-dir) target-edge candidate-tiles)
              (if (> (count candidate-tiles) 1)
                  (println "oh no no no no")
                  (recur rest-path (conj cur-path (first candidate-tiles)) (first candidate-tiles))))))))

(defn empty-coordinate-map [n]
  (into (sorted-map)
    (for [x (range n)
          y (range n)]
      {[x y] nil})))

(defn generate-adjacency [[x y]  n]
  (filter (fn [[x y _]]
            (and (< -1 x n) (< -1 y n)))
          [[(- x 1) y :left]
           [(+ x 1) y :right]
           [x (+ y 1) :top]
           [x (- y 1) :bottom]]))

(defn tile-ids-in-map [tile-map]
  (set (map :id (filter associative? (vals tile-map)))))

(defn possible-fits [tile-map tile-info coord]
  ;(println "searching possible fits for" coord)
  (let [adj-set (generate-adjacency coord (tile-info :n))
        edge-to-tile-map (tile-info :edge-map)
        placed-tiles (tile-ids-in-map tile-map)]
    (filter
      (fn [[id transform]] (not (contains? placed-tiles id)))
      (apply set/union
        (for [[x y dir] adj-set
              :let [con-tile (tile-map [x y])
                    recip-dir (match-sides dir)]
              :when (some? con-tile)]
          (edge-to-tile-map [dir (con-tile recip-dir)]))))))

(defn populate-next-tile [cur-map tile-info]
  ;(println "populating" cur-map)
  (let [{:keys [all-tiles tile-id-map edge-map]} tile-info
        [next-value _] (first (filter (fn [[k v]] (nil? v)) cur-map))
        candidates (if next-value (possible-fits cur-map tile-info next-value))]
    ;(println "candidates for" next-value candidates)
    (cond (nil? next-value) cur-map
          (empty? candidates) nil
          :else (first (filter some? (map #(populate-next-tile (assoc cur-map next-value (tile-id-map %)) tile-info) candidates))))))

(defn gen-tile-info [tiles]
  (let [pop-tiles (populate-edges-for-tiles tiles)
        all-tiles (mapcat transformation-set-tile pop-tiles)]
    {:pop-tiles pop-tiles
     :all-tiles all-tiles
     :tile-id-map (create-id-to-tile-map all-tiles)
     :edge-map (edge-to-tile-map all-tiles)
     :n (Math/sqrt (count tiles))}))

(defn solve-map [tiles]
  (let [tile-info (gen-tile-info tiles)
        empty-map (empty-coordinate-map (:n tile-info))]
    (first (filter some?
             (map #(populate-next-tile (assoc empty-map [0 0] %) tile-info)
                  (:all-tiles tile-info))))))

(defn multiply-corners [tile-map]
  (let [n (- (Math/sqrt (count tile-map)) 1)]
    (apply * (map #(get-in tile-map [% :id]) [[0 0] [0 n] [n 0] [n n]]))))

(defmethod ifaces/run-problem ["day20-2020" "1"] [x y z]
  (let [solution (solve-map (parse-data z))]
    (multiply-corners solution)))
