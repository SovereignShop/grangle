(ns main.grangle.core
  (:require
   [sicmutils.env :as e]
   [sicmutils.generic :refer [dot-product magnitude negate cross-product]]
   [instaparse.core :as insta]
   [clojure.string :as s]
   [clojure.core.match :refer [match]])
  (:import
   [java.io BufferedReader FileReader FileWriter BufferedWriter]))

(def gcode-grammar
  "file = header line+

   header = flavor time filament-used layer-height
   flavor = <';FLAVOR:'>#'[a-zA-Z]+'<newline>
   time = <';TIME:'>#'[0-9]+' <newline>
   filament-used = <';Filament used: '> digit <'m'> <newline>
   layer-height = <';Layer height: '> digit <newline>

   <line> = (command <spaces> <comment>? | comment | layer | type | '') <newline>
   command = (letter digits <spaces>)+
   <letter> = 'G' | 'M' | 'X' | 'Y' | 'Z' | 'E' | 'I' | 'J' | 'K' | 'F' | 'S' | 'T' | 'P'
   <digits> = '-'? digit+
   <digit> = #'[-+]?\\d*\\.?\\d+([eE][-+]?\\d+)?'
   spaces = ' '*
   newline = #'\\n' | #'^$'
   comment = <';'>#'.*'
   layer = <';LAYER:'>#'[0-9]+'
   type = <';TYPE:'>#'[A-Z]+'")

(def gcode-header-grammar
  "header = flavor time filament-used layer-height
   flavor = <';FLAVOR:'>#'[a-zA-Z]+'<newline>
   time = <';TIME:'>#'[0-9]+' <newline>
   filament-used = <';Filament used: '> digit <'m'> <newline>
   layer-height = <';Layer height: '> digit <newline>

   <digits> = '-'? digit+
   <digit> = #'[-+]?\\d*\\.?\\d+([eE][-+]?\\d+)?'
   newline = #'\\n' | #'^$'
   spaces = ' '*")

(def gcode-command-grammar
  "command = (letter digits <spaces>)+
   <letter> = 'G' | 'M' | 'X' | 'Y' | 'Z' | 'E' | 'I' | 'J' | 'K' | 'F' | 'S' | 'T' | 'P'
   <digits> = '-'? digit+
   <digit> = #'[-+]?\\d*\\.?\\d+([eE][-+]?\\d+)?'
   spaces = ' '*")

(def header-parser (insta/parser gcode-header-grammar))
(def command-parser (insta/parser gcode-command-grammar))

(def parser (insta/parser gcode-grammar))

(defn parse-gcode [gcode-str]
  (insta/parse parser gcode-str))

(defn parse-double [x]
  (Double/parseDouble x))

(defn parse-int [x]
  (Integer/parseInt x))

(defn parse-header [header]
  (-> (into {} (next header))
      (update :layer-height parse-double)
      (update :time parse-int)
      (update :filament-used parse-double)))

(defn parse-command [line]
  (let [[name & args] (->> (insta/parse command-parser line)
                           (next)
                           (partition 2))]
    (->> args
         (map (fn [[letter digit]]
                (when (map-entry? digit)
                  digit)
                [(keyword letter) (parse-double digit)]))
         (into {:name (apply str name)}))))

(defn write-lines-to-file [filename lines]
  (with-open [writer (BufferedWriter. (FileWriter. filename))]
    (doseq [s lines]
      (.write writer s)
      (.newLine writer))))

(defn format-double [num]
  (format (str "%." 5 "f") (double num)))

(defn format-and-trim [num]
  (let [formatted (format (str "%." 5 "f") num)]
    (loop [i (- (count formatted) 1)]
      (cond
        (< i 0) formatted
        (or (= (.charAt formatted i) \0) (= (.charAt formatted i) \.))
        (recur (dec i))
        :else (subs formatted 0 (inc i))))))

(defn to-command [parsed-command]
  (let [sb (StringBuilder.)
        name (:name parsed-command)]
    (.append sb name)
    (.append sb \space)
    (when-let [f (:F parsed-command)]
      (doto sb (.append \F) (.append (format-and-trim f)) (.append \space)))
    (when-let [x (:X parsed-command)]
      (doto sb (.append \X) (.append (format-and-trim x)) (.append \space)))
    (when-let [y (:Y parsed-command)]
      (doto sb (.append \Y) (.append (format-and-trim y)) (.append \space)))
    (when-let [z (:Z parsed-command)]
      (doto sb (.append \Z) (.append (format-and-trim z)) (.append \space)))
    (when-let [e (:E parsed-command)]
      (doto sb (.append \E) (.append (format-and-trim e))))
    (str sb)))

(defn normalise [v]
  (e// v (magnitude v)))

(defn find-command-indices [commands start-idx command-names]
  (let [i (volatile! (- start-idx 1))
        s (volatile! (nth commands @i))]
    (vec (for [command-name command-names]
           (do (while (not (.startsWith @s command-name))
                 (vreset! i (dec @i))
                 (vreset! s (nth commands @i)))
               (let [ret @i]
                 (vreset! i (dec @i))
                 ret))))))

(defn buffered-reader-to-array [br]
  (let [lines (java.util.ArrayList.)]
    (loop [^String line (.readLine br)]
      (when line
        (.add lines line)
        (recur (.readLine br))))
    (into-array String lines)))

(defn parse-gcode-move [gcode-str]
  (let [x-index (.indexOf gcode-str "X")
        x-end   (.indexOf gcode-str " " (inc x-index))
        x-val   (subs gcode-str (inc x-index) x-end)

        y-index (.indexOf gcode-str "Y" x-end)
        y-end   (let [i (.indexOf gcode-str " " (inc y-index))]
                  (if (= i -1) (.length gcode-str) i))
        y-val   (subs gcode-str (inc y-index) y-end)]
    (double-array [(Double/parseDouble x-val) (Double/parseDouble y-val)])))

(defn find-pos [lines idx]
  (if (< idx 0)
    (throw (Exception. "No pos found. Index is negative"))
    (let [^String line (aget lines idx)]
      (if (or (.startsWith line "G1 X")
              (.startsWith line "G0 X"))
        ^doubles (parse-gcode-move line)
        (recur lines (dec idx))))))

(defn next-command-index
  ([lines idx]
   (if (>= idx (alength lines))
     nil
     (let [line (aget lines idx)]
       (if (.startsWith line ";")
         (recur lines (inc idx))
         idx))))
  ([lines idx command-name]
   (if (>= idx (alength lines))
     nil
     (let [line (aget lines idx)]
       (if (.startsWith line ";")
         (recur lines (inc idx) command-name)
         (if (.startsWith line command-name)
           idx
           (recur lines (inc idx) command-name)))))))

(defn process-gcode
  [& {:keys [filename coast-distance meshes min-retract-dists z-hop]}]
  (with-open [reader (BufferedReader. (FileReader. filename))]
    (let [header-lines (for [_ (range 4)]
                         (.readLine reader))
          {:keys [layer-height]}
          (parse-header
           (insta/parse header-parser (s/join "\n" header-lines)))
          lines (buffered-reader-to-array reader)]
      (loop [commands (transient (vec header-lines))
             mesh nil
             layer 0
             idx 0]
        (if (>= idx (alength lines))
          (persistent! commands)
          (let [^String line (aget lines idx)]
            (cond
              (.startsWith line "G10")
              (let [unretract-index (next-command-index lines (inc idx) "G11")]
                (if-not unretract-index
                  (recur (conj! commands line) mesh layer (inc idx))
                  (let [unretract-index (next-command-index lines (inc idx) "G11")
                        travel-dist (let [next-idx (next-command-index lines (inc idx) "G0")
                                          _ (when (> next-idx unretract-index)
                                              (throw (Exception. "walked passed unretract index!")))
                                          next-line (aget lines next-idx)
                                          ^doubles current-pos (find-pos lines idx)]
                                           (if (.startsWith next-line "G0")
                                             (let [^doubles next-pos (parse-gcode-move next-line)
                                                   dx (Math/abs (- (aget next-pos 0) (aget current-pos 0)))
                                                   dy (Math/abs (- (aget next-pos 1) (aget current-pos 1)))
                                                   dist (Math/sqrt (+ (Math/pow dx 2) (Math/pow dy 2)))]
                                               dist)
                                             (throw (Exception. "Unknown Error!"))))
                        skip-retract (when-let [min-dist (get min-retract-dists mesh)]
                                       (<= travel-dist min-dist))
                        last-line (nth lines (dec idx))
                        llast-line (nth lines (- idx 2))
                        is-coast (and (> layer 0)
                                      (contains? meshes mesh)
                                      (.startsWith last-line "G1 ")
                                      (.startsWith llast-line "G1 "))
                        commands (if is-coast
                                   (let [last-command (parse-command last-line)
                                         llast-command (parse-command llast-line)
                                         e2 (:E last-command)
                                         p2 [(:X last-command) (:Y last-command)]
                                         p1 [(:X llast-command) (:Y llast-command)]
                                         e1 (:E llast-command)
                                         direction (e/- p2 p1)
                                         distance (magnitude direction)
                                         new-extrude-length (- distance coast-distance)
                                         new-p (e/+ p1 (e/* (normalise direction) new-extrude-length))
                                         _ (magnitude (e/- new-p p1))
                                         extrusion (- e2 e1)
                                         new-e (e/+ e1 (* extrusion (/ new-extrude-length distance)))]
                                     (conj! (pop! commands) (to-command (assoc last-command :E new-e))))
                                   commands)

                        [cmds layer mesh]
                        (loop [cmds commands
                               i idx
                               new-layer layer
                               mesh mesh]
                          (if (> i unretract-index)
                            [cmds new-layer mesh]
                            (let [l (aget lines i)]
                              (cond
                                (and skip-retract (or (.startsWith l "G10") (.startsWith l "G11")))
                                (recur cmds (inc i) new-layer mesh)

                                (and z-hop
                                     (not skip-retract)
                                     (>= layer (or (:start-layer z-hop) 0))
                                     (<= layer (or (:end-layer z-hop) 0))
                                     (>= travel-dist (or (:min-dist z-hop) 0))
                                     (or (.startsWith l "G10")
                                         (.startsWith l "G11")))
                                (let [{:keys [speed height]} z-hop
                                      current-height (* new-layer layer-height)]
                                  (recur
                                   (if (= i idx)
                                     (-> cmds
                                         (conj! l)
                                         (conj! (str "G0 F" speed " Z" (+ current-height height))))
                                     (-> cmds
                                         (conj! l)
                                         (conj! (str "G0 F" speed " Z" current-height))))
                                   (inc i)
                                   new-layer
                                   mesh))

                                (.startsWith l ";LAYER:")
                                (do (println (:end-layer z-hop) (<= layer (or (:end-layer z-hop) 0)))
                                    (recur (conj! cmds l) (inc i) (inc new-layer) mesh))
                                (.startsWith l ";MESH:") (recur (conj! cmds l) (inc i) new-layer (subs l 6))
                                (.startsWith l ";") (recur (conj! cmds l) (inc i) new-layer mesh)

                                :else (recur (conj! cmds l) (inc i) new-layer mesh)))))]
                    (recur cmds
                           mesh
                           layer
                           (inc unretract-index)))))

              (.startsWith line ";LAYER:")
              (recur (conj! commands line) mesh (inc layer) (inc idx))

              (.startsWith line ";MESH:")
              (recur (conj! commands line) (subs line 6) layer (inc idx))

              :else
              (recur (conj! commands line) mesh layer (inc idx)))))))))

;; 671190
;;
(comment

  (def lines (time (process-gcode :filename "CFFFP_six-pod-segments-2-9.gcode"
                                  :coast-distance 1.0
                                  :meshes #{"six-pod-segments-2-9.glb"
                                            "first-six-pod-segment.glb"}
                                  :min-retract-dists {"five-gallon-bucket-lid.glb" 3.5}
                                  :z-hop {:height 1.0
                                          :speed 10000
                                          :start-layer 0
                                          :min-dist 10
                                          :end-layer (+ 1 (/ 12 1/4))})))


  ;; 1172661
  ;; 1148904
  ;; 1191958
  (count lines)

  (write-lines-to-file "CFFFP_six-pod-tower-all-segments-processed.gcode" lines)




  )
