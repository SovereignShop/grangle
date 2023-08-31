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

(def gcode (slurp "CFFFP_six-pod-segments-7-9.gcode"))
(def lines (s/split-lines gcode))
(def first-line (first lines))

(count lines)

(def parsed-gcode (parse-gcode (s/join "\n" (take 10000 lines))))

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
                  #dbg digit)
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

(defn process-gcode
  [& {:keys [filename coast-distance meshes min-retract-dists]}]
  (with-open [reader (BufferedReader. (FileReader. filename))]
    (let [header-lines (for [_ (range 4)]
                         (.readLine reader))
          {:keys [layer-height]}
          (parse-header
           (insta/parse header-parser (s/join "\n" header-lines)))]
      (loop [commands (transient (vec header-lines))
             mesh nil
             layer 0
             idx 0]
        (let [^String line (.readLine reader)]
          (cond
            (nil? line)
            (persistent! commands)

            (.startsWith line "G10")
            #_(and (contains? meshes mesh)
                 (.startsWith line "G10")
                 (> layer 0))
            (let [commands (if (contains? min-retract-dists mesh)
                             (let []))
                  last-line (nth commands (dec idx))
                  llast-line (nth commands (- idx 2))]
              (if (and (> layer 0)
                       (.startsWith last-line "G1 ")
                       (.startsWith llast-line "G1 "))
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
                  (recur (-> (pop! commands)
                             (conj! (to-command (assoc last-command :E new-e)))
                             (conj! line))
                         mesh
                         layer
                         (+ idx 1)))
                (recur (conj! commands line) mesh layer (inc idx))))

            (.startsWith line ";LAYER:")
            (recur (conj! commands line) mesh (inc layer) (inc idx))

            (.startsWith line ";MESH:")
            (recur (conj! commands line) (subs line 6) layer (inc idx))

            :else
            (recur (conj! commands line) mesh layer (inc idx))))))))

;; 671190
;;
(comment

  (def lines (time (process-gcode "CFFFP_six-pod-segments-2-9.gcode" 8.0 #{"six-pod-segments-2-9.glb"
                                                                           "first-six-pod-segment.glb"})))

  (write-lines-to-file "CFFFP_six-pod-tower-all-segments.gcode" lines)




  )
