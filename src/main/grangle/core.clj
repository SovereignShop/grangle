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

(defn process-gcode [filename coast-distance ]
  (with-open [reader (BufferedReader. (FileReader. filename))]
    (let [{:keys [layer-height]}
          (parse-header
           (insta/parse header-parser
                        (s/join "\n" (for [_ (range 4)]
                                       (.readLine reader)))))]
      (loop [commands (transient [])
             layer 0
             idx 0]
        (let [^String line (.readLine reader)]
          (cond
            (nil? line) (persistent! commands)
            (.startsWith line "G10") (let [last-line (nth commands (dec idx))
                                           llast-line (nth commands (- idx 2))]
                                       (if (and (> layer 2)
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
                                               new-e (e/+ e2 (* extrusion (/ new-extrude-length distance)))]
                                           (recur (-> (pop! commands)
                                                      (conj! (to-command (assoc last-command
                                                                                :X (nth new-p 0)
                                                                                :Y (nth new-p 1)
                                                                                :E new-e)))
                                                      (conj! (to-command (assoc last-command
                                                                                :E new-e)))
                                                      (conj! line))
                                                  layer
                                                  (+ idx 2)))
                                         (recur (conj! commands line) layer (inc idx))))
            (.startsWith line ";LAYER:") (recur (conj! commands line) (inc layer) (inc idx))
            :else (recur (conj! commands line) layer (inc idx))))))))

(comment
  (parse-gcode (s/join "\n" (take 100 lines)))

  (e/orthonormalize [4 2])

  (parse-gcode "G1 X5.385 Y-111.456 E0.70762 ;wtf")

  (parse-gcode "M82 ;absolute extrusion mode")

  (import )

  (def lines (time (process-gcode "CFFFP_six-pod-segments-7-9.gcode" 1.5)))

  (write-lines-to-file "CFFFP_six-pod-segments-7-9-with-coasting.gcode" lines)


  (count lines)



  (count lines)


  )

(defn walk-gcode [parsed-gcode]
  (let [header (parse-header (second parsed-gcode))
        layer-height (:layer-height header)
        lines (drop 3 parsed-gcode)]
    (reductions
     (fn [state line]
       (match line
              [:layer layer-number]
              (let [n (Integer/parseInt layer-number)]
                (-> state
                    (assoc :layer n)
                    (assoc :Z (* n layer-height))))

              [:type layer-type]
              (-> state
                  (assoc :type (keyword layer-type)))

              [:command "G" (:or "1" "0") & args]
              (let [m (->> args
                           (partition 2)
                           (map (fn [[letter digit]]
                                  [(keyword letter) (parse-double digit)]))
                           (into {}))]
                (cond-> (merge state m)))

              [:command "G" "10" & args]
              #dbg state

              :else state))
     header
     lines)))

(last (walk-gcode parsed-gcode))
