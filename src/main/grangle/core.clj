(ns main.grangle.core
  (:require
   [instaparse.core :as insta]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]))

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
  (->> (insta/parse command-parser line)
       (next)
       (partition 2)
       (map (fn [[letter digit]]
              [(keyword letter) (parse-double digit)]))
       (into {})))

(comment
  (parse-gcode (s/join "\n" (take 100 lines)))

  (parse-gcode "G1 X5.385 Y-111.456 E0.70762 ;wtf")

  (parse-gcode "M82 ;absolute extrusion mode")

  (import 'java.io.BufferedReader
          'java.io.FileReader)

  (defn process-retraction [commands idx layer layer-height]
    #dbg (let [last-command (parse-command (nth commands (dec idx)))
          llast-command (parse-command (nth commands (- idx 2)))]
      commands))

  (defn process-gcode [filename]
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
              (.startsWith line "G10") (recur (if (> layer 2)
                                                (let [last-command (parse-command (nth commands (dec idx)))
                                                      llast-command (parse-command (nth commands (- idx 2)))]
                                                  (conj! commands line))
                                                (conj! commands line))
                                              layer
                                              (inc idx))
              (.startsWith line ";LAYER:") (recur (conj! commands line) (inc layer) (inc idx))
              :else (recur (conj! commands line) layer (inc idx))))))))

  (def lines (time (process-gcode "CFFFP_six-pod-segments-7-9.gcode")))



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
