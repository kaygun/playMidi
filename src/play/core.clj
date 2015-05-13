(ns play.core
  (:gen-class)
  (:import
     (java.util Random)
     (java.io File)
     (javax.sound.midi MidiSystem Sequence MidiMessage MidiEvent ShortMessage Track)))

(defn getDev []
  (let [dev (MidiSystem/getSynthesizer)]
    (.open dev)
    dev))

(defn getInst [dev chanNum instNum]
  (let [chan (-> dev .getChannels (nth chanNum))
        inst (-> dev .getDefaultSoundbank .getInstruments (nth instNum))]
    (.loadInstrument dev inst)
    (.programChange chan instNum) ; Important!!!!
    (fn [key duration volume]
      (.noteOn chan key volume)
      (Thread/sleep duration)
      (.noteOff chan key))))

(defn play [inst scor]
  (future (doseq [x scor]
            (println x)
            (apply inst x))))

(defn parser [file-name track-no stretch]
  (let* [raw (-> (File. file-name)
                 (MidiSystem/getSequence)
                 .getTracks
                 (nth track-no))
         trk (filter #(instance? ShortMessage (.getMessage %))
                     (map #(.get raw %) (->> raw .size range)))
         seq (map #(.getTick %) trk)
         tim (map - (take-nth 2 (rest seq)) (take-nth 2 seq))
         nts (map #(-> % .getMessage .getMessage (nth 1)) (take-nth 2 trk))]
        (map #(vector (- %1 16)
                      (int (* stretch (+ (rand 8) -4 %2)))
                      (int (+ (rand 80) 80)))
             nts tim)))

(defn random-wlk [key n stretch]
   (loop [m n
          center key
          carry nil]
      (if (= m 0)
         carry
         (recur (- m 1) 
                (+ center (int (rand 3)) -1)
                (conj carry (vector center
                                    (int (* stretch (+ 125 (rand 125))))
                                    200))))))

(defn random-scr [key n str]
  (let [maj [0 2 4 5 7 9 11 12 11 9 7 5 4 2 0]]
    (repeatedly n (fn [] [(+ key (nth maj (int (rand 15))))
                          (+ 100 (* 25 str (int (rand 12))))
                          200]))))

(defn -main [file-name inst str & args]
  (let* [
         stretch (read-string str)
         instNum (Integer/parseInt inst)         
         dev   (getDev)
         left  (getInst dev 0 instNum)
         right (getInst dev 1 instNum)
         scorl (parser file-name 1 stretch)
         scorr (parser file-name 2 stretch)
         ; scorl (random-wlk 58 40 stretch)
         ; scorr (random-wlk 63 40 stretch)
         ; scorl (random-scr 49 40 stretch)
         ; scorr (random-scr 52 40 stretch)
         ]
     (play left scorl)
     (play right scorr)
     (shutdown-agents)))
