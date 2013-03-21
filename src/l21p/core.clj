(ns l21p.core
  (:gen-class
    :name l21p.core
    :main true))

(defn parse-int
  ([s]
   (parse-int s nil))
  ([s default]
   (try
     (Integer/parseInt s)
     (catch NumberFormatException _ default))))

(defn date-str->vec
  [s]
  [(parse-int (subs s 0 4))
   (parse-int (subs s 5 7))
   (parse-int (subs s 8 10))])

(defn print-seq
  [coll]
  (dorun
    (doseq [line coll]
      (println line))))

(defn parse-paper-file-line
  [line]
  (let [re (re-find #"([^\t]+)\t([^\t]+)\t(\d+)\t([\d-]*)\t([^\t]*)\t(\d*)" line)]
    {:title    (re 2)
     :issue    (-> (re 3) parse-int)
     :date     (-> (re 4) date-str->vec)
     :headline (re 5)
     :price    (-> (re 6) parse-int)}))

(defn parse-journal-file-line
  [line]
  (let [re (re-find #"([^\t]+)\t([^\t]+)\t(\d+)\t([\d-]*)\t(\d*)" line)]
    {:title (re 2)
     :issue (-> (re 3) parse-int)
     :date  (-> (re 4) date-str->vec)
     :price (-> (re 5) parse-int)}))

(defn parse-pamphlet-file-line
  [line]
  (let [re (re-find #"([^\t]+)\t([^\t]*)\t([^\t]*)\t([\d-]*)\t(\d*)" line)]
    {:id     (re 1)
     :title  (re 2)
     :author (re 3)
     :date   (-> (re 4) date-str->vec)
     :price  (-> (re 5) parse-int)}))

(defn parse-extra-file-line
  [line]
  (let [re (re-find #"([^\t]+)\t([^\t]+)\t([\d-]*)\t([^\t]*)\t(\d*)" line)]
    {:id       (re 1)
     :title    (re 2)
     :date     (-> (re 3) date-str->vec)
     :headline (re 4)
     :price    (-> (re 5) parse-int)}))

(defn parse-goods-file-line
  [line]
  (let [re (re-find #"([^\t]+)\t([^\t]*)\t([^\t]*)\t(\d*)" line)]
    {:id    (re 1)
     :type  (re 2)
     :name  (re 3)
     :price (-> (re 4) parse-int)}))

(defmacro parse-file
  [target]
  (let [target-str#  (name target)
        line-parser# (symbol (str "parse-" target-str# "-file-line"))
        file#        (str "./data/" target-str#)]
    `(with-open [rdr# (java.io.BufferedReader. 
                        (java.io.FileReader. ~file#))]
       (let [lines# (line-seq rdr#)]
         (doall (map ~line-parser#
              lines#))))))

(defn -main []
  (let [paper    (parse-file :paper)
        journal  (parse-file :journal)
        pamphlet (parse-file :pamphlet)
        extra    (parse-file :extra)
        goods    (parse-file :goods)]
    (dorun (map #(print-seq %) paper))
    (dorun (map #(print-seq %) journal))
    (dorun (map #(print-seq %) pamphlet))
    (dorun (map #(print-seq %) extra))
    (dorun (map #(print-seq %) goods))))

