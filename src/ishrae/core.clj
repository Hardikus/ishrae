(ns ishrae.core
  (require [clj-webdriver.taxi :as taxi]
           [fs.core :as fs]
           [conch.core :as sh]
           [clj-webdriver.firefox :as ff]))

(def file-op "ishrae-contents.csv")

(def base-url "http://www.ishrae.in/acredex/co_details.asp?co=%d")

(comment
  Name,Address,City,Pincode,Phones,Fax,Email ,Website,Turnover,Sub Products
ZED AIRCON PVT. LTD," WZ 86R, Indl. Lane, Todapur, New Delhi",Delhi,110 012,"(011)5741933, 5749274",(011)5759288,NA,NA,Rs. 40 Lakhs,DX Chillers
ZED AIRCON PVT. LTD," WZ 86R, Indl. Lane, Todapur, New Delhi",Delhi,111 012,"(011)5741933, 5749275",(011)5759289,NA,NA,Rs. 40 Lakhs,Chilled Water Coils
ZED AIRCON PVT. LTD," WZ 86R, Indl. Lane, Todapur, New Delhi",Delhi,112 012,"(011)5741933, 5749276",(011)5759290,NA,NA,Rs. 40 Lakhs,Shell & Tube Condensers
)


(defn substring?
  "Checks for substring returns boolean value, true if a substring."
  [sub string]
  (> (.indexOf (clojure.string/lower-case string)
               (clojure.string/lower-case sub)) -1))

(defn run-cmd
  "Runs the given command and returns output.
   For example whoami, pwd.
   Optionally removes the newline appended to the output. Good for whoami, pwd etc commands."
  [cmd remove-newline?]
  (let [output (sh/stream-to-string (sh/proc cmd) :out)]
    (if (true? remove-newline?)
      (clojure.string/replace output "\n" "")
      output)))

(def path-firebug "ff-extensions/firebug@software.joehewitt.com.xpi")

(defn init-firefox-local-driver
  "Initializes Fir efox driver.
   And sets a global implicit-wait"
  [wait-timeout]
  (taxi/set-driver! {:browser-name :firefox
                     :profile (doto (ff/new-profile)
                                (ff/set-preferences {"app.update.enabled" false})
                                (ff/set-preferences {"extensions.firebug.showFirstRunPage" false})
                                (ff/enable-extension path-firebug))})
  (taxi/implicit-wait wait-timeout))

(defn extract-str
  "Returns the string after removing given regex.
   Replaces comma with semicolon."
  [elem regex]
  (let [un-clean (clojure.string/replace elem regex "")
        clean-str (clojure.string/replace un-clean #"," ";")]
    (clojure.string/trim clean-str)))

(defn key-exists?
  "Checks if given regex matches."
  [regex elem]
  (not (nil? (re-seq regex elem))))


(defn get-match
  "Checks if given row has which key and associates that key val with given map."
  [m elem]
  (cond
    (key-exists? #"^Address : " elem) (assoc m :address (extract-str elem #"^Address : "))
    (key-exists? #"^City : " elem) (assoc m :city (extract-str elem #"^City : "))
    (key-exists? #"^Pincode : " elem) (assoc m :pincode (extract-str elem #"^Pincode : "))
    (key-exists? #"^Fax : " elem) (assoc m :fax (extract-str elem #"^Fax : "))
    (key-exists? #"^Phones : " elem) (assoc m :phones (extract-str elem #"^Phones : "))
    (key-exists? #"^Email : " elem) (assoc m :email (extract-str elem #"^Email : "))
    (key-exists? #"^Website : " elem) (assoc m :website (extract-str elem #"^Website : "))
    (key-exists? #"^Turnover : " elem) (assoc m :turnover (extract-str elem #"^Turnover : "))
    :else m))

(defn get-products
  "Gets all the Actual Product for a given company and returns them as one list."
  [m]
  (assoc m :products (mapv #(extract-str % #"^• ")
        (filter #(key-exists? #"^• " %)
                (clojure.string/split-lines (taxi/text ".main_col form"))))))


(defn get-other-data
  "Returns a map data of one company."
  [m]
  (reduce #(get-match %1 %2)
          m
          (clojure.string/split-lines (taxi/text ".main_col form"))))

(defn get-company-name
  "Returns the name of the company"
  [m]
  (assoc m :name
         (clojure.string/trim (second (clojure.string/split-lines (taxi/text ".main_col form"))))))

(defn get-link-data
  "Goes to the company number url on the link and gets the data as a map."
  [co]
  (taxi/to (format base-url co))
  (-> {}
      get-company-name
      get-other-data
      get-products))

;;;   Name,Address,City,Pincode,Phones,Fax,Email ,Website,Turnover,Sub Products

(defn write-one-line
  "Writes each company data and given product on one newline."
  [m product]
  (spit file-op (str (get m :name "") ",") :append true)
  (spit file-op (str (get m :address "") ",") :append true)
  (spit file-op (str (get m :city "") ",") :append true)
  (spit file-op (str (get m :pincode "") ",") :append true)
  (spit file-op (str (get m :phones "") ",") :append true)
  (spit file-op (str (get m :fax "") ",") :append true)
  (spit file-op (str (get m :email "") ",") :append true)
  (spit file-op (str (get m :website "") ",") :append true)
  (spit file-op (str (get m :turnover "") ",") :append true)
  (spit file-op (str product "") :append true)
  (spit file-op "\n" :append true))


(defn doseq-over-products
  "If there are multiple products, duplicate data for each product one per line.
   If no product just add email as empty and add rest of the content."
  [data-list]
  (doseq [m data-list]
    (if (nil? (get m :products))
      (write-one-line m "")
      (do
        (doseq [products (get m :products)]
          (write-one-line m products))))))


(defn get-ranged-data
  "Gets ma"
  [start-range end-range]
  (doseq-over-products
   (map #(get-link-data %) (range start-range (inc end-range)))))
