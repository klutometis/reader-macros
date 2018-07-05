(ns reader-macros.core
  (:use    [clojure.string :only [lower-case join]])
  (:import (clojure.lang LispReader
                         LispReader$WrappingReader)))

;; Make the translation tables accessible.
(def macros
  (let [m (.getDeclaredField LispReader "macros")
        _ (.setAccessible macros true)]
    (.get macros nil)))

(def +default-macros+ (aclone macros))

(defn set-macro-character [character read]
  (aset macros (int character) read))

(defn get-macro-character
  [character]
  (aget macros (int character)))

(defmacro with-macro-character [character read & body]
  `(let [char# ~character
         read#     ~read
         original# (get-macro-character char#)]
     (try (do (set-macro-character char# read#)
              ~@body)
          (finally (set-macro-character char# original#)))))

(def lisp-readers
  (->> (seq macros)
       (map-indexed (fn [idx r] (when r [(char idx)  (str r)])))
       (filter identity)
       (into {})))

(def dispatch-macros
  (let [dm (.getDeclaredField LispReader "dispatchMacros")
        _  (.setAccessible dm true)]
    (.get dm nil)))

(def +default-dispatch-macros+ (aclone dispatch-macros))

(defn set-dispatch-macro-character  [character read]
  (aset dispatch-macros (int character) read))

(defn get-dispatch-macro-character [character]
  (aget dispatch-macros (int character)))

(def dispatch-readers
  (->> (seq dispatch-macros)
       (map-indexed (fn [idx r] (when r [(char idx)  (str r)])))
       (filter identity)
       (into {})))

(defmacro with-dispatch-macro-character [character read & body]
  `(let [char# ~character
         read#     ~read
         original# (get-dispatch-macro-character char#)]
     (try (do (set-dispatch-macro-character char# read#)
              ~@body)
          (finally (set-dispatch-macro-character char# original#)))))


(defn reset-read-tables!
  "Undo all the damage we've wrought!"
  []
  (doseq [[c r] (map-indexed vector +default-macros+)]
    (set-macro-character c r))
  (doseq [[c r] (map-indexed vector +default-dispatch-macros+)]
    (set-dispatch-macro-character c r)))

;;;; Dynamically define convenience functions.
(defn class->predicates [class]
  (map lower-case (drop-last (re-seq #"[A-Z][a-z]+" class))))

(defn class->read-class [class]
  (symbol (format "macro-read-%s" (join "-" (class->predicates class)))))

(defn nullary-constructor [class]
  (loop [constructors (into '() (:declaredConstructors (bean class)))]
    (if (empty? constructors)
      false
      (let [constructor (first constructors)]
        (if (zero? (count (:parameterTypes (bean constructor))))
          constructor
          (recur (rest constructors)))))))

(def nullary-constructor?
  #(and (nullary-constructor %) true))
    
;;no longer nullary?
(def nullary-readers
  (map (fn [class]
         {:class (symbol (.getName class))
          :constructor (nullary-constructor class)
          :read-class (class->read-class (.getSimpleName class))})
       (filter (fn [class]
                 (and (re-find #"Reader$" (.getSimpleName class))
                      (nullary-constructor? class)))
               (into '() (:declaredClasses (bean LispReader))))))

;;these are no longer nullary...
;;reads take the form
;;:: Object reader, Object doublequote, Object opts, Object pendingForms

;;; Gather a list of these somehow for a dynamic API, or can we do
;;; some namespace-tricks?
(defmacro def-read-macros []
  `(do ~@(map (fn [{class       :class
                    constructor :constructor
                    read-class  :read-class}]
                `(let [constructor# (nullary-constructor ~class)]
                   (.setAccessible constructor# true)
                   (let [class-reader# (.newInstance constructor# nil)]
                     (defn ~read-class
                       ([reader# character# opts# pendingforms#]
                        (.invoke class-reader# reader# character# opts# pendingforms#))
                       ([reader# character#]
                        (~read-class reader# character# ~'+default-opts+ nil))))))
              nullary-readers)))

(def-read-macros)

;;; Couple of unary exceptions
(let [macro-deref-reader (LispReader$WrappingReader. 'deref)]
  (defn macro-read-deref [reader character opts pending-forms]
    (.invoke macro-deref-reader reader character opts pending-forms)))

(let [macro-quote-reader (LispReader$WrappingReader. 'quote)]
  (defn macro-read-quote [reader character opts pending-forms]
    (.invoke macro-quote-reader reader character opts pending-forms)))

;;;; Some more utility functions from LispReader
(def +default-opts+  {:features #{"clj"}
                      :eofthrow :eof})

(defn read-delimited-list
  "Interestingly enough, this returns a vector.  Clojure's reader
   uses vector's internally, likely because they're simpler to conj onto?"
  ([delimiter reader recursive? opts pending-forms]
   (LispReader/readDelimitedList delimiter reader recursive? opts pending-forms))
  ([delimiter reader recursive?]
   (read-delimited-list delimiter reader recursive? +default-opts+ nil)))

(defmacro with-read-table
  [read-table & body]
  (if-let [bind (first (seq read-table))]
    (let [[k v] bind]
    `(with-macro-character
       ~k ~v
       (with-read-table ~(dissoc read-table k)
         ~@body)))
     `(do ~@body)))



(comment
  ;;examples
  ;;reversed strings from blog...
  (def reversed-string-reader
    (fn [reader quote opts pending-forms]
      (clojure.string/reverse (macro-read-string  reader quote opts pending-forms))))

  (def testdata
    "\"Hello!  This is a string of text, hopefully it's
     sdrawkcab\"")
  (with-macro-character \" reversed-string-reader
    (read-string testdata))
  
  (def reversed-list-reader
    (fn [reader quote opts pending-forms]
      (reverse (macro-read-list  reader quote opts pending-forms))))

  (def testlist
    "(a b c d)")
  
  (with-macro-character \( reversed-list-reader
    (read-string testlist))

  (def read-list (fn [reader quote opts pending-forms]
      (seq (read-delimited-list \) reader false))))

  (with-macro-character \( read-list
    (read-string testlist))

 (def read-vector (fn [reader quote opts pending-forms]
      (vec (read-delimited-list \] reader false))))
  
  (def testvector
      "[a b c d]")

  ;;maybe your vector is "really"
  ;;a sequence or a set!  Let the reader decide!
  (def nondeterministic-reader
    (fn [reader quote opts pending-forms]
      (let [stuff (read-delimited-list \] reader false)]
        (case (rand-nth [:vector :list :set])
          :vector   (vec stuff)
          :list      (into '() stuff)
          (set stuff)))))

  (with-macro-character \[ nondeterministic-reader 
    (read-string testvector))

  (with-macro-character \[ nondeterministic-reader 
    (frequencies (repeatedly 1000 #(read-string testvector))))
  ;;{[a b c d] 681, #{a c b d} 319}

  (def string-vector "[\"hello\" \"world\" :a :b :c]")

  (defn wierd-clojure!
    ([txt]
  ;;tie it all together with a read-table that jacks
  ;;everything up!
    (with-read-table {\" reversed-string-reader
                      \[ nondeterministic-reader}
      (read-string txt)))
    ([] (wierd-clojure! string-vector)))

;;reader-macros.core> (wierd-clojure!)
;; (:c :b :a "dlrow" "olleh")
;; reader-macros.core> (wierd-clojure!)
;; ["olleh" "dlrow" :a :b :c]
;; reader-macros.core> (wierd-clojure!)
;; #{:c "dlrow" :b "olleh" :a}
;; reader-macros.core> (wierd-clojure!)
;; (:c :b :a "dlrow" "olleh")
;; reader-macros.core> (wierd-clojure!)
;; ["olleh" "dlrow" :a :b :c]
  
)
