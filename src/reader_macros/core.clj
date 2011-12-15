(ns reader-macros.core
  (:use [cadr.core :only (car cdr)]
        [lambda.core :only (λ)]
        [clojure.string :only (lower-case join)])
  (:import (clojure.lang LispReader
                         LispReader$WrappingReader)))

;;;; Make the translation tables accessible.

(let [macros (.getDeclaredField LispReader "macros")]
  (.setAccessible macros true)
  (let [macros (.get macros nil)]
    (def set-macro-character
      (λ [character read]
        (aset macros (int character) read)))

    (def get-macro-character
      (λ [character]
        (aget macros (int character))))))

(let [dispatch-macros (.getDeclaredField LispReader "dispatchMacros")]
  (.setAccessible dispatch-macros true)
  (let [dispatch-macros (.get dispatch-macros nil)]
    (def set-dispatch-macro-character
      (λ [character read]
        (aset dispatch-macros (int character) read)))

    (def get-dispatch-macro-character
      (λ [character]
        (aget dispatch-macros (int character))))))

;;;; Dynamically define convenience functions.

(def class->predicates
  (λ [class]
    (map lower-case (drop-last (re-seq #"[A-Z][a-z]+" class)))))

(def class->read-class
  (λ [class]
    (symbol (format "macro-read-%s" (join "-" (class->predicates class))))))

(def nullary-constructor
  (λ [class]
    (loop [constructors (into '() (:declaredConstructors (bean class)))]
      (if (empty? constructors)
        false
        (let [constructor (car constructors)]
          (if (zero? (count (:parameterTypes (bean constructor))))
            constructor
            (recur (cdr constructors))))))))

(def nullary-constructor?
  #(and (nullary-constructor %) true))

(def nullary-readers
  (map (λ [class]
         {:class (symbol (.getName class))
          :constructor (nullary-constructor class)
          :read-class (class->read-class (.getSimpleName class))})
       (filter (λ [class]
                 (and (re-find #"Reader$" (.getSimpleName class))
                      (nullary-constructor? class)))
               (into '() (:declaredClasses (bean LispReader))))))

;;; Gather a list of these somehow for a dynamic API, or can we do
;;; some namespace-tricks?
(defmacro def-read-macros []
  `(do ~@(map (λ [{class :class
                   constructor :constructor
                   read-class :read-class}]
                `(let [constructor# (nullary-constructor ~class)]
                   (.setAccessible constructor# true)
                   (let [class-reader# (.newInstance constructor# nil)]
                     (def ~read-class
                       (λ [reader# character#]
                         (.invoke class-reader# reader# character#))))))
              nullary-readers)))

(def-read-macros)

;;; Couple of unary exceptions
(let [macro-deref-reader (LispReader$WrappingReader. 'deref)]
  (def macro-read-deref
    (λ [reader character]
      (.invoke macro-deref-reader reader character))))

(let [macro-quote-reader (LispReader$WrappingReader. 'quote)]
  (def macro-read-quote
    (λ [reader character]
      (.invoke macro-quote-reader reader character))))

;;;; Some more utility functions from LispReader

(def read-delimited-list
  (λ [delimiter reader recursive?]
    (LispReader/readDelimitedList delimiter reader recursive?)))

;;; We'd have to make this one accessible.
;; (def read-unicode-char
;;     (λ
;;       ([reader character base length exact?]
;;          (LispReader/readUnicodeChar reader character base length exact?))
;;       ([token offset length base]
;;          (LispReader/readUnicodeChar token offset length base))))
