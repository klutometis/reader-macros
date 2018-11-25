(ns reader-macros.test.core
  (:use [reader-macros.core]
        [clojure.test]
        [lambda.core :only (λ)]))

(letfn [(rotate [character]
          (char (+ (mod (+ (- (int character) 97) 13) 26) 97)))]
  (def rot13
    "Only works for lower case letters."
    (λ [string] (apply str (map rotate string)))))

(defn macro-read-rot13
  [reader character _ _]
  (let [string (macro-read-string reader character)]
    (rot13 string)))

(set-macro-character \" macro-read-rot13)

(deftest rot13
  (is (= "uryyb" (apply str '(\h \e \l \l \o)))))
