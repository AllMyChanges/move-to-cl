;; Посмотрим, насколько быстр cl-who

(defpackage :playground.templates
  (:use :cl)
  (:import-from #:cl-who
                #:with-html-output-to-string
                #:str
                #:htm))
(in-package :playground.templates)


(ql:quickload 'cl-who)


(defun get-value (obj name)
  (declare (type symbol name))
  (cdr (assoc name obj)))


(defun index ()
  (let ((title "User Profile")
        (user '((|username| . "art")
                (|contacts| . (((|type| . "email")
                                (|value| . "sasha@example.com")))))))
    (with-html-output-to-string (s)
      (:html
       (:head
        (:title (str title)))
       (:body
        (:h1 :class "page-title"
             (str title))
        (:div :class "user-profile"
              (:h1 :class "user-profile__username"
                   (str (get-value user '|username|)))
              (:ul :class "user-profile__contacts"
                   (loop
                      :for contact
                      :in (get-value user '|contacts|)
                      :collect (htm (:li :class (concatenate 'string
                                                             "contact__type-"
                                                             (get-value contact '|type|))
                                         (str (get-value contact '|value|))))))))))))

;; run as:
;; (time (dotimes (i 100000) (index)))

;; Evaluation took:
;;   0.313 seconds of real time
;;   0.312444 seconds of total run time (0.292880 user, 0.019564 system)
;;   [ Run times consist of 0.040 seconds GC time, and 0.273 seconds non-GC time. ]
;;   99.68% CPU
;;   811,876,286 processor cycles
;;   294,373,696 bytes consed
;;
;;   3.1244401e-6 на один запуск
;;   или 3.1244401 µs

;; если сделать optimize speed, то ничего особо не меняется:
;; Evaluation took:
;;   0.332 seconds of real time
;;   0.331090 seconds of total run time (0.306530 user, 0.024560 system)
;;   [ Run times consist of 0.048 seconds GC time, and 0.284 seconds non-GC time. ]
;;   99.70% CPU
;;   862,939,869 processor cycles
;;   294,406,512 bytes consed

;; если объявить get-value как inline
;; Evaluation took:
;;   0.321 seconds of real time
;;   0.321472 seconds of total run time (0.298103 user, 0.023369 system)
;;   [ Run times consist of 0.049 seconds GC time, and 0.273 seconds non-GC time. ]
;;   100.00% CPU
;;   834,824,942 processor cycles
;;   294,406,384 bytes consed


;; если вынести создание title и user за пределы функции:
;; Evaluation took:
;;   0.326 seconds of real time
;;   0.324423 seconds of total run time (0.304109 user, 0.020314 system)
;;   [ Run times consist of 0.044 seconds GC time, and 0.281 seconds non-GC time. ]
;;   99.39% CPU
;;   847,369,264 processor cycles
;;   294,411,456 bytes consed


;; определение типа для `name` в get-value тоже особо не работает:
;; Evaluation took:
;;   0.332 seconds of real time
;;   0.330976 seconds of total run time (0.308174 user, 0.022802 system)
;;   [ Run times consist of 0.049 seconds GC time, and 0.282 seconds non-GC time. ]
;;   99.70% CPU
;;   863,169,063 processor cycles
;;   294,379,184 bytes consed
