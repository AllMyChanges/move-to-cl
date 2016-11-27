(defpackage :playground.datastructures
  (:use :cl))
(in-package :playground.datastructures)

;; проверим скорость доступа к структурам данных


(defvar *user-alist*
  '((|username| . "art")
    (|contacts| . (((|type| . "email")
                    (|value| . "sasha@example.com"))))))

(defvar *user-hash*
  (alexandria:alist-hash-table *user-alist*))


(defclass contact ()
  ((type :accessor contact-type
         :initarg :type
         :type string)
   (value :accessor contact-value
          :initarg :value)))


(defclass user ()
  ((username :accessor user-username
             :initarg :username
             :type string)
   (contacts :accessor user-contacts
             :initarg :contacts
             :type list)))


(defvar *user-obj*
  (make-instance 'user
                 :username "art"
                 :contacts (list
                            (make-instance 'contact
                                           :type "email"
                                           :value "sasha@example.com"))))


(defstruct contact-struct
  (type "email" :type string)
  (value nil))


(defstruct user-struct
  (username "" :type string)
  (contacts nil :type list))


(defvar *user-struct*
  (make-user-struct :username "art"
                    :contacts (list
                               (make-contact-struct
                                :type "email"
                                :value "sasha@example.com"))))

;; > (time (dotimes (i 1000000000) (get-value *user-alist* '|contacts|)))
;; Evaluation took:
;;   0.926 seconds of real time
;;   0.921461 seconds of total run time (0.914981 user, 0.006480 system)
;;   99.46% CPU
;;   2,408,295,572 processor cycles
;;   0 bytes consed

;; > (time (dotimes (i 1000000000) (gethash '|contacts| *user-hash*)))
;; Evaluation took:
;;   0.913 seconds of real time
;;   0.908370 seconds of total run time (0.902029 user, 0.006341 system)
;;   99.45% CPU
;;   2,373,787,968 processor cycles
;;   0 bytes consed


;; > (time (dotimes (i 1000000000) (user-contacts *user-obj*)))
;; Evaluation took:
;;   6.750 seconds of real time
;;   6.632589 seconds of total run time (6.579839 user, 0.052750 system)
;;   98.27% CPU
;;   17,550,067,449 processor cycles
;;   23,328 bytes consed


;; > (time (dotimes (i 1000000000) (with-slots (contacts) *user-obj* contacts)))
;; Evaluation took:
;;   5.905 seconds of real time
;;   5.835520 seconds of total run time (5.787756 user, 0.047764 system)
;;   98.83% CPU
;;   15,353,781,409 processor cycles
;;   33,056 bytes consed


;; > (time (dotimes (i 1000000000) (slot-value *user-obj* 'contacts)))
;; Evaluation took:
;;   5.969 seconds of real time
;;   5.896564 seconds of total run time (5.846683 user, 0.049881 system)
;;   98.79% CPU
;;   15,519,055,738 processor cycles
;;   65,872 bytes consed


;; > (time (dotimes (i 1000000000) (user-struct-contacts *user-struct*)))
;; Evaluation took:
;;   0.915 seconds of real time
;;   0.911598 seconds of total run time (0.904423 user, 0.007175 system)
;;   99.67% CPU
;;   2,379,950,325 processor cycles
;;   0 bytes consed
