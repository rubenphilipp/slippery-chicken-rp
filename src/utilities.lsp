;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc-rp/utilities
;;; FILE
;;; utilities.lsp
;;;
;;; NAME
;;; utilities
;;;
;;; PURPOSE
;;; A loose collection of mainly helper functions.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-03-21
;;;
;;; $$ Last modified:  23:12:33 Tue Mar 21 2023 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIRST-N
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* list/first-n
;;; DATE
;;; 2023-03-21
;;;
;;; DESCRIPTION
;;; Gets the first n elements of a list.
;;;
;;; ARGUMENTS
;;; - The number of elements to return. Must be an integer.
;;; - The original list. 
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :auto-stop. A boolean indicating whether the method should stop
;;;   retrieving elements from the list when the desired number of elements
;;;   is larger than the length of the list (i.e. resulting in a sequence
;;;   of NILs in the end of the resulting list). Default: NIL.
;;;
;;; RETURN VALUE
;;; The filtered list. 
;;;
;;; SYNOPSIS
(defmethod first-n (n (lst list) &key (auto-stop nil))
;;; ****
  (let ((n (if (and auto-stop
                    (>= n (length lst)))
               (length lst)
               n)))
    (loop for i from 0 to (1- n)
          collect
          (nth i lst))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* sclist/first-n
;;; DATE
;;; 2023-03-21
;;;
;;; DESCRIPTION
;;; Gets the first n elements of a sclist.
;;;
;;; ARGUMENTS
;;; - The number of elements to return. Must be an integer.
;;; - The original list. 
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :auto-stop. A boolean indicating whether the method should stop
;;;   retrieving elements from the list when the desired number of elements
;;;   is larger than the length of the list (i.e. resulting in a sequence
;;;   of NILs in the end of the resulting list). Default: NIL.
;;;
;;; RETURN VALUE
;;; The filtered sclist. 
;;;
;;; SYNOPSIS
(defmethod first-n (n (lst sclist) &key (auto-stop nil))
  ;;; ****
  (let ((n (if (and auto-stop
                    (>= n (sclist-length lst)))
               (sclist-length lst)
               n))
        (new-lst (make-sclist '())))
    (loop for i from 0 to (1- n)
          do
             (sclist-econs new-lst (get-nth i lst)))
    new-lst))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* chord/first-n
;;; DATE
;;; 2023-03-21
;;;
;;; DESCRIPTION
;;; Gets the first n pitches of a chord.
;;;
;;; ARGUMENTS
;;; - The number of pitches to return. Must be an integer.
;;; - The original chord.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :auto-stop. A boolean indicating whether the method should stop
;;;   retrieving pitches from the chord when the desired number of elements
;;;   is larger than the number of pitches in the chord (i.e. resulting in a 
;;;   sequence of NILs in the end of the resulting list). Default: T.
;;;
;;; RETURN VALUE
;;; The filtered chord.
;;;
;;; SYNOPSIS
(defmethod first-n (n (lst chord) &key (auto-stop nil))
  ;;; ****
  (let ((n (if (and auto-stop
                    (>= n (num-notes lst)))
               (num-notes lst)
               n))
        (new-lst (make-chord '())))
    (loop for i from 1 to n
          do
             (add-pitches new-lst (get-pitch lst i)))
    new-lst))
