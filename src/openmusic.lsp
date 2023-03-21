;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* sc-rp/openmusic
;;; FILE
;;; openmusic.lsp
;;;
;;; NAME
;;; openmusic
;;;
;;; PURPOSE
;;; Implementation of functions related with the interoperability
;;; with OpenMusic (e.g. rhythm tree conversion)
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-03-18
;;;
;;; $$ Last modified:  22:57:18 Sat Mar 18 2023 CET
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sc)

;; required for om-time-sig-to-sc
;; RP  Sat Mar 18 21:20:15 2023
(ql:quickload "cl-ppcre")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* openmusic/om-time-sig-to-sc
;;; DATE
;;; 2023-03-18
;;;
;;; DESCRIPTION
;;; Convert time signatures in OpenMusic's format (e.g. (4//4))
;;; to slippery-chicken time signatures.
;;;
;;; ARGUMENTS
;;; - The OpenMusic time signature. Either a symbol or a list.
;;;
;;; RETURN VALUE
;;; The slippery-chicken time signature (e.g. (4 4))
;;;
;;; SYNOPSIS
(defun om-time-sig-to-sc (ts)
  ;;; ****
  (when (listp ts)
    (setq ts (car ts)))
  (mapcar #'read-from-string
          (ppcre:split "//" (if (stringp ts)
                                ts
                                (write-to-string ts :escape nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* openmusic/make-rthm-seq-from-om-rhythm-tree
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; DATE
;;; 2023-03-18
;;;
;;; DESCRIPTION
;;; Convert an OpenMusic rhythm tree to a slippery-chicken rthm-seq.
;;;
;;; TODO
;;; - Handle ties etc.
;;; - Handle subdivisions
;;; - Handle rests (negative values)
;;; - Dotted notes
;;;
;;; ARGUMENTS
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;;
;;; RETURN VALUE
;;; A slippery chicken rthm-seq.
;;;
;;; SYNOPSIS
;;; ****


;;; UNFINISHED

;; (defun make-rthm-seq-from-om-rhythm-tree-aux (bar-item)
;;   (cond ((listp bar-item)
;;          ;; assuming this is a properly formed group
;;          ;; according to OM's implementation RQQ-implementation
;;          ;; which is supposed to be compatible with sc
;;          ;; RP  Sat Mar 18 22:39:58 2023
;;          bar-item)
;;         (t 
         

(let* ((om-tree '(? (((4//4) ((1 (1 1 1)) 1 1 1)) ((2 4) (1.0 1))))))
  ;; ignore OM duration (car) for now
  ;; RP  Sat Mar 18 18:21:06 2023
  (let* ((bars (cadr om-tree)))
    (loop for bar in bars
          collect
          (let* ((time-signature (if (= 1 (length (car bar)))
                                     (om-time-sig-to-sc (car bar))
                                     (car bar)))
                 (subdivisions (cdr bar)))
            (loop for item in (cdr bar)
                  do
                     (loop for bar-item in item
                           do
                              (print bar-item)))))))


  ;; ternary, binary or single beat?
  

  
  (cond ((integerp (/ enum 3))
         ;; ternary

  
  (if (evenp enum)
      (or (= 1 total)
          (integerp (/ total 2)))
      (or (= 1 total)
          (integerp (/ total 3)))))



(let* ((ts '(4 4))
       (bar '(1 2 1)))
  (let* ((total (apply #'+ bar))
         ;; test if proportions demand a tuplet
         ;; this is the case when total is not a simple
         ;; multiple of the enumerator
         ;; RP  Sat Mar 18 21:33:07
         
    (loop for note in bar
          collect
          (/ total note))))
            
          
    
             




;;;(defun make-rthm-seq-from-om-rhythm-tree 
