;;; tables.lisp --- international standard atmosphere

;; Copyright (C) 2020 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(defpackage #:de.ralph-schleicher.iso-2533.tables
  (:use #:common-lisp #:iterate #:named-readtables #:iso-2533)
  ;; As a thermodynamic engineer, I really absolutely
  ;; need the symbol T for temperature.
  (:shadow #:t)
  ;; Program entry point.
  (:export #:main))

(in-package #:de.ralph-schleicher.iso-2533.tables)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable case-sensitive
    (:merge :standard)
    (:case :invert)))

(in-readtable case-sensitive)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *lines-per-block* 10)
  (defparameter *blocks-per-page* 6)
  (defparameter *page-separator* #\Page)
  (defparameter *standard-steps* '(#+iso-2533-addendum-2 -5000
                                   #-iso-2533-addendum-2 -2000
                                   50 32000 100 51000 200 80000)))

(defmacro print-table (((var start step end &rest steps) head row &rest options) &body body)
  ;; VAR START [STEP END]+
  ;;      Iteration variable and stepping.
  ;; HEAD Table header lines (a list of strings).
  ;; ROW  Table row format template string.
  `(%print-table (lambda (,var) ,@body)
                 ,start (list ,step ,end ,@steps)
                 ,head ,row ,@options))

(defun %print-table (fun start steps head row &key (column-delimiter #\|))
  ;; Create a table row and use it as a template for building
  ;; an empty line and a horizontal rule.
  (let* ((empty-line (nsubstitute-if
                      #\Space (lambda (char)
                                (char/= char column-delimiter))
                      (apply #'format nil row (funcall fun start))))
         (horizontal-rule (substitute-if
                           #\- (lambda (char)
                                 (char= char #\Space))
                           empty-line))
         ;; Printed lines per block.
         (line-count 0)
         ;; Printed blocks per page.
         (block-count 0)
         ;; Whether or not to start a new block.
         (new-block nil)
         ;; Whether or not to start a new page.
         (new-page cl:t))
    (labels ((table-header ()
               "Print the table header."
               (when head
                 (dolist (line head)
                   (princ line)
                   (terpri))
                 (princ horizontal-rule)
                 (terpri)))
             (table-row (arg)
               "Print a table row."
               (when (= line-count *lines-per-block*)
                 (setf new-block cl:t
                       line-count 0)
                 (incf block-count))
               (when (= block-count *blocks-per-page*)
                 (when *page-separator*
                   (princ *page-separator*))
                 (setf new-page cl:t
                       block-count 0))
               (when new-page
                 (table-header)
                 (setf new-block nil
                       new-page nil))
               (when new-block
                 (princ empty-line)
                 (terpri)
                 (setf new-block nil))
               (apply #'format cl:t row (funcall fun arg))
               (terpri)
               (incf line-count)))
      (iter (for (step end . rest) :on steps :by #'cddr)
            (iter (for arg :from start :below end :by step)
                  (table-row arg))
            (when (null rest)
              (table-row end))
            (setf start end))
      )))

(define-symbol-macro To 273.15D0)

(defun table-5a ()
  (print-table
      ((h . #.*standard-steps*)
       '("    h    |    H    |    T    |    t    |     p      |     p      |     ρ      |    g    "
         "    m    |    m    |    K    |   °C    |    hPa     |    mmHg    |   kg/m³    |  m/s²   ")
       " ~7@A | ~7,1F | ~7,3F | ~7,3F | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~7,5F ")
    (let ((H (geopotential-altitude h)))
      (multiple-value-bind (p T)
          (atm H)
        (list h H
              T (- T To)
              (/ p 100) (* 760 (/ p standard-pressure))
              (density p T) (acceleration-of-gravity h))))))

(defun table-5b ()
  (print-table
      ((H . #.*standard-steps*)
       '("    H    |    h    |    T    |    t    |     p      |     p      |     ρ      |    g    "
         "    m    |    m    |    K    |   °C    |    hPa     |    mmHg    |   kg/m³    |  m/s²   ")
       " ~7@A | ~7,1F | ~7,3F | ~7,3F | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~7,5F ")
    (let ((h (geometric-altitude H)))
      (multiple-value-bind (p T)
          (atm H)
        (list H h
              T (- T To)
              (/ p 100) (* 760 (/ p standard-pressure))
              (density p T) (acceleration-of-gravity h))))))

(defun table-6a ()
  (print-table
      ((h . #.*standard-steps*)
       '("    h    |    p/pₙ    |    ρ/ρₙ    |   1/SMOE   |    a    |     µ     |     ν     |     λ     "
         "    m    |     1      |     1      |     1      |   m/s   |   Pa⋅s    |   m²/s    |   W/m/K   ")
       " ~7@A | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~7,3F | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE ")
    (let ((H (geopotential-altitude h)))
      (multiple-value-bind (p T)
          (atm H)
        (let ((rho (density p T)))
          (list h
                (/ p standard-pressure)
                (/ rho standard-density)
                (sqrt (/ rho standard-density))
                (speed-of-sound T)
                (dynamic-viscosity T)
                (kinematic-viscosity p T)
                (thermal-conductivity T)))))))

(defun table-6b ()
  (print-table
      ((H . #.*standard-steps*)
       '("    H    |    p/pₙ    |    ρ/ρₙ    |   1/SMOE   |    a    |     µ     |     ν     |     λ     "
         "    m    |     1      |     1      |     1      |   m/s   |   Pa⋅s    |   m²/s    |   W/m/K   ")
       " ~7@A | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~10,5,1,,,,'EE | ~7,3F | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE ")
    (multiple-value-bind (p T)
        (atm H)
      (let ((rho (density p T)))
        (list H
              (/ p standard-pressure)
              (/ rho standard-density)
              (sqrt (/ rho standard-density))
              (speed-of-sound T)
              (dynamic-viscosity T)
              (kinematic-viscosity p T)
              (thermal-conductivity T))))))

(defun table-7a ()
  (print-table
      ((h . #.(cons -2000 (rest *standard-steps*))) ;format control does not work for -5000 m
       '("    h    |    Hₚ   |     γ     |     n      |    v    |     ω     |     l     "
         "    m    |    m    |    N/m³   |    1/m³    |   m/s   |    1/s    |     m     ")
       " ~7@A | ~7,1F | ~9,4,1,,,,'EE | ~10,4,2,,,,'EE | ~7,2F | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE ")
    (let ((H (geopotential-altitude h)))
      (multiple-value-bind (p T)
          (atm H)
        (list h
              (pressure-scale-height h T)
              (specific-weight h p T)
              (number-density p T)
              (mean-speed T)
              (collision-frequency p T)
              (mean-free-path p T))))))

(defun table-7b ()
  (print-table
      ((H . #.(cons -2000 (rest *standard-steps*)))
       '("    H    |    Hₚ   |     γ     |     n      |    v    |     ω     |     l     "
         "    m    |    m    |    N/m³   |    1/m³    |   m/s   |    1/s    |     m     ")
       " ~7@A | ~7,1F | ~9,4,1,,,,'EE | ~10,4,2,,,,'EE | ~7,2F | ~9,4,1,,,,'EE | ~9,4,1,,,,'EE ")
    (let ((h (geometric-altitude H)))
      (multiple-value-bind (p T)
          (atm H)
        (list H
              (pressure-scale-height h T)
              (specific-weight h p T)
              (number-density p T)
              (mean-speed T)
              (collision-frequency p T)
              (mean-free-path p T))))))

(defun table-add1 ()
  (print-table
      ((p 5 1/10 199/10)
       '("         |   0.00  |   0.01  |   0.02  |   0.03  |   0.04  |   0.05  |   0.06  |   0.07  |   0.08  |   0.09  "
         "---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------"
         "    p    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    "
         "   hPa   |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    ")
       " ~7,2F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F ")
    (list p
          (pressure-altitude (* (+ p 0/100) 100))
          (pressure-altitude (* (+ p 1/100) 100))
          (pressure-altitude (* (+ p 2/100) 100))
          (pressure-altitude (* (+ p 3/100) 100))
          (pressure-altitude (* (+ p 4/100) 100))
          (pressure-altitude (* (+ p 5/100) 100))
          (pressure-altitude (* (+ p 6/100) 100))
          (pressure-altitude (* (+ p 7/100) 100))
          (pressure-altitude (* (+ p 8/100) 100))
          (pressure-altitude (* (+ p 9/100) 100))))
  (when *page-separator*
    (princ *page-separator*))
  (print-table
      ((p 20 1 1199)
       '("         |   0.0   |   0.1   |   0.2   |   0.3   |   0.4   |   0.5   |   0.6   |   0.7   |   0.8   |   0.9   "
         "---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------"
         "    p    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    |    H    "
         "   hPa   |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    |    m    ")
       " ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F | ~7,1F ")
    (list p
          (pressure-altitude (* (+ p 0/10) 100))
          (pressure-altitude (* (+ p 1/10) 100))
          (pressure-altitude (* (+ p 2/10) 100))
          (pressure-altitude (* (+ p 3/10) 100))
          (pressure-altitude (* (+ p 4/10) 100))
          (pressure-altitude (* (+ p 5/10) 100))
          (pressure-altitude (* (+ p 6/10) 100))
          (pressure-altitude (* (+ p 7/10) 100))
          (pressure-altitude (* (+ p 8/10) 100))
          (pressure-altitude (* (+ p 9/10) 100)))))

(defun print-to-file (file-name function)
  (with-open-file (stream (merge-pathnames
                           (uiop:parse-unix-namestring file-name)
                           (asdf:system-source-directory "iso-2533/tables"))
                          :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*standard-output* stream))
      (funcall function))))

(defun main ()
  (let ((*page-separator* (coerce (list #\Page #\Newline) 'string)))
    (print-to-file "Table_5a.txt" #'table-5a)
    (print-to-file "Table_5b.txt" #'table-5b)
    (print-to-file "Table_6a.txt" #'table-6a)
    (print-to-file "Table_6b.txt" #'table-6b)
    (print-to-file "Table_7a.txt" #'table-7a)
    (print-to-file "Table_7b.txt" #'table-7b)
    (let ((*lines-per-block* 5)
          (*blocks-per-page* 10))
      (print-to-file "Table_Add1.txt" #'table-add1))
    ()))
#+()
(main)

(in-readtable :standard)

;;; tables.lisp ends here
