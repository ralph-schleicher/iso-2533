;;; packages.lisp --- international standard atmosphere

;; Copyright (C) 2014 Ralph Schleicher

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :iso-2533-derived-quantities *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :iso-2533-addendum-2 *features*))

(defpackage :iso-2533
  (:use :common-lisp
	:iterate)
  (:documentation "ISO 2533:1975 standard atmosphere.

Includes ISO 2533:1975/Add 1:1985 which provides hypsometrical
tables, that is geopotential altitude as a function of barometric
pressure.

Includes ISO 2533:1975/Add 2:1997 which extends the standard
atmosphere from -2000 m down to -5000 m geopotential altitude.

Since ISO 2533 is intended for use in calculations and design of
flying vehicles, we use the term ‘altitude’ instead of ‘height’
or ‘elevation’.

The ISO 2533 standard atmosphere is also known as international
standard atmosphere (ISA).  The ISO 2533 standard atmosphere is
equal to the ICAO standard atmosphere.

Non-standard ISA day conditions are usually modeled by adding
a temperature offset to the ISA day temperature.  All functions
of the ISO 2533 library are designed in such a way that you can
calculate air properties for a non-standard temperature.

Air properties are always static quantities unless explicitly
documented otherwise."))

(in-package :iso-2533)

(defmacro defconst (name value &optional doc)
  "Define a constant variable.

This is like ‘defconstant’ except that the initially set value
is reused when the ‘defconst’ form is evaluated again."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defsubst (name arg-list &body body)
  "Define an inline function.

This is like ‘defun’ except that the function is globally marked
for inline expansion by the compiler."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,arg-list
       ,@body)))

(defun ensure-type (object type)
  "Signal a type error if OBJECT is not of the type TYPE.
Otherwise, return OBJECT."
  (if (typep object type)
      object
    (error (make-condition 'type-error :datum object :expected-type type))))

(defsubst square (z)
  "Return Z squared, that is Z raised to the power two.

Argument Z has to be a number."
  (declare (type number z))
  (* z z))

;;; packages.lisp ends here
