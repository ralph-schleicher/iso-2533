;;; packages.lisp --- package definitions

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

(defpackage #:de.ralph-schleicher.iso-2533
  (:nicknames :iso-2533)
  (:use :common-lisp
	:iterate)
  (:export
   ;; iso-2533.lisp
   #:standard-acceleration-of-gravity
   #:standard-pressure
   #:standard-temperature
   #:standard-density
   #:avogadro-constant
   #:molar-gas-constant
   #:molar-mass
   #:specific-gas-constant
   #:ratio-of-specific-heats
   #:earth-radius
   #:atm
   #:pressure-altitude
   #:acceleration-of-gravity
   #:geometric-altitude
   #:geopotential-altitude
   #:flight-level
   #:density
   #:specific-weight
   #:pressure-scale-height
   #:number-density
   #:mean-speed
   #:mean-free-path
   #:collision-frequency
   #:speed-of-sound
   #:dynamic-viscosity
   #:kinematic-viscosity
   #:thermal-conductivity)
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

;;; packages.lisp ends here
