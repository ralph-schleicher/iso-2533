;;; generate-doc.lisp --- generate documentation

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

(ql:quickload "iso-2533")
(ql:quickload "rs-doc") ;private

(in-package :rs-doc-user)

(let ((data (gather-doc
             :package :iso-2533
             :symbols '(iso-2533:standard-acceleration-of-gravity
                        iso-2533:standard-pressure
                        iso-2533:standard-temperature
                        iso-2533:standard-density
                        iso-2533:avogadro-constant
                        iso-2533:molar-gas-constant
                        iso-2533:molar-mass
                        iso-2533:specific-gas-constant
                        iso-2533:ratio-of-specific-heats
                        iso-2533:earth-radius
                        iso-2533:atm
                        iso-2533:pressure-altitude
                        iso-2533:acceleration-of-gravity
                        iso-2533:geometric-altitude
                        iso-2533:geopotential-altitude
                        iso-2533:flight-level
                        iso-2533:density
                        iso-2533:specific-weight
                        iso-2533:pressure-scale-height
                        iso-2533:number-density
                        iso-2533:mean-speed
                        iso-2533:mean-free-path
                        iso-2533:collision-frequency
                        iso-2533:speed-of-sound
                        iso-2533:dynamic-viscosity
                        iso-2533:kinematic-viscosity
                        iso-2533:thermal-conductivity)))
      (doc-dir (merge-pathnames
                (make-pathname :directory '(:relative "doc"))
                (asdf:system-source-directory "iso-2533"))))
  (generate-doc
   :data data
   :output-format :html
   :output (merge-pathnames (uiop:parse-unix-namestring "iso-2533.html") doc-dir))
  (generate-doc
   :data data
   :output-format :text
   :output (merge-pathnames (uiop:parse-unix-namestring "iso-2533.txt") doc-dir))
  ())

;;; generate-doc.lisp ends here
