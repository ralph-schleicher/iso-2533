;;; iso-2533.lisp --- international standard atmosphere

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

(in-package :iso-2533)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'double-float))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconst standard-acceleration-of-gravity 9.80665
    "Standard acceleration of gravity at sea level in meter per square second.

Value is 9.80665 m/s².")

  (defconst standard-pressure 101325.0
    "Standard air pressure at sea level in pascal.

Value is 101325 Pa.")

  (defconst standard-temperature 288.15
    "Standard air temperature at sea level in kelvin.

Value is 288.15 K (15 °C).")

  (defconst standard-density 1.225
    "Standard air density at sea level in kilogram per cubic meter.

Value is 1.225 kg/m³.")

  (defconst avogadro-constant 6.02257E+23
    "Avogadro constant in one per mole.

Value is 6.02257×10²³ 1/mol.

The CODATA recommended value of this constant is 6.02214076×10²³.
However, to reproduce the numerical values defined in ISO 2533 it
is required to utilize the value 6.02257×10²³.")

  (defconst molar-gas-constant 8.31432
    "Molar gas constant in joule per mole kelvin.

Value is 8.31432 J/mol/K.

The CODATA recommended value of this constant is 8.314462618...
However, to reproduce the numerical values defined in ISO 2533 it
is required to utilize the value 8.31432.")

  ;; Quote from ISO 2533: Air molar mass at sea level, as
  ;; obtained from the perfect gas law (2) when introducing
  ;; the adopted values pₙ, ρₙ, Tₙ, R*.
  (defconst molar-mass (progn
                         #+iso-2533-derived-quantities
                         (* (/ (* standard-temperature
	                          standard-density)
	                       standard-pressure)
                            molar-gas-constant)
                         #-iso-2533-derived-quantities
                         0.02896442)
    "Molar mass of dry air in kilogram per mole.

Value is 0.02896442 kg/mol.")

  (defconst specific-gas-constant (progn
                                    #+iso-2533-derived-quantities
				    (/ molar-gas-constant
				       molar-mass)
                                    #-iso-2533-derived-quantities
				    287.05287)
    "Specific gas constant of dry air in joule per kilogram kelvin.

Value is 287.05287 J/kg/K.")
  #+()
  (and (= (coerce (* (/ (* 28815/100
			   1225/1000)
			101325)
		     831432/100000)
		  'double-float)
	  (* (/ (* standard-temperature
		   standard-density)
		standard-pressure)
	     molar-gas-constant))
       (= (coerce (/ 101325
		     (* 28815/100
			1225/1000))
		  'double-float)
	  (/ standard-pressure
	     (* standard-temperature
		standard-density))
	  (/ molar-gas-constant
	     molar-mass)))

  (defconst ratio-of-specific-heats 1.4
    "Ratio of specific heats of dry air.

Value is 1.4.")

  (defconst earth-radius 6356766.0
    "Radius of the earth in meter.

Value is 6356766 m.")

  (define-symbol-macro NA avogadro-constant)
  (define-symbol-macro R* molar-gas-constant)
  (define-symbol-macro R specific-gas-constant)
  (define-symbol-macro M molar-mass)
  (define-symbol-macro gamma ratio-of-specific-heats)
  (define-symbol-macro gn standard-acceleration-of-gravity)
  ;; Effective collision diameter of an air molecule in meter.
  (define-symbol-macro sigma 0.365E-9)
  ;; Zero-point temperature of the Celsius temperature scale.
  (define-symbol-macro To 273.15)

  (defun p/pb (H Hb Tb L)
    "Calculate pressure ratio in an atmosphere layer.

First argument H is the geopotential altitude in meter.
Second argument HB is the geopotential altitude at base level in meter.
Third argument TB is the air temperature at base level in kelvin.
Fourth argument L is the lapse rate in kelvin per meter.

Value is the ratio of the air pressure to the air pressure at base level."
    (let ((dH (- H Hb)))
      (cond ((zerop dH)
	     1)
	    ((zerop L)
	     (exp (- (* (/ gn (* R Tb)) dH))))
	    (t
	     (expt (+ 1 (* (/ L Tb) dH)) (- (/ gn (* L R))))))))

  (defclass layer ()
    ((altitude
      :initarg :altitude
      :type double-float
      :documentation "Geopotential altitude at base level in meter.")
     (pressure
      :initarg :pressure
      :type double-float
      :documentation "Air pressure at base level in pascal.")
     (temperature
      :initarg :temperature
      :type double-float
      :documentation "Air temperature at base level in kelvin.")
     (lapse-rate
      :initarg :lapse-rate
      :type double-float
      :documentation "Air temperature lapse rate in kelvin per meter.")))

  ;; Make Clozure Common Lisp happy.
  (defmethod make-load-form ((object layer) &optional environment)
    (make-load-form-saving-slots object :environment environment))

  (defconst layers (labels ((make-layer (Hb Tb L &optional neighbor)
			      ;; Air pressure at base level.
			      (let ((pb (if (not neighbor)
					    standard-pressure
					  (with-slots ((Hn altitude)
						       (pn pressure)
						       (Tn temperature)
						       (Ln lapse-rate)) neighbor
					    (if (> Hb Hn)
						(* pn (p/pb Hb Hn Tn Ln))
					      (/ pn (p/pb Hn Hb Tb L)))))))
				(make-instance 'layer
				               :altitude Hb
				               :pressure pb
				               :temperature Tb
				               :lapse-rate L))))
		     (let (b (n (make-layer     0.0 288.15 -0.0065)))
		       (vector
			#+iso-2533-addendum-2
			(setf b (make-layer -5000.0 320.65 -0.0065 n))
			#-iso-2533-addendum-2
			(setf b (make-layer -2000.0 301.15 -0.0065 n))
			n
			(setf b (make-layer 11000.0 216.65  0.0    n))
			(setf b (make-layer 20000.0 216.65  0.001  b))
			(setf b (make-layer 32000.0 228.65  0.0028 b))
			(setf b (make-layer 47000.0 270.65  0.0    b))
			(setf b (make-layer 51000.0 270.65 -0.0028 b))
			(setf b (make-layer 71000.0 214.65 -0.002  b))
			;; The mesopause starts at approximately 85 km.
			;; Therefore, continue with the lapse rate of
			;; the mesophere.
			(setf b (make-layer 80000.0 196.65 -0.002  b)))))
    "Sequence of atmosphere layers in increasing order of altitude.")
  (values))

(defconst last-layer-index (1- (length layers))
  "Index of the last atmosphere layer.")

;; Add one permille so that we can also calculate air properties for
;; the geometric altitude down at this level.
(defconst minimum-geopotential-altitude (fround (* (slot-value (svref layers 0) 'altitude) 1.001))
  "Minimum geopotential altitude in meter.")

(defconst maximum-geopotential-altitude (slot-value (svref layers last-layer-index) 'altitude)
  "Maximum geopotential altitude in meter.")

(defconst minimum-pressure-altitude (slot-value (svref layers last-layer-index) 'pressure)
  "Minimum pressure altitude in pascal.")

(defconst maximum-pressure-altitude (slot-value (svref layers 0) 'pressure)
  "Maximum pressure altitude in pascal.")

(defun find-layer (H)
  "Return layer object for a given geopotential altitude.

Argument H is the geopotential altitude in meter."
  (ensure-type H `(real ,minimum-geopotential-altitude
			,maximum-geopotential-altitude))
  (svref layers (iter (for j :from 1 :to last-layer-index)
		      (for layer = (svref layers j))
		      (when (< H (slot-value layer 'altitude))
			(return (1- j)))
		      (finally
		       (return last-layer-index)))))

(defun find-layer* (p)
  "Return layer object for a given pressure altitude.

Argument P is the air pressure in pascal."
  (ensure-type p `(real ,minimum-pressure-altitude
			,maximum-pressure-altitude))
  (svref layers (iter (for j :from 1 :to last-layer-index)
		      (for layer = (svref layers j))
		      (when (> p (slot-value layer 'pressure))
			(return (1- j)))
		      (finally
		       (return last-layer-index)))))

(defun atm (geopotential-altitude &optional (temperature-offset 0))
  "Calculate air pressure and air temperature as a function of altitude.

First argument GEOPOTENTIAL-ALTITUDE is the geopotential altitude in
 meter.  Use the ‘geopotential-altitude’ or ‘flight-level’ function
 for calculating the geopotential altitude.
Optional second argument TEMPERATURE-OFFSET is the temperature offset
 for a non-standard day in kelvin.  Default is 0 K.

Values are the air pressure in pascal and the air temperature in
kelvin."
  (let* ((H geopotential-altitude)
	 (layer (find-layer H)))
    (with-slots ((Hb altitude)
		 (pb pressure)
		 (Tb temperature)
		 (L  lapse-rate)) layer
      (values
       ;; Air pressure.
       (* pb (p/pb H Hb Tb L))
       ;; Air temperature.
       (+ Tb (* L (- H Hb)) temperature-offset)
       ))))

(defun pressure-altitude (pressure)
  "Convert altitude from barometric pressure to geopotential altitude.

Argument PRESSURE is the air pressure in pascal.

Value is the geopotential altitude in meter.

The ‘pressure-altitude’ function is the inverse of the ‘atm’ function."
  (let* ((p pressure)
	 (layer (find-layer* p)))
    (with-slots ((Hb altitude)
		 (pb pressure)
		 (Tb temperature)
		 (L  lapse-rate)) layer
      (cond ((= p pb)
	     Hb)
	    ((zerop L)
	     (- Hb (* (/ (* R Tb) gn) (log (/ p pb)))))
	    (t
	     (+ Hb (* (/ Tb L) (- (exp (- (* (/ (* L R) gn) (log (/ p pb))))) 1)))
	     )))))

(defun acceleration-of-gravity (geometric-altitude)
  "Calculate acceleration of gravity as a function of altitude.

Argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.

Value is the acceleration of gravity in kilogram per square second."
  (* gn (square (/ earth-radius (+ earth-radius geometric-altitude)))))

(defun geometric-altitude (geopotential-altitude)
  "Convert altitude from geopotential altitude to geometric altitude.

Argument GEOPOTENTIAL-ALTITUDE is the geopotential altitude in meter.

Value is the geometric altitude in meter."
  (/ (* earth-radius geopotential-altitude)
     (- earth-radius geopotential-altitude)))

(defun geopotential-altitude (geometric-altitude)
  "Convert altitude from geometric altitude to geopotential altitude.

Argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.

Value is the geopotential altitude in meter."
  (/ (* earth-radius geometric-altitude)
     (+ earth-radius geometric-altitude)))

(defun flight-level (number)
  "Convert altitude from flight level to geopotential altitude.

Argument NUMBER is the flight level in hundreds of feet.

Value is the geopotential altitude in meter."
  (* number 30.48))

(defun density (pressure temperature)
  "Calculate the density of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the air density in kilogram per cubic meter."
  (/ pressure (* R temperature)))

(defun specific-weight (geometric-altitude &optional (pressure 0 pressure-supplied-p) (temperature 0 temperature-supplied-p))
  "Calculate the specific weight of air.

First argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.
Optional second argument PRESSURE is the air pressure in pascal.
Optional third argument TEMPERATURE is the air temperature in kelvin.

Value is the specific weight of air in newton per cubic meter.

If any of the optional arguments PRESSURE and TEMPERATURE is not
supplied, then calculate the air pressure and air temperature as a
function of the geometric altitude."
  (unless (and pressure-supplied-p temperature-supplied-p)
    (multiple-value-setq (pressure temperature)
      (atm (geopotential-altitude geometric-altitude))))
  (* (acceleration-of-gravity geometric-altitude)
     (density pressure temperature)))

(defun pressure-scale-height (geometric-altitude &optional (temperature 0 temperature-supplied-p))
  "Calculate the pressure scale height of air.

First argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.
Optional second argument TEMPERATURE is the air temperature in kelvin.

Value is the pressure scale height of air in meter.

If optional argument TEMPERATURE is not supplied, then calculate the
air temperature as a function of the geometric altitude."
  (let ((pressure 0))
    (declare (ignorable pressure))
    (unless temperature-supplied-p
      (multiple-value-setq (pressure temperature)
	(atm (geopotential-altitude geometric-altitude)))))
  (/ (* R temperature) (acceleration-of-gravity geometric-altitude)))

(defun number-density (pressure temperature)
  "Calculate the number density of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the number of air particles per cubic meter."
  (/ (* NA pressure)
     (* R* temperature)))

(defun mean-speed (temperature)
  "Calculate the mean speed of an air particle, that is
the expected value of the Maxwell speed distribution.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the mean speed in meter per second."
  (* #.(sqrt (/ 8 pi)) (sqrt (* R temperature))))

(defun mean-free-path (pressure temperature)
  "Calculate the mean free path of an air particle.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the mean free path of an air particle in meter."
  (* #.(/ R* (* (sqrt 2.0) pi NA (square sigma)))
     (/ temperature pressure)))

(defun collision-frequency (pressure temperature)
  "Calculate the collision frequency of an air particle.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the collision frequency of an air particle in hertz."
  (* #.(* 4 NA (square sigma) (sqrt (/ pi (* R* M))))
     (/ pressure (sqrt temperature))))

(defun speed-of-sound (temperature)
  "Calculate the speed of sound of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the speed of sound of air in meter per second."
  (sqrt (* gamma R temperature)))

(defun dynamic-viscosity (temperature)
  "Calculate the dynamic viscosity of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the dynamic viscosity of air in pascal second.

The dynamic viscosity µ is calculated from the following empirical
formula:
                  -6  3/2
          1.458×10   T
     µ = -----------------
             T + 110.4

where T is the temperature."
  (/ (* 1.458E-6 (expt temperature 3/2))
     (+ temperature 110.4)))

(defun kinematic-viscosity (pressure temperature)
  "Calculate the kinematic viscosity of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the kinematic viscosity of air in square meter per second."
  (/ (dynamic-viscosity temperature)
     (density pressure temperature)))

(defun thermal-conductivity (temperature)
  "Calculate the thermal conductivity of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the thermal conductivity of air in watt per meter kelvin.

The thermal conductivity λ is calculated from the following empirical
formula:
                     -3  3/2
          2.648151×10   T
     λ = --------------------
                      -12/T
          T + 245.4×10

where T is the temperature."
  (/ (* 2.648151E-3 (expt temperature 3/2))
     (+ temperature (* 245.4 (expt 10 (/ -12 temperature))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *read-default-float-format* 'single-float))

;;; iso-2533.lisp ends here
