;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defvar *platform* nil "The platform currently being built for.")

(defvar *ulisp-features* '()
  "Symbols that represent the features to be present in the built uLisp. Empty by default.")

(defparameter *platforms*
  '((:avr
     (:types zzero symbol code number stream string pair)
     (:streams serial i2c spi sd)
     (:keywords
      ("CPU_ATmega328P"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT INTERNAL EXTERNAL))
      ("CPU_ATmega1284P"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT INTERNAL1V1 INTERNAL2V56 EXTERNAL))
      ("CPU_ATmega2560"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT INTERNAL1V1 INTERNAL2V56 EXTERNAL))
      ("CPU_ATmega4809"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT INTERNAL VDD INTERNAL0V55 INTERNAL1V1 INTERNAL1V5
                        INTERNAL2V5 INTERNAL4V3 EXTERNAL))
      ("CPU_AVR128DX48"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT VDD INTERNAL1V024 INTERNAL2V048 INTERNAL4V096
                        INTERNAL2V5 EXTERNAL)
       (ANALOGREAD ADC_DAC0 ADC_TEMPERATURE)))
     (:features :code :dacreference))
    (:arm
     (:types zzero code number stream float array string pair)
     (:streams serial i2c spi sd string gfx)
     (:keywords
      ("CPU_ATSAMD21"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
       (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL1V0 AR_INTERNAL1V65 AR_INTERNAL2V23
                        AR_EXTERNAL))
      ("CPU_ATSAMD51"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
       (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL1V0 AR_INTERNAL1V1 AR_INTERNAL1V2
                        AR_INTERNAL1V25 AR_INTERNAL2V0 AR_INTERNAL2V2
                        AR_INTERNAL2V23 AR_INTERNAL2V4 AR_INTERNAL2V5 AR_INTERNAL1V65
                        AR_EXTERNAL))
      ("CPU_NRF51822"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
       (ANALOGREFERENCE AR_DEFAULT AR_VBG AR_SUPPLY_ONE_HALF AR_SUPPLY_ONE_THIRD
                        AR_EXT0 AR_EXT1))
      ("CPU_NRF52840"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
       (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL AR_INTERNAL_3_0 AR_INTERNAL_2_4
                        AR_INTERNAL_1_8 AR_INTERNAL_1_2 AR_VDD4))
      ("CPU_NRF52833"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)
       (ANALOGREFERENCE AR_DEFAULT AR_INTERNAL AR_VDD4))
      ("CPU_iMXRT1062"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT OUTPUT_OPENDRAIN))
      ("CPU_MAX32620"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT)
       (ANALOGREFERENCE DEFAULT EXTERNAL)))
     (:features :float :gfx :code :array :stringstream :write-resolution))
    (:esp
     (:types zzero number stream float array string pair)
     (:streams serial i2c spi sd wifi string gfx)
     (:keywords 
      ("ESP8266"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP OUTPUT))
      ("ESP32"
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)))
     (:features :float :gfx :code :array :stringstream :ethernet))
    (:riscv
     (:types zzero code number stream float array string pair)
     (:streams serial i2c spi sd string gfx)
     (:keywords
      (nil
       (DIGITALWRITE HIGH LOW)
       (PINMODE INPUT INPUT_PULLUP INPUT_PULLDOWN OUTPUT)))
     (:features :float :gfx :code :array :stringstream :write-resolution)))
  "An alist of alists, representing various characteristics of the platforms uLisp runs on.")

(defun get-streams (platform)
  "Returns a list of streams supported on a given platform."
  (cdr (assoc :streams (cdr (assoc platform *platforms*)))))

(defun get-types (platform)
  "Returns a list of types to be used on a given platform."
  (cdr (assoc :types (cdr (assoc platform *platforms*)))))

(defun get-features (platform)
  "Returns a list of features supported on a given platform."
  (cons platform (cdr (assoc :features (cdr (assoc platform *platforms*))))))

(defun get-keywords-full (platform)
  "Returns information about keywords supported on a given platform in full detail."
  (cdr (assoc :keywords (cdr (assoc platform *platforms*)))))

(defun get-keywords (platform)
  "Returns information about keywords supported on a given platform."
  (flet ((simplify (board)
           "Remove context not always needed."
             (list (first board)
                   (apply #'append (mapcar #'rest (cdr board))))))
    (mapcar #'simplify (cdr (assoc :keywords (cdr (assoc platform *platforms*)))))))
