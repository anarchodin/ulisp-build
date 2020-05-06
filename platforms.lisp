;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defvar *platform* nil "The platform currently being built for.")

(defvar *ulisp-features* '()
  "Symbols that represent the features to be present in the built uLisp. Empty by default.")

(defparameter *platforms*
  '((:avr
     (:types zzero symbol number stream character string pair)
     (:streams serialstream i2cstream spistream sdstream)
     (:features))
    (:arm
     (:types zzero symbol code number stream character float array string pair)
     (:streams serialstream i2cstream spistream sdstream stringstream gfxstream)
     (:features :float :gfx :code :array :stringstream))
    (:riscv
     (:types zzero symbol code number stream character float array string pair)
     (:streams serialstream i2cstream spistream sdstream stringstream gfxstream)
     (:features :float :gfx :code :array :stringstream)))
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
