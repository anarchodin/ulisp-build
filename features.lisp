;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

;; TODO: Take this flag away once things have been refactored.
(defvar *include-lowlevel* nil
  "Whether to include certain very low-level features.")

(defparameter *base-sources*
  '("src/specials.c" "src/accessors.c" "src/control-flow.c" "src/core.c" "src/cxr.c" "src/list.c"
    "src/num-test.c" "src/bitwise.c" "src/string.c" "src/printer.c" "src/system.c" "src/char.c"
    "src/arduino.cc")
  "Source files to be included in all variants.")

(defparameter *float-sources*
  '("src/float/arith.c"
    "src/float/compare.c"
    "src/float/ops.c"
    "src/float/in-place.c")
  "Source files to be included in floating-point variants.")

(defparameter *int-sources*
  '("src/int/arith.c"
    "src/int/compare.c"
    "src/int/ops.c"
    "src/int/in-place.c")
  "Source files to be included in integer-only variants.")

(defparameter *array-sources*
  '("src/arrays.c")
  "Source files to be included in array-supporting variants.")

(defparameter *gfx-sources*
  '("src/gfx.cc")
  "Source files to be included for the graphics extension.")

(defparameter *wifi-sources*
  '("src/wifi.cc")
  "Source files to be included for the WiFi extension.")

(defun get-sources (platform)
  (let ((features (get-features platform)))
    (append *base-sources*
            (if (member :float features)
                *float-sources*
                *int-sources*)
            (if (member :array features)
                *array-sources*)
            (if (member :ethernet features)
                *wifi-sources*)
            (if (member :gfx features)
                *gfx-sources*)
            (if *include-lowlevel* '("src/low-level.c")))))
