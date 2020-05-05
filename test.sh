#!/bin/sh
ecl --norc --eval '(defvar *variant* :arm)' --load run-generator.lisp
arduino-cli compile -b adafruit:samd:adafruit_itsybitsy_m4 ulisp/
