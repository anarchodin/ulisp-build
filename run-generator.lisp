(require 'asdf)
(asdf:initialize-source-registry `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))

;; Note that e.g. ECL gives a way to override this. See test.sh
(defvar *variant* :avr "Which variant to build.")

(asdf:load-system "ulisp-build")
(import '*variant* :ulisp-build)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :ulisp-build))

(defvar *output-file*
  #-lispworks (asdf:system-relative-pathname "ulisp-build" "ulisp/ulisp.ino")
  #+lispworks (capi:prompt-for-file "Output File" :operation :save
                                                  :pathname "/Users/david/Desktop/"))

;; The system reads information from platforms.lisp for information about the
;; features to include. Currently defined there are :arm, :avr, and :riscv.

(format t "Building uLisp for variant ~a.~%" *variant*)
(generate *output-file* *variant*)

#+ecl (ext:quit)
