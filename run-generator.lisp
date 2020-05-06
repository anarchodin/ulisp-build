(require 'asdf)
(asdf:initialize-source-registry `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))

;; Note that e.g. ECL gives a way to override this. See test.sh
(defvar *variant* :avr "Which variant to build.")

(asdf:load-system "ulisp-build")
(import '*variant* :ulisp-build)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :ulisp-build))

;; The system reads information from platforms.lisp for information about the
;; features to include. Currently defined there are :arm, :avr, and :riscv.

(format t "Building uLisp for variant ~a.~%" *variant*)
(generate *variant*)

#+ecl (ext:quit)
