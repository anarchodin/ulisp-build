(require 'asdf)
(asdf:initialize-source-registry `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))

;; Note that e.g. ECL gives a way to override this. See test.sh
(defvar *variant* :avr "Which variant to build.")

(asdf:load-system "ulisp-build")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (in-package :ulisp-build))

(defvar *output-dir*
  (asdf:system-relative-pathname "ulisp-build" "ulisp/"))

;; The system reads information from platforms.lisp for information about the
;; features to include. Currently defined there are :arm, :avr, and :riscv.

(dolist (platform (mapcar #'car *platforms*))
  (format t "Building uLisp for variant ~a.~%" platform)
  (generate (merge-pathnames *output-dir* (format nil "ulisp-~a.ino" (string-downcase platform)))
            platform))

#+abcl (extensions:quit)
#+ecl (ext:quit)
