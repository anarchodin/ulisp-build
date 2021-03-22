;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defun generate (output-file &optional (platform :avr))
  "Generate uLisp .ino files using the facilities defined by the system."
 (let ((*ulisp-features* (get-features platform))
       (*platform* platform)
       (symbols (append *core-symbols* (mapcan #'get-symbol-entries (get-sources platform))))
       (definitions *definitions*))
    ;;
   (with-open-file (*standard-output* output-file :direction :output
                                                  :if-does-not-exist :create
                                                  :if-exists :supersede)
    ;; Write preamble

    (format t "/* uLisp for ~a - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - unreleased

   This version includes alterations by Herbert Snorrason.

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/
" platform)

    (write-section :header)
    (write-section :workspace)

    (write-constants platform) ; Per-platform constants.
    (write-section :macros) ; Common macro definitions.
    (write-section :typedefs)

    ;; Write enum declarations - print-enums is defined in definitions.lisp
    (terpri)
    (write-string "// See uLisp internals documentation for details on how the ")
    (write-line "following enum is derived.")
    (write-enums symbols definitions)
    (terpri)
    (write-kw-enums platform)
    (terpri)

    (write-section :globals)
    (write-section :error)
    (write-section :setup-workspace)
    (write-section :make-objects)
    (write-section :gc)
    (write-section :compactimage)
    (write-section :make-filename)
    (write-section :saveimage)
    (write-section :trace)
    (write-radix-encoding platform)
    (write-section :helpers)
    (write-section :alist)
    (unless (eq platform :avr)
      (write-section :array))
    (write-section :string)
    (when (member :ethernet *ulisp-features*)
      (write-section :stringconv))
    (write-section :closure)
    (write-section :in-place)
    (write-section :i2c-interface)
    (write-section :stream-interface)
    ; (include :watchdog str)
    (write-section :check-pins)
    (write-section :note)
    (write-section :sleep)
    (write-section :pprint)
    (when (member :interrupts *ulisp-features*)
      (write-section :interrupts))
    ;; Write function definitions
    (dolist (source-file (get-sources platform))
      (let ((code (uiop:read-file-string
                   (asdf:system-relative-pathname :ulisp-build source-file))))
        (format t "#line 1 \"~a\"~%" source-file)
        (princ code)))
    (write-definitions definitions)

    ;; Write PROGMEM strings
    (format t "~%// Insert your own function definitions here~%")
    (format t "~%// Built-in symbol names~%")

    (loop for i from 0
          for symbol in symbols do
            (format t "const char symbol~a[] PROGMEM = \"~a\";~%" i
                    (get-symbol-name symbol)))
    (write-table-strings definitions)
    (write-kw-strings platform)
    (format t "// Insert your own function names here~%")

    ;; Write table
    (write-lookup-table symbols definitions)

    ;; Write rest -- postscript.lisp
    (write-section :table)
    (write-section :eval)
    (write-section :print-functions)
    (write-section :read-functions)
    ;(when (eq platform :tlc) (write-string *tiny-lisp-computer*))
    ;(when (eq platform :badge) (write-string *lisp-badge*))
    (write-section :setup)
    (write-section :repl)
    (write-section :loop)
  't)))
