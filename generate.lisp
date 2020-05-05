;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defvar *ulisp-outfile* #P"ulisp/ulisp.ino")

;; Generate *********************************************************************************************

(defun write-no-comments (stream string comments)
  (cond
   (comments
    (write-string string stream)
    (terpri stream))
   (t
    (let ((start 0))
      (loop
       (let* ((com (search "/*" string :start2 start))
              (ment (when com (search "*/" string :start2 com))))
         (cond
          ((and com ment (> (- ment com) 32)) (write-string string stream :start start :end com)
           (setq start (+ ment 3))) ; Swallow return too
          (t (write-string string stream :start start)
             (terpri stream)
             (return)))))))))

(defun generate (&optional (platform :avr) (comments nil))
 (let ((*ulisp-features* (get-features platform))
       (*platform* platform)
       (definitions *definitions*)) ; (case platform (:zero *definitions-zero*) (t *definitions*))
   (flet ((include (section &optional (str *standard-output*))
           (let ((special (intern (format nil "*~a-~a*" section platform) :ulisp-build))
                 (default (intern (format nil "*~a*" section) :ulisp-build)))
             (cond
              ((boundp special) 
               (let ((inc (eval special)))
                 (cond
                  ((listp inc) (map nil #'(lambda (x) (write-no-comments str x comments)) inc))
                  (t (write-no-comments str inc comments)))))
              ((boundp default) 
               (let ((inc (eval default)))
                 (cond
                  ((listp inc) (map nil #'(lambda (x) (write-no-comments str x comments)) inc))
                  (t (write-no-comments str inc comments)))))
              (t nil)))))
    ;;
     (with-open-file (*standard-output* #-lispworks *ulisp-outfile*
                          #+lispworks (capi:prompt-for-file "Output File" :operation :save
                                                            :pathname "/Users/david/Desktop/")
                          :direction :output :if-does-not-exist :create :if-exists :supersede)
    ;; Write preamble

    (write-section :header)

    (write-constants platform) ; Per-platform constants.
    (write-section :macros) ; Common macro definitions.

    ;; Write enum declarations - print-enums is defined in definitions.lisp
    (terpri)
    (write-string "// See uLisp internals documentation for details on how the ")
    (write-line "following enum is derived.")
    (print-enums definitions)
    (terpri)

    (write-section :typedefs)
    (write-section :workspace)
    (write-section :globals)
    (write-section :error)
    (write-section :setup-workspace)
    (write-section :make-objects)
    (write-section :gc)
    (write-section :compactimage)
    (write-section :make-filename)
    (write-section :saveimage)
    (write-section :trace)
    (write-section :helpers)
    (write-radix-encoding platform)
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
    (when (member :code *ulisp-features*)
      (write-section :assembler))
    (when (member :interrupts *ulisp-features*)
      (write-section :interrupts))
    ;; Write function definitions
    (write-definitions definitions)

    ;; Write PROGMEM strings
    (format t "~%// Insert your own function definitions here~%")
    (format t "~%// Built-in procedure names - stored in PROGMEM~%~%")
    (write-table-strings definitions)

    ;; Write table
    (write-lookup-table definitions)

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
  't))))
