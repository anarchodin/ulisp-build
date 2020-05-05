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
       (maxsymbol 0)
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
                          :direction :output)
    ;; Write preamble

    (write-section :header)

    (write-constants platform) ; Per-platform constants.
    (write-section :macros) ; Common macro definitions.

    ;; Write enum declarations - print-enums is defined in extras.lisp
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
    (include :prettyprint) ; TODO: Move pretty printer to new format
    (when (member :code *ulisp-features*)
      (write-section :assembler))
    (when (member :interrupts *ulisp-features*)
      (write-section :interrupts))
    ;; Write function definitions
    ;; FIXME: Put this in its own function (and file).
    (dolist (section definitions)
      (destructuring-bind (comment defs &optional prefix) section
        (declare (ignore prefix))
        (unless (string= comment "Symbols") (format t "~%// ~a~%" comment))
        (dolist (item defs)
          (destructuring-bind (enum string min max definition) item
            (declare (ignore string min max definition))
            (let ((source (get-definition enum)))
              (if source
                  (write-string (subseq source (1+ (position #\Linefeed source))))))))))
    ;; Write PROGMEM strings
    (format t "~%// Insert your own function definitions here~%")
    (format t "~%// Built-in procedure names - stored in PROGMEM~%~%")
    (let ((i 0))
      (dolist (section definitions)
        (destructuring-bind (comment defs &optional prefix) section
          (declare (ignore comment prefix))
          (dolist (item defs)
            (destructuring-bind (enum string min max definition) item
              (declare (ignore definition min max))
              (let ((lower (string-downcase enum)))
                (format t "const char string~a[] PROGMEM = \"~a\";~%" i (or string lower))
                (setq maxsymbol (max maxsymbol (length (or string lower))))
                (incf i)))))))
    ;; Write table
    (let ((i 0)
          (comment "// Third parameter is no. of arguments; 1st hex digit is min, 2nd hex digit is max, 0xF is unlimited"))
      (format t "~%~a~%const tbl_entry_t lookup_table[] PROGMEM = {~%" comment)
      (dolist (section definitions)
        (destructuring-bind (comment defs &optional (prefix "fn")) section
          (declare (ignore comment))
          (dolist (item defs)
            (destructuring-bind (enum string min max definition) item
              (declare (ignore string))
              (let ((lower (cond
                            ((consp definition) (string-downcase (car definition)))
                            (t (string-downcase enum)))))
                (format t "  { string~a, ~:[NULL~2*~;~a_~a~], 0x~2,'0x },~%" i definition prefix lower (+ (ash min 4) (min max 15)))
                (incf i))))))
      (format t "};~%"))
    ;; Write rest -- postscript.lisp
    (write-section :table)
    (write-section :eval)
    (include :print-functions) ; TODO: Move to new format
    (include :read-functions) ; TODO: Move to new format
    ;(when (eq platform :tlc) (write-string *tiny-lisp-computer*))
    ;(when (eq platform :badge) (write-string *lisp-badge*))
    (write-section :setup)
    (write-section :repl)
    (write-section :loop)
  maxsymbol))))
