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
       (maxsymbol 0)
       (definitions (case platform (:zero *definitions-zero*) (t *definitions*))))
   (flet ((include (section str)
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
     (with-open-file (str #-lispworks *ulisp-outfile*
                          #+lispworks (capi:prompt-for-file "Output File" :operation :save
                                                            :pathname "/Users/david/Desktop/")
                          :direction :output)
    ;; Write preamble
    ; (include :header str)
    (write-no-comments str (eval (intern (format nil "*~a-~a*" :header platform) :ulisp-build)) t)

    ;; The next two are from preface.lisp
    (write-macros str) ; Common macro definitions.
    (write-constants platform str) ; Per-platform constants.

    ;; Write enum declarations - print-enums is defined in extras.lisp
    (fresh-line str)
    (write-string "// See uLisp internals documentation for details on how the " str)
    (write-line "following enum is derived." str)
    (print-enums definitions str)
    (include :typedefs str)
    (include :workspace str)
    (include :global-variables str)
    (include :error-handling str)
    (include :setup-workspace str)
    (include :make-objects str)
    ;; Write utilities
    (include :garbage-collection str)
    (include :compactimage str)
    (include :make-filename str)
    (include :saveimage str)
    (include :tracing str)
    (include :helper-functions str)
    (include :association-lists str)
    (include :array-utilities str)
    (include :string-utilities str)
    (include :string-utilities2 str)
    (include :closures str)
    (include :in-place str)
    (include :i2c-interface str)
    (include :stream-interface str)
    ; (include :watchdog str)
    (include :check-pins str)
    (include :note str)
    (include :sleep str)
    (include :prettyprint str)
    ;; See assembler.lisp
    (write-assembler str)
    #+interrupts
    (include :interrupts str)
    ;; Write function definitions
    ;; FIXME: Put this in its own function (and file).
    (dolist (section definitions)
      (destructuring-bind (comment defs &optional prefix) section
        (declare (ignore prefix))
        (unless (string= comment "Symbols") (format str "~%// ~a~%" comment))
        (dolist (item defs)
          (destructuring-bind (enum string min max definition) item
            (declare (ignore min max definition))
            (let ((source (get-definition enum)))
              (if source
                  (write-string (subseq source (1+ (position #\Linefeed source))) str)))))))
    ;; Write PROGMEM strings
    (format str "~%// Insert your own function definitions here~%")
    (format str "~%// Built-in procedure names - stored in PROGMEM~%~%")
    (let ((i 0))
      (dolist (section definitions)
        (destructuring-bind (comment defs &optional prefix) section
          (declare (ignore comment prefix))
          (dolist (item defs)
            (destructuring-bind (enum string min max definition) item
              (declare (ignore definition min max))
              (let ((lower (string-downcase enum)))
                (format str "const char string~a[] PROGMEM = \"~a\";~%" i (or string lower))
                (setq maxsymbol (max maxsymbol (length (or string lower))))
                (incf i)))))))
    ;; Write table
    (let ((i 0)
          (comment "// Third parameter is no. of arguments; 1st hex digit is min, 2nd hex digit is max, 0xF is unlimited"))
      (format str "~%~a~%const tbl_entry_t lookup_table[] PROGMEM = {~%" comment)
      (dolist (section definitions)
        (destructuring-bind (comment defs &optional (prefix "fn")) section
          (declare (ignore comment))
          (dolist (item defs)
            (destructuring-bind (enum string min max definition) item
              (declare (ignore string))
              (let ((lower (cond
                            ((consp definition) (string-downcase (car definition)))
                            (t (string-downcase enum)))))
                (format str "  { string~a, ~:[NULL~2*~;~a_~a~], 0x~2,'0x },~%" i definition prefix lower (+ (ash min 4) (min max 15)))
                (incf i))))))
      (format str "};~%"))
    ;; Write rest
    (include :table str)
    (include :eval str)
    (include :print-functions str)
    (include :read-functions str)
    (when (eq platform :tlc) (write-string *tiny-lisp-computer* str))
    (when (eq platform :badge) (write-string *lisp-badge* str))
    (include :setup str)
    (include :repl str)
    (include :loop str)
  maxsymbol))))
