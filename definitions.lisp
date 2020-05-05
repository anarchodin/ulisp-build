;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defun print-enums (defs &optional (stream *standard-output*) (margin 90))
  "Take a set of definitions and print the enums. Defaults to standard out and 90 columns."
  (let ((*standard-output* (or stream *standard-output*))
        (*print-right-margin* margin)
        (symbols (mapcar #'car (apply #'append (mapcar #'cadr defs)))))
    (pprint-logical-block (nil nil)
      (write-string "enum function {")
      (terpri)
      (write-string "  ")
      (pprint-indent :current 0)
      (pprint-logical-block (nil symbols :suffix "ENDFUNCTIONS")
        (loop
           (pprint-exit-if-list-exhausted)
           (write (pprint-pop))
           (write-string ", ")
           (pprint-newline :fill)))
      (terpri)
      (write-string "};")
      (terpri))))

(defun fn-pathname (enum &optional feature)
  "Returns path to the C file implementing a given function with a given feature, or the default file if feature is not provided. If no such file exists, return nil."
  (let ((fn-directory (asdf:system-relative-pathname "ulisp-build" "functions/"))
        (subdir (if feature (make-pathname :directory (list :relative (string-downcase feature)))))
        (filename (make-pathname :name (string-downcase enum) :type "c")))
    (probe-file (merge-pathnames filename (if subdir
                                              (merge-pathnames subdir fn-directory)
                                              fn-directory)))))

(defun get-definition (enum)
  "Retrieve source code for a given function. Consults *ulisp-features* to choose variant. Warns if multiple variants are applicable. Returns null if none is found."
  (let ((sources '()))
    (dolist (feature *ulisp-features*)
      (let ((filename (fn-pathname enum feature)))
        (if (and filename (probe-file filename))
            (push (uiop:read-file-string filename) sources))))
    (case (length sources)
      (1 (car sources)) ; Single variant.
      (0 (let ((filename (fn-pathname enum))) ; No variant, so check default.
           (if (and filename (probe-file filename))
               (uiop:read-file-string filename))))
      (t (warn "Found multiple definitions for ~a." enum)
         (car sources)))))

(defun write-definitions (definitions &optional (stream *standard-output*))
  "Writes out the code of user-visible functions based on the provided declarations."
  (dolist (section definitions)
    (destructuring-bind (comment defs &optional prefix) section
      (declare (ignore prefix))
      (unless (string= comment "Symbols") (format t "~%// ~a~%" comment))
      (dolist (item defs)
        (destructuring-bind (enum string min max &optional feature) item
          (declare (ignore string min max))
          (unless (and (keywordp feature)
                       (not (member feature *ulisp-features*)))
            (let ((source (get-definition enum)))
              (if source
                  (write-string (subseq source (1+ (position #\Linefeed source))) stream)))))))))

(defun write-table-strings (definitions &optional (stream *standard-output*))
  "Writes out the strings for the symbols in the provided definitions."
  (let ((i 0))
    (dolist (section definitions)
      (destructuring-bind (comment defs &optional prefix) section
        (declare (ignore comment prefix))
        (dolist (item defs)
          (destructuring-bind (enum string min max &optional feature) item
            (declare (ignore min max))
            (unless (and (keywordp feature)
                         (not (member feature *ulisp-features*)))
              (let ((lower (string-downcase enum)))
                (format stream "const char string~a[] PROGMEM = \"~a\";~%" i (or string lower))
                (incf i)))))))))

(defun write-lookup-table (definitions &optional (stream *standard-output*))
  "Write the lookup table itself."
  (let ((i 0))
    (terpri)
    (write-line "// Third parameter is no. of arguments; 1st hex digit is min, 2nd hex digit is max, 0xF is unlimited" stream)
    (write-line "const tbl_entry_t lookup_table[] PROGMEM = {" stream)
    (dolist (section definitions)
      (destructuring-bind (comment defs &optional (prefix "fn")) section
        (declare (ignore comment))
        (dolist (item defs)
          (destructuring-bind (enum string min max &optional feature) item
            (declare (ignore string))
            (unless (and (keywordp feature)
                         (not (member feature *ulisp-features*)))
              (let ((definedp (or (get-definition enum) (consp feature)))
                    (lower (cond
                             ((consp feature) (string-downcase (car feature)))
                             (t (string-downcase enum)))))
                (format stream "  { string~a, ~:[NULL~2*~;~a_~a~], 0x~2,'0x },~%" i definedp prefix lower (+ (ash min 4) (min max 15)))
                (incf i)))))))
      (format stream "};~%")))
