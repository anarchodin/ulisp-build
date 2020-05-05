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
