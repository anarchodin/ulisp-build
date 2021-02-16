;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defun filter-symbols (definitions)
  "Remove symbols that have features not currently in *ulisp-features*."
  (remove 'nil
   (mapcar #'(lambda (entry) (let ((enum (first entry))
                                   (feature (fifth entry)))
                               (if (keywordp feature)
                                   (if (member feature *ulisp-features*)
                                       enum)
                                   enum)))
           definitions) :from-end t))

(defun write-enums (symbols defs &optional (stream *standard-output*) (margin 90))
  "Take a set of definitions and print the enums. Defaults to standard out and 90 columns."
  (let ((*standard-output* (or stream *standard-output*))
        (*print-right-margin* margin)
        (*print-pretty* t) ;; Better make sure.
        (enums (append (mapcar #'get-symbol-enum symbols)
                       (filter-symbols (apply #'append (mapcar #'cadr defs))))))
    (pprint-logical-block (nil nil)
      (write-string "enum function {")
      (terpri)
      (write-string "  ")
      (pprint-indent :current 0)
      (pprint-logical-block (nil enums :suffix "ENDFUNCTIONS")
        (loop
           (pprint-exit-if-list-exhausted)
           (princ (pprint-pop))
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

(defun write-lookup-table (symbols definitions &optional (stream *standard-output*))
  "Write the lookup table itself."
  (terpri)
  (write-line "// Built-in symbol lookup table" stream)
  (write-line "const tbl_entry_t lookup_table[] PROGMEM = {" stream)
  ;; There are three sections to this now.
  ;; First, the "new-style" symbol definitions.
  (loop for i from 0
        for symbol in symbols do
          (format stream "  { symbol~a, ~a, ~a },~%" i
                  (get-symbol-label symbol)
                  (get-symbol-callc symbol)))
  ;; Second, the "old-style" definitions.
  (let ((i 0))
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
                (incf i))))))))
  ;; Finally, write out the keywords.
  (write-kw-table *platform* stream)
  (format stream "// Insert your own table entries here~%};~%"))

;;; Keyword things.
; TODO: These are kinda similar. Macro?

(defun write-kw-enums (platform &optional (stream *standard-output*) (margin 90))
  "Writes out the keyword enums for the current platform. Defaults to standard out and 90 columns."
  (flet ((write-kw-enum (keywords)
           "Writes the enum with the given keywords."
           (format t "enum keywords {~%  ~<KEYWORDS = ENDFUNCTIONS, ~@{K_~a, ~:_~}~:>ENDKEYWORDS~%};~%" keywords)))
    (let ((*standard-output* (or stream *standard-output*))
          (*print-right-margin* margin))
      (let ((entries (get-keywords platform)))
        (case (length entries)
          (0 (error "No keywords found!"))
          (1 (if (caar entries) (error "Single board entry has a specifier."))
             (write-kw-enum (cadar entries)))
          (t (if (notevery #'car entries) (error "Board entry without specifier, but other entries are present."))
             (format t "#if defined(~a)~%" (caar entries))
             (write-kw-enum (cadar entries))
             (dolist (entry (cdr entries))
               (format t "#elif defined(~a)~%" (car entry))
               (write-kw-enum (cadr entry)))
             (format t "#endif~%")))))))

(defun write-kw-strings (platform &optional (stream *standard-output*))
  "Writes out strings for the keywords on a given platform."
  (labels ((kw->lisp (keyword)
             "Translate the C-style keyword name into a Lisp-friendly one."
             (concatenate 'string ":" (substitute #\- #\_ (string-downcase keyword))))
           (write-the-strings (keywords)
             (let ((i -1))
               (dolist (keyword (mapcar #'kw->lisp keywords))
                 (format stream "const char keyword~a[] PROGMEM = \"~a\";~%" (incf i) keyword)))))
    (format stream "const char emptystring[] PROGMEM = \"\";~%")
    (let ((entries (get-keywords platform)))
      (case (length entries)
        (0 (error "No keywords found!"))
        (1 (if (caar entries) (error "Single board entry has a specifier."))
           (write-the-strings (cadar entries)))
        (t (if (notevery #'car entries) (error "Board entry without specifier, but others are present."))
           (format stream "#if defined(~a)~%" (caar entries))
           (write-the-strings (cadar entries))
           (dolist (entry (cdr entries))
             (format stream "#elif defined(~a)~%" (car entry))
             (write-the-strings (cadr entry)))
           (format stream "#endif~%"))))))

(defun write-kw-table (platform &optional (stream *standard-output*))
  "Writes out the lookup tables for keywords on the current platform."
  (labels ((collapse (entry)
             (mapcar #'(lambda (x) (cons (car entry) x)) (cdr entry)))
           (simplify (board)
             (cons (car board)
                   (mapcan #'collapse (cdr board))))
           (write-kw-entries (keywords)
             (let ((i -1))
               (dolist (keyword keywords)
                 (format stream "  { keyword~a, (fn_ptr_type)((~a << 8) | ~a), CC_KEYWORD }, ~%" (incf i) (cdr keyword) (car keyword))))))
    (format stream "  { emptystring, NULL, 0x00 },~%")
    (let ((entries (mapcar #'simplify (get-keywords-full platform))))
      (case (length entries)
        (0 (error "No keywords found!"))
        (1 (if (caar entries) (error "Single board entry has a specifier."))
           (write-kw-entries (cdar entries)))
        (t (if (notevery #'car entries) (error "Board entry without a specifier, but others are present."))
           (format stream "#if defined(~a)~%" (caar entries))
           (write-kw-entries (cdar entries))
           (dolist (entry (cdr entries))
             (format stream "#elif defined(~a)~%" (car entry))
             (write-kw-entries (cdr entry)))
           (format stream "#endif~%"))))))
