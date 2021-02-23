;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

;;;; Functions to deal with symbol entries.

;; NOTE: This may belong elsewhere.
(defun get-symbol-entries (source-file)
  "Return symbol entries for a given C source file."
  (with-open-file (source-stream (asdf:system-relative-pathname :ulisp-build source-file))
    (do ((symbols '())
         (line ""))
        ((null line)
         (nreverse symbols))
      (setf line (read-line source-stream nil))
      (if (string= "//;;" (subseq line 0 (min 4 (length line))))
          (push (read-from-string line nil nil :start 4) symbols)))))

(defun get-symbol-name (symbol-entry)
  "Return the symbol name for use in a C string."
  (destructuring-bind (symbol &key (type :function) &allow-other-keys)
      symbol-entry
    (string-downcase (if (eq type :keyword)
                         (concatenate 'string ":" (string symbol))
                         (string symbol)))))

(defun get-symbol-enum (symbol-entry)
  "Return a string suitable as a C enum value."
  (destructuring-bind (symbol &key
                                (enum (string symbol))
                                (type :function)
                       &allow-other-keys)
      symbol-entry
    (remove #\- (string-upcase (if (eq type :keyword)
                                   (concatenate 'string "K_" enum)
                                   enum)))))

(defun get-symbol-label (symbol-entry)
  "Return a string naming the linker label for an implementing function."
  (destructuring-bind (symbol &key
                                (type :function)
                                (enum (string symbol))
                                (label (remove #\- (string-downcase enum)) label-given-p)
                                (value (substitute #\_ #\- (string symbol)))
                                context
                       &allow-other-keys)
      symbol-entry
    (case type
      (:symbol "NULL")
      (:special (if label-given-p label (concatenate 'string "sp_" label)))
      (:tail (if label-given-p label (concatenate 'string "tf_" label)))
      (:keyword (format nil "(fn_ptr_type)((~a << 8) | ~a)" value context))
      (:function (if label-given-p label (concatenate 'string "fn_" label)))
      (t (error "Unknown symbol type: ~a." type)))))

(defun get-symbol-callc (symbol-entry)
  "Return a value suitable for callc."
  (destructuring-bind (symbol &key (type :function) (min 0) (max 15) &allow-other-keys)
      symbol-entry
    (declare (ignore symbol))
    (check-type min (integer 0 7))
    (check-type max (integer 0 15))
    (assert (<= min max) (min max)
            "Minimum (~a) can't be higher than maximum (~a)." min max)
    (case type
      (:symbol "CC_SYMBOL")
      (:special "CC_SPECIAL")
      (:tail "CC_TAIL")
      (:keyword "CC_KEYWORD")
      (:function (format nil "0x~x" (logior (ash min 4) max)))
      (t (error "Unknown symbol type: ~a." type)))))
