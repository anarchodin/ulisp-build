;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

;; FIXME: Don't know where this makes sense, so here it goes.
(defmacro defsection (name filename)
  "Generates a function, called name, that writes the contents of filename to a stream."
  `(defun ,name (stream)
     (let* ((source-file (asdf:system-relative-pathname "ulisp-build" ,filename))
            (source-code (uiop:read-file-string source-file)))
       (write-string source-code stream))))

; To run do (generate)

; Sharp-double-quote

(defun sharp-double-quote-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character
  #\# #\" #'sharp-double-quote-reader)

; Code generation functions

(defun comparison-nofloat (str enum string)
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int arg1 = checkinteger(~a, first(args));
  args = cdr(args);
  while (args != NULL) {
    int arg2 = checkinteger(~a, first(args));
    if (!(arg1 ~a arg2)) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}" (string-downcase enum) 
          enum enum
          (cond
           ((string= string "=") "==")
           (t string))))

(defun comparison-float (str enum string)
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  object *arg1 = first(args);
  args = cdr(args);
  while (args != NULL) {
    object *arg2 = first(args);
    if (integerp(arg1) && integerp(arg2)) {
      if (!((arg1->integer) ~a (arg2->integer))) return nil;
    } else if (!(checkintfloat(~a, arg1) ~a checkintfloat(~a, arg2))) return nil;
    arg1 = arg2;
    args = cdr(args);
  }
  return tee;
}" (string-downcase enum)
          (cond
           ((string= string "=") "==")
           (t string))
          enum
          (cond
           ((string= string "=") "==")
           (t string))
          enum))

(setf (fdefinition 'comparison) (function #+float comparison-float #-float comparison-nofloat))

(defun float-function (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  return makefloat(~a(checkintfloat(~a, first(args))));
}" (string-downcase enum) (string-downcase enum) enum))

(defun truncate-function (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  object *arg = first(args);
  args = cdr(args);
  if (args != NULL) return number(~a(checkintfloat(~a, arg) / checkintfloat(~a, first(args))));
  else return number(~a(checkintfloat(~a, arg)));
}" (string-downcase enum) 
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum enum
          (cdr (assoc enum '((CEILING . "ceil") (FLOOR . "floor") (TRUNCATE . "trunc") (ROUND . "round"))))
          enum))

(defun numeric1 (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int arg = checkinteger(~a, first(args));
  return number(~a(arg));
}" (string-downcase enum) enum (string-downcase enum)))

(defun bitwise (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int result = ~a;
  while (args != NULL) {
    result = result ~a checkinteger(~a, first(args));
    args = cdr(args);
  }
  return number(result);
}" (string-downcase enum)
          (cdr (assoc enum '((LOGAND . "-1") (LOGIOR . "0") (LOGXOR . "0"))))
          (cdr (assoc enum '((LOGAND . "&") (LOGIOR . "|") (LOGXOR . "^"))))
          enum))

; For max or min

(defun numeric2 (str enum string)
  (declare (ignore string))
  (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  int result = integer(first(args));
  args = cdr(args);
  while (args != NULL) {
    result = ~a(result,integer(car(args)));
    args = cdr(args);
  }
  return number(result);
}" (string-downcase enum) (string-downcase enum)))

(defun cxr (str enum string)
  (declare (ignore string))
  (let ((lower (string-downcase enum)))
    (format str "
object *fn_~a (object *args, object *env) {
  (void) env;
  return ~{c~arx(~}first(args)~a;
}" lower (cdr (butlast (coerce lower 'list))) (make-string (- (length lower) 2) :initial-element #\)))))

;; FIXME: Should be in a sensible place. definitions.lisp ?
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
