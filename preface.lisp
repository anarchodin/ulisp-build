;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

;; FIXME: This belongs elsewhere.
(defvar *maximum-trace-count* 3 "The number of functions that can be traced at one time.")

;; NOTE: Done as CPP macros rather than an enum in preparation for further changes.
(defun print-types (typelist &optional (stream *standard-output*))
  "Output type definitions for the given types."
  (format stream "~&~%// Types~%")
  (let ((value -1))
    (dolist (type typelist)
      (format stream "#define ~a ~d~%" type (ash (incf value) 1))))
  (terpri stream))

(defun print-streams (streamlist &optional (stream *standard-output*) (margin 90))
  "Output a streams enum for a given list of streams."
  (let ((*standard-output* (or stream *standard-output*))
        (*print-right-margin* margin)
        (stream-names (mapcar #'(lambda (x) (string-downcase (symbol-name x))) streamlist)))
    (format stream "enum stream { ~<~@{~aSTREAM~#[ ~:;, ~]~:_~:}~:>};~%~%" streamlist) ; Eek?
    (format stream "// Stream names used by printobject~%")
    (format stream "~{const char ~astream[] PROGMEM = \"~:*~a\";~%~}" stream-names)
    (format stream "const char * const streamname[] PROGMEM = {~{~astream~#[~:;, ~]~}};~%" stream-names)))

(defun write-constants (platform &optional (stream *standard-output*))
  "Write out the constants table used by a platform."
  (format stream "~&~%// Constants~%~%")
  (format stream "const int TRACEMAX = ~d; // Number of traced functions~%" *maximum-trace-count*)
  (print-types (get-types platform) stream)
  (write-line "enum token { UNUSED, BRA, KET, QUO, DOT };" stream)
  (print-streams (get-streams platform) stream)
  (terpri stream))
