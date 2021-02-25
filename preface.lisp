;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

;; FIXME: This belongs elsewhere.
(defvar *maximum-trace-count* 3 "The number of functions that can be traced at one time.")

(defun print-tokens (platform &optional (stream *standard-output*))
  "Output token definitions for a given platform."
  (let* ((byte-size (if (eq platform :avr) 16 32))
         (shift-size (if (= byte-size 16) 12 28))
         (base-num (if (= byte-size 16) #x7FE #x7FFFFFE))
         (num-length (if (= byte-size 16) 4 8)))
    (loop for token in '(:BRA :KET :QUO :DOT)
          for i from 0
          do (format stream "#define ~a 0x~v,'0x~%"
                     token num-length
                     (logior (ash i shift-size)
                             base-num)))))

(defun print-types (typelist &optional (stream *standard-output*))
  "Output type definitions for the given types."
  (format stream "~&~%// Type identifiers. Four last bits fixed at 6.~%")
  (let ((value -1))
    (dolist (type typelist)
      (format stream "#define ~a ~d // (~d << 4 | 6)~%"
              type
              (logior (ash (incf value) 4) 6)
              value)))
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
  (print-tokens platform stream)
  (print-streams (get-streams platform) stream)
  (terpri stream))
