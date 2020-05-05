;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defun write-radix-encoding (platform &optional (stream *standard-output*))
  "Write the appropriate radix encoding code for a given platform."
  (let* ((filename (ecase platform
                     ((:arm :stm32 :esp :riscv) "sections/radix32.c")
                     ((:avr :msp430 :badge) "sections/radix16.c")))
         (source-file (asdf:system-relative-pathname "ulisp-build" filename))
         (source-code (uiop:read-file-string source-file)))
    (write-string source-code stream)
    (terpri)))
