;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

;; Extract error messages *********************************************************************************************

(defun extract ()
 (with-open-file (str (capi:prompt-for-file "Input File" :operation :load :pathname "/Users/david/Desktop/") :direction :input)
   (let (result)
     (loop
      (let* ((line (read-line str nil nil))
             (start1 (search "error(" line))
             (start2 (search "error2(" line)))
        (unless line (return))
        (cond
         (start1 
          (let* ((start (position #\" line :start start1))
                 (end (position #\" line :start (1+ start)))
                 (text (subseq line (1+ start) end))
                 (choptext (if (char= (char text 0) #\') (subseq text 1) text)))
            (setq result (push (cons text choptext) result))))
         (start2
          (let* ((start (position #\" line :start start2))
                 (end (position #\" line :start (1+ start)))
                 (text (subseq line (1+ start) end)))
            (setq result (push (cons (format nil "'x' ~a" text) text) result)))))))
     (format nil "~{~a~%~}" (map 'list #'car (sort result #'string-lessp :key #'cdr))))))

        
        