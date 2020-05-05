;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defun section-pathname (section &optional platform)
  "Returns the pathname for the C file implementing a given section for a given platform, or the default platform if none is specified. If no such file exists, returns nil."
  (let ((section-dir (asdf:system-relative-pathname "ulisp-build" "sections/"))
        (subdir (if platform (make-pathname :directory (list :relative (string-downcase platform)))))
        (filename (make-pathname :name (string-downcase section) :type "c")))
    (probe-file (merge-pathnames filename (if subdir
                                              (merge-pathnames subdir section-dir)
                                              section-dir)))))

(defun get-section (section platform)
  "Retrieve source code for a given section on a given platform. Returns null if none is found."
  (let ((filename (or (section-pathname section platform) (section-pathname section))))
    (if filename (uiop:read-file-string filename))))

(defun write-section (section &key (platform *platform*) (stream *standard-output*))
  "Writes the source code of a given section for a given platform to a given stream."
  (let ((code (get-section section platform)))
    (if code (write-line code stream)
        (warn "No code for section ~a on platform ~a." section platform))))
