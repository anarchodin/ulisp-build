;;;-*- Mode: Lisp; Package: ulisp-build -*-

(in-package :ulisp-build)

(defparameter *core-symbols*
  '((nil :type :symbol)
    (t :type :symbol :enum "tee")
    (nothing :type :symbol)
    (&optional :type :symbol :enum "optional")
    (&rest :type :symbol :enum "amprest")
    (lambda :type :symbol)
    (let :type :symbol)
    (let* :type :symbol :enum "letstar")
    (closure :type :symbol))
  "Symbols with no source file that have special meaning to the runtime or interpreter.")

;; TODO: Move remaining definitions to new form.
(defparameter *definitions*
  '(("Other special forms"
     ((WITHOUTPUTTOSTRING "with-output-to-string" 8 1 :stringstream)
      (WITHSERIAL "with-serial" 8 1)
      (WITHI2C "with-i2c" 8 1)
      (WITHSPI "with-spi" 8 1)
      (WITHSDCARD "with-sd-card" 8 1))
     "sp")
    ("Assembler"
     ((DEFCODE NIL 8 1 :code))
     "sp")
    ("System functions"
     ((RESTARTI2C "restart-i2c" 1 2)
      (DUMPIMAGE "dump-image" 0 0 :ignore)))
    ("Arduino procedures"
     ((WATCHDOG NIL 0 1 :ignore)
      (ANALOGREFERENCE NIL 1 1)
      (ANALOGREADRESOLUTION NIL 1 1)
      (ANALOGWRITERESOLUTION NIL 1 1 :write-resolution)
      (dacreference nil 1 1 :dacreference)
      (SHIFTOUT NIL 4 4 :ignore)
      (SHIFTIN NIL 3 3 :ignore)
      (ATTACHINTERRUPT "attach-interrupt" 1 3 :interrupt)))))
