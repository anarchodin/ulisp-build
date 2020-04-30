(defsystem "ulisp-build"
  :author "David Johnson-Davies"
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "extras")
               (:file "functions")
               (:file "preface")
               (:file "utilities")
               (:file "saveload")
               (:file "prettyprint")
               (:file "assembler")
               (:file "postscript")
               (:file "avr")
               (:file "msp430")
               (:file "tlc")
               (:file "zero")
               (:file "arm")
               (:file "esp")
               (:file "stm32")
               (:file "badge")
               (:file "riscv")
               (:file "generate")))
