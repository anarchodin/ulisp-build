(require 'asdf)
(asdf:initialize-source-registry `(:source-registry (:directory ,(uiop:getcwd)) :inherit-configuration))

;; Options: :avr :arm :msp430 :esp :stm32 :badge :zero :interrupts :riscv

(push :arm *features*)
;(push :interrupts *features*)


#+(or arm esp stm32 riscv)
(push :float *features*)

#+(or esp)
(push :ethernet *features*)

#+(or riscv arm)
(push :gfx *features*)

#+(or riscv arm)
(push :code *features*)

(asdf:load-system "ulisp-build")
