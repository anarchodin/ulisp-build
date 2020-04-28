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


(load "/Users/david/Projects/Preprocessor/preprocessor defsys.lisp")

(compile-system "preprocessor" :load t) 