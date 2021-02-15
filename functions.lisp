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

;; This structure holds definitions for the symbols we want to build into uLisp.
(defparameter *definitions*
  '(("Symbols"
     ((INITIALELEMENT ":initial-element" 8 0 :array)
      (ELEMENTTYPE ":element-type" 8 0 :array)
      (BIT NIL 8 0 :array)))
    ("Accessors"
     ((INCF NIL 8 1)
      (DECF NIL 8 1))
     "sp")
    ("Other special forms"
     ((TRACE NIL 8 1)
      (UNTRACE NIL 8 1)
      (FORMILLIS "for-millis" 8 1)
      (WITHOUTPUTTOSTRING "with-output-to-string" 8 1 :stringstream)
      (WITHSERIAL "with-serial" 8 1)
      (WITHI2C "with-i2c" 8 1)
      (WITHSPI "with-spi" 8 1)
      (WITHSDCARD "with-sd-card" 8 1)
      (WITHGFX "with-gfx" 8 1 :gfx)
      (WITHLCD "with-lcd" 8 1 :lcd)
      (WITHCLIENT "with-client" 8 1 :ethernet))
     "sp")
    ("Assembler"
     ((DEFCODE NIL 8 1 :code))
     "sp")
    ("Core functions"
     ((ARRAYP NIL 1 1 :array)))
    ("List functions"
     ((CAR NIL 1 1)
      (FIRST NIL 1 1 (car))
      (CDR NIL 1 1)
      (REST NIL 1 1 (cdr))
      (CAAR NIL 1 1)
      (CADR NIL 1 1)
      (SECOND NIL 1 1 (cadr))
      (CDAR NIL 1 1)
      (CDDR NIL 1 1)
      (CAAAR NIL 1 1)
      (CAADR NIL 1 1)
      (CADAR NIL 1 1)
      (CADDR NIL 1 1)
      (THIRD NIL 1 1 (caddr))
      (CDAAR NIL 1 1)
      (CDADR NIL 1 1)
      (CDDAR NIL 1 1)
      (CDDDR NIL 1 1)
      (ARRAYDIMENSIONS "array-dimensions" 1 1 :array)
      (MAKEARRAY "make-array" 1 5 :array)
      (AREF NIL 2 127 :array)))
    ("Arithmetic functions"
     ((ADD "+" 0 127)
      (SUBTRACT "-" 1 127)
      (MULTIPLY "*" 0 127)
      (DIVIDE "/" 1 127)
      (MOD NIL 2 2)
      (ONEPLUS "1+" 1 1)
      (ONEMINUS "1-" 1 1)
      (ABS NIL 1 1)
      (RANDOM NIL 1 1)
      (MAXFN "max" 1 127)
      (MINFN "min" 1 127)))
    ("Arithmetic comparisons"
     ((NOTEQ "/=" 1 127)
      (NUMEQ "=" 1 127)
      (LESS "<" 1 127)
      (LESSEQ "<=" 1 127)
      (GREATER ">" 1 127)
      (GREATEREQ ">=" 1 127)
      (PLUSP NIL 1 1)
      (MINUSP NIL 1 1)
      (ZEROP NIL 1 1)
      (ODDP NIL 1 1)
      (EVENP NIL 1 1)))
    ("Number functions"
     ((INTEGERP NIL 1 1)
      (NUMBERP NIL 1 1)))
    ("Floating-point functions"
     ((FLOATFN "float" 1 1 :float)
      (FLOATP NIL 1 1 :float)
      (SIN NIL 1 1 :float)
      (COS NIL 1 1 :float)
      (TAN NIL 1 1 :float)
      (ASIN NIL 1 1 :float)
      (ACOS NIL 1 1 :float)
      (ATAN NIL 1 2 :float)
      (SINH NIL 1 1 :float)
      (COSH NIL 1 1 :float)
      (TANH NIL 1 1 :float)
      (EXP NIL 1 1 :float)
      (SQRT NIL 1 1 :float)
      (LOG NIL 1 2 :float)
      (EXPT NIL 2 2 :float)
      (CEILING NIL 1 2 :float)
      (FLOOR NIL 1 2 :float)
      (TRUNCATE NIL 1 2 :float)
      (ROUND NIL 1 2 :float)))
    ("Characters"
     ((CHAR "char" 2 2)
      (CHARCODE "char-code" 1 1)
      (CODECHAR "code-char" 1 1)
      (CHARACTERP NIL 1 1)))
    ("Strings"
     ((STRINGP NIL 1 1)
      (STRINGEQ "string=" 2 2)
      (STRINGLESS "string<" 2 2)
      (STRINGGREATER "string>" 2 2)
      (SORT "sort" 2 2)
      (STRINGFN "string" 1 1)
      (CONCATENATE NIL 1 127)
      (SUBSEQ NIL 2 3)
      (READFROMSTRING "read-from-string" 1 1)
      (PRINCTOSTRING "princ-to-string" 1 1)
      (PRIN1TOSTRING "prin1-to-string" 1 1)))
    ("Bitwise operators"
     ((LOGAND NIL 0 127)
      (LOGIOR NIL 0 127)
      (LOGXOR NIL 0 127)
      (LOGNOT NIL 1 1)
      (ASH NIL 2 2)
      (LOGBITP NIL 2 2)))
    ("System functions"
     ((EVAL NIL 1 1)
      (GLOBALS NIL 0 0)
      (LOCALS NIL 0 0)
      (MAKUNBOUND NIL 1 1)
      (BREAK NIL 0 0)
      (READ NIL 0 1)
      (PRIN1 NIL 1 2)
      (PRINT NIL 1 2)
      (PRINC NIL 1 2)
      (TERPRI NIL 0 1)
      (READBYTE "read-byte" 0 2)
      (READLINE "read-line" 0 1)
      (WRITEBYTE "write-byte" 1 2)
      (WRITESTRING "write-string" 1 2)
      (WRITELINE "write-line" 1 2)
      (RESTARTI2C "restart-i2c" 1 2)
      (GC NIL 0 0)
      (ROOM NIL 0 0)
      (SAVEIMAGE "save-image" 0 1)
      (LOADIMAGE "load-image" 0 1)
      (DUMPIMAGE "dump-image" 0 0 :ignore)
      (CLS "cls" 0 0)))
    ("Arduino procedures"
     ((WATCHDOG NIL 0 1 :ignore)
      (PINMODE NIL 2 2)
      (DIGITALREAD NIL 1 1)
      (DIGITALWRITE NIL 2 2)
      (ANALOGREAD NIL 1 1)
      (ANALOGREFERENCE NIL 1 1)
      (ANALOGREADRESOLUTION NIL 1 1)
      (ANALOGWRITE NIL 2 2)
      (ANALOGWRITERESOLUTION NIL 1 1 :write-resolution)
      (dacreference nil 1 1 :dacreference)
      (DELAY NIL 1 1)
      (MILLIS NIL 0 0)
      (SLEEP NIL 1 1)
      (SHIFTOUT NIL 4 4 :ignore)
      (SHIFTIN NIL 3 3 :ignore)
      (NOTE NIL 0 3)
      (ATTACHINTERRUPT "attach-interrupt" 1 3 :interrupt)))
    ("Tree Editor"
     ((EDIT NIL 1 1)))
    ("Pretty printer"
     ((PPRINT NIL 1 2)
      (PPRINTALL NIL 0 1)))
    ("Format"
     ((FORMAT NIL 2 127)))
    ("LispLibrary"
     ((REQUIRE NIL 1 1)
      (LISTLIBRARY "list-library" 0 0)))
    ("Wi-fi"
     ((AVAILABLE NIL 1 1 :ethernet)
      (WIFISERVER "wifi-server" 0 0 :ethernet)
      (WIFISOFTAP "wifi-softap" 0 4 :ethernet)
      (CONNECTED NIL 1 1 :ethernet)
      (WIFILOCALIP "wifi-localip" 0 0 :ethernet)
      (WIFICONNECT "wifi-connect" 0 2 :ethernet)))
    ("Graphics functions"
     ((DRAWPIXEL "draw-pixel" 2 3 :gfx)
      (DRAWLINE "draw-line" 4 5 :gfx)
      (DRAWRECT "draw-rect" 4 5 :gfx)
      (FILLRECT "fill-rect" 4 5 :gfx)
      (DRAWCIRCLE "draw-circle" 3 4 :gfx)
      (FILLCIRCLE "fill-circle" 3 4 :gfx)
      (DRAWROUNDRECT "draw-round-rect" 5 6 :gfx)
      (FILLROUNDRECT "fill-round-rect" 5 6 :gfx)
      (DRAWTRIANGLE "draw-triangle" 6 7 :gfx)
      (FILLTRIANGLE "fill-triangle" 6 7 :gfx)
      (DRAWCHAR "draw-char" 3 6 :gfx)
      (SETCURSOR "set-cursor" 2 2 :gfx)
      (SETTEXTCOLOR "set-text-color" 1 2 :gfx)
      (SETTEXTSIZE "set-text-size" 1 1 :gfx)
      (SETTEXTWRAP "set-text-wrap" 1 1 :gfx)
      (FILLSCREEN "fill-screen" 0 1 :gfx)
      (SETROTATION "set-rotation" 1 1 :gfx)
      (INVERTDISPLAY "invert-display" 1 1 :gfx)))
    ("Lisp Badge plotting"
     ((PLOT NIL 0 6 :plot)
      (PLOT3D NIL 0 3 :plot)))))
