// C Macros

#if UINTPTR_MAX == UINT64_MAX
#define PTRWIDTH 64
#elif UINTPTR_MAX == UINT32_MAX
#define PTRWIDTH 32
#elif UINTPTR_MAX == UINT16_MAX
#define PTRWIDTH 16
#else
#error "What is this, a PDP-8?"
#endif

#define nil                NULL
#define tee                ((object *)46) // (1 << 5) | 14)

#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           (((object *) (x))->car)
#define second(x)          (car(cdr(x)))
#define cddr(x)            (cdr(cdr(x)))
#define third(x)           (car(cdr(cdr(x))))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

// Immediate types
#define immediatep(x)      (((uintptr_t)(x) & 2) == 2)
#define fixnump(x)         (((uintptr_t)(x) & 6) == 2)

#if PTRWIDTH == 16
#define builtinp(x)        (((uintptr_t)(x) & 30) == 14)
#define characterp(x)      (((uintptr_t)(x) & 254) == 126)
#else
#define symbolp(x)         (((uintptr_t)(x) & 30) == 14)
#define characterp(x)      (((uintptr_t)(x) & 2046) == 1022)
#endif

// Boxed types
#define boxedp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && ((x)->type & 14) == 6)
#define integerp(x)        ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == FLOAT)
#if PTRWIDTH == 16
#define usymbolp(x)        ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == SYMBOL)
#endif
#define stringp(x)         ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == STRING)
#ifdef ARRAY
#define arrayp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == ARRAY)
#endif
#ifdef CODE
#define codep(x)           ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == CODE)
#endif
#define streamp(x)         ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == STREAM)

// Extracting immediates
#if PTRWIDTH == 16
#define getcharacter(x)    ((unsigned char)((uintptr_t)x>>8))
#else
#define getcharacter(x)    ((uintptr_t)x>>11)
#endif

// Encoding immediates
#define sym(x)             ((object *)(((x) << 5) | 14))

// Dealing with types that can be either
#define intp(x)            (integerp(x) || fixnump(x))
#define getint(x)          (fixnump(x) ? (intptr_t)(x)>>3 : (x)->integer)

#if PTRWIDTH == 16
#define symbolp(x)         (usymbolp(x) || builtinp(x))
#define getname(x)         (builtinp(x) ? (symbol_t)(x)>>5 : (x)->name)
#else
#define getname(x)         ((symbol_t)(x)>>5)
#endif

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#define setflag(x)         (Flags = Flags | 1<<(x))
#define clrflag(x)         (Flags = Flags & ~(1<<(x)))
#define tstflag(x)         (Flags & 1<<(x))

#define issp(x)            (x == ' ' || x == '\n' || x == '\r' || x == '\t')

#if defined(__AVR)
#define SDCARD_SS_PIN 10
#endif

#if defined(CPU_ATmega4809)
#define PROGMEM
#define PSTR(s) (s)
#endif

#ifdef CODE
// Code marker stores start and end of code block
#ifdef __AVR__
#define CODESHIFT 8
#define startblock(x)      ((x->integer) & 0xFF)
#define endblock(x)        ((x->integer) >> 8 & 0xFF)
#else
#define CODESHIFT 16
#define startblock(x)      ((x->integer) & 0xFFFF)
#define endblock(x)        ((x->integer) >> 16 & 0xFFFF)
#endif
#endif

// Calling conventions.
#define CC_SYMBOL  0x80
#define CC_SPECIAL 0x81
#define CC_TAIL    0x82
#define CC_KEYWORD 0x83
