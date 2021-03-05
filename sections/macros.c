// C Macros

#define nil                NULL
#define car(x)             (((object *) (x))->car)
#define cdr(x)             (((object *) (x))->cdr)

#define first(x)           (((object *) (x))->car)
#define second(x)          (car(cdr(x)))
#define cddr(x)            (cdr(cdr(x)))
#define third(x)           (car(cdr(cdr(x))))

#define push(x, y)         ((y) = cons((x),(y)))
#define pop(y)             ((y) = cdr(y))

// Immediate types
#define immediatep(x)      ((x) != NULL && ((uintptr_t)(x) & 2) == 2)
#define fixnump(x)         ((x) != NULL && ((uintptr_t)(x) & 6) == 2)

// Boxed types
#define boxedp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && ((x)->type & 14) == 6)
#define integerp(x)        ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == NUMBER)
#define floatp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == FLOAT)
#define symbolp(x)         ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == SYMBOL)
#define stringp(x)         ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == STRING)
#define characterp(x)      ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == CHARACTER)
#ifdef ARRAY
#define arrayp(x)          ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == ARRAY)
#endif
#ifdef CODE
#define codep(x)           ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == CODE)
#endif
#define streamp(x)         ((x) != NULL && ((uintptr_t)(x) & 2) == 0 && (x)->type == STREAM)

// Dealing with types that can be either
#define intp(x)            (integerp(x) || fixnump(x))
#define getint(x)          (integerp(x) ? (x)->integer : (intptr_t)(x)>>3)

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
#define startblock(x)      ((x->integer) & 0xFFFF)
#define endblock(x)        ((x->integer) >> 16 & 0xFFFF)
#endif

// Calling conventions.
#define CC_SYMBOL  0x80
#define CC_SPECIAL 0x81
#define CC_TAIL    0x82
#define CC_KEYWORD 0x83
