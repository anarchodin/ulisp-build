// Typedefs

typedef uintptr_t symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      uintptr_t type;
      union {
        symbol_t name;
        intptr_t integer;
        int chars; // For strings
#ifdef FLOAT
        float single_float;
#endif
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef void (*mapfun_t)(object *, object **);
#ifdef CODE
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);
#endif

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t callc;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
#if defined(CPU_ATmega328P) || defined(CPU_ATmega2560) || defined(CPU_ATmega1284P) || defined(CPU_AVR128DX48)
typedef int BitOrder;
#endif
typedef int PinMode;
