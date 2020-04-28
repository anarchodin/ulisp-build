;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

#+avr
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1284P__)
typedef int BitOrder;
typedef int PinMode;
#endif"#)

#+(or msp430 badge)
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);"#)

#+arm
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;"#)

#+riscv
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

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
        int integer;
        int chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);
typedef int (*intfn_ptr_type)(int w, int x, int y, int z);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;"#)

#+(or stm32 esp)
(defparameter *typedefs* #"
// Typedefs

typedef unsigned int symbol_t;

typedef struct sobject {
  union {
    struct {
      sobject *car;
      sobject *cdr;
    };
    struct {
      unsigned int type;
      union {
        symbol_t name;
        int integer;
        int chars; // For strings
        float single_float;
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  const char *string;
  fn_ptr_type fptr;
  uint8_t minmax;
} tbl_entry_t;

typedef int (*gfun_t)();
typedef void (*pfun_t)(char);
typedef int PinMode;"#)

(defparameter *global-variables* 
  '(#"
// Global variables

jmp_buf exception;
unsigned int Freespace = 0;
object *Freelist;
char *SymbolTop = SymbolTable;
unsigned int I2CCount;
unsigned int TraceFn[TRACEMAX];
unsigned int TraceDepth[TRACEMAX];

object *GlobalEnv;
object *GCStack = NULL;
object *GlobalString;
int GlobalStringIndex = 0;
uint8_t PrintCount = 0;
uint8_t BreakLevel = 0;
char LastChar = 0;
char LastPrint = 0;"#

#"
// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC };
volatile char Flags = 0b00001; // PRINTREADABLY set by default"#

#+(or msp430 avr badge)
#"
// Forward references
object *tee;
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read (gfun_t gfun);
void repl (object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);
void pfstring (PGM_P s, pfun_t pfun);"#

#+(or arm esp stm32 riscv)
#"
// Forward references
object *tee;
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read (gfun_t gfun);
void repl (object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);"#))

(defparameter *error-handling* '(

#"
// Error handling

void errorsub (symbol_t fname, PGM_P string) {
  pfl(pserial); pfstring(PSTR("Error: "), pserial);
  if (fname) {
    pserial('\''); 
    pstring(symbolname(fname), pserial);
    pfstring(PSTR("' "), pserial);
  }
  pfstring(string, pserial);
}

void error (symbol_t fname, PGM_P string, object *symbol) {
  errorsub(fname, string);
  pfstring(PSTR(": "), pserial); printobject(symbol, pserial);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

void error2 (symbol_t fname, PGM_P string) {
  errorsub(fname, string);
  pln(pserial);
  GCStack = NULL;
  longjmp(exception, 1);
}

// Save space as these are used multiple times
const char notanumber[] PROGMEM = "argument is not a number";
const char notastring[] PROGMEM = "argument is not a string";
const char notalist[] PROGMEM = "argument is not a list";
const char notasymbol[] PROGMEM = "argument is not a symbol";
const char notproper[] PROGMEM = "argument is not a proper list";
const char toomanyargs[] PROGMEM = "too many arguments";
const char toofewargs[] PROGMEM = "too few arguments";
const char noargument[] PROGMEM = "missing argument";
const char nostream[] PROGMEM = "missing stream argument";
const char overflow[] PROGMEM = "arithmetic overflow";
const char invalidarg[] PROGMEM = "invalid argument";
const char invalidpin[] PROGMEM = "invalid pin";
const char resultproper[] PROGMEM = "result is not a proper list";
const char oddargs[] PROGMEM = "odd number of arguments";"#))

(defparameter *setup-workspace* #"
// Set up workspace

void initworkspace () {
  Freelist = NULL;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    car(obj) = NULL;
    cdr(obj) = Freelist;
    Freelist = obj;
    Freespace++;
  }
}

object *myalloc () {
  if (Freespace == 0) error2(0, PSTR("no room"));
  object *temp = Freelist;
  Freelist = cdr(Freelist);
  Freespace--;
  return temp;
}

inline void myfree (object *obj) {
  car(obj) = NULL;
  cdr(obj) = Freelist;
  Freelist = obj;
  Freespace++;
}"#)

(defparameter *make-objects* 
 
 '(#"
// Make each type of object

object *number (int n) {
  object *ptr = myalloc();
  ptr->type = NUMBER;
  ptr->integer = n;
  return ptr;
}"#

 #+float
 #"
object *makefloat (float f) {
  object *ptr = myalloc();
  ptr->type = FLOAT;
  ptr->single_float = f;
  return ptr;
}"#

 #"
object *character (char c) {
  object *ptr = myalloc();
  ptr->type = CHARACTER;
  ptr->integer = c;
  return ptr;
}"#

 #"
object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}"#

 #"
object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}"#

#+(or arm riscv)
 #"
object *codehead (int entry) {
  object *ptr = myalloc();
  ptr->type = CODE;
  ptr->integer = entry;
  return ptr;
}"#

 #"
object *newsymbol (symbol_t name) {
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (obj->type == SYMBOL && obj->name == name) return obj;
  }
  return symbol(name);
}"#

 #"
object *stream (unsigned char streamtype, unsigned char address) {
  object *ptr = myalloc();
  ptr->type = STREAM;
  ptr->integer = streamtype<<8 | address;
  return ptr;
}"#))

(defparameter *garbage-collection* '(

#"
// Garbage collection"#

#+avr
#"
void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);
  
  if (type >= PAIR || type == ZZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if (type == STRING) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}"#

#-avr
#"
void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);
  
  if (type >= PAIR || type == ZZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }

  if (type == ARRAY) {
    obj = cdr(obj);
    goto MARK;
  }
  
  if (type == STRING) {
    obj = cdr(obj);
    while (obj != NULL) {
      arg = car(obj);
      mark(obj);
      obj = arg;
    }
  }
}"#

#"
void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

void gc (object *form, object *env) {
  #if defined(printgcs)
  int start = Freespace;
  #endif
  markobject(tee);
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  sweep();
  #if defined(printgcs)
  pfl(pserial); pserial('{'); pint(Freespace - start, pserial); pserial('}');
  #endif
}"#))

(defparameter *tracing* #"
// Tracing

bool tracing (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) return i+1;
    i++;
  }
  return 0;
}

void trace (symbol_t name) {
  if (tracing(name)) error(TRACE, PSTR("already being traced"), symbol(name));
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == 0) { TraceFn[i] = name; TraceDepth[i] = 0; return; }
    i++;
  }
  error2(TRACE, PSTR("already tracing 3 functions"));
}

void untrace (symbol_t name) {
  int i = 0;
  while (i < TRACEMAX) {
    if (TraceFn[i] == name) { TraceFn[i] = 0; return; }
    i++;
  }
  error(UNTRACE, PSTR("not tracing"), symbol(name));
}"#)

(defparameter *helper-functions* 

  '(#"
// Helper functions

bool consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}"#

    #"
bool atom (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type < PAIR && type != ZZERO;
}"#

    #"
bool listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZZERO;
}"#

    #"
bool improperp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type < PAIR && type != ZZERO;
}

object *quote (object *arg) {
  return cons(symbol(QUOTE), cons(arg,NULL));
}"#

#+(or arm stm32 esp riscv)
    #"
// Radix 40 encoding

#define MAXSYMBOL 4096000000

int toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+30;
  if (ch == '$') return 27; if (ch == '*') return 28; if (ch == '-') return 29;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+1;
  return -1; // Invalid
}

int fromradix40 (int n) {
  if (n >= 1 && n <= 26) return 'a'+n-1;
  if (n == 27) return '$'; if (n == 28) return '*'; if (n == 29) return '-';
  if (n >= 30 && n <= 39) return '0'+n-30;
  return 0;
}

int pack40 (char *buffer) {
  int x = 0;
  for (int i=0; i<6; i++) x = x * 40 + toradix40(buffer[i]);
  return x;
}

bool valid40 (char *buffer) {
  for (int i=0; i<6; i++) if (toradix40(buffer[i]) == -1) return false;
  return true;
}

char *symbolname (symbol_t x) {
  if (x < ENDFUNCTIONS) return lookupbuiltin(x);
  else if (x >= MAXSYMBOL) return lookupsymbol(x);
  char *buffer = SymbolTop;
  buffer[3] = '\0'; buffer[4] = '\0'; buffer[5] = '\0'; buffer[6] = '\0';
  for (int n=5; n>=0; n--) {
    buffer[n] = fromradix40(x % 40);
    x = x / 40;
  }
  return buffer;
}"#

#+(or avr msp430 badge)
    #"
// Radix 40 encoding

#define MAXSYMBOL 64000

int toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+30;
  if (ch == '$') return 27; if (ch == '*') return 28; if (ch == '-') return 29;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+1;
  return -1; // Invalid
}

int fromradix40 (int n) {
  if (n >= 1 && n <= 26) return 'a'+n-1;
  if (n == 27) return '$'; if (n == 28) return '*'; if (n == 29) return '-';
  if (n >= 30 && n <= 39) return '0'+n-30;
  return 0;
}

int pack40 (char *buffer) {
  return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}

bool valid40 (char *buffer) {
 return (toradix40(buffer[0]) >= 0 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}

char *symbolname (symbol_t x) {
  if (x < ENDFUNCTIONS) return lookupbuiltin(x);
  else if (x >= MAXSYMBOL) return lookupsymbol(x);
  char *buffer = SymbolTop;
  buffer[3] = '\0';
  for (int n=2; n>=0; n--) {
    buffer[n] = fromradix40(x % 40);
    x = x / 40;
  }
  return buffer;
}"#

   #"
int digitvalue (char d) {
  if (d>='0' && d<='9') return d-'0';
  d = d | 0x20;
  if (d>='a' && d<='f') return d-'a'+10;
  return 16;
}"#

    #"
int checkinteger (symbol_t name, object *obj) {
  if (!integerp(obj)) error(name, notanumber, obj);
  return obj->integer;
}"#

    #+float
    #"
float checkintfloat (symbol_t name, object *obj){
  if (integerp(obj)) return obj->integer;
  if (floatp(obj)) return obj->single_float;
  error(name, notanumber, obj);
}"#

    #"
int checkchar (symbol_t name, object *obj) {
  if (!characterp(obj)) error(name, PSTR("argument is not a character"), obj);
  return obj->integer;
}"#

    #"
int isstream (object *obj){
  if (!streamp(obj)) error(0, PSTR("not a stream"), obj);
  return obj->integer;
}"#

    #"
int issymbol (object *obj, symbol_t n) {
  return symbolp(obj) && obj->name == n;
}"#

    #"
void checkargs (symbol_t name, object *args) {
  int nargs = listlength(name, args);
  if (name >= ENDFUNCTIONS) error(0, PSTR("not valid here"), symbol(name));
  checkminmax(name, nargs);
}"#

    #-float
    #"
int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}"#

    #+float
    #"
int eq (object *arg1, object *arg2) {
  if (arg1 == arg2) return true;  // Same object
  if ((arg1 == nil) || (arg2 == nil)) return false;  // Not both values
  if (arg1->cdr != arg2->cdr) return false;  // Different values
  if (symbolp(arg1) && symbolp(arg2)) return true;  // Same symbol
  if (integerp(arg1) && integerp(arg2)) return true;  // Same integer
  if (floatp(arg1) && floatp(arg2)) return true; // Same float
  if (characterp(arg1) && characterp(arg2)) return true;  // Same character
  return false;
}"#

    #"
int listlength (symbol_t name, object *list) {
  int length = 0;
  while (list != NULL) {
    if (improperp(list)) error2(name, notproper);
    list = cdr(list);
    length++;
  }
  return length;
}"#))

(defparameter *association-lists* #"
// Association lists

object *assoc (object *key, object *list) {
  while (list != NULL) {
    if (improperp(list)) error(ASSOC, notproper, list);
    object *pair = first(list);
    if (!listp(pair)) error(ASSOC, PSTR("element is not a list"), pair);
    if (pair != NULL && eq(key,car(pair))) return pair;
    list = cdr(list);
  }
  return nil;
}

object *delassoc (object *key, object **alist) {
  object *list = *alist;
  object *prev = NULL;
  while (list != NULL) {
    object *pair = first(list);
    if (eq(key,car(pair))) {
      if (prev == NULL) *alist = cdr(list);
      else cdr(prev) = cdr(list);
      return key;
    }
    prev = list;
    list = cdr(list);
  }
  return nil;
}"#)

(defparameter *array-utilities* '(

#-avr
#"
// Array utilities

int nextpower2 (int n) {
  n--; n |= n >> 1; n |= n >> 2; n |= n >> 4;
  n |= n >> 8; n |= n >> 16; n++;
  return n;
}

object *buildarray (int n, int s, object *def) {
  int s2 = s>>1;
  if (s2 == 1) { if (n == 2) return cons(def, def); else return cons(def, NULL); }
  else if (n >= s2) return cons(buildarray(s2, s2, def), buildarray(n - s2, s2, def));
  else return cons(buildarray(n, s2, def), nil);
}

object *makearray (int xd, int yd, object *dimensions, object *def) {
  int size = xd * yd;
  object *ptr = myalloc();
  ptr->type = ARRAY;
  object *tree = nil;
  if (size != 0) tree = buildarray(size, max(nextpower2(size), 2), def);
  ptr->cdr = cons(tree, dimensions);
  return ptr;
}

object **getarray (symbol_t name, object *array, int x, int y) {
  object *dimensions = cddr(array);
  int xd, yd = 1;
  xd = first(dimensions)->integer;
  if (cdr(dimensions) != NULL) yd = second(dimensions)->integer;
  int size = xd * yd;
  int index = x * yd + y;
  if (x >= xd || x < 0 || y >= yd || y < 0) error2(name, PSTR("index out of range"));
  int mask = max(nextpower2(size), 2)>>1;
  object **p = &car(cdr(array));
  while (mask) {
    if ((index & mask) == 0) p = &(car(*p)); else p = &(cdr(*p));
    mask = mask>>1;
  }
  return p;
}

object *listtovector (object *list) {
  int xd = listlength(0, list);
  object *array = makearray(xd, 1, cons(number(xd), NULL), NULL);
  int p = 0;
  while (list != NULL) {
    object **loc = getarray(0, array, p++, 0);
    *loc = first(list);
    list = cdr(list);
  }
  return array;
}

object *listto2darray (object *list) {
  int yd, xd = listlength(0, list);
  if (list == NULL) yd = 0;
  else if (listp(first(list))) yd = listlength(0, first(list));
  else error2(0, PSTR("initial contents not 2d array"));
  object *array = makearray(xd, yd, cons(number(xd), cons(number(yd), NULL)), NULL);
  int x = 0;
  while (list != NULL) {
    object *item = first(list);
    int y = 0;
    while (item != NULL) {
      object **loc = getarray(0, array, x, y++);
      *loc = first(item);
      item = cdr(item);
    }
    x++;
    list = cdr(list);
  }
  return array;
}

void printarray (object *array, pfun_t pfun) {
  object *dimensions = cddr(array);
  int xd = first(dimensions)->integer;
  pfun('#');
  if (cdr(dimensions) == NULL) {
    pfun('(');
    for (int x=0; x<xd; x++) {
      if (x) pfun(' ');
      printobject(*getarray(0, array, x, 0), pfun);
    }
  } else {
    int yd = second(dimensions)->integer;
    pfstring(PSTR("2A("), pfun);
    for (int x=0; x<xd; x++) {
      if (x) pfun(' '); pfun('(');
      for (int y=0; y<yd; y++) {
        if (y) pfun(' ');
        printobject(*getarray(0, array, x, y), pfun);
      }
      pfun(')');
    }  
  }
  pfun(')');
}"#))

(defparameter *string-utilities* #"
// String utilities

void indent (int spaces, char ch, pfun_t pfun) {
  for (uint8_t i=0; i<spaces; i++) pfun(ch);
}

object *startstring (symbol_t name) {
  object *string = myalloc();
  string->type = STRING;
  GlobalString = NULL;
  GlobalStringIndex = 0;
  return string;
}

void buildstring (char ch, int *chars, object **head) {
  static object* tail;
  static uint8_t shift;
  if (*chars == 0) {
    shift = (sizeof(int)-1)*8;
    *chars = ch<<shift;
    object *cell = myalloc();
    if (*head == NULL) *head = cell; else tail->car = cell;
    cell->car = NULL;
    cell->chars = *chars;
    tail = cell;
  } else {
    shift = shift - 8;
    *chars = *chars | ch<<shift;
    tail->chars = *chars;
    if (shift == 0) *chars = 0;
  }
}

object *readstring (char delim, gfun_t gfun) {
  object *obj = myalloc();
  obj->type = STRING;
  int ch = gfun();
  if (ch == -1) return nil;
  object *head = NULL;
  int chars = 0;
  while ((ch != delim) && (ch != -1)) {
    if (ch == '\\') ch = gfun();
    buildstring(ch, &chars, &head);
    ch = gfun();
  }
  obj->cdr = head;
  return obj;
}

int stringlength (object *form) {
  int length = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = form->chars;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      if (chars>>i & 0xFF) length++;
    }
    form = car(form);
  }
  return length;
}

char nthchar (object *string, int n) {
  object *arg = cdr(string);
  int top;
  if (sizeof(int) == 4) { top = n>>2; n = 3 - (n&3); }
  else { top = n>>1; n = 1 - (n&1); }
  for (int i=0; i<top; i++) {
    if (arg == NULL) return 0;
    arg = car(arg);
  }
  if (arg == NULL) return 0;
  return (arg->chars)>>(n*8) & 0xFF;
}

int gstr () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  char c = nthchar(GlobalString, GlobalStringIndex++);
  if (c != 0) return c;
  return '\n'; // -1?
}

void pstr (char c) {
  buildstring(c, &GlobalStringIndex, &GlobalString);
}"#)

#+ethernet
(defparameter *string-utilities2* #"
char *cstringbuf (object *arg) {
  cstring(arg, SymbolTop, SYMBOLTABLESIZE-(SymbolTop-SymbolTable));
  return SymbolTop;
}

char *cstring (object *form, char *buffer, int buflen) {
  int index = 0;
  form = cdr(form);
  while (form != NULL) {
    int chars = form->integer;
    for (int i=(sizeof(int)-1)*8; i>=0; i=i-8) {
      char ch = chars>>i & 0xFF;
      if (ch) {
        if (index >= buflen-1) error2(0, PSTR("no room for string"));
        buffer[index++] = ch;
      }
    }
    form = car(form);
  }
  buffer[index] = '\0';
  return buffer;
}

object *lispstring (char *s) {
  object *obj = myalloc();
  obj->type = STRING;
  char ch = *s++;
  object *head = NULL;
  int chars = 0;
  while (ch) {
    if (ch == '\\') ch = *s++;
    buildstring(ch, &chars, &head);
    ch = *s++;
  }
  obj->cdr = head;
  return obj;
}"#)

(defparameter *closures* #"
// Lookup variable in environment

object *value (symbol_t n, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair)->name == n) return pair;
    env = cdr(env);
  }
  return nil;
}

bool boundp (object *var, object *env) {
  symbol_t varname = var->name;
  if (value(varname, env) != NULL) return true;
  if (value(varname, GlobalEnv) != NULL) return true;
  return false;
}

object *findvalue (object *var, object *env) {
  symbol_t varname = var->name;
  object *pair = value(varname, env);
  if (pair == NULL) pair = value(varname, GlobalEnv);
  if (pair == NULL) error(0, PSTR("unknown variable"), var);
  return pair;
}

// Handling closures
  
object *closure (int tc, symbol_t name, object *state, object *function, object *args, object **env) {
  int trace = 0;
  if (name) trace = tracing(name);
  if (trace) {
    indent(TraceDepth[trace-1]<<1, ' ', pserial);
    pint(TraceDepth[trace-1]++, pserial);
    pserial(':'); pserial(' '); pserial('('); pstring(symbolname(name), pserial);
  }
  object *params = first(function);
  function = cdr(function);
  // Dropframe
  if (tc) {
    if (*env != NULL && car(*env) == NULL) {
      pop(*env);
      while (*env != NULL && car(*env) != NULL) pop(*env);
    } else push(nil, *env);
  }
  // Push state
  while (state != NULL) {
    object *pair = first(state);
    push(pair, *env);
    state = cdr(state);
  }
  // Add arguments to environment
  bool optional = false;
  while (params != NULL) {
    object *value;
    object *var = first(params);
    if (symbolp(var) && var->name == OPTIONAL) optional = true;  
    else {
      if (consp(var)) {
        if (!optional) error(name, PSTR("invalid default value"), var);
        if (args == NULL) value = eval(second(var), *env);
        else { value = first(args); args = cdr(args); }
        var = first(var);
        if (!symbolp(var)) error(name, PSTR("illegal optional parameter"), var); 
      } else if (!symbolp(var)) {
        error2(name, PSTR("illegal parameter"));     
      } else if (var->name == AMPREST) {
        params = cdr(params);
        var = first(params);
        value = args;
        args = NULL;
      } else {
        if (args == NULL) {
          if (optional) value = nil; 
          else {
            if (name) error2(name, toofewargs);
            else error2(0, PSTR("function has too few arguments"));
          }
        } else { value = first(args); args = cdr(args); }
      }
      push(cons(var,value), *env);
      if (trace) { pserial(' '); printobject(value, pserial); }
    }
    params = cdr(params);  
  }
  if (args != NULL) {
    if (name) error2(name, toomanyargs);
    else error2(0, PSTR("function has too many arguments"));
  }
  if (trace) { pserial(')'); pln(pserial); }
  // Do an implicit progn
  if (tc) push(nil, *env);
  return tf_progn(function, *env);
}

object *apply (symbol_t name, object *function, object *args, object *env) {
  if (symbolp(function)) {
    symbol_t fname = function->name;
    checkargs(fname, args);
    return ((fn_ptr_type)lookupfn(fname))(args, env);
  }
  if (consp(function) && issymbol(car(function), LAMBDA)) {
    function = cdr(function);
    object *result = closure(0, 0, NULL, function, args, &env);
    return eval(result, env);
  }
  if (consp(function) && issymbol(car(function), CLOSURE)) {
    function = cdr(function);
    object *result = closure(0, 0, car(function), cdr(function), args, &env);
    return eval(result, env);
  }
  error(name, PSTR("illegal function"), function);
  return NULL;
}"#)

(defparameter *in-place* '(

#"
// In-place operations"#

#+avr
#"
object **place (symbol_t name, object *args, object *env) {
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (issymbol(function, CAR) || issymbol(function, FIRST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take car"), value);
    return &car(value);
  }
  if (issymbol(function, CDR) || issymbol(function, REST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take cdr"), value);
    return &cdr(value);
  }
  if (issymbol(function, NTH)) {
    int index = checkinteger(NTH, eval(second(args), env));
    object *list = eval(third(args), env);
    if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
    while (index > 0) {
      list = cdr(list);
      if (list == NULL) error2(name, PSTR("index to nth is out of range"));
      index--;
    }
    return &car(list);
  }
  error2(name, PSTR("illegal place"));
  return nil;
}"#

#-avr
#"
object **place (symbol_t name, object *args, object *env) {
  if (atom(args)) return &cdr(findvalue(args, env));
  object* function = first(args);
  if (issymbol(function, CAR) || issymbol(function, FIRST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take car"), value);
    return &car(value);
  }
  if (issymbol(function, CDR) || issymbol(function, REST)) {
    object *value = eval(second(args), env);
    if (!listp(value)) error(name, PSTR("can't take cdr"), value);
    return &cdr(value);
  }
  if (issymbol(function, NTH)) {
    int index = checkinteger(NTH, eval(second(args), env));
    object *list = eval(third(args), env);
    if (atom(list)) error(name, PSTR("second argument to nth is not a list"), list);
    while (index > 0) {
      list = cdr(list);
      if (list == NULL) error2(name, PSTR("index to nth is out of range"));
      index--;
    }
    return &car(list);
  }
  if (issymbol(function, AREF)) {
    object *array = eval(second(args), env);
    if (!arrayp(array)) error(AREF, PSTR("first argument is not an array"), array);
    object *dimensions = cddr(array);
    args = cdr(args); 
    int y = 0, x = checkinteger(AREF, eval(second(args), env));
    if (listp(dimensions)) {
      if (cddr(args) == NULL) error2(AREF, PSTR("array needs two subscripts"));
      y = checkinteger(AREF, eval(third(args), env));
    } else if (cddr(args) != NULL) error2(AREF, PSTR("array needs one subscript"));
    return getarray(AREF, array, x, y);
  }
  error2(name, PSTR("illegal place"));
  return nil;
}"#

#"
// Checked car and cdr

inline object *carx (object *arg) {
  if (!listp(arg)) error(0, PSTR("can't take car"), arg);
  if (arg == nil) return nil;
  return car(arg);
}

inline object *cdrx (object *arg) {
  if (!listp(arg)) error(0, PSTR("can't take cdr"), arg);
  if (arg == nil) return nil;
  return cdr(arg);
}"#))


(defparameter *i2c-interface* '(

#-(or avr badge)
#"
// I2C interface

void I2Cinit (bool enablePullup) {
  (void) enablePullup;
  Wire.begin();
}

inline int I2Cread () {
  return Wire.read();
}

inline bool I2Cwrite (uint8_t data) {
  return Wire.write(data);
}

bool I2Cstart (uint8_t address, uint8_t read) {
 int ok = true;
 if (read == 0) {
   Wire.beginTransmission(address);
   ok = (Wire.endTransmission(true) == 0);
   Wire.beginTransmission(address);
 }
 else Wire.requestFrom(address, I2CCount);
 return ok;
}

bool I2Crestart (uint8_t address, uint8_t read) {
  int error = (Wire.endTransmission(false) != 0);
  if (read == 0) Wire.beginTransmission(address);
  else Wire.requestFrom(address, I2CCount);
  return error ? false : true;
}

void I2Cstop (uint8_t read) {
  if (read == 0) Wire.endTransmission(); // Check for error?
}"#

#+(or avr badge)
#"
// I2C interface

#if defined(__AVR_ATmega328P__)
uint8_t const TWI_SDA_PIN = 18;
uint8_t const TWI_SCL_PIN = 19;
#elif defined(__AVR_ATmega1280__) || defined(__AVR_ATmega2560__)
uint8_t const TWI_SDA_PIN = 20;
uint8_t const TWI_SCL_PIN = 21;
#elif defined(__AVR_ATmega644P__) || defined(__AVR_ATmega1284P__)
uint8_t const TWI_SDA_PIN = 17;
uint8_t const TWI_SCL_PIN = 16;
#elif defined(__AVR_ATmega32U4__)
uint8_t const TWI_SDA_PIN = 6;
uint8_t const TWI_SCL_PIN = 5;
#endif

#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
uint32_t const FREQUENCY = 400000L;  // Hardware I2C clock in Hz
uint32_t const T_RISE = 300L;        // Rise time
#else
uint32_t const F_TWI = 400000L;  // Hardware I2C clock in Hz
uint8_t const TWSR_MTX_DATA_ACK = 0x28;
uint8_t const TWSR_MTX_ADR_ACK = 0x18;
uint8_t const TWSR_MRX_ADR_ACK = 0x40;
uint8_t const TWSR_START = 0x08;
uint8_t const TWSR_REP_START = 0x10;
uint8_t const I2C_READ = 1;
uint8_t const I2C_WRITE = 0;
#endif

void I2Cinit(bool enablePullup) {
#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
  if (enablePullup) {
    pinMode(PIN_WIRE_SDA, INPUT_PULLUP);
    pinMode(PIN_WIRE_SCL, INPUT_PULLUP);
  }
  uint32_t baud = ((F_CPU/FREQUENCY) - (((F_CPU*T_RISE)/1000)/1000)/1000 - 10)/2;
  TWI0.MBAUD = (uint8_t)baud;
  TWI0.MCTRLA = TWI_ENABLE_bm;                                        // Enable as master, no interrupts
  TWI0.MSTATUS = TWI_BUSSTATE_IDLE_gc;
#else
  TWSR = 0;                        // no prescaler
  TWBR = (F_CPU/F_TWI - 16)/2;     // set bit rate factor
  if (enablePullup) {
    digitalWrite(TWI_SDA_PIN, HIGH);
    digitalWrite(TWI_SCL_PIN, HIGH);
  }
#endif
}

int I2Cread() {
#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
  if (I2CCount != 0) I2CCount--;
  while (!(TWI0.MSTATUS & TWI_RIF_bm));                               // Wait for read interrupt flag
  uint8_t data = TWI0.MDATA;
  // Check slave sent ACK?
  if (I2CCount != 0) TWI0.MCTRLB = TWI_MCMD_RECVTRANS_gc;             // ACK = more bytes to read
  else TWI0.MCTRLB = TWI_ACKACT_bm | TWI_MCMD_RECVTRANS_gc;           // Send NAK
  return data;
#else
  if (I2CCount != 0) I2CCount--;
  TWCR = 1<<TWINT | 1<<TWEN | ((I2CCount == 0) ? 0 : (1<<TWEA));
  while (!(TWCR & 1<<TWINT));
  return TWDR;
#endif
}

bool I2Cwrite(uint8_t data) {
#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
  while (!(TWI0.MSTATUS & TWI_WIF_bm));                               // Wait for write interrupt flag
  TWI0.MDATA = data;
  TWI0.MCTRLB = TWI_MCMD_RECVTRANS_gc;                                // Do nothing
  return !(TWI0.MSTATUS & TWI_RXACK_bm);                              // Returns true if slave gave an ACK
#else
  TWDR = data;
  TWCR = 1<<TWINT | 1 << TWEN;
  while (!(TWCR & 1<<TWINT));
  return (TWSR & 0xF8) == TWSR_MTX_DATA_ACK;
#endif
}

bool I2Cstart(uint8_t address, uint8_t read) {
#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
  TWI0.MADDR = address<<1 | read;                                     // Send START condition
  while (!(TWI0.MSTATUS & (TWI_WIF_bm | TWI_RIF_bm)));                // Wait for write or read interrupt flag
  if ((TWI0.MSTATUS & TWI_ARBLOST_bm)) return false;                  // Return false if arbitration lost or bus error
  return !(TWI0.MSTATUS & TWI_RXACK_bm);                              // Return true if slave gave an ACK
#else
  uint8_t addressRW = address<<1 | read;
  TWCR = 1<<TWINT | 1<<TWSTA | 1<<TWEN;    // Send START condition
  while (!(TWCR & 1<<TWINT));
  if ((TWSR & 0xF8) != TWSR_START && (TWSR & 0xF8) != TWSR_REP_START) return false;
  TWDR = addressRW;  // send device address and direction
  TWCR = 1<<TWINT | 1<<TWEN;
  while (!(TWCR & 1<<TWINT));
  if (addressRW & I2C_READ) return (TWSR & 0xF8) == TWSR_MRX_ADR_ACK;
  else return (TWSR & 0xF8) == TWSR_MTX_ADR_ACK;
#endif
}

bool I2Crestart (uint8_t address, uint8_t read) {
  return I2Cstart(address, read);
}

void I2Cstop (uint8_t read) {
#if defined(__AVR_ATmega4809__) || defined(ARDUINO_AVR_ATmega4809)
  (void) read;
  TWI0.MCTRLB = TWI_ACKACT_bm | TWI_MCMD_STOP_gc;                     // Send STOP
#else
  (void) read;
  TWCR = 1<<TWINT | 1<<TWEN | 1<<TWSTO;
  while (TWCR & 1<<TWSTO); // wait until stop and bus released
#endif
}"#))
        