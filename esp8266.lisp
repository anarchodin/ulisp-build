;;;-*- Mode: Lisp; Package: cl-user -*-

(in-package :cl-user)

; Zero

(defparameter *header-esp8266*
#"/* uLisp Zero 1.0a - version for Arduino Zero and Adafruit Feather M0 - www.ulisp.com
   David Johnson-Davies - www.technoblogy.com - 29th April 2017

   Licensed under the MIT license: https://opensource.org/licenses/MIT
*/

#include <setjmp.h>
"#)


(defparameter *compile-options-esp8266* #"
// Compile options

#define printfreespace
"#)


(defparameter *macros-esp8266* #"
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

#define symbolp(x)         ((x)->type == SYMBOL)

#define mark(x)            (car(x) = (object *)(((uintptr_t)(car(x))) | MARKBIT))
#define unmark(x)          (car(x) = (object *)(((uintptr_t)(car(x))) & ~MARKBIT))
#define marked(x)          ((((uintptr_t)(car(x))) & MARKBIT) != 0)
#define MARKBIT            1

#if defined(ARDUINO_SAMD_ZERO)
#define pgm_read_ptr2(addr) (*(const void* const *)(addr))
#else
#define pgm_read_ptr2(addr) pgm_read_word(addr)
#endif

// Constants

enum type { ZERO=0, SYMBOL=2, PAIR=4 };  // PAIR must be last
enum token { UNUSED, BRA, KET, QUO, DOT };
"#)


(defparameter *workspace-esp8266* #"
// Workspace - sizes in bytes
#define WORDALIGNED __attribute__((aligned (2)))
#define BUFFERSIZE 18
#define WORKSPACESIZE 330               /* Cells (4*bytes) */

object Workspace[WORKSPACESIZE] WORDALIGNED;
char Buffer[BUFFERSIZE];
"#)


(defparameter *typedefs-esp8266* #"
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
      };
    };
  };
} object;

typedef object *(*fn_ptr_type)(object *, object *);

typedef struct {
  PGM_P string;
  fn_ptr_type fptr;
  int min;
  int max;
} tbl_entry_t;
"#)


(defparameter *global-variables-esp8266* #"
// Global variables

jmp_buf exception;
unsigned int Freespace = 0;
char ReturnFlag = 0;
object *Freelist;
extern uint8_t _end;

object *GlobalEnv;
object *GCStack = NULL;
char LastChar = 0;
char LastPrint = 0;
volatile char Escape = 0;

// Forward references
object *tee;

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
  if (Freespace == 0) error(PSTR("No room"));
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
}

// Make each type of object

object *cons (object *arg1, object *arg2) {
  object *ptr = myalloc();
  ptr->car = arg1;
  ptr->cdr = arg2;
  return ptr;
}

object *symbol (symbol_t name) {
  object *ptr = myalloc();
  ptr->type = SYMBOL;
  ptr->name = name;
  return ptr;
}
"#)


(defparameter *garbage-collection-esp8266* #"
// Garbage collection

void markobject (object *obj) {
  MARK:
  if (obj == NULL) return;
  if (marked(obj)) return;

  object* arg = car(obj);
  unsigned int type = obj->type;
  mark(obj);
  
  if (type >= PAIR || type == ZERO) { // cons
    markobject(arg);
    obj = cdr(obj);
    goto MARK;
  }
}

void sweep () {
  Freelist = NULL;
  Freespace = 0;
  for (int i=WORKSPACESIZE-1; i>=0; i--) {
    object *obj = &Workspace[i];
    if (!marked(obj)) myfree(obj); else unmark(obj);
  }
}

void gc (object *form, object *env) {
  markobject(tee); 
  markobject(GlobalEnv);
  markobject(GCStack);
  markobject(form);
  markobject(env);
  sweep();
}
"#)


(defparameter *compactimage-esp8266* "")


(defparameter *error-handling-esp8266* #"
// Error handling

void error (PGM_P string) {
  pfl(); pfstring(PSTR("Error: "));
  pfstring(string); pln();
  GCStack = NULL;
  longjmp(exception, 1);
}

void error2 (object *symbol, PGM_P string) {
  pfl(); pfstring(PSTR("Error: "));
  if (symbol == NULL) pfstring(PSTR("function "));
  else { pchar('\''); printobject(symbol); pfstring(PSTR("' ")); }
  pfstring(string); pln();
  GCStack = NULL;
  longjmp(exception, 1);
}
"#)

(defparameter *tracing-esp8266* "")


(defparameter *helper-functions-esp8266* #"
// Helper functions

boolean consp (object *x) {
  if (x == NULL) return false;
  unsigned int type = x->type;
  return type >= PAIR || type == ZERO;
}

boolean atom (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type < PAIR && type != ZERO;
}

boolean listp (object *x) {
  if (x == NULL) return true;
  unsigned int type = x->type;
  return type >= PAIR || type == ZERO;
}

int toradix40 (char ch) {
  if (ch == 0) return 0;
  if (ch >= '0' && ch <= '9') return ch-'0'+30;
  ch = ch | 0x20;
  if (ch >= 'a' && ch <= 'z') return ch-'a'+1;
  return -1; // Invalid
}

int fromradix40 (int n) {
  if (n >= 1 && n <= 26) return 'a'+n-1;
  if (n >= 30 && n <= 39) return '0'+n-30;
  return 0;
}

int pack40 (char *buffer) {
  return (((toradix40(buffer[0]) * 40) + toradix40(buffer[1])) * 40 + toradix40(buffer[2]));
}

boolean valid40 (char *buffer) {
 return (toradix40(buffer[0]) >= 0 && toradix40(buffer[1]) >= 0 && toradix40(buffer[2]) >= 0);
}

char *name (object *obj) {
  if(!symbolp(obj)) error(PSTR("Error in name"));
  symbol_t x = obj->name;
  if (x < ENDFUNCTIONS) return lookupbuiltin(x);
  Buffer[3] = '\0';
  for (int n=2; n>=0; n--) {
    Buffer[n] = fromradix40(x % 40);
    x = x / 40;
  }
  return Buffer;
}

int issymbol (object *obj, symbol_t n) {
  return symbolp(obj) && obj->name == n;
}

int eq (object *arg1, object *arg2) {
  int same_object = (arg1 == arg2);
  int same_symbol = (symbolp(arg1) && symbolp(arg2) && arg1->name == arg2->name);
  return (same_object || same_symbol);
}

object *progn (object *args, object *env) {
  if (args == NULL) return nil;
  object *more = cdr(args);
  while (more != NULL) {
  eval(car(args), env);
    args = more;
    more = cdr(args);
  }
  return car(args);
}
"#)


(defparameter *association-lists-esp8266* "")


(defparameter *string-utilities-esp8266* "")


(defparameter *closures-esp8266* #"
// Lookup variable in environment

object *value (symbol_t n, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair)->name == n) return pair;
    env = cdr(env);
  }
  return nil;
}

object *findvalue (object *var, object *env) {
  symbol_t varname = var->name;
  object *pair = value(varname, env);
  if (pair == NULL) pair = value(varname, GlobalEnv);
  if (pair == NULL) error2(var,PSTR("unknown variable"));
  return pair;
}

object *findtwin (object *var, object *env) {
  while (env != NULL) {
    object *pair = car(env);
    if (pair != NULL && car(pair) == var) return pair;
    env = cdr(env);
  }
  return NULL;
}

void dropframe (int tc, object **env) {
  if (tc) {
    while (*env != NULL && car(*env) != NULL) {
      pop(*env);
    }
  } else {
    push(nil, *env);
  }
}

// Handling closures
  
object *closure (object *fname, object *function, object *args, object **env) {
  object *params = first(function);
  function = cdr(function);
  // Add arguments to environment
  while (params != NULL && args != NULL) {
    object *value;
    object *var = first(params);
    value = first(args);
    args = cdr(args);
    push(cons(var,value), *env);
    params = cdr(params);
  }
  if (params != NULL) error2(fname, PSTR("has too few parameters"));
  if (args != NULL) error2(fname, PSTR("has too many parameters"));
  // Do an implicit progn
  return progn(function, *env);
}
"#)


(defparameter *in-place-esp8266* #"
// Checked car and cdr

inline object *carx (object *arg) {
  if (!listp(arg)) error(PSTR("Can't take car"));
  if (arg == nil) return nil;
  return car(arg);
}

inline object *cdrx (object *arg) {
  if (!listp(arg)) error(PSTR("Can't take cdr"));
  if (arg == nil) return nil;
  return cdr(arg);
}
"#)


(defparameter *definitions-esp8266*

  '(("Symbols"
     ((SYMBOLS nil nil nil nil)
      (NIL "nil" 0 0 nil)
      (TEE "t" 1 0 nil)
      (LAMBDA nil 0 127 nil)))

    ("Special forms"
     ((SPECIAL_FORMS nil nil nil nil)
      (QUOTE nil 1 1 "
object *sp_quote (object *args, object *env) {
  (void) env;
  return first(args);
}")

      (DEFUN nil 0 127 #"
object *sp_defun (object *args, object *env) {
  (void) env;
  object *var = first(args);
  if (!symbolp(var)) error2(var, PSTR("is not a symbol"));
  object *val = cons(symbol(LAMBDA), cdr(args));
  object *pair = value(var->name,GlobalEnv);
  if (pair != NULL) { cdr(pair) = val; return var; }
  push(cons(var, val), GlobalEnv);
  return var;
}"#)

      (DEFVAR nil 2 2 #"
object *sp_defvar (object *args, object *env) {
  object *var = first(args);
  if (!symbolp(var)) error2(var, PSTR("is not a symbol"));
  object *val = eval(second(args), env);
  object *pair = value(var->name,GlobalEnv);
  if (pair != NULL) { cdr(pair) = val; return var; }
  push(cons(var, val), GlobalEnv);
  return var;
}"#)

     (SETQ nil 2 2 "
object *sp_setq (object *args, object *env) {
  object *arg = eval(second(args), env);
  object *pair = findvalue(first(args), env);
  cdr(pair) = arg;
  return arg;
}")

     (IF nil 2 3 "
object *sp_if (object *args, object *env) {
  if (eval(first(args), env) != nil) return eval(second(args), env);
  return eval(third(args), env);
}"))
    "sp")

  ("Core functions"
   ((FUNCTIONS nil nil nil nil)
    (NOT nil 1 1 "
object *fn_not (object *args, object *env) {
  (void) env;
  return (first(args) == nil) ? tee : nil;
}")

    (NULLFN "null" 1 1 (not))

    (CONS nil 2 2 "
object *fn_cons (object *args, object *env) {
  (void) env;
  return cons(first(args),second(args));
}")

    (ATOM nil 1 1 "
object *fn_atom (object *args, object *env) {
  (void) env;
  return atom(first(args)) ? tee : nil;
}")

    (LISTP nil 1 1 "
object *fn_listp (object *args, object *env) {
  (void) env;
  return listp(first(args)) ? tee : nil;
}")

    (CONSP nil 1 1 "
object *fn_consp (object *args, object *env) {
  (void) env;
  return consp(first(args)) ? tee : nil;
}")

    (SYMBOLP nil 1 1 "
object *fn_symbolp (object *args, object *env) {
  (void) env;
  return symbolp(first(args)) ? tee : nil;
}")

    (EQ nil 2 2 "
object *fn_eq (object *args, object *env) {
  (void) env;
  return eq(first(args), second(args)) ? tee : nil;
}")))

  ("List functions"
   ((CAR nil 1 1 cxr)
    (CDR nil 1 1 cxr)))

    ("System functions"
     ((EVAL nil 1 1 "
object *fn_eval (object *args, object *env) {
  return eval(first(args), env);
}")

      (GLOBALS nil 0 0 "
object *fn_globals (object *args, object *env) {
  (void) args, (void) env;
  return GlobalEnv;
}")

      (LOCALS nil 0 0 "
object *fn_locals (object *args, object *env) {
  (void) args;
  return env;
}")))))


(defparameter *eval-esp8266* #"
// Table lookup functions

int builtin (char* n) {
  int entry = 0;
  while (entry < ENDFUNCTIONS) {
   if (strcmp_P(n, (PGM_P)pgm_read_ptr2(&lookup_table[entry].string)) == 0 )
      return entry;
    entry++;
  }
  return ENDFUNCTIONS;
}

fn_ptr_type lookupfn (symbol_t name) {
  return (fn_ptr_type)pgm_read_ptr2(&lookup_table[name].fptr);
}

int lookupmin (symbol_t name) {
  return pgm_read_word(&lookup_table[name].min);
}

int lookupmax (symbol_t name) {
  return pgm_read_word(&lookup_table[name].max);
}

char *lookupbuiltin (symbol_t name) {
  strcpy_P(Buffer, (PGM_P)(pgm_read_ptr2(&lookup_table[name].string)));
  return Buffer;
}

// Main evaluator

object *eval (object *form, object *env) {
  // Enough space?
  if (Freespace < 20) gc(form, env);
  // if (_end != 0xA5) error(PSTR("Stack overflow"));
  // Escape
  if (Escape) { Escape = 0; error(PSTR("Escape!"));}
  
  if (form == NULL) return nil;

  if (symbolp(form)) {
    symbol_t name = form->name;
    if (name == NIL) return nil;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (name <= ENDFUNCTIONS) return form;
    error2(form, PSTR("undefined"));
  }
  
  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  // List starts with a symbol?
  if (symbolp(function)) {
    symbol_t name = function->name;

    if (name == LAMBDA) {
      if (env == NULL) return form;
      error(PSTR("closures not supported"));
    }
    
    if ((name > SPECIAL_FORMS) && (name < FUNCTIONS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }
  }
        
  // Evaluate the parameters - result in head
  object *fname = car(form);
  object *head = cons(eval(car(form), env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL) {
    object *obj = cons(eval(car(form),env),NULL);
    cdr(tail) = obj;
    tail = obj;
    form = cdr(form);
    nargs++;
  }
    
  function = car(head);
  args = cdr(head);
 
  if (symbolp(function)) {
    symbol_t name = function->name;
    if (name >= ENDFUNCTIONS) error2(fname, PSTR("is not valid here"));
    if (nargs<lookupmin(name)) error2(fname, PSTR("has too few arguments"));
    if (nargs>lookupmax(name)) error2(fname, PSTR("has too many arguments"));
    object *result = ((fn_ptr_type)lookupfn(name))(args, env);
    pop(GCStack);
    return result;
  }
      
  if (listp(function) && issymbol(car(function), LAMBDA)) {
    dropframe(0, &env);
    form = closure(fname, cdr(function), args, &env);
    pop(GCStack);
    return eval(form, env);
  } 
  
  error2(fname, PSTR("is an illegal function")); return nil;
}
"#)


(defparameter *print-functions-esp8266* #"
// Print functions

void pchar (char c) {
  LastPrint = c;
  Serial.write(c);
  if (c == '\r') Serial.write('\n');
}

void pstring (char *s) {
  while (*s) pchar(*s++);
}

void pfstring (PGM_P s) {
  int p = (int)s;
  while (1) {
    char c = pgm_read_byte(p++);
    if (c == 0) return;
    pchar(c);
  }
}

void pint (int i) {
  int lead = 0;
  if (i<0) pchar('-');
  for (int d=10000; d>0; d=d/10) {
    int j = i/d;
    if (j!=0 || lead || d==1) { pchar(abs(j)+'0'); lead=1;}
    i = i - j*d;
  }
}

void pln () {
  pchar('\r');
}

void pfl () {
  if (LastPrint != '\r') pchar('\r');
}

void printobject(object *form){
  if (form == NULL) pfstring(PSTR("nil"));
  else if (listp(form)) {
    pchar('(');
    printobject(car(form));
    form = cdr(form);
    while (form != NULL && listp(form)) {
      pchar(' ');
      printobject(car(form));
      form = cdr(form);
    }
    if (form != NULL) {
      pfstring(PSTR(" . "));
      printobject(form);
    }
    pchar(')');
  } else if (symbolp(form)) {
    pstring(name(form));
  } else
    error(PSTR("Error in print."));
}
"#)


(defparameter *read-functions-esp8266* #"
int gchar () {
  if (LastChar) { 
    char temp = LastChar;
    LastChar = 0;
    return temp;
  }
  while (!Serial.available()) ;
  char temp = Serial.read();
  if (temp != '\r') pchar(temp);
  return temp;
}

object *nextitem() {
  int ch = gchar();
  while(isspace(ch)) ch = gchar();

  if (ch == ';') {
    while(ch != '(') ch = gchar();
    ch = '(';
  }
  if (ch == '\r') ch = gchar();
  if (ch == EOF) exit(0);

  if (ch == ')') return (object *)KET;
  if (ch == '(') return (object *)BRA;
  if (ch == '\'') return (object *)QUO;
  if (ch == '.') return (object *)DOT;
  
  // Parse variable
  int index = 0;
  Buffer[2] = '\0'; // In case variable is one letter

  while(!isspace(ch) && ch != ')' && ch != '(' && index < BUFFERSIZE-1) {
    Buffer[index++] = ch;
    ch = gchar();
  }

  Buffer[index] = '\0';
  if (ch == ')') LastChar = ')';
  if (ch == '(') LastChar = '(';
  
  int x = builtin(Buffer);
  if (x == NIL) return nil;
  if (x < ENDFUNCTIONS) return symbol(x);
  else if (index < 4 && valid40(Buffer)) return symbol(pack40(Buffer));
  error(PSTR("Illegal symbol"));
  return nil;
}

object *readrest() {
  object *item = nextitem();

  if(item == (object *)KET) return NULL;
  
  if(item == (object *)DOT) {
    object *arg1 = read();
    if (readrest() != NULL) error(PSTR("Malformed list"));
    return arg1;
  }

  if(item == (object *)QUO) {
    object *arg1 = read();
    return cons(cons(symbol(QUOTE), cons(arg1, NULL)), readrest());
  }
   
  if(item == (object *)BRA) item = readrest(); 
  return cons(item, readrest());
}

object *read() {
  object *item = nextitem();
  if (item == (object *)BRA) return readrest();
  if (item == (object *)DOT) return read();
  if (item == (object *)QUO) return cons(symbol(QUOTE), cons(read(), NULL)); 
  return item;
}
"#)


(defparameter *setup-esp8266* #"
// Setup

void initenv() {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup() {
  Serial.begin(9600);
  while (!Serial);  // wait for Serial to initialize
  initworkspace();
  initenv();
  _end = 0xA5;      // Canary to check stack
  pfstring(PSTR("uLisp Zero 1.0")); pln();
}
"#)


(defparameter *repl-esp8266* #"
// Read/Evaluate/Print loop

void repl(object *env) {
  for (;;) {
    gc(NULL, env);
    #if defined (printfreespace)
    pint(Freespace);
    #endif
    pfstring(PSTR("> "));
    object *line = read();
    if (line == (object *)KET) error(PSTR("Unmatched right bracket"));
    push(line, GCStack);
    pfl();
    line = eval(line, env);
    pfl();
    printobject(line);
    pop(GCStack);
    pfl();
    pln();
  }
}

void loop() {
  if (!setjmp(exception)) {
    #if defined(resetautorun)
    autorunimage();
    #endif
  }
  repl(NULL);
}
"#)