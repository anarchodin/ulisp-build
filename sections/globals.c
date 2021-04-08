// Global variables

#if defined(MEMBANK)
object Workspace[WORKSPACESIZE] WORDALIGNED MEMBANK;
#else
object Workspace[WORKSPACESIZE] WORDALIGNED;
#endif
char SymbolTable[SYMBOLTABLESIZE];
#if defined(CODESIZE)
RAMFUNC uint8_t MyCode[CODESIZE] WORDALIGNED;
#endif

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
char LastPrint = 0;

// Flags
enum flag { PRINTREADABLY, RETURNFLAG, ESCAPE, EXITEDITOR, LIBRARYLOADED, NOESC, NOECHO };
volatile uint8_t Flags = 0b00001; // PRINTREADABLY set by default

// Forward references
object *tf_progn (object *form, object *env);
object *eval (object *form, object *env);
object *read (gfun_t gfun);
void repl (object *env);
void printobject (object *form, pfun_t pfun);
char *lookupbuiltin (symbol_t name);
intptr_t lookupfn (symbol_t name);
int builtin (char* n);
void pfstring (PGM_P s, pfun_t pfun);

void pserial(char c);
void pint(int i, pfun_t pfun);
void pintbase(unsigned int i, uint8_t power2, pfun_t pfun);
#ifdef CODE
void printhex4(int i, pfun_t pfun);
#endif
void pstring(char *s, pfun_t pfun);
void printstring(object *form, pfun_t pfun);
void pfl(pfun_t pfun);
void pln(pfun_t pfun);
void prin1object(object *form, pfun_t pfun);

char *symbolname(symbol_t x);
int maxbuffer(char *buffer);
uint8_t nthchar(object *string, int n);

void FlashWrite(uint8_t data);
uint8_t FlashRead();

bool listp(object *x);
object *apply(symbol_t name, object *function, object *args, object *env);
char *lookupsymbol (symbol_t name);

uint8_t getcallc (symbol_t name);
int listlength (symbol_t name, object *list);

int glibrary ();
int gserial ();
