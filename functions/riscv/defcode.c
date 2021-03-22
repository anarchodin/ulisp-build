#include "ulisp.h"

object *sp_defcode (object *args, object *env) {
  setflag(NOESC);
  checkargs(DEFCODE, 0x0F, args); // REVIEW: Is this actually needed?
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(DEFCODE, PSTR("not a symbol"), var);

  // Make parameters into synonyms for registers a0, a1, etc
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(DEFCODE, PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), newsymbol((1*40+30+regn)*64000)); // Symbol for a0 etc
    push(regpair,env);
    regn++;
    params = cdr(params);
  }
  
  // Make *pc* a local variable
  object *pcpair = cons(newsymbol(pack40((char*)"*pc*\0\0")), number(0));
  push(pcpair,env);
  args = cdr(args);
  
  // Make labels into local variables
  object *entries = cdr(args);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      object *pair = cons(arg,number(0));
      push(pair,env);
    }
    entries = cdr(entries);
  } 

  // First pass
  int origin = 0;
  int codesize = assemble(1, origin, cdr(args), env, pcpair);

  // See if it will fit
  object *globals = GlobalEnv;
  while (globals != NULL) {
    object *pair = car(globals);
    if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codeid->type == CODE) {
        codesize = codesize + endblock(codeid) - startblock(codeid);
      }
    }
    globals = cdr(globals);
  }
  if (codesize > CODESIZE) error(DEFCODE, PSTR("not enough room for code"), var);
  
  // Compact the code block, removing gaps
  origin = 0;
  object *block;
  int smallest;

  do {
    smallest = CODESIZE;
    globals = GlobalEnv;
    while (globals != NULL) {
      object *pair = car(globals);
      if (pair != NULL && car(pair) != var) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codeid->type == CODE) {
          if (startblock(codeid) < smallest && startblock(codeid) >= origin) {
            smallest = startblock(codeid);
            block = codeid;
          }        
        }
      }
      globals = cdr(globals);
    }

    // Compact fragmentation if necessary
    if (smallest == origin) origin = endblock(block); // No gap
    else if (smallest < CODESIZE) { // Slide block down
      int target = origin;
      for (int i=startblock(block); i<endblock(block); i++) {
        MyCode[target] = MyCode[i];
        target++;
      }
      block->integer = target<<16 | origin;
      origin = target;
    }
    
  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<16 | origin), args);
  object *pair = value(getname(var), GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);
  clrflag(NOESC);
  return var;
}
