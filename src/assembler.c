// Assembler

#if defined(CPU_ATmega1284P)
#define CODE_ADDRESS 0x1bb00
#elif defined(CPU_AVR128DX48)
#define CODE_ADDRESS 0x1be00
#endif

object *call (int entry, int nargs, object *args, object *env) {
#if defined(CODESIZE)
  (void) env;
  int param[4];
  for (int i=0; i<nargs; i++) {
    object *arg = first(args);
    if (intp(arg)) param[i] = getint(arg);
    else param[i] = (uintptr_t)arg;
    args = cdr(args);
  }
  #ifdef __riscv
  asm("fence.i");
  #endif
  #ifdef __AVR__
  uint32_t address = (CODE_ADDRESS + entry)>>1; // Code addresses are word addresses on AVR
  int w = ((intfn_ptr_type)address)(param[0], param[1], param[2], param[3]);
  #else
  int w = ((intfn_ptr_type)&MyCode[entry])(param[0], param[1], param[2], param[3]);
  #endif
  return number(w);
#else
  return nil;
#endif
}

void putcode (object *arg, int origin, int pc) {
#if defined(CODESIZE)
  int code = checkinteger(DEFCODE, arg);
  MyCode[origin+pc] = code & 0xff;
  MyCode[origin+pc+1] = (code>>8) & 0xff;
  #if defined(assemblerlist)
  printhex4(pc, pserial);
  printhex4(code, pserial);
  #endif
#endif
}

int assemble (int pass, int origin, object *entries, object *env, object *pcpair) {
  int pc = 0; cdr(pcpair) = number(pc);
  while (entries != NULL) {
    object *arg = first(entries);
    if (symbolp(arg)) {
      if (pass == 2) {
        #if defined(assemblerlist)
        printhex4(pc, pserial);
        pfstring(PSTR("     "), pserial);
        printobject(arg, pserial); pln(pserial);
        #endif
      } else {
        object *pair = findvalue(arg, env);
        cdr(pair) = number(pc);
      }
    } else {
      object *argval = eval(arg, env);
      if (listp(argval)) {
        object *arglist = argval;
        while (arglist != NULL) {
          if (pass == 2) {
            putcode(first(arglist), origin, pc);
            #if defined(assemblerlist)
            if (arglist == argval) superprint(arg, 0, pserial);
            pln(pserial);
            #endif
          }
          pc = pc + 2;
          cdr(pcpair) = number(pc);
          arglist = cdr(arglist);
        }
      } else if (intp(argval)) {
        if (pass == 2) {
          putcode(argval, origin, pc);
          #if defined(assemblerlist)
          superprint(arg, 0, pserial); pln(pserial);
          #endif
        }
        pc = pc + 2;
        cdr(pcpair) = number(pc);
      } else error(DEFCODE, PSTR("illegal entry"), arg);
    }
    entries = cdr(entries);
  }
  #ifdef __AVR__
  // Round up to multiple of 2 to give code size
  if (pc%2 != 0) pc = pc + 2 - pc%2;
  #else
  // Round up to multiple of 4 to give code size
  if (pc%4 != 0) pc = pc + 4 - pc%4;
  #endif
  return pc;
}

// The REGSYMBOL macro encodes symbols for registers.
// These differ by platform; ARM uses 'r0' and RISC-V uses 'a0'.
#if defined(__arm__)
#define REGSYMBOL (18*40+30+regn)*64000)
#elif defined(__riscv)
#define REGSYMBOL (1*40+30+regn)*64000)
#endif

//;; (defcode :type :special)
object *sp_defcode (object *args, object *env) {
#if defined(CODESIZE)
  setflag(NOESC);
  checkargs(DEFCODE, 0x0F, args); // REVIEW: Is this actually needed?
  object *var = first(args);
  object *params = second(args);
  if (!symbolp(var)) error(DEFCODE, PSTR("not a symbol"), var);

  #ifdef __AVR__
  // Make *p* a local variable for program counter
  object *pcpair = cons(newsymbol(pack40((char*)"*p*")), number(0));
  push(pcpair,env);
  args = cdr(args);
  #else /* ARM or RISC-V */
  // Make parameters into synonyms for registers
  int regn = 0;
  while (params != NULL) {
    if (regn > 3) error(DEFCODE, PSTR("more than 4 parameters"), var);
    object *regpair = cons(car(params), newsymbol(REGSYMBOL);
    push(regpair,env);
    regn++;
    params = cdr(params);
  }

  // Make *pc* a local variable
  object *pcpair = cons(newsymbol(pack40((char*)"*pc*\0")), number(0));
  push(pcpair,env);
  args = cdr(args);
  #endif /* end ISA conditional */

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
    if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
      object *codeid = second(pair);
      if (codep(codeid)) {
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
      if (pair != NULL && car(pair) != var && consp(cdr(pair))) { // Exclude me if I already exist
        object *codeid = second(pair);
        if (codep(codeid)) {
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
      block->integer = target<<CODESHIFT | origin;
      origin = target;
    }
    
  } while (smallest < CODESIZE);

  // Second pass - origin is first free location
  codesize = assemble(2, origin, cdr(args), env, pcpair);

  object *val = cons(codehead((origin+codesize)<<CODESHIFT | origin), args);
  object *pair = value(getname(var), GlobalEnv);
  if (pair != NULL) cdr(pair) = val;
  else push(cons(var, val), GlobalEnv);

  #if defined(CPU_ATmega1284P)
  // Use Optiboot Flasher in MightyCore with 256 byte page from CODE_ADDRESS 0x1bb00 to 0x1bbff
  optiboot_page_erase(CODE_ADDRESS);
  for (unsigned int i=0; i<CODESIZE/2; i++) optiboot_page_fill(CODE_ADDRESS + i*2, MyCode[i*2] | MyCode[i*2+1]<<8);
  optiboot_page_write(CODE_ADDRESS);
  #elif defined (CPU_AVR128DX48)
  // Use Flash Writer in DxCore with 512 byte page from CODE_ADDRESS 0x1be00 to 0x1c000
  if (Flash.checkWritable()) error2(DEFCODE, PSTR("flash write not supported"));
  if (Flash.erasePage(CODE_ADDRESS, 1)) error2(DEFCODE, PSTR("problem erasing flash"));
  Flash.writeBytes(CODE_ADDRESS, MyCode, CODESIZE);
  #endif

  clrflag(NOESC);
  return var;
#else
  error2(DEFCODE, PSTR("not available"));
  return nil;
#endif
}

// Undefine macro we know won't see more use.
#undef REGSYMBOL
