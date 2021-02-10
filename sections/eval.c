// Main evaluator

// TODO: These linker variables could probably be simplified.

#if defined(__arm__)

#if defined(ARDUINO_TEENSY40) || defined(ARDUINO_TEENSY41)
#define ENDSTACK _ebss
#else
#define ENDSTACK end
#endif

extern uint32_t ENDSTACK;  // Bottom of stack

#elif defined(__riscv)
char end[0];

#elif defined(__AVR__)
extern char __bss_end[];

#else /* fallthrough */
uint8_t End;
#endif

object *eval (object *form, object *env) {
#if defined(__arm__)
  register int *sp asm ("r13");
#elif defined(__riscv)
  register int *sp asm ("sp");
#elif defined(__AVR__)
  uint8_t sp[0];
#endif
  int TC=0;
  EVAL:
  // FIXME: Find the macro to test for on the next line.
  //yield(); // Needed on ESP8266 to avoid Soft WDT Reset
  // Enough space?
#if defined(__arm__)
  // Serial.println((uint32_t)sp - (uint32_t)&ENDSTACK); // Find best STACKDIFF value
  if (((uint32_t)sp - (uint32_t)&ENDSTACK) < STACKDIFF) error2(0, PSTR("stack overflow"));
#elif defined(__riscv)
  // Serial.println((uintptr_t)sp - (uintptr_t)end); // Find best STACKDIFF value
  if ((uintptr_t)sp - (uintptr_t)end < STACKDIFF) error2(0, PSTR("Stack overflow"));
#elif defined(__AVR__)
  // Serial.println((uint16_t)sp - (uint16_t)__bss_end); // Find best STACKDIFF value
  if ((uint16_t)sp - (uint16_t)__bss_end < STACKDIFF) error2(0, PSTR("stack overflow"));
#else
  if (End != 0xA5) error2(0, PSTR("Stack overflow"));
#endif
  if (Freespace <= WORKSPACESIZE>>4) gc(form, env);      // GC when 1/16 of workspace left
  // Escape
  if (tstflag(ESCAPE)) { clrflag(ESCAPE); error2(0, PSTR("Escape!"));}
  if (!tstflag(NOESC)) testescape();

  if (form == NULL) return nil;

  if (form->type >= NUMBER && form->type <= STRING) return form;

  if (symbolp(form)) {
    symbol_t name = form->name;
    if (name == NIL) return nil;
    object *pair = value(name, env);
    if (pair != NULL) return cdr(pair);
    pair = value(name, GlobalEnv);
    if (pair != NULL) return cdr(pair);
    else if (name <= ENDKEYWORDS) return form;
    error(0, PSTR("undefined"), form);
  }

#ifdef CODE
  if (form->type == CODE) error2(0, PSTR("can't evaluate CODE header"));
#endif

  // It's a list
  object *function = car(form);
  object *args = cdr(form);

  if (function == NULL) error(0, PSTR("illegal function"), nil);
  if (!listp(args)) error(0, PSTR("can't evaluate a dotted pair"), args);

  // List starts with a symbol?
  if (symbolp(function)) {
    symbol_t name = function->name;

    if ((name == LET) || (name == LETSTAR)) {
      int TCstart = TC;
      if (args == NULL) error2(name, noargument);
      object *assigns = first(args);
      if (!listp(assigns)) error(name, notalist, assigns);
      object *forms = cdr(args);
      object *newenv = env;
      push(newenv, GCStack);
      while (assigns != NULL) {
        object *assign = car(assigns);
        if (!consp(assign)) push(cons(assign,nil), newenv);
        else if (cdr(assign) == NULL) push(cons(first(assign),nil), newenv);
        else push(cons(first(assign),eval(second(assign),env)), newenv);
        car(GCStack) = newenv;
        if (name == LETSTAR) env = newenv;
        assigns = cdr(assigns);
      }
      env = newenv;
      pop(GCStack);
      form = tf_progn(forms,env);
      TC = TCstart;
      goto EVAL;
    }

    if (name == LAMBDA) {
      if (env == NULL) return form;
      object *envcopy = NULL;
      while (env != NULL) {
        object *pair = first(env);
        if (pair != NULL) push(pair, envcopy);
        env = cdr(env);
      }
      return cons(symbol(CLOSURE), cons(envcopy,args));
    }

    if ((name > SPECIAL_FORMS) && (name < TAIL_FORMS)) {
      return ((fn_ptr_type)lookupfn(name))(args, env);
    }

    if ((name > TAIL_FORMS) && (name < FUNCTIONS)) {
      form = ((fn_ptr_type)lookupfn(name))(args, env);
      TC = 1;
      goto EVAL;
    }

    if ((name < SPECIAL_FORMS) || (name > ENDFUNCTIONS)) error2(name, PSTR("can't be used as a function"));
  }

  // Evaluate the parameters - result in head
  object *fname = car(form);
  int TCstart = TC;
  object *head = cons(eval(fname, env), NULL);
  push(head, GCStack); // Don't GC the result list
  object *tail = head;
  form = cdr(form);
  int nargs = 0;

  while (form != NULL){
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
    if (name >= ENDFUNCTIONS) error(0, notvalid, fname);
    // HACK: Use fixnum encoding so we can use the same function here as elsewhere.
    checkargs(name, getminmax(name), (object *)((nargs<<3)|2));
    object *result = ((fn_ptr_type)lookupfn(name))(args, env);
    pop(GCStack);
    return result;
  }

  if (consp(function)) {
    symbol_t name = 0;
    if (!listp(fname)) name = fname->name;

    if (issymbol(car(function), LAMBDA)) {
      form = closure(TCstart, name, NULL, cdr(function), args, &env);
      pop(GCStack);
      int trace = tracing(fname->name);
      if (trace) {
        object *result = eval(form, env);
        indent((--(TraceDepth[trace-1]))<<1, ' ', pserial);
        pint(TraceDepth[trace-1], pserial);
        pserial(':'); pserial(' ');
        printobject(fname, pserial); pfstring(PSTR(" returned "), pserial);
        printobject(result, pserial); pln(pserial);
        return result;
      } else {
        TC = 1;
        goto EVAL;
      }
    }

    if (issymbol(car(function), CLOSURE)) {
      function = cdr(function);
      form = closure(TCstart, name, car(function), cdr(function), args, &env);
      pop(GCStack);
      TC = 1;
      goto EVAL;
    }

#ifdef CODE
    if (car(function)->type == CODE) {
      int n = listlength(DEFCODE, second(function));
      if (nargs<n) error2(fname->name, toofewargs);
      if (nargs>n) error2(fname->name, toomanyargs);
      // REVIEW: Why are these two different?
#if defined(__arm__)
      uint32_t entry = startblock(car(function)) + 1;
#elif defined(__riscv)
      uint32_t entry = startblock(car(function));
#endif
      pop(GCStack);
      return call(entry, n, args, env);
    }
#endif
  }

  error(0, PSTR("illegal function"), fname); return nil;
}
