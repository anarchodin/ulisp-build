#include "ulisp.h"

object *fn_princtostring (object *args, object *env) {   
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRINCTOSTRING);
  prin1object(arg, pstr);
  obj->cdr = GlobalString;
  return obj;
}
