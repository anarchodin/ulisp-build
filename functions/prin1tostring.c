#include "ulisp.h"

object *fn_prin1tostring (object *args, object *env) {   
  (void) env;
  object *arg = first(args);
  object *obj = startstring(PRIN1TOSTRING);
  printobject(arg, pstr);
  obj->cdr = GlobalString;
  return obj;
}
