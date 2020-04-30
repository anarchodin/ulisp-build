#include "ulisp.h"

object *fn_available (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(AVAILABLE, PSTR("invalid stream"));
  return number(client.available());
}
