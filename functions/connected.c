#include "ulisp.h"

object *fn_connected (object *args, object *env) {
  (void) env;
  if (isstream(first(args))>>8 != WIFISTREAM) error2(CONNECTED, PSTR("invalid stream"));
  return client.connected() ? tee : nil;
}
