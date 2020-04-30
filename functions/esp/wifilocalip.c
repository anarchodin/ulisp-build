#include "ulisp.h"

object *fn_wifilocalip (object *args, object *env) {
  (void) args, (void) env;
  return lispstring((char*)WiFi.localIP().toString().c_str());
}
