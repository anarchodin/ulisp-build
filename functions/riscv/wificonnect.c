#include "ulisp.h"

object *fn_wificonnect (object *args, object *env) {
  (void) env;
  char ssid[33], pass[65];
  int status = WL_IDLE_STATUS;     // the Wifi radio's status
  // if (args == NULL) { WiFi.disconnect(true); return nil; }
  // if (cdr(args) == NULL) WiFi.begin(cstring(first(args), ssid, 33));
  while ( status != WL_CONNECTED) {
    status = WiFi.begin(cstring(first(args), ssid, 33), cstring(second(args), pass, 65));
  // int result = WiFi.waitForConnectResult();
  }
  return nil;
}
