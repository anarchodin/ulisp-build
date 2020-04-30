#include "ulisp.h"

object *fn_dumpimage(object *args, object *env) {
  (void) args, (void) env;
  int imagesize = workspacesize; // compactimage(NULL);
  char tmp[16];
  Serial.println(); 
  sprintf(tmp, "freelist: %04x, ", (int)freelist);
  Serial.print(tmp);
  sprintf(tmp, "GlobalEnv: %04x, ", (int)GlobalEnv);
  Serial.print(tmp);
  sprintf(tmp, "GCStack: %04x, ", (int)GCStack);
  Serial.print(tmp);
      
  for (int i=0; i<imagesize; i++) {
    if (i%16 == 0) {
      Serial.println(); 
      sprintf(tmp, "%04x: ", (int)&workspace[i]);
      Serial.print(tmp);
    }
    sprintf(tmp, "%04x.%04x ", (unsigned int)car(&workspace[i]) , (unsigned int)cdr(&workspace[i]));
    Serial.print(tmp);
  }
  Serial.println();
  return nil;
}
