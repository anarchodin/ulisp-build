// Setup

void initgfx () {
#if defined(gfxsupport)
  tft.begin(15000000, COLOR_BLACK);
  tft.setRotation(2);
#endif
}

void initenv () {
  GlobalEnv = NULL;
  tee = symbol(TEE);
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp 3.2 "), pserial); pln(pserial);
}
