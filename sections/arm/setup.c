// Setup

void initgfx () {
#if defined(gfxsupport)
  tft.initR(INITR_BLACKTAB);
  tft.setRotation(1);
  pinMode(TFT_BACKLIGHT, OUTPUT);
  digitalWrite(TFT_BACKLIGHT, HIGH);
  tft.fillScreen(ST77XX_BLACK);
#endif
}

void initenv () {
  GlobalEnv = NULL;
}

void setup () {
  Serial.begin(9600);
  int start = millis();
  while ((millis() - start) < 5000) { if (Serial) break; }
  initworkspace();
  initenv();
  initsleep();
  initgfx();
  pfstring(PSTR("uLisp 3.6 "), pserial); pln(pserial);
}
