// Setup

void initgfx () {
#if defined(gfxsupport)
  Wire.begin();
  tft.begin(SSD1306_SWITCHCAPVCC, 0x3C);
  tft.fillScreen(COLOR_BLACK);
  tft.display();
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
  pfstring(PSTR("uLisp 3.5 "), pserial); pln(pserial);
}
