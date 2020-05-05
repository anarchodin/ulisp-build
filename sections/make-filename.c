// Make SD card filename

char *MakeFilename (object *arg) {
  char *buffer = SymbolTop;
  int max = maxbuffer(buffer);
  int i = 0;
  do {
    char c = nthchar(arg, i);
    if (c == '\0') break;
    buffer[i++] = c;
  } while (i<max);
  buffer[i] = '\0';
  return buffer;
}
