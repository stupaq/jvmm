#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define STRING_LINE_BUFFER 1024

void printInt(int32_t x) {
  printf("%d\n", x);
}

int32_t readInt() {
  int32_t x;
  scanf("%d", &x);
  return x;
}

void printString(char* s) {
  printf("%s\n", s);
}

char* readString() {
  static char buffer[STRING_LINE_BUFFER];
  fgets(buffer, STRING_LINE_BUFFER, stdin);
  char* s = (char*) malloc(strlen(buffer));
  return s;
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
}
