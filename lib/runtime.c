#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

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
  char* s = NULL;
  size_t n = 0;
  getline(&s, &n, stdin);
  return s;
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
}
