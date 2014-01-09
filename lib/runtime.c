#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

/** Reference counted structures */
#define RC_HEADER_SIZE (sizeof(struct rc_header))

struct rc_header {
  int32_t count;
};

static inline void* rc_header_to_ptr(struct rc_header* header) {
  return ((char*) header) + RC_HEADER_SIZE;
}

static inline struct rc_header* rc_ptr_to_header(void* ptr) {
  return (struct rc_header*) ((char*) ptr) - RC_HEADER_SIZE;
}

static inline bool rc_is_const(struct rc_header* header) {
  return header->count < 0;
}

void* rc_malloc(int32_t size) {
  struct rc_header* header = (struct rc_header*) malloc(((size_t) size) + RC_HEADER_SIZE);
  header->count = 1;
  return rc_header_to_ptr(header);
}

void rc_retain(void* ptr) {
  if (ptr != NULL) {
    struct rc_header* header = rc_ptr_to_header(ptr);
    if (!rc_is_const(header)) {
      header->count++;
    }
  }
}

void rc_release(void* ptr) {
  if (ptr != NULL) {
    struct rc_header* header = rc_ptr_to_header(ptr);
    if (!rc_is_const(header)) {
      if (header->count-- == 0) {
        free(header);
      }
    }
  }
}

/** Strings */
// Null terminated strings
char* concat(char* first, char* second) {
  char* both = rc_malloc(strlen(first) + strlen(second) + 1);
  strcpy(both, first);
  strcat(both, second);
  return both;
}

/** Arrays */
struct array {
  int32_t length;
  void* data[0];
};

int32_t array_length(struct array* array) {
  return array->length;
}

void* array_data(struct array* array) {
  return array->data;
}

struct array* array_malloc(int32_t length, int32_t element) {
  struct array* array =  rc_malloc(length * element);
  array->length = length;
  return array;
}

/** IO library functions */
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
  char* line = NULL;
  ssize_t ret = getline(&line, NULL, stdin);
  char* str = (char*) rc_malloc(strlen(line) + 1);
  strcpy(str, line);
  return str;
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
}
