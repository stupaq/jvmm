#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

/** Debug */
#ifndef NODEBUG
#define debug(format, ...) fprintf (stderr, "debug: " format, __VA_ARGS__)
#else
#define debug(format, ...)
#endif

/** Object initialization */
void object_init(int32_t size, void* object, void* proto) {
  debug("object_init(): size %d object %p prototype %p\n", size, object, proto);
  memmove(object, proto, size);
}

/** Reference counted structures */
#define RC_HEADER_SIZE (sizeof(struct rc_header))

struct rc_header {
  int32_t count;
} __attribute__((__packed__));

static inline void* rc_header_to_ptr(struct rc_header* header) {
  return ((char*) header) + RC_HEADER_SIZE;
}

static inline struct rc_header* rc_ptr_to_header(void* ptr) {
  return (struct rc_header*) (((char*) ptr) - RC_HEADER_SIZE);
}

static inline bool rc_is_const(struct rc_header* header) {
  return header->count < 0;
}

void* rc_malloc(int32_t size) {
  struct rc_header* header = (struct rc_header*) malloc(((size_t) size) + RC_HEADER_SIZE);
  if (header == NULL) {
    debug("malloc(): size %d failed!\n", size);
    exit(EXIT_FAILURE);
  }
  header->count = 1;
  void* ptr = rc_header_to_ptr(header);
  debug("malloc(): size %d count %d object  %p\n", size, header->count, ptr);
  return ptr;
}

void rc_retain(void* ptr) {
  if (ptr != NULL) {
    struct rc_header* header = rc_ptr_to_header(ptr);
    if (!rc_is_const(header)) {
      debug("retain(): count %d object %p\n", header->count, ptr);
      header->count++;
    } else {
      debug("retain(): constant %p\n", ptr);
    }
  } else {
    debug("release(): %s\n", "null");
  }
}

void rc_release(void* ptr) {
  if (ptr != NULL) {
    struct rc_header* header = rc_ptr_to_header(ptr);
    if (!rc_is_const(header)) {
      header->count--;
      debug("release(): count %d object %p\n", header->count, ptr);
      if (header->count == 0) {
        debug("free(): object %p\n", ptr);
        free(header);
      }
    } else {
      debug("release(): constant %p\n", ptr);
    }
  } else {
    debug("release(): %s\n", "null");
  }
}

/** Strings */
// Null terminated strings
char* string_concat(char* first, char* second) {
  char* both = rc_malloc(strlen(first) + strlen(second) + 1);
  strcpy(both, first);
  strcat(both, second);
  return both;
}

int32_t string_length(char* str) {
  return strlen(str);
}

/** Arrays */
#define ARRAY_HEADER_SIZE (sizeof(struct array_header))

struct array_header {
  int32_t length;
};

static inline void* array_header_to_ptr(struct array_header* header) {
  return ((char*) header) + ARRAY_HEADER_SIZE;
}

static inline struct array_header* array_ptr_to_header(void* ptr) {
  return (struct array_header*) (((char*) ptr) - ARRAY_HEADER_SIZE);
}

void* array_create(int32_t length, int32_t element) {
  struct array_header* array =  rc_malloc(length * element + ARRAY_HEADER_SIZE);
  array->length = length;
  void* ptr = array_header_to_ptr(array);
  memset(ptr, 0, length * element);
  return ptr;
}

int32_t array_length(void* array) {
  struct array_header* header = array_ptr_to_header(array);
  return header->length;
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
  getline(&line, NULL, stdin);
  char* str = (char*) rc_malloc(strlen(line) + 1);
  strcpy(str, line);
  return str;
}

void error() {
  printf("runtime error\n");
  exit(EXIT_FAILURE);
}
