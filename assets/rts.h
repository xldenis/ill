#include <stdlib.h>

#include <string.h>
#include <stdio.h>

typedef struct {
  size_t string_length;
  char* data;
} String;

typedef struct {
  uint64_t tag;
  uint64_t val;
} Int;

String* showInt(Int* x);
String* mkString(size_t x);
Int* plusInt(Int* a, Int* b);
String* plusStr(String* a, String* b);
