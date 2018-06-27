#include <stdlib.h>

#include <string.h>
#include <stdio.h>

typedef struct {
  size_t string_length;
  char* data;
} String;

typedef struct {
  uint64_t val;
} Int;

typedef struct {
  double val;
} Double;

typedef struct {
  uint64_t tag;
} Bool;

String* mkString(size_t x);

Int* plusInt(Int* a, Int* b);
Int* minusInt(Int* a, Int* b);
Int* multInt(Int* a, Int* b);
Int* divInt(Int* a, Int* b);
Int* modInt(Int* a, Int* b);
String* showInt(Int* x);

Bool* ltInt(Int* a, Int* b);
Bool* gtInt(Int* a, Int* b);
Bool* eqInt(Int* a, Int* b);
Bool* geqInt(Int* a, Int* b);
Bool* leqInt(Int* a, Int* b);


Double* plusDouble(Double* a, Double* b);
Double* minusDouble(Double* a, Double* b);
Double* multDouble(Double* a, Double* b);
Double* divDouble(Double* a, Double* b);
Double* modDouble(Double* a, Double* b);

String* showDouble(Double* x);

Bool* ltDouble(Double* a, Double* b);
Bool* gtDouble(Double* a, Double* b);
Bool* eqDouble(Double* a, Double* b);
Bool* geqDouble(Double* a, Double* b);
Bool* leqDouble(Double* a, Double* b);

String* omgDebug(String* a);

String* plusStr(String* a, String* b);
String* cloneStr(String* str, Int* start, Int* stop);
