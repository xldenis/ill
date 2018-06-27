#include <stdlib.h>

#include <string.h>
#include <stdio.h>
#include <math.h>

#include "rts.h"


#define MKBOOL(X)  (X ? True() : False())

extern Int* mkInt(uint64_t);
extern Double* mkDouble(double);

extern Bool* True();
extern Bool* False();

// returns a pointer to the beginning of the pos'th utf8 codepoint
// in the buffer at s
char *utf8index(char *s, size_t pos)
{
    ++pos;
    for (; *s; ++s) {
        if ((*s & 0xC0) != 0x80) --pos;
        if (pos == 0) return s;
    }
    return NULL;
}


size_t utf8_codepoints_len(const char *s) {
    size_t count = 0;
    while (*s) {
        count += (*s++ & 0xC0) != 0x80;
    }
    return count;
}

// converts codepoint indexes start and end to byte offsets in the buffer at s
void utf8slice(char *s, ssize_t *start, ssize_t *end)
{
    char *p = utf8index(s, *start);
    *start = p ? p - s : -1;
    p = utf8index(s, *end);
    *end = p ? p - s : -1;
}

String* mkString(size_t bytelength)
{
    String* strPtr = (String*)(malloc(sizeof(String)));
    char* buffer = malloc((bytelength + 1) * sizeof(char));

    strPtr->string_length = 0;
    strPtr->data = buffer;

    return strPtr;
}

String* plusStr(String* a, String* b)
{
    int strlength = a->string_length + b->string_length;
    int num_chars = strlen(a->data) + strlen(b->data);

    String* str = mkString(strlen(a->data) + strlen(b->data));
    str->string_length = strlength;

    snprintf(str->data, num_chars + 1, "%s%s", a->data, b->data);
    return str;
}

String* cloneStr(String* str, Int* start, Int* end)
{
    ssize_t* startIx = alloca(sizeof(ssize_t*));
    ssize_t* endIx = alloca(sizeof(ssize_t*));

    *startIx = start->val;
    *endIx = end->val;

    utf8slice(str->data, startIx, endIx);

    *endIx = *endIx < 0 ? strlen(str->data) : *endIx;
    int num_chars = *endIx - *startIx;
    String* out = mkString(num_chars + 1);

    snprintf(out->data, num_chars + 1, "%s", str->data + *startIx);
    out->string_length = utf8_codepoints_len(out->data);

    return out;
}

Int* lenStr(String* str)
{
    return mkInt(str->string_length);
}

String* showInt(Int* x)
{
    int num_chars = snprintf(NULL, 0, "%lld", x->val) + 1;
    String* str = mkString(num_chars);
    snprintf(str->data, num_chars, "%lld", x->val);
    str->string_length = utf8_codepoints_len(str->data);

    return str; // caller is expected to invoke free() on this buffer to release memory
}

Int* plusInt(Int* a, Int* b)
{
    return mkInt(a->val + b->val);
}

Int* minusInt(Int* a, Int* b)
{
    return mkInt(a->val - b->val);
}

Int* multInt(Int* a, Int* b)
{
    return mkInt(a->val * b->val);
}

Int* divInt(Int* a, Int* b)
{
    return mkInt(a->val / b->val);
}

Int* modInt(Int* a, Int* b)
{
    return mkInt(a->val % b->val);
}

Bool* ltInt(Int* a, Int* b)
{
    return MKBOOL(a->val < b->val);
}

Bool* gtInt(Int* a, Int* b)
{
    return MKBOOL(a->val > b->val);
}

Bool* eqInt(Int* a, Int* b)
{
    return MKBOOL(a->val == b->val);
}

Bool* leqInt(Int* a, Int* b)
{
    return MKBOOL(a->val <= b->val);
}

Bool* geqInt(Int* a, Int* b)
{
    return MKBOOL(a->val >= b->val);
}

String* showDouble(Double* x)
{
    int num_chars = snprintf(NULL, 0, "%f", x->val) + 1;
    String* str = mkString(num_chars);
    snprintf(str->data, num_chars, "%f", x->val);
    str->string_length = utf8_codepoints_len(str->data);

    return str; // caller is expected to invoke free() on this buffer to release memory
}

Double* plusDouble(Double* a, Double* b)
{
    return mkDouble(a->val + b->val);
}

Double* minusDouble(Double* a, Double* b)
{
    return mkDouble(a->val - b->val);
}

Double* multDouble(Double* a, Double* b)
{
    return mkDouble(a->val * b->val);
}

Double* divDouble(Double* a, Double* b)
{
    return mkDouble(a->val / b->val);
}

Double* modDouble(Double* a, Double* b)
{
    return mkDouble(fmod(a->val, b->val));
}

Bool* ltDouble(Double* a, Double* b)
{
    return MKBOOL(a->val < b->val);
}

Bool* gtDouble(Double* a, Double* b)
{
    return MKBOOL(a->val > b->val);
}

Bool* eqDouble(Double* a, Double* b)
{
    return MKBOOL(a->val == b->val);
}

Bool* leqDouble(Double* a, Double* b)
{
    return MKBOOL(a->val <= b->val);
}

Bool* geqDouble(Double* a, Double* b)
{
    return MKBOOL(a->val >= b->val);
}

String* omgDebug(String* a)
{
    printf("%s\n", a->data);
    return a;
}

extern String* module_main();

int main()
{
    // printf("omg\n");
    String* returnVal = module_main();
    printf("%s\n", returnVal->data);

    return 0;
}
