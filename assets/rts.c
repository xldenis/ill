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

String* mkString(size_t length)
{
    String* strPtr = (String*)(malloc(sizeof(String)));
    char* buffer = malloc((length + 1) * sizeof(char));

    strPtr->string_length = length;
    strPtr->data = buffer;

    return strPtr;
}

String* plusStr(String* a, String* b)
{
    int num_chars = a->string_length + b->string_length;
    String* str = mkString(num_chars);
    snprintf(str->data, num_chars + 1, "%s%s", a->data, b->data);
    return str;
}

String* showInt(Int* x)
{
    int num_chars = snprintf(NULL, 0, "%lld", x->val) + 1;
    String* str = mkString(num_chars);
    snprintf(str->data, num_chars, "%lld", x->val);
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
