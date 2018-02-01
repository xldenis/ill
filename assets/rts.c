#include <stdlib.h>

#include <string.h>
#include <stdio.h>

#include "rts.h"

String* showInt(Int* x)
{
    int num_chars = snprintf(NULL, 0, "%lld", x->val) + 1;
    String* str = mkString(num_chars);
    snprintf(str->data, num_chars, "%lld", x->val);

    return str; // caller is expected to invoke free() on this buffer to release memory
}

String* mkString(size_t length)
{
    String* strPtr = (String*)(malloc(sizeof(String)));
    char* buffer = malloc(length);

    strPtr->string_length = length;
    strPtr->data = buffer;

    return strPtr;
}

String* plusStr(String* a, String* b)
{
    int num_chars = a->string_length + b->string_length;
    String* str = mkString(num_chars);

    snprintf(str->data, num_chars, "%s%s", a->data, b->data);
    return str;
}

extern String* module_main();

int main()
{
    String* returnVal = module_main();
    printf("%s\n", returnVal->data);

    return 0;
}

// Int* plusInt(Int* a, Int* b)
// {
//     Int* newInt = (Int*)(malloc(sizeof(Int)));

//     newInt->tag = 0;
//     newInt->val = a->val + b->val;

//     return newInt;
// }
