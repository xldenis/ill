#include <stdlib.h>

#include <string.h>
#include <stdio.h>

#include "rts.h"

String* showInt(int x)
{
    int num_chars = snprintf(NULL, 0, "%d", x) + 1;
    String* buffer = (String*)(malloc(num_chars));

    snprintf(buffer->data, num_chars, "%d", x);
    buffer->string_length = num_chars;
    return buffer; // caller is expected to invoke free() on this buffer to release memory
}

String* plusStr(String* a, String* b)
{
    int num_chars = a->string_length + b-> string_length;
    String* buffer = (String*)(malloc(num_chars));

    snprintf(buffer->data, num_chars, "%s%s", a->data, b->data);
    buffer->string_length = num_chars;
    return buffer;
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
