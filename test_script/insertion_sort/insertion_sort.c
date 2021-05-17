#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void inseriont_sort_r8(double x[], int64_t n){

    __asm__ __volatile__(
        "\n\t"
        "subq  $-1*8, %[x] \n\t"
        "\n\t"
        :[x]"=r"(x), [n]"=r"(n)
        :"0"(x), "1"(n)
    );

    __asm__ __volatile__(
        "\n\t"
        "head:"
        "mov  0*8(%[x]), %%r8 \n\t"
        "mov  1*8(%[x]), %%r9 \n\t"
        "cmpg %%r8, %%r9 \n\t"
        "\n\t"
        :[x]"=r"(x), [n]"=r"(n)
        :"0"(x), "1"(n)
    );
    return;
}