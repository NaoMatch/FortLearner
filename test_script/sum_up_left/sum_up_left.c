#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

int64_t sum_up_left_naive_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        if(x[i] <= v){
            res += y[i];
        }
    }
    return(res);
}

int64_t sum_up_left_naive_branchless_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0e0;
    int64_t i, factor;
    for (i=0; i<n; i++){
        factor = (x[i] <= v);
        res += y[i] * factor;
    }
    return(res);
}

int64_t sum_up_left_assembler_04_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 2;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm6, %%ymm6, %%ymm6    \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm3       \n\t" // broadcast
        "vmovupd 0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd 0*8(%[y]), %%ymm1       \n\t" // load y
        "VCMPLEPD %%ymm3, %%ymm0, %%ymm4 \n\t" // x <= v
        "subq $-4*8, %[x]                \n\t"
        "subq $-4*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm1, %%ymm4, %%ymm5   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "vaddpd %%ymm5, %%ymm6, %%ymm6   \n\t"
            "\n\t"
            "VCMPLEPD %%ymm3, %%ymm0, %%ymm4 \n\t"
            "subq $-4*8, %[x]                \n\t"
            "subq $-4*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm1, %%ymm4, %%ymm5   \n\t"
        "vaddpd %%ymm5, %%ymm6, %%ymm6   \n\t"
		"vperm2f128 $0x01, %%ymm6, %%ymm6, %%ymm7\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm7, %%ymm6, %%ymm6\n\t"
		"vhaddpd           %%ymm6, %%ymm6, %%ymm6\n\t"
		"movsd             %%xmm6, %[v] \n\t"
		:[v]"=m"(res)
        :
    );

    int64_t n_rem;
    n_rem=(n%pow2);
    if (n_rem>0){
        __asm__ __volatile__ (
            "pxor %%xmm3, %%xmm3        \n\t"
            "VPBROADCASTQ %[v], %%ymm2  \n\t"
            "mov %[n], %%rcx            \n\t"
            "loop_rem_sum_04_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_04_i8    \n\t"
            "   movq %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

int64_t sum_up_left_assembler_08_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 3;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm7, %%ymm7, %%ymm7    \n\t" // zero clear
        "vpxor %%ymm8, %%ymm8, %%ymm8    \n\t" // zero clear
        "\n\t"
        "vmovupd 0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd 0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd 4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd 4*8(%[y]), %%ymm3       \n\t" // load y
        "VPBROADCASTQ %[v], %%ymm4       \n\t" // broadcast
        "VCMPLEPD %%ymm4, %%ymm0, %%ymm5 \n\t" // x <= v
        "VCMPLEPD %%ymm4, %%ymm2, %%ymm6 \n\t" // x <= v
        "subq $-8*8, %[x]                \n\t"
        "subq $-8*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm1, %%ymm5, %%ymm9   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "vaddpd %%ymm9, %%ymm7, %%ymm7   \n\t"
            "\n\t"
            "vANDPD %%ymm3, %%ymm6, %%ymm10   \n\t"
            "vmovupd 4*8(%[x]), %%ymm2       \n\t"
            "vmovupd 4*8(%[y]), %%ymm3       \n\t"
            "vaddpd %%ymm10, %%ymm8, %%ymm8   \n\t"
            "\n\t"
            "VCMPLEPD %%ymm4, %%ymm0, %%ymm5 \n\t"
            "VCMPLEPD %%ymm4, %%ymm2, %%ymm6 \n\t"
            "subq $-8*8, %[x]                \n\t"
            "subq $-8*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm1, %%ymm5, %%ymm9    \n\t"
        "vANDPD %%ymm3, %%ymm6, %%ymm10   \n\t"
        "vaddpd %%ymm9, %%ymm7, %%ymm7    \n\t"
        "vaddpd %%ymm10, %%ymm8, %%ymm8   \n\t"
        "\n\t"
        "vaddpd %%ymm7, %%ymm8, %%ymm11   \n\t"
		"vperm2f128 $0x01, %%ymm11, %%ymm11, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm11, %%ymm11\n\t"
		"vhaddpd           %%ymm11, %%ymm11, %%ymm11\n\t"
		"movsd             %%xmm11, %[v] \n\t"
		:[v]"=m"(res)
        :
    );

    int64_t n_rem;
    n_rem=(n%pow2);
    if (n_rem>0){
        __asm__ __volatile__ (
            "pxor %%xmm3, %%xmm3        \n\t"
            "VPBROADCASTQ %[v], %%ymm2  \n\t"
            "mov %[n], %%rcx            \n\t"
            "loop_rem_sum_08_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_08_i8    \n\t"
            "   movq %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

double sum_up_left_naive_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        if(x[i] <= v){
            res += y[i];
        }
    }
    return(res);
}

double sum_up_left_naive_branchless_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0;
    int64_t i, factor;
    for (i=0; i<n; i++){
        factor = (x[i] <= v);
        res += y[i] * factor;
    }
    return(res);
}

double sum_up_left_assembler_04_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 2;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm6, %%ymm6, %%ymm6    \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm3       \n\t" // broadcast
        "vmovupd 0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd 0*8(%[y]), %%ymm1       \n\t" // load y
        "VCMPLEPD %%ymm3, %%ymm0, %%ymm4 \n\t" // x <= v
        "subq $-4*8, %[x]                \n\t"
        "subq $-4*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm1, %%ymm4, %%ymm5   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "vaddpd %%ymm5, %%ymm6, %%ymm6   \n\t"
            "\n\t"
            "VCMPLEPD %%ymm3, %%ymm0, %%ymm4 \n\t"
            "subq $-4*8, %[x]                \n\t"
            "subq $-4*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm1, %%ymm4, %%ymm5   \n\t"
        "vaddpd %%ymm5, %%ymm6, %%ymm6   \n\t"
		"vperm2f128 $0x01, %%ymm6, %%ymm6, %%ymm7\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm7, %%ymm6, %%ymm6\n\t"
		"vhaddpd           %%ymm6, %%ymm6, %%ymm6\n\t"
		"movsd             %%xmm6, %[v] \n\t"
		:[v]"=m"(res)
        :
    );

    int64_t n_rem;
    n_rem=(n%pow2);
    if (n_rem>0){
        __asm__ __volatile__ (
            "pxor %%xmm3, %%xmm3        \n\t"
            "VPBROADCASTQ %[v], %%ymm2  \n\t"
            "mov %[n], %%rcx            \n\t"
            "loop_rem_sum_04_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_04_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

double sum_up_left_assembler_08_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 3;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm9, %%ymm9, %%ymm9    \n\t" // zero clear
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t" // zero clear
        "\n\t"
        "vmovupd 0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd 0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd 4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd 4*8(%[y]), %%ymm3       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm4       \n\t" // broadcast threshold value
        "VCMPLEPD %%ymm4, %%ymm0, %%ymm5 \n\t" // create mask x <= v
        "VCMPLEPD %%ymm4, %%ymm2, %%ymm6 \n\t" // create mask x <= v
        "subq $-8*8, %[x]                \n\t"
        "subq $-8*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm1, %%ymm5, %%ymm7   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "vaddpd %%ymm7, %%ymm9, %%ymm9   \n\t"
            "\n\t"
            "vANDPD %%ymm3, %%ymm6, %%ymm8    \n\t"
            "vmovupd 4*8(%[x]), %%ymm2        \n\t"
            "vmovupd 4*8(%[y]), %%ymm3        \n\t"
            "vaddpd %%ymm8, %%ymm10, %%ymm10  \n\t"
            "\n\t"
            "VCMPLEPD %%ymm4, %%ymm0, %%ymm5 \n\t"
            "VCMPLEPD %%ymm4, %%ymm2, %%ymm6 \n\t"
            "subq $-8*8, %[x]                \n\t"
            "subq $-8*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm1, %%ymm5, %%ymm7   \n\t"
        "vANDPD %%ymm3, %%ymm6, %%ymm8   \n\t"
        "vaddpd %%ymm7, %%ymm9, %%ymm9   \n\t"
        "vaddpd %%ymm8, %%ymm10, %%ymm10 \n\t"
        "\n\t"
        "vaddpd %%ymm9, %%ymm10, %%ymm11   \n\t"
		"vperm2f128 $0x01, %%ymm11, %%ymm11, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm11, %%ymm11\n\t"
		"vhaddpd           %%ymm11, %%ymm11, %%ymm11\n\t"
		"movsd             %%xmm11, %[v] \n\t"
		:[v]"=m"(res)
        :
    );

    int64_t n_rem;
    n_rem=(n%pow2);
    if (n_rem>0){
        __asm__ __volatile__ (
            "pxor %%xmm3, %%xmm3        \n\t"
            "VPBROADCASTQ %[v], %%ymm2  \n\t"
            "mov %[n], %%rcx            \n\t"
            "loop_rem_sum_08_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_08_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}
