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

int64_t sum_up_left_unroll_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0;
    int64_t i, n_unroll, n_rem, factor;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06, r07;
    int64_t r08, r09, r10, r11;
    int64_t r12, r13, r14, r15;

    n_unroll = (n>>2);
    r12=0;
    r13=0;
    r14=0;
    r15=0;

    i=0;
    while(n_unroll--){
        r00 = x[i];
        r01 = x[i+1];
        r02 = x[i+2];
        r03 = x[i+3];

        r04 = y[i];
        r05 = y[i+1];
        r06 = y[i+2];
        r07 = y[i+3];

        r08 = (r00 <= v);
        r09 = (r01 <= v);
        r10 = (r02 <= v);
        r11 = (r03 <= v);

        r12 += r04 * r08;
        r13 += r05 * r09;
        r14 += r06 * r10;
        r15 += r07 * r11;

        i+=4;
    }

    n_rem=(n%4);
    while(n_rem--){
        factor = ( x[i]<= v );
        r15 += y[i] * factor;
        i+=1;
    }

    return(r12+r13+r14+r15);
}

int64_t sum_up_left_assembler_02_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    if (n<2){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }
    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 1;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm6, %%ymm6, %%ymm6    \n\t" // zero clear
        "\n\t"
        "vPBROADCASTQ %[v], %%ymm3       \n\t" // broadcast
        "movupd 0*8(%[x]), %%xmm0        \n\t" // load x
        "movupd 0*8(%[y]), %%xmm1        \n\t" // load y
        "CMPLEPD %%xmm3, %%xmm0          \n\t" // x <= v
        "subq $-2*8, %[x]                \n\t"
        "subq $-2*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "PAND  %%xmm0, %%xmm1   \n\t"
            "PADDQ %%xmm1, %%xmm6   \n\t"
            "movupd 0*8(%[x]), %%xmm0       \n\t"
            "movupd 0*8(%[y]), %%xmm1       \n\t"
            "\n\t"
            "CMPLEPD %%xmm3, %%xmm0 \n\t"
            "subq $-2*8, %[x]                \n\t"
            "subq $-2*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "PAND  %%xmm0, %%xmm1    \n\t"
        "PADDQ %%xmm1, %%xmm6   \n\t"
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
            "loop_rem_sum_02_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_02_i8    \n\t"
            "   movq %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

int64_t sum_up_left_assembler_04_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    if (n<4){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }
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
            "VPAND  %%ymm1, %%ymm4, %%ymm5   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "VPADDQ %%ymm5, %%ymm6, %%ymm6   \n\t"
            "\n\t"
            "VCMPLEPD %%ymm3, %%ymm0, %%ymm4 \n\t"
            "subq $-4*8, %[x]                \n\t"
            "subq $-4*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND  %%ymm1, %%ymm4, %%ymm5   \n\t"
        "VPADDQ %%ymm5, %%ymm6, %%ymm6   \n\t"
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
    if (n<8){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }

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
            "VPAND  %%ymm1, %%ymm5, %%ymm9   \n\t"
            "vmovupd 0*8(%[x]), %%ymm0       \n\t"
            "vmovupd 0*8(%[y]), %%ymm1       \n\t"
            "VPADDQ %%ymm9, %%ymm7, %%ymm7   \n\t"
            "\n\t"
            "VPAND  %%ymm3, %%ymm6, %%ymm10   \n\t"
            "vmovupd 4*8(%[x]), %%ymm2       \n\t"
            "vmovupd 4*8(%[y]), %%ymm3       \n\t"
            "VPADDQ %%ymm10, %%ymm8, %%ymm8   \n\t"
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
        "VPAND  %%ymm1, %%ymm5, %%ymm9    \n\t"
        "VPAND  %%ymm3, %%ymm6, %%ymm10   \n\t"
        "VPADDQ %%ymm9, %%ymm7, %%ymm7    \n\t"
        "VPADDQ %%ymm10, %%ymm8, %%ymm8   \n\t"
        "\n\t"
        "VPADDQ %%ymm7, %%ymm8, %%ymm11   \n\t"
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

int64_t sum_up_left_assembler_16_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    if (n<16){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 4;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm8        \n\t" // broadcast
        "VCMPLEPD %%ymm8, %%ymm0, %%ymm0  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm2, %%ymm2  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm4, %%ymm4  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm6, %%ymm6  \n\t" // x <= v
        "subq $-16*8, %[x]                \n\t"
        "subq $-16*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPAND  %%ymm0, %%ymm1, %%ymm9   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "VPADDQ %%ymm9, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "VPAND  %%ymm2, %%ymm3, %%ymm10   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "VPADDQ %%ymm10, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VPAND  %%ymm4, %%ymm5, %%ymm11   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "VPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VPAND  %%ymm6, %%ymm7, %%ymm12   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "VPADDQ %%ymm12, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm8, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm6, %%ymm6  \n\t" 
            "subq $-16*8, %[x]                \n\t"
            "subq $-16*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND  %%ymm0, %%ymm1, %%ymm9   \n\t"
        "VPADDQ %%ymm9, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPAND  %%ymm2, %%ymm3, %%ymm10   \n\t"
        "VPADDQ %%ymm10, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND  %%ymm4, %%ymm5, %%ymm11   \n\t"
        "VPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPAND  %%ymm6, %%ymm7, %%ymm12   \n\t"
        "VPADDQ %%ymm12, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPADDQ %%ymm13, %%ymm14, %%ymm14   \n\t"
        "VPADDQ %%ymm14, %%ymm15, %%ymm15   \n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_16_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_16_i8    \n\t"
            "   movq %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

int64_t sum_up_left_assembler_20_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    if (n<20){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    pow2=20;
    n_unroll = (n/pow2);

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "vmovupd 16*8(%[x]), %%ymm8       \n\t" // load x
        "vmovupd 16*8(%[y]), %%ymm9       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm10        \n\t" // broadcast
        "VCMPLEPD %%ymm10, %%ymm0, %%ymm0  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm2, %%ymm2  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm4, %%ymm4  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm6, %%ymm6  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm8, %%ymm8  \n\t" // x <= v
        "subq $-20*8, %[x]                \n\t"
        "subq $-20*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPAND  %%ymm0, %%ymm1, %%ymm11   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "VPADDQ %%ymm11, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "VPAND  %%ymm2, %%ymm3, %%ymm12   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "VPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VPAND  %%ymm4, %%ymm5, %%ymm11   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "VPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VPAND  %%ymm6, %%ymm7, %%ymm12   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "VPADDQ %%ymm12, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "VPAND  %%ymm8, %%ymm9, %%ymm11   \n\t"
            "vmovupd 16*8(%[x]), %%ymm8        \n\t"
            "vmovupd 16*8(%[y]), %%ymm9        \n\t"
            "VPADDQ %%ymm11, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm10, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm6, %%ymm6  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm8, %%ymm8  \n\t" 
            "subq $-20*8, %[x]                \n\t"
            "subq $-20*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND  %%ymm0, %%ymm1, %%ymm11   \n\t"
        "VPADDQ %%ymm11, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPAND  %%ymm2, %%ymm3, %%ymm12   \n\t"
        "VPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND  %%ymm4, %%ymm5, %%ymm11   \n\t"
        "VPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPAND  %%ymm6, %%ymm7, %%ymm12   \n\t"
        "VPADDQ %%ymm12, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPAND  %%ymm8, %%ymm9, %%ymm11   \n\t"
        "VPADDQ %%ymm11, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPADDQ %%ymm13, %%ymm14, %%ymm14   \n\t"
        "VPADDQ %%ymm14, %%ymm15, %%ymm15   \n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_20_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_20_i8    \n\t"
            "   movq %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

int64_t sum_up_left_assembler_24_c_i8_i8(int64_t x[], int64_t y[], int64_t n, int64_t v){
    int64_t res=0, res_rem=0;
    if (n<24){
        return(sum_up_left_naive_c_i8_i8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    pow2=24;
    n_unroll = (n/pow2);

    __asm__ __volatile__ (
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "vmovupd 16*8(%[x]), %%ymm8       \n\t" // load x
        "vmovupd 16*8(%[y]), %%ymm9       \n\t" // load y
        "vmovupd 20*8(%[x]), %%ymm10       \n\t" // load x
        "vmovupd 20*8(%[y]), %%ymm11       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm12        \n\t" // broadcast
        "VCMPLEPD %%ymm12, %%ymm0, %%ymm0  \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm2, %%ymm2  \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm4, %%ymm4  \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm6, %%ymm6  \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm8, %%ymm8  \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm10, %%ymm10  \n\t" // x <= v
        "subq $-24*8, %[x]                \n\t"
        "subq $-24*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPAND  %%ymm0, %%ymm1, %%ymm13   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VPAND  %%ymm2, %%ymm3, %%ymm13   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VPAND  %%ymm4, %%ymm5, %%ymm13   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VPAND  %%ymm6, %%ymm7, %%ymm13   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VPAND  %%ymm8, %%ymm9, %%ymm13   \n\t"
            "vmovupd 16*8(%[x]), %%ymm8        \n\t"
            "vmovupd 16*8(%[y]), %%ymm9        \n\t"
            "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VPAND  %%ymm10, %%ymm11, %%ymm13   \n\t"
            "vmovupd 20*8(%[x]), %%ymm10        \n\t"
            "vmovupd 20*8(%[y]), %%ymm11        \n\t"
            "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm12, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm6, %%ymm6  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm8, %%ymm8  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm10, %%ymm10  \n\t" 
            "subq $-24*8, %[x]                \n\t"
            "subq $-24*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND  %%ymm0, %%ymm1, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND  %%ymm2, %%ymm3, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPAND  %%ymm4, %%ymm5, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND  %%ymm6, %%ymm7, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPAND  %%ymm8, %%ymm9, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND  %%ymm10, %%ymm11, %%ymm13   \n\t"
        "VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPADDQ %%ymm14, %%ymm15, %%ymm15   \n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_24_i8:        \n\t"
            "   movq 0*8(%[x]), %%xmm0 \n\t"
            "   movq 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   pand %%xmm0, %%xmm1     \n\t"
            "   PADDQ %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_24_i8    \n\t"
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

double sum_up_left_unroll_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0;
    int64_t i, n_unroll, n_rem, factor;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    int64_t r08, r09, r10, r11;
    double r12, r13, r14, r15;

    n_unroll = (n>>2);
    r12=0;
    r13=0;
    r14=0;
    r15=0;
    
    i=0;
    while(n_unroll--){
        r00 = x[i];
        r01 = x[i+1];
        r02 = x[i+2];
        r03 = x[i+3];

        r04 = y[i];
        r05 = y[i+1];
        r06 = y[i+2];
        r07 = y[i+3];

        r08 = (r00 <= v);
        r09 = (r01 <= v);
        r10 = (r02 <= v);
        r11 = (r03 <= v);
        
        r12 += r04 * r08;
        r13 += r05 * r09;
        r14 += r06 * r10;
        r15 += r07 * r11;

        i+=4;
    }

    n_rem=(n%4);
    while(n_rem--){
        factor = ( x[i]<= v );
        r15 += y[i] * factor;
        i+=1;
    }
    
    return(r12+r13+r14+r15);
}

double sum_up_left_assembler_02_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    if (n<2){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 1;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm6, %%ymm6, %%ymm6    \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm3       \n\t" // broadcast
        "movupd 0*8(%[x]), %%xmm0         \n\t" // load x
        "movupd 0*8(%[y]), %%xmm1         \n\t" // load y
        "CMPLEPD %%xmm3, %%xmm0 \n\t" // x <= v
        "subq $-2*8, %[x]                \n\t"
        "subq $-2*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "ANDPD %%xmm1, %%xmm0   \n\t"
            "addpd %%xmm0, %%xmm6   \n\t"
            "movupd 0*8(%[x]), %%xmm0       \n\t"
            "movupd 0*8(%[y]), %%xmm1       \n\t"
            "\n\t"
            "CMPLEPD %%xmm3, %%xmm0 \n\t"
            "subq $-2*8, %[x]                \n\t"
            "subq $-2*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "ANDPD %%xmm1, %%xmm0   \n\t"
        "addpd %%xmm0, %%xmm6   \n\t"
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
            "loop_rem_sum_02_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_02_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

double sum_up_left_assembler_04_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    if (n<4){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

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
    if (n<8){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

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

double sum_up_left_assembler_16_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    if (n<16){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    n_pow = 4;
    pow2=pow(2,n_pow);
    n_unroll = (n>>n_pow);

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm8        \n\t" // broadcast
        "VCMPLEPD %%ymm8, %%ymm0, %%ymm0  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm2, %%ymm2  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm4, %%ymm4  \n\t" // x <= v
        "VCMPLEPD %%ymm8, %%ymm6, %%ymm6  \n\t" // x <= v
        "subq $-16*8, %[x]                \n\t"
        "subq $-16*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm0, %%ymm1, %%ymm9   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "vaddpd %%ymm9, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "vANDPD %%ymm2, %%ymm3, %%ymm10   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "vaddpd %%ymm10, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vANDPD %%ymm4, %%ymm5, %%ymm11   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm6, %%ymm7, %%ymm12   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "vaddpd %%ymm12, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm8, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm8, %%ymm6, %%ymm6  \n\t" 
            "subq $-16*8, %[x]                \n\t"
            "subq $-16*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm0, %%ymm1, %%ymm9   \n\t"
        "vaddpd %%ymm9, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "vANDPD %%ymm2, %%ymm3, %%ymm10   \n\t"
        "vaddpd %%ymm10, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vANDPD %%ymm4, %%ymm5, %%ymm11   \n\t"
        "vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm6, %%ymm7, %%ymm12   \n\t"
        "vaddpd %%ymm12, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "vaddpd %%ymm13, %%ymm14, %%ymm14   \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15   \n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_16_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_16_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

double sum_up_left_assembler_20_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    if (n<20){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    pow2=20;
    n_unroll = (n/pow2);

    __asm__ __volatile__ (
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "vmovupd 16*8(%[x]), %%ymm8       \n\t" // load x
        "vmovupd 16*8(%[y]), %%ymm9       \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm10        \n\t" // broadcast
        "VCMPLEPD %%ymm10, %%ymm0, %%ymm0  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm2, %%ymm2  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm4, %%ymm4  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm6, %%ymm6  \n\t" // x <= v
        "VCMPLEPD %%ymm10, %%ymm8, %%ymm8  \n\t" // x <= v
        "subq $-20*8, %[x]                \n\t"
        "subq $-20*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm0, %%ymm1, %%ymm11   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "vaddpd %%ymm11, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vANDPD %%ymm2, %%ymm3, %%ymm12   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "vaddpd %%ymm12, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm4, %%ymm5, %%ymm13   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "vaddpd %%ymm13, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vANDPD %%ymm6, %%ymm7, %%ymm11   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm8, %%ymm9, %%ymm12   \n\t"
            "vmovupd 16*8(%[x]), %%ymm8        \n\t"
            "vmovupd 16*8(%[y]), %%ymm9        \n\t"
            "vaddpd %%ymm12, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm10, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm6, %%ymm6  \n\t" 
            "VCMPLEPD %%ymm10, %%ymm8, %%ymm8  \n\t"
            "subq $-20*8, %[x]                \n\t"
            "subq $-20*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm0, %%ymm1, %%ymm11   \n\t"
        "vaddpd %%ymm11, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vANDPD %%ymm2, %%ymm3, %%ymm12   \n\t"
        "vaddpd %%ymm12, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm4, %%ymm5, %%ymm13   \n\t"
        "vaddpd %%ymm13, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vANDPD %%ymm6, %%ymm7, %%ymm11   \n\t"
        "vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm8, %%ymm9, %%ymm12   \n\t"
        "vaddpd %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15   \n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_20_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_20_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}

double sum_up_left_assembler_24_c_r8_r8(double x[], double y[], int64_t n, double v){
    double res=0e0, res_rem=0e0;
    if (n<24){
        return(sum_up_left_naive_c_r8_r8(x,y,n,v));
    }

    int64_t i, factor;
    int64_t n_unroll, n_pow, pow2;
    pow2=24;
    n_unroll = (n/pow2);

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0       \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1       \n\t" // load y
        "vmovupd  4*8(%[x]), %%ymm2       \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3       \n\t" // load y
        "vmovupd  8*8(%[x]), %%ymm4       \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5       \n\t" // load y
        "vmovupd 12*8(%[x]), %%ymm6       \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7       \n\t" // load y
        "vmovupd 16*8(%[x]), %%ymm8       \n\t" // load x
        "vmovupd 16*8(%[y]), %%ymm9       \n\t" // load y
        "vmovupd 20*8(%[x]), %%ymm10      \n\t" // load x
        "vmovupd 20*8(%[y]), %%ymm11      \n\t" // load y
        "\n\t"
        "VPBROADCASTQ %[v], %%ymm12          \n\t" // broadcast
        "VCMPLEPD %%ymm12, %%ymm0, %%ymm0    \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm2, %%ymm2    \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm4, %%ymm4    \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm6, %%ymm6    \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm8, %%ymm8    \n\t" // x <= v
        "VCMPLEPD %%ymm12, %%ymm10, %%ymm10  \n\t" // x <= v
        "subq $-24*8, %[x]                \n\t"
        "subq $-24*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD %%ymm0, %%ymm1, %%ymm13   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0        \n\t"
            "vmovupd  0*8(%[y]), %%ymm1        \n\t"
            "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm2, %%ymm3, %%ymm14   \n\t"
            "vmovupd  4*8(%[x]), %%ymm2        \n\t"
            "vmovupd  4*8(%[y]), %%ymm3        \n\t"
            "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm4, %%ymm5, %%ymm13   \n\t"
            "vmovupd  8*8(%[x]), %%ymm4        \n\t"
            "vmovupd  8*8(%[y]), %%ymm5        \n\t"
            "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm6, %%ymm7, %%ymm14   \n\t"
            "vmovupd 12*8(%[x]), %%ymm6        \n\t"
            "vmovupd 12*8(%[y]), %%ymm7        \n\t"
            "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm8, %%ymm9, %%ymm13   \n\t"
            "vmovupd 16*8(%[x]), %%ymm8        \n\t"
            "vmovupd 16*8(%[y]), %%ymm9        \n\t"
            "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vANDPD %%ymm10, %%ymm11, %%ymm14   \n\t"
            "vmovupd 20*8(%[x]), %%ymm10        \n\t"
            "vmovupd 20*8(%[y]), %%ymm11        \n\t"
            "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "VCMPLEPD %%ymm12, %%ymm0, %%ymm0  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm2, %%ymm2  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm4, %%ymm4  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm6, %%ymm6  \n\t" 
            "VCMPLEPD %%ymm12, %%ymm8, %%ymm8  \n\t"
            "VCMPLEPD %%ymm12, %%ymm10, %%ymm10  \n\t"
            "subq $-24*8, %[x]                \n\t"
            "subq $-24*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%ymm0, %%ymm1, %%ymm13   \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm2, %%ymm3, %%ymm14   \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm4, %%ymm5, %%ymm13   \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm6, %%ymm7, %%ymm14   \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm8, %%ymm9, %%ymm13   \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vANDPD %%ymm10, %%ymm11, %%ymm14   \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm12\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm12, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
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
            "loop_rem_sum_24_r8:        \n\t"
            "   movsd 0*8(%[x]), %%xmm0 \n\t"
            "   movsd 0*8(%[y]), %%xmm1 \n\t"
            "   CMPLEPD  %%xmm2, %%xmm0 \n\t"
            "   andpd %%xmm0, %%xmm1    \n\t"
            "   addpd %%xmm1, %%xmm3    \n\t"
            "   subq $-1*8, %[x]        \n\t"
            "   subq $-1*8, %[y]        \n\t"
            "loop loop_rem_sum_24_r8    \n\t"
            "   movsd %%xmm3, %[r]      \n\t"
            :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [n]"=r"(n_rem), [r]"=m"(res_rem)
            :"0"(x), "1"(y), "2"(v), "3"(n_rem)
        );
        res+=res_rem;
    }
    return(res);
}
