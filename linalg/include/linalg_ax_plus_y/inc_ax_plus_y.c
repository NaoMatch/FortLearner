#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

void ax_plus_y_01_C(double a, double x[], double y[], int64_t n){
    int64_t unroll=1, n_unroll=n/unroll, n_remain=n%unroll;
    double r00, r01;

    while(n_unroll--){
        r00 = *(y);
        r01 = *(x);

        r00 += r01 * a;

        *(y) = r00;

        y++;
        x++;
    }
}

void ax_plus_y_02_C(double a, double x[], double y[], int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;
    double r00, r01;
    double r02, r03;

    while(n_unroll--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        r02 = *(y+1);
        r03 = *(x+1);
        r02 += r03 * a;

        *(y)   = r00;
        *(y+1) = r02;

        y+=unroll;
        x+=unroll;
    }

    while(n_remain--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        *(y)   = r00;

        y++;
        x++;
    }
}

void ax_plus_y_04_C(double a, double x[], double y[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;
    double r00, r01;
    double r02, r03;
    double r04, r05;
    double r06, r07;

    while(n_unroll--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        r02 = *(y+1);
        r03 = *(x+1);
        r02 += r03 * a;

        r04 = *(y+2);
        r05 = *(x+2);
        r04 += r05 * a;

        r06 = *(y+3);
        r07 = *(x+3);
        r06 += r07 * a;

        *(y)   = r00;
        *(y+1) = r02;
        *(y+2) = r04;
        *(y+3) = r06;

        y+=unroll;
        x+=unroll;
    }

    while(n_remain--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        *(y)   = r00;

        y++;
        x++;
    }
}

void ax_plus_y_08_C(double a, double x[], double y[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;
    double r00, r01;
    double r02, r03;
    double r04, r05;
    double r06, r07;
    double r08, r09;
    double r10, r11;
    double r12, r13;
    double r14, r15;

    while(n_unroll--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        r02 = *(y+1);
        r03 = *(x+1);
        r02 += r03 * a;

        r04 = *(y+2);
        r05 = *(x+2);
        r04 += r05 * a;

        r06 = *(y+3);
        r07 = *(x+3);
        r06 += r07 * a;

        r08 = *(y+4);
        r09 = *(x+4);
        r08 += r09 * a;

        r10 = *(y+5);
        r11 = *(x+5);
        r10 += r11 * a;

        r12 = *(y+6);
        r13 = *(x+6);
        r12 += r13 * a;

        r14 = *(y+7);
        r15 = *(x+7);
        r14 += r15 * a;

        *(y)   = r00;
        *(y+1) = r02;
        *(y+2) = r04;
        *(y+3) = r06;
        *(y+4) = r08;
        *(y+5) = r10;
        *(y+6) = r12;
        *(y+7) = r14;

        y+=unroll;
        x+=unroll;
    }

    while(n_remain--){
        r00 = *(y);
        r01 = *(x);
        r00 += r01 * a;

        *(y)   = r00;

        y++;
        x++;
    }
}



void ax_plus_y_02x_A(double a, double *x, double *y, int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%ymm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[x]), %%xmm0         \n\t"
            "movupd 0*8(%[y]), %%xmm1         \n\t"
            "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
            "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
            "movupd %%xmm1,    0*8(%[y])      \n\t"
            "subq $-2*8, %[x]                 \n\t"
            "subq $-2*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        __asm__ __volatile__ (
            "movsd  0*8(%[x]), %%xmm0         \n\t"
            "movsd  0*8(%[y]), %%xmm1         \n\t"
            "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
            "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
            "movsd  %%xmm1,    0*8(%[y])      \n\t"
            "subq $-1*8, %[x]                 \n\t"
            "subq $-1*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }
}

void ax_plus_y_04x_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%ymm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[x]), %%xmm0         \n\t"
            "movupd 0*8(%[y]), %%xmm1         \n\t"
            "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
            "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
            "movupd %%xmm1,    0*8(%[y])      \n\t"
            "\n\t"
            "movupd 2*8(%[x]), %%xmm2         \n\t"
            "movupd 2*8(%[y]), %%xmm3         \n\t"
            "vmulpd %%xmm15,   %%xmm2, %%xmm2 \n\t"
            "vaddpd %%xmm2,    %%xmm3, %%xmm3 \n\t"
            "movupd %%xmm3,    2*8(%[y])      \n\t"
            "subq $-4*8, %[x]                 \n\t"
            "subq $-4*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0         \n\t"
                "movupd 0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movupd %%xmm1,    0*8(%[y])      \n\t"
                "subq $-2*8, %[x]                 \n\t"
                "subq $-2*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0         \n\t"
                "movsd  0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movsd  %%xmm1,    0*8(%[y])      \n\t"
                "subq $-1*8, %[x]                 \n\t"
                "subq $-1*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }

}

void ax_plus_y_04y_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%ymm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%ymm0         \n\t"
            "vmovupd 0*8(%[y]), %%ymm1         \n\t"
            "vmulpd %%ymm15,    %%ymm0, %%ymm0 \n\t"
            "vaddpd %%ymm0,     %%ymm1, %%ymm1 \n\t"
            "vmovupd %%ymm1,    0*8(%[y])      \n\t"
            "subq $-4*8, %[x]                  \n\t"
            "subq $-4*8, %[y]                  \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0         \n\t"
                "movupd 0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movupd %%xmm1,    0*8(%[y])      \n\t"
                "subq $-2*8, %[x]                 \n\t"
                "subq $-2*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0         \n\t"
                "movsd  0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movsd  %%xmm1,    0*8(%[y])      \n\t"
                "subq $-1*8, %[x]                 \n\t"
                "subq $-1*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }
}

void ax_plus_y_08x_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%ymm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[x]), %%xmm0         \n\t"
            "movupd 0*8(%[y]), %%xmm1         \n\t"
            "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
            "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
            "movupd %%xmm1,    0*8(%[y])      \n\t"
            "\n\t"
            "movupd 2*8(%[x]), %%xmm2         \n\t"
            "movupd 2*8(%[y]), %%xmm3         \n\t"
            "vmulpd %%xmm15,   %%xmm2, %%xmm2 \n\t"
            "vaddpd %%xmm2,    %%xmm3, %%xmm3 \n\t"
            "movupd %%xmm3,    2*8(%[y])      \n\t"
            "\n\t"
            "movupd 4*8(%[x]), %%xmm4         \n\t"
            "movupd 4*8(%[y]), %%xmm5         \n\t"
            "vmulpd %%xmm15,   %%xmm4, %%xmm4 \n\t"
            "vaddpd %%xmm4,    %%xmm5, %%xmm5 \n\t"
            "movupd %%xmm5,    4*8(%[y])      \n\t"
            "\n\t"
            "movupd 6*8(%[x]), %%xmm6         \n\t"
            "movupd 6*8(%[y]), %%xmm7         \n\t"
            "vmulpd %%xmm15,   %%xmm6, %%xmm6 \n\t"
            "vaddpd %%xmm6,    %%xmm7, %%xmm7 \n\t"
            "movupd %%xmm7,    6*8(%[y])      \n\t"
            "\n\t"
            "subq $-8*8, %[x]                 \n\t"
            "subq $-8*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0         \n\t"
                "vmovupd 0*8(%[y]), %%ymm1         \n\t"
                "vmulpd  %%ymm15,   %%ymm0, %%ymm0 \n\t"
                "vaddpd  %%ymm0,    %%ymm1, %%ymm1 \n\t"
                "vmovupd %%ymm1,    0*8(%[y])      \n\t"
                "subq $-4*8, %[x]                 \n\t"
                "subq $-4*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0         \n\t"
                "movupd 0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movupd %%xmm1,    0*8(%[y])      \n\t"
                "subq $-2*8, %[x]                 \n\t"
                "subq $-2*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0         \n\t"
                "movsd  0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movsd  %%xmm1,    0*8(%[y])      \n\t"
                "subq $-1*8, %[x]                 \n\t"
                "subq $-1*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }
}

void ax_plus_y_08y_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%ymm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%ymm0         \n\t"
            "vmovupd 0*8(%[y]), %%ymm1         \n\t"
            "vmulpd  %%ymm15,   %%ymm0, %%ymm0 \n\t"
            "vaddpd  %%ymm0,    %%ymm1, %%ymm1 \n\t"
            "vmovupd %%ymm1,    0*8(%[y])      \n\t"
            "\n\t"
            "vmovupd 4*8(%[x]), %%ymm2         \n\t"
            "vmovupd 4*8(%[y]), %%ymm3         \n\t"
            "vmulpd  %%ymm15,   %%ymm2, %%ymm2 \n\t"
            "vaddpd  %%ymm2,    %%ymm3, %%ymm3 \n\t"
            "vmovupd %%ymm3,    4*8(%[y])      \n\t"
            "\n\t"
            "subq $-8*8, %[x]                 \n\t"
            "subq $-8*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0         \n\t"
                "vmovupd 0*8(%[y]), %%ymm1         \n\t"
                "vmulpd  %%ymm15,   %%ymm0, %%ymm0 \n\t"
                "vaddpd  %%ymm0,    %%ymm1, %%ymm1 \n\t"
                "vmovupd %%ymm1,    0*8(%[y])      \n\t"
                "subq $-4*8, %[x]                 \n\t"
                "subq $-4*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0         \n\t"
                "movupd 0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movupd %%xmm1,    0*8(%[y])      \n\t"
                "subq $-2*8, %[x]                 \n\t"
                "subq $-2*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0         \n\t"
                "movsd  0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movsd  %%xmm1,    0*8(%[y])      \n\t"
                "subq $-1*8, %[x]                 \n\t"
                "subq $-1*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }
}

void ax_plus_y_08z_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a], %%zmm15 \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%zmm0         \n\t"
            "vmovupd 0*8(%[y]), %%zmm1         \n\t"
            "vmulpd %%zmm15,   %%zmm0, %%zmm0 \n\t"
            "vaddpd %%zmm0,    %%zmm1, %%zmm1 \n\t"
            "vmovupd %%zmm1,    0*8(%[y])      \n\t"
            "subq $-8*8, %[x]                 \n\t"
            "subq $-8*8, %[y]                 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0         \n\t"
                "vmovupd 0*8(%[y]), %%ymm1         \n\t"
                "vmulpd  %%ymm15,   %%ymm0, %%ymm0 \n\t"
                "vaddpd  %%ymm0,    %%ymm1, %%ymm1 \n\t"
                "vmovupd %%ymm1,    0*8(%[y])      \n\t"
                "subq $-4*8, %[x]                 \n\t"
                "subq $-4*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0         \n\t"
                "movupd 0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movupd %%xmm1,    0*8(%[y])      \n\t"
                "subq $-2*8, %[x]                 \n\t"
                "subq $-2*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0         \n\t"
                "movsd  0*8(%[y]), %%xmm1         \n\t"
                "vmulpd %%xmm15,   %%xmm0, %%xmm0 \n\t"
                "vaddpd %%xmm0,    %%xmm1, %%xmm1 \n\t"
                "movsd  %%xmm1,    0*8(%[y])      \n\t"
                "subq $-1*8, %[x]                 \n\t"
                "subq $-1*8, %[y]                 \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }
}

void ax_plus_y_16y_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a],  %%zmm15         \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd  0*8(%[x]),  %%ymm0         \n\t"
            "vmovupd  0*8(%[y]),  %%ymm1         \n\t"
            "vmovupd  4*8(%[x]),  %%ymm2         \n\t"
            "vmovupd  4*8(%[y]),  %%ymm3         \n\t"
            "vmovupd  8*8(%[x]),  %%ymm4         \n\t"
            "vmovupd  8*8(%[y]),  %%ymm5         \n\t"
            "vmovupd 12*8(%[x]),  %%ymm6         \n\t"
            "vmovupd 12*8(%[y]),  %%ymm7         \n\t"
            "\n\t"
            "VFMADD231PD %%ymm0, %%ymm15, %%ymm1 \n\t"
            "VFMADD231PD %%ymm2, %%ymm15, %%ymm3 \n\t"
            "VFMADD231PD %%ymm4, %%ymm15, %%ymm5 \n\t"
            "VFMADD231PD %%ymm6, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vmovupd  %%ymm1,  0*8(%[y])         \n\t"
            "vmovupd  %%ymm3,  4*8(%[y])         \n\t"
            "vmovupd  %%ymm5,  8*8(%[y])         \n\t"
            "vmovupd  %%ymm7, 12*8(%[y])         \n\t"
            "\n\t"
            "subq $-16*8, %[x]                   \n\t"
            "subq $-16*8, %[y]                   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if (n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-8*8, %[x]                   \n\t"
                "subq $-8*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%ymm0         \n\t"
                "vmovupd  0*8(%[y]),  %%ymm1         \n\t"
                "\n\t"
                "VFMADD231PD %%ymm0, %%ymm15, %%ymm1 \n\t"
                "VFMADD231PD %%ymm2, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vmovupd  %%ymm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-4*8, %[x]                   \n\t"
                "subq $-4*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd   0*8(%[x]),  %%xmm0         \n\t"
                "movupd   0*8(%[y]),  %%xmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movupd   %%xmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-2*8, %[x]                   \n\t"
                "subq $-2*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]),  %%xmm0           \n\t"
                "movsd  0*8(%[y]),  %%xmm1           \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movsd  %%xmm1,  0*8(%[y])           \n\t"
                "\n\t"
                "subq $-1*8, %[x]                    \n\t"
                "subq $-1*8, %[y]                    \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }

}

void ax_plus_y_16z_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a],  %%zmm15         \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
            "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
            "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
            "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
            "\n\t"
            "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
            "VFMADD231PD %%zmm2, %%zmm15, %%zmm3 \n\t"
            "\n\t"
            "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
            "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
            "\n\t"
            "subq $-16*8, %[x]                   \n\t"
            "subq $-16*8, %[y]                   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if (n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-8*8, %[x]                   \n\t"
                "subq $-8*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%ymm0         \n\t"
                "vmovupd  0*8(%[y]),  %%ymm1         \n\t"
                "\n\t"
                "VFMADD231PD %%ymm0, %%ymm15, %%ymm1 \n\t"
                "VFMADD231PD %%ymm2, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vmovupd  %%ymm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-4*8, %[x]                   \n\t"
                "subq $-4*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd   0*8(%[x]),  %%xmm0         \n\t"
                "movupd   0*8(%[y]),  %%xmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movupd   %%xmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-2*8, %[x]                   \n\t"
                "subq $-2*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]),  %%xmm0           \n\t"
                "movsd  0*8(%[y]),  %%xmm1           \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movsd  %%xmm1,  0*8(%[y])           \n\t"
                "\n\t"
                "subq $-1*8, %[x]                    \n\t"
                "subq $-1*8, %[y]                    \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }

}

void ax_plus_y_32z_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a],  %%zmm15         \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
            "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
            "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
            "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
            "vmovupd 16*8(%[x]),  %%zmm4         \n\t"
            "vmovupd 16*8(%[y]),  %%zmm5         \n\t"
            "vmovupd 24*8(%[x]),  %%zmm6         \n\t"
            "vmovupd 24*8(%[y]),  %%zmm7         \n\t"
            "\n\t"
            "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
            "VFMADD231PD %%zmm2, %%zmm15, %%zmm3 \n\t"
            "VFMADD231PD %%zmm4, %%zmm15, %%zmm5 \n\t"
            "VFMADD231PD %%zmm6, %%zmm15, %%zmm7 \n\t"
            "\n\t"
            "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
            "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
            "vmovupd  %%zmm5, 16*8(%[y])         \n\t"
            "vmovupd  %%zmm7, 24*8(%[y])         \n\t"
            "\n\t"
            "subq $-32*8, %[x]                   \n\t"
            "subq $-32*8, %[y]                   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if (n_remain>0){
        if(n_remain&16){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
                "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "VFMADD231PD %%zmm2, %%zmm15, %%zmm3 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
                "\n\t"
                "subq $-16*8, %[x]                   \n\t"
                "subq $-16*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-8*8, %[x]                   \n\t"
                "subq $-8*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%ymm0         \n\t"
                "vmovupd  0*8(%[y]),  %%ymm1         \n\t"
                "\n\t"
                "VFMADD231PD %%ymm0, %%ymm15, %%ymm1 \n\t"
                "VFMADD231PD %%ymm2, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vmovupd  %%ymm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-4*8, %[x]                   \n\t"
                "subq $-4*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd   0*8(%[x]),  %%xmm0         \n\t"
                "movupd   0*8(%[y]),  %%xmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movupd   %%xmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-2*8, %[x]                   \n\t"
                "subq $-2*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]),  %%xmm0           \n\t"
                "movsd  0*8(%[y]),  %%xmm1           \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movsd  %%xmm1,  0*8(%[y])           \n\t"
                "\n\t"
                "subq $-1*8, %[x]                    \n\t"
                "subq $-1*8, %[y]                    \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }

}

void ax_plus_y_56z_A(double a, double x[], double y[], int64_t n){
    int64_t unroll=56, n_unroll=n/unroll, n_remain=n%unroll;

    __asm__ __volatile__ (
        "VBROADCASTSD %[a],  %%zmm15         \n\t"
        :[a]"=X"(a)
        :"0"(a)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
            "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
            "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
            "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
            "vmovupd 16*8(%[x]),  %%zmm4         \n\t"
            "vmovupd 16*8(%[y]),  %%zmm5         \n\t"
            "vmovupd 24*8(%[x]),  %%zmm6         \n\t"
            "vmovupd 24*8(%[y]),  %%zmm7         \n\t"
            "vmovupd 32*8(%[x]),  %%zmm8         \n\t"
            "vmovupd 32*8(%[y]),  %%zmm9         \n\t"
            "vmovupd 40*8(%[x]),  %%zmm10        \n\t"
            "vmovupd 40*8(%[y]),  %%zmm11        \n\t"
            "vmovupd 48*8(%[x]),  %%zmm12        \n\t"
            "vmovupd 48*8(%[y]),  %%zmm13        \n\t"
            "\n\t"
            "VFMADD231PD %%zmm0,  %%zmm15, %%zmm1 \n\t"
            "VFMADD231PD %%zmm2,  %%zmm15, %%zmm3 \n\t"
            "VFMADD231PD %%zmm4,  %%zmm15, %%zmm5 \n\t"
            "VFMADD231PD %%zmm6,  %%zmm15, %%zmm7 \n\t"
            "VFMADD231PD %%zmm8,  %%zmm15, %%zmm9 \n\t"
            "VFMADD231PD %%zmm10, %%zmm15, %%zmm11 \n\t"
            "VFMADD231PD %%zmm12, %%zmm15, %%zmm13 \n\t"
            "\n\t"
            "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
            "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
            "vmovupd  %%zmm5, 16*8(%[y])         \n\t"
            "vmovupd  %%zmm7, 24*8(%[y])         \n\t"
            "vmovupd  %%zmm9, 32*8(%[y])         \n\t"
            "vmovupd  %%zmm11, 40*8(%[y])         \n\t"
            "vmovupd  %%zmm13, 48*8(%[y])         \n\t"
            "\n\t"
            "subq $-56*8, %[x]                   \n\t"
            "subq $-56*8, %[y]                   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :    "0"(x),     "1"(y)
        );
    }

    if (n_remain>0){
        if(n_remain&32){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
                "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
                "vmovupd 16*8(%[x]),  %%zmm4         \n\t"
                "vmovupd 16*8(%[y]),  %%zmm5         \n\t"
                "vmovupd 24*8(%[x]),  %%zmm6         \n\t"
                "vmovupd 24*8(%[y]),  %%zmm7         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0,  %%zmm15, %%zmm1 \n\t"
                "VFMADD231PD %%zmm2,  %%zmm15, %%zmm3 \n\t"
                "VFMADD231PD %%zmm4,  %%zmm15, %%zmm5 \n\t"
                "VFMADD231PD %%zmm6,  %%zmm15, %%zmm7 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
                "vmovupd  %%zmm5, 16*8(%[y])         \n\t"
                "vmovupd  %%zmm7, 24*8(%[y])         \n\t"
                "\n\t"
                "subq $-32*8, %[x]                   \n\t"
                "subq $-32*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&16){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "vmovupd  8*8(%[x]),  %%zmm2         \n\t"
                "vmovupd  8*8(%[y]),  %%zmm3         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "VFMADD231PD %%zmm2, %%zmm15, %%zmm3 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "vmovupd  %%zmm3,  8*8(%[y])         \n\t"
                "\n\t"
                "subq $-16*8, %[x]                   \n\t"
                "subq $-16*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%zmm0         \n\t"
                "vmovupd  0*8(%[y]),  %%zmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%zmm0, %%zmm15, %%zmm1 \n\t"
                "\n\t"
                "vmovupd  %%zmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-8*8, %[x]                   \n\t"
                "subq $-8*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[x]),  %%ymm0         \n\t"
                "vmovupd  0*8(%[y]),  %%ymm1         \n\t"
                "\n\t"
                "VFMADD231PD %%ymm0, %%ymm15, %%ymm1 \n\t"
                "VFMADD231PD %%ymm2, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vmovupd  %%ymm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-4*8, %[x]                   \n\t"
                "subq $-4*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd   0*8(%[x]),  %%xmm0         \n\t"
                "movupd   0*8(%[y]),  %%xmm1         \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movupd   %%xmm1,  0*8(%[y])         \n\t"
                "\n\t"
                "subq $-2*8, %[x]                   \n\t"
                "subq $-2*8, %[y]                   \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]),  %%xmm0           \n\t"
                "movsd  0*8(%[y]),  %%xmm1           \n\t"
                "\n\t"
                "VFMADD231PD %%xmm0, %%xmm15, %%xmm1 \n\t"
                "\n\t"
                "movsd  %%xmm1,  0*8(%[y])           \n\t"
                "\n\t"
                "subq $-1*8, %[x]                    \n\t"
                "subq $-1*8, %[y]                    \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :    "0"(x),     "1"(y)
            );
        }
    }

}