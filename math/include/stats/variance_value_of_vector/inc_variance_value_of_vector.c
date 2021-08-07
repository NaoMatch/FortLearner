#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double variance_C_r8(double *x, int64_t n){
}

double variance_C_i8(int64_t *x, int64_t n){
}

double variance_02_C_r8(double *x, int64_t n){
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double r00, r01, r02;
    double avg=0e0;
    double r15=0e0;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);

        r02 = r00 + r01;
        avg += r02;

        r00 *= r00;
        r01 *= r01;

        r00 += r01;

        r15 += r00;

        x+=unroll_size;
    }
    if (n_remain>0){
        if (n_remain&1){
            r00 = *(x);
            avg += r00;
            r00 *= r00;

            r15 += r00;

            x+=1;
        }
    }

    avg = avg / n;
    r15 -= avg*avg*n;
    return r15/n;    
}

double variance_02_C_i8(int64_t *x, int64_t n){
}

double variance_04_C_r8(double *x, int64_t n){
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double avg=0e0;
    double r00, r01, r02, r03;
    double r04, r05;
    double r15=0e0;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);

        r04 = r00 + r01;
        r05 = r02 + r03;

        r04 += r05;
        avg += r04;

        r00 *= r00;
        r01 *= r01;
        r02 *= r02;
        r03 *= r03;

        r00 += r01;
        r02 += r03;

        r00 += r02;

        r15 += r00;

        x+=unroll_size;
    }

    if (n_remain>0){
        if (n_remain&2){
            r00 = *(x);
            r01 = *(x+1);
            avg += r00 + r01;

            r00 *= r00;
            r01 *= r01;

            r00 += r01;

            r15 += r00;

            x+=2;
        }

        if (n_remain&1){
            r00 = *(x);
            avg += r00;

            r00 *= r00;

            r15 += r00;

            x+=1;
        }
    }

    avg = avg / n;
    r15 -= avg*avg*n;
    return r15/n;    
}

double variance_04_C_i8(int64_t *x, int64_t n){
}

double variance_08_C_r8(double *x, int64_t n){
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double avg=0e0;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r15=0e0;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);

        r08 = r00 + r01;
        r09 = r02 + r03;
        r10 = r04 + r05;
        r11 = r06 + r07;
        
        r08 += r09;
        r10 += r11;
        r08 += r10;
        avg += r08;

        r00 *= r00;
        r01 *= r01;
        r02 *= r02;
        r03 *= r03;
        r04 *= r04;
        r05 *= r05;
        r06 *= r06;
        r07 *= r07;

        r00 += r01;
        r02 += r03;
        r04 += r05;
        r06 += r07;

        r00 += r02;
        r04 += r06;

        r00 += r04;

        r15 += r00;

        x+=unroll_size;
    }

    if (n_remain>0){
        if (n_remain&4){
            r00 = *(x);
            r01 = *(x+1);
            r02 = *(x+2);
            r03 = *(x+3);

            r04 = r00 + r01;
            r05 = r02 + r03;

            r04 += r05;

            avg += r04;

            r00 *= r00;
            r01 *= r01;
            r02 *= r02;
            r03 *= r03;

            r00 += r01;
            r02 += r03;

            r00 += r02;

            r15 += r00;

            x+=4;
        }

        if (n_remain&2){
            r00 = *(x);
            r01 = *(x+1);
            avg += r00 + r01;

            r00 *= r00;
            r01 *= r01;

            r00 += r01;

            r15 += r00;

            x+=2;
        }

        if (n_remain&1){
            r00 = *(x);
            avg += r00;

            r00 *= r00;

            r15 += r00;

            x+=1;
        }
    }

    avg = avg / n;
    r15 -= avg*avg*n;
    return r15/n;    
}

double variance_08_C_i8(int64_t *x, int64_t n){
}

double variance_16_C_r8(double *x, int64_t n){
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double avg=0e0;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r15=0e0;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);

        r08 = r00 + r01;
        r09 = r02 + r03;
        r10 = r04 + r05;
        r11 = r06 + r07;
        
        r08 += r09;
        r10 += r11;
        r08 += r10;
        avg += r08;

        r00 *= r00;
        r01 *= r01;
        r02 *= r02;
        r03 *= r03;
        r04 *= r04;
        r05 *= r05;
        r06 *= r06;
        r07 *= r07;

        r00 += r01;
        r02 += r03;
        r04 += r05;
        r06 += r07;

        r00 += r02;
        r04 += r06;

        r00 += r04;

        r15 += r00;

        r00 = *(x+8);
        r01 = *(x+9);
        r02 = *(x+10);
        r03 = *(x+11);
        r04 = *(x+12);
        r05 = *(x+13);
        r06 = *(x+14);
        r07 = *(x+15);

        r08 = r00 + r01;
        r09 = r02 + r03;
        r10 = r04 + r05;
        r11 = r06 + r07;
        
        r08 += r09;
        r10 += r11;
        r08 += r10;
        avg += r08;

        r00 *= r00;
        r01 *= r01;
        r02 *= r02;
        r03 *= r03;
        r04 *= r04;
        r05 *= r05;
        r06 *= r06;
        r07 *= r07;

        r00 += r01;
        r02 += r03;
        r04 += r05;
        r06 += r07;

        r00 += r02;
        r04 += r06;

        r00 += r04;

        r15 += r00;

        x+=unroll_size;
    }

    if (n_remain>0){
        if (n_remain&8){
            r00 = *(x);
            r01 = *(x+1);
            r02 = *(x+2);
            r03 = *(x+3);
            r04 = *(x+4);
            r05 = *(x+5);
            r06 = *(x+6);
            r07 = *(x+7);

            r08 = r00 + r01;
            r09 = r02 + r03;
            r10 = r04 + r05;
            r11 = r06 + r07;
            
            r08 += r09;
            r10 += r11;
            r08 += r10;
            avg += r08;

            r00 *= r00;
            r01 *= r01;
            r02 *= r02;
            r03 *= r03;
            r04 *= r04;
            r05 *= r05;
            r06 *= r06;
            r07 *= r07;

            r00 += r01;
            r02 += r03;
            r04 += r05;
            r06 += r07;

            r00 += r02;
            r04 += r06;

            r00 += r04;

            r15 += r00;

            x+=8;
        }

        if (n_remain&4){
            r00 = *(x);
            r01 = *(x+1);
            r02 = *(x+2);
            r03 = *(x+3);

            r04 = r00 + r01;
            r05 = r02 + r03;

            r04 += r05;

            avg += r04;

            r00 *= r00;
            r01 *= r01;
            r02 *= r02;
            r03 *= r03;

            r00 += r01;
            r02 += r03;

            r00 += r02;

            r15 += r00;

            x+=4;
        }

        if (n_remain&2){
            r00 = *(x);
            r01 = *(x+1);
            avg += r00 + r01;

            r00 *= r00;
            r01 *= r01;

            r00 += r01;

            r15 += r00;

            x+=2;
        }

        if (n_remain&1){
            r00 = *(x);
            avg += r00;

            r00 *= r00;

            r15 += r00;

            x+=1;
        }
    }

    avg = avg / n;
    r15 -= avg*avg*n;
    return r15/n;    
}

double variance_16_C_i8(int64_t *x, int64_t n){
}

double variance_32_C_r8(double *x, int64_t n){
}

double variance_32_C_i8(int64_t *x, int64_t n){
}

double variance_02_A_r8(double *x, int64_t n){
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double res, avg;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[x]), %%xmm0           \n\t"
            "vaddpd %%xmm0,    %%xmm14, %%xmm14 \n\t"
            "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
            "vaddpd %%xmm1,    %%xmm15, %%xmm15 \n\t"
            "subq $-2*8, %[x]                   \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){
        __asm__ __volatile__ (
            "vpxor  %%ymm0,    %%ymm0,  %%ymm0  \n\t"
            "movsd  0*8(%[x]), %%xmm0           \n\t"
            "vaddpd %%xmm0,    %%xmm14, %%xmm14 \n\t"
            "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
            "vaddpd %%xmm1,    %%xmm15, %%xmm15 \n\t"
            "subq $-1*8, %[x]                   \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[r]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm2  \n\t"
        "PSRLDQ $8,      %%xmm2  \n\t"
        "addpd  %%xmm2,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a]    \n\t"
        :[r]"=m"(res), [a]"=m"(avg)
        :
    );
    avg /= n;
    return res/n - avg*avg;
}

double variance_02_A_i8(int64_t *x, int64_t n){
}

double variance_04_A_r8(double *x, int64_t n){
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double res, avg;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%ymm0           \n\t"
            "vaddpd %%ymm0,     %%ymm14, %%ymm14 \n\t"
            "vmulpd %%ymm0,     %%ymm0,  %%ymm1  \n\t"
            "vaddpd %%ymm1,     %%ymm15, %%ymm15 \n\t"
            "subq $-4*8, %[x]                    \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){
        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm13, %%xmm13 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm13, %%xmm13 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        "vaddpd              %%ymm12, %%ymm14, %%ymm14   \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm1             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm1,  %%xmm0, %%xmm14    \n\t" // sum
        "\n\t"
        "vaddpd              %%ymm13, %%ymm15, %%ymm15   \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm1,  %%xmm0, %%xmm15    \n\t" // sum
        "\n\t"
        "movupd %%xmm14, %%xmm2  \n\t"
        "PSRLDQ $8,      %%xmm2  \n\t"
        "addpd  %%xmm2,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[r]    \n\t"
        :[r]"=m"(res), [a]"=m"(avg)
        :
    );
    avg /= n;
    return res/n - avg*avg;
}

double variance_04_A_i8(int64_t *x, int64_t n){
}

double variance_08_A_r8(double *x, int64_t n){
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double sqsum, avg;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%ymm0           \n\t"
            "vmovupd 4*8(%[x]), %%ymm1           \n\t"
            "vaddpd %%ymm0,     %%ymm12, %%ymm12 \n\t"
            "vaddpd %%ymm1,     %%ymm13, %%ymm13 \n\t"
            "vmulpd %%ymm0,     %%ymm0,  %%ymm2  \n\t"
            "vmulpd %%ymm1,     %%ymm1,  %%ymm3  \n\t"
            "vaddpd %%ymm2,     %%ymm14, %%ymm14 \n\t"
            "vaddpd %%ymm3,     %%ymm15, %%ymm15 \n\t"
            "subq $-8*8, %[x]                    \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm8           \n\t"
                "vaddpd %%xmm8,    %%xmm10, %%xmm10 \n\t"
                "vmulpd %%xmm8,    %%xmm8,  %%xmm9  \n\t"
                "vaddpd %%xmm9,    %%xmm11, %%xmm11 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm6           \n\t"
                "vaddpd %%xmm6,    %%xmm10, %%xmm10 \n\t"
                "vmulpd %%xmm6,    %%xmm6,  %%xmm7  \n\t"
                "vaddpd %%xmm7,    %%xmm11, %%xmm11 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm4           \n\t"
                "vaddpd %%ymm4,     %%ymm10, %%ymm10 \n\t"
                "vmulpd %%ymm4,     %%ymm4,  %%ymm5  \n\t"
                "vaddpd %%ymm5,     %%ymm11, %%ymm11 \n\t"
                "subq $-4*8, %[x]                    \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        "vaddpd              %%ymm10, %%ymm12, %%ymm12   \n\t"
        "vaddpd              %%ymm12, %%ymm13, %%ymm13   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm13, %%xmm6             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm13, %%xmm7             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%ymm11, %%ymm14, %%ymm14   \n\t"
        "vaddpd              %%ymm14, %%ymm15, %%ymm15   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm15, %%xmm8             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm15, %%xmm9             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%xmm7,  %%xmm6, %%xmm13    \n\t" // sqsum
        "vaddpd              %%xmm9,  %%xmm8, %%xmm15    \n\t" // square sqsum
        "\n\t"
        "movupd              %%xmm13, %%xmm4             \n\t" // copy  
        "movupd              %%xmm15, %%xmm5             \n\t" // copy
        "\n\t"
        "PSRLDQ        $8,   %%xmm4                      \n\t" // logical left shift
        "PSRLDQ        $8,   %%xmm5                      \n\t" // logical left shift
        "\n\t"
        "addpd               %%xmm4,  %%xmm13            \n\t"
        "addpd               %%xmm5,  %%xmm15            \n\t"
        "\n\t"
        "movsd               %%xmm13, %[a]               \n\t"
        "movsd               %%xmm15, %[s]               \n\t"
        :[a]"=m"(avg), [s]"=m"(sqsum)
        :
    );

    avg /= n;
    return sqsum/n - avg*avg;
}

double variance_08_A_i8(int64_t *x, int64_t n){
}

double variance_08z_A_r8(double *x, int64_t n){
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double res, avg;

    __asm__ __volatile__ (
        "VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
        "VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
        "VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
        "VXORPD %%zmm12, %%zmm12, %%zmm12 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%zmm0           \n\t"
            "vaddpd %%zmm0,     %%zmm14, %%zmm14 \n\t"
            "vmulpd %%zmm0,     %%zmm0,  %%zmm1  \n\t"
            "vaddpd %%zmm1,     %%zmm15, %%zmm15 \n\t"
            "subq $-8*8, %[x]                    \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){
        if (n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0           \n\t"
                "vaddpd  %%ymm0,    %%ymm12, %%ymm12 \n\t"
                "vmulpd  %%ymm0,    %%ymm0,  %%ymm1  \n\t"
                "vaddpd  %%ymm1,    %%ymm13, %%ymm13 \n\t"
                "subq $-4*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm13, %%xmm13 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm13, %%xmm13 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        "vaddpd              %%zmm12, %%zmm14, %%zmm14   \n\t"
        "VEXTRACTF64X4  $0,  %%zmm14, %%ymm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4  $1,  %%zmm14, %%ymm1             \n\t" // extract higher 128 bits
        "vaddpd              %%ymm1,  %%ymm0, %%ymm14    \n\t" // sum
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm1             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm1,  %%xmm0, %%xmm14    \n\t" // sum
        "\n\t"
        "vaddpd              %%zmm13, %%zmm15, %%zmm15   \n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm1             \n\t" // extract higher 128 bits
        "vaddpd              %%ymm1,  %%ymm0, %%ymm15    \n\t" // sum
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm1,  %%xmm0, %%xmm15    \n\t" // sum
        "\n\t"
        "movupd %%xmm14, %%xmm2  \n\t"
        "PSRLDQ $8,      %%xmm2  \n\t"
        "addpd  %%xmm2,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[r]    \n\t"
        :[r]"=m"(res), [a]"=m"(avg)
        :
    );
    avg /= n;
    return res/n - avg*avg;
}

double variance_08z_A_i8(int64_t *x, int64_t n){
}

double variance_16_A_r8(double *x, int64_t n){
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double sqsum, avg;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
        "vpxor %%ymm9,  %%ymm9,  %%ymm9  \n\t"
        "vpxor %%ymm8,  %%ymm8,  %%ymm8  \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd  0*8(%[x]), %%ymm0           \n\t"
            "vmovupd  4*8(%[x]), %%ymm1           \n\t"
            "vmovupd  8*8(%[x]), %%ymm2           \n\t"
            "vmovupd 12*8(%[x]), %%ymm3           \n\t"
            "\n\t"
            "vaddpd %%ymm0,      %%ymm8,  %%ymm8  \n\t"
            "vaddpd %%ymm1,      %%ymm9,  %%ymm9  \n\t"
            "vaddpd %%ymm2,      %%ymm10, %%ymm10 \n\t"
            "vaddpd %%ymm3,      %%ymm11, %%ymm11 \n\t"
            "\n\t"
            "vmulpd %%ymm0,      %%ymm0,  %%ymm4  \n\t"
            "vmulpd %%ymm1,      %%ymm1,  %%ymm5  \n\t"
            "vmulpd %%ymm2,      %%ymm2,  %%ymm6  \n\t"
            "vmulpd %%ymm3,      %%ymm3,  %%ymm7  \n\t"
            "\n\t"
            "vaddpd %%ymm4,      %%ymm12, %%ymm12 \n\t"
            "vaddpd %%ymm5,      %%ymm13, %%ymm13 \n\t"
            "vaddpd %%ymm6,      %%ymm14, %%ymm14 \n\t"
            "vaddpd %%ymm7,      %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-16*8, %[x]                    \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){
        if (n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0           \n\t"
                "vmovupd 4*8(%[x]), %%ymm1           \n\t"
                "vaddpd %%ymm0,     %%ymm8,  %%ymm8  \n\t"
                "vaddpd %%ymm1,     %%ymm9,  %%ymm9  \n\t"
                "vmulpd %%ymm0,     %%ymm0,  %%ymm4  \n\t"
                "vmulpd %%ymm1,     %%ymm1,  %%ymm5  \n\t"
                "vaddpd %%ymm4,     %%ymm12, %%ymm12 \n\t"
                "vaddpd %%ymm5,     %%ymm13, %%ymm13 \n\t"
                "subq $-8*8, %[x]                    \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0           \n\t"
                "vaddpd %%ymm0,     %%ymm8,  %%ymm8  \n\t"
                "vmulpd %%ymm0,     %%ymm0,  %%ymm4  \n\t"
                "vaddpd %%ymm4,     %%ymm12, %%ymm12 \n\t"
                "subq $-4*8, %[x]                    \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm8,  %%xmm8  \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm4  \n\t"
                "vaddpd %%xmm4,    %%xmm12, %%xmm12 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm8,  %%xmm8  \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm4  \n\t"
                "vaddpd %%xmm4,    %%xmm12, %%xmm12 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        "vaddpd              %%ymm8,  %%ymm9,  %%ymm9    \n\t"
        "vaddpd              %%ymm9,  %%ymm10, %%ymm10   \n\t"
        "vaddpd              %%ymm10, %%ymm11, %%ymm11   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm11, %%xmm4             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm11, %%xmm5             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%ymm12, %%ymm13, %%ymm13   \n\t"
        "vaddpd              %%ymm13, %%ymm14, %%ymm14   \n\t"
        "vaddpd              %%ymm14, %%ymm15, %%ymm15   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm15, %%xmm6             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm15, %%xmm7             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%xmm5,  %%xmm4, %%xmm11    \n\t" // sqsum
        "vaddpd              %%xmm7,  %%xmm6, %%xmm15    \n\t" // square sqsum
        "\n\t"
        "movupd              %%xmm11, %%xmm4             \n\t" // copy  
        "movupd              %%xmm15, %%xmm5             \n\t" // copy
        "\n\t"
        "PSRLDQ        $8,   %%xmm4                      \n\t" // logical left shift
        "PSRLDQ        $8,   %%xmm5                      \n\t" // logical left shift
        "\n\t"
        "addpd               %%xmm4,  %%xmm11            \n\t"
        "addpd               %%xmm5,  %%xmm15            \n\t"
        "\n\t"
        "movsd               %%xmm11, %[a]               \n\t"
        "movsd               %%xmm15, %[s]               \n\t"
        :[a]"=m"(avg), [s]"=m"(sqsum)
        :
    );
    // printf("AVG = %f \n", avg);
    // printf("SUM = %f \n", sqsum);

    avg /= n;
    return sqsum/n - avg*avg;
}

double variance_16_A_i8(int64_t *x, int64_t n){
}

double variance_16z_A_r8(double *x, int64_t n){
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double sqsum, avg;

    __asm__ __volatile__ (
        "VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
        "VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
        "VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
        "VXORPD %%zmm12, %%zmm12, %%zmm12 \n\t"
        "VXORPD %%zmm11, %%zmm11, %%zmm11 \n\t"
        "VXORPD %%zmm10, %%zmm10, %%zmm11 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%zmm0           \n\t"
            "vmovupd 8*8(%[x]), %%zmm1           \n\t"
            "vaddpd %%zmm0,     %%zmm12, %%zmm12 \n\t"
            "vaddpd %%zmm1,     %%zmm13, %%zmm13 \n\t"
            "vmulpd %%zmm0,     %%zmm0,  %%zmm2  \n\t"
            "vmulpd %%zmm1,     %%zmm1,  %%zmm3  \n\t"
            "vaddpd %%zmm2,     %%zmm14, %%zmm14 \n\t"
            "vaddpd %%zmm3,     %%zmm15, %%zmm15 \n\t"
            "subq $-16*8, %[x]                   \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){
        if (n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%zmm0           \n\t"
                "vaddpd  %%zmm0,    %%zmm12, %%zmm12 \n\t"
                "vmulpd  %%zmm0,    %%zmm0,  %%zmm1  \n\t"
                "vaddpd  %%zmm1,    %%zmm14, %%zmm14 \n\t"
                "subq $-8*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm0           \n\t"
                "vaddpd  %%ymm0,    %%ymm12, %%ymm12 \n\t"
                "vmulpd  %%ymm0,    %%ymm0,  %%ymm1  \n\t"
                "vaddpd  %%ymm1,    %%ymm14, %%ymm14 \n\t"
                "subq $-4*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm14, %%xmm14 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm0           \n\t"
                "vaddpd %%xmm0,    %%xmm12, %%xmm12 \n\t"
                "vmulpd %%xmm0,    %%xmm0,  %%xmm1  \n\t"
                "vaddpd %%xmm1,    %%xmm14, %%xmm14 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        // "vaddpd              %%zmm10, %%zmm12, %%zmm12   \n\t"
        "vaddpd              %%zmm12, %%zmm13, %%zmm13   \n\t"
        "VEXTRACTF64X4  $0,  %%zmm13, %%ymm0             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4  $1,  %%zmm13, %%ymm1             \n\t" // extract higher 128 bits
        "vaddpd              %%ymm1,  %%ymm0, %%ymm13    \n\t" // sum
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm2             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm3             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm3,  %%xmm2, %%xmm13    \n\t" // sum
        "movupd %%xmm13, %%xmm4  \n\t"
        "PSRLDQ $8,      %%xmm4  \n\t"
        "addpd  %%xmm4,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a]    \n\t"
        :[a]"=m"(avg)
        :
    );
    // printf("AVG = %f \n", avg);
    avg /= n;

    __asm__ __volatile__ (
        // "vaddpd              %%zmm11, %%zmm14, %%zmm14   \n\t"
        "vaddpd              %%zmm14, %%zmm15, %%zmm15   \n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm5             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm6             \n\t" // extract higher 128 bits
        "vaddpd              %%ymm6,  %%ymm5, %%ymm15    \n\t" // sum
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm7             \n\t" // extract lower  128 bits
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm8             \n\t" // extract higher 128 bits
        "vaddpd              %%xmm8,  %%xmm7, %%xmm15    \n\t" // sum
        "movupd %%xmm15, %%xmm9  \n\t"
        "PSRLDQ $8,      %%xmm9  \n\t"
        "addpd  %%xmm9,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[s]    \n\t"
        :[s]"=m"(sqsum)
        :
    );
    // printf("SUM = %f \n", sqsum);
    sqsum /= n;

    return sqsum - pow(avg,2);
}

double variance_16z_A_i8(int64_t *x, int64_t n){
}

double variance_32_A_r8(double *x, int64_t n){
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    double sqsum, avg;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
        ::
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[x]), %%zmm0           \n\t"
            "vmovupd 8*8(%[x]), %%zmm1           \n\t"
            "vaddpd %%zmm0,     %%zmm12, %%zmm12 \n\t"
            "vaddpd %%zmm1,     %%zmm13, %%zmm13 \n\t"
            "vmulpd %%zmm0,     %%zmm0,  %%zmm2  \n\t"
            "vmulpd %%zmm1,     %%zmm1,  %%zmm3  \n\t"
            "vaddpd %%zmm2,     %%zmm14, %%zmm14 \n\t"
            "vaddpd %%zmm3,     %%zmm15, %%zmm15 \n\t"
            "subq $-16*8, %[x]                   \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    if(n_remain>0){

        if (n_remain&1){
            __asm__ __volatile__ (
                "movsd  0*8(%[x]), %%xmm6           \n\t"
                "vaddpd %%xmm6,    %%xmm10, %%xmm10 \n\t"
                "vmulpd %%xmm6,    %%xmm6,  %%xmm7  \n\t"
                "vaddpd %%xmm7,    %%xmm11, %%xmm11 \n\t"
                "subq $-1*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[x]), %%xmm4           \n\t"
                "vaddpd %%xmm4,    %%xmm10, %%xmm10 \n\t"
                "vmulpd %%xmm4,    %%xmm4,  %%xmm5  \n\t"
                "vaddpd %%xmm5,    %%xmm11, %%xmm11 \n\t"
                "subq $-2*8, %[x]                   \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%ymm2           \n\t"
                "vaddpd %%ymm2,     %%ymm10, %%ymm10 \n\t"
                "vmulpd %%ymm2,     %%ymm2,  %%ymm3  \n\t"
                "vaddpd %%ymm3,     %%ymm11, %%ymm11 \n\t"
                "subq $-4*8, %[x]                    \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }

        if (n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[x]), %%zmm0           \n\t"
                "vaddpd %%zmm0,     %%zmm10, %%zmm10 \n\t"
                "vmulpd %%zmm0,     %%zmm0,  %%zmm1  \n\t"
                "vaddpd %%zmm1,     %%zmm11, %%zmm11 \n\t"
                "subq $-8*8, %[x]                    \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

    __asm__ __volatile__ (
        "vaddpd              %%zmm10, %%zmm12, %%zmm12   \n\t"
        "vaddpd              %%zmm12, %%zmm13, %%zmm13   \n\t"
        "\n\t"
        "VEXTRACTF64X4 $0,   %%zmm13, %%ymm6             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4 $1,   %%zmm13, %%ymm7             \n\t" // extract higher 128 bits
        "vaddpd              %%ymm6,  %%ymm7,  %%ymm13   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm13, %%xmm6             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm13, %%xmm7             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%zmm11, %%zmm14, %%zmm14   \n\t"
        "vaddpd              %%zmm14, %%zmm15, %%zmm15   \n\t"
        "\n\t"
        "VEXTRACTF64X4 $0,   %%zmm15, %%ymm8             \n\t" // extract lower  128 bits
        "VEXTRACTF64X4 $0,   %%zmm15, %%ymm9             \n\t" // extract lower  128 bits
        "vaddpd              %%ymm8,  %%ymm9,  %%ymm15   \n\t"
        "\n\t"
        "VEXTRACTF128  $0,   %%ymm15, %%xmm8             \n\t" // extract lower  128 bits
        "VEXTRACTF128  $1,   %%ymm15, %%xmm9             \n\t" // extract higher 128 bits
        "\n\t"
        "vaddpd              %%xmm7,  %%xmm6, %%xmm13    \n\t" // sqsum
        "vaddpd              %%xmm9,  %%xmm8, %%xmm15    \n\t" // square sqsum
        "\n\t"
        "movupd              %%xmm13, %%xmm4             \n\t" // copy  
        "movupd              %%xmm15, %%xmm5             \n\t" // copy
        "\n\t"
        "PSRLDQ        $8,   %%xmm4                      \n\t" // logical left shift
        "PSRLDQ        $8,   %%xmm5                      \n\t" // logical left shift
        "\n\t"
        "addpd               %%xmm4,  %%xmm13            \n\t"
        "addpd               %%xmm5,  %%xmm15            \n\t"
        "\n\t"
        "movsd               %%xmm13, %[a]               \n\t"
        "movsd               %%xmm15, %[s]               \n\t"
        :[a]"=m"(avg), [s]"=m"(sqsum)
        :
    );

    printf("%f, %f \n", avg ,sqsum);
    avg /= n;
    return sqsum/n - avg*avg;
}

double variance_32_A_i8(int64_t *x, int64_t n){
}