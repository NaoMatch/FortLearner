#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double covariance_loop_C(double vec1[], double vec2[], int64_t n){
    int64_t unroll=1, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04;

    r02 = 0;
    r03 = 0;
    r04 = 0;
    while(n_unroll--){
        r00 = *(vec1);
        r01 = *(vec2);

        r02 += r00;
        r03 += r01;
        r04 += r00*r01;

        vec1+=unroll;
        vec2+=unroll;
    }

    return r04/n - r02*r03/n/n;
}

double covariance_loop_02_C(double vec1[], double vec2[], int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;

    r04 = 0;
    r05 = 0;
    r06 = 0;
    while(n_unroll--){
        r00 = *(vec1);
        r01 = *(vec2);
        r02 = *(vec1+1);
        r03 = *(vec2+1);

        r04 += r00;
        r05 += r01;
        r06 += r00 * r01;

        r04 += r02;
        r05 += r03;
        r06 += r02 * r03;

        vec1+=unroll;
        vec2+=unroll;
    }

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        r04 += r00;
        r05 += r01;
        r06 += r00 * r01;

        vec1+=1;
        vec2+=1;
    }

    return r06/n - r05*r04/n/n;
}

double covariance_loop_04_C(double vec1[], double vec2[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10;

    r08 = 0;
    r09 = 0;
    r10 = 0;
    while(n_unroll--){
        r00 = *(vec1);
        r01 = *(vec2);
        r02 = *(vec1+1);
        r03 = *(vec2+1);
        r04 = *(vec1+2);
        r05 = *(vec2+2);
        r06 = *(vec1+3);
        r07 = *(vec2+3);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        vec1+=unroll;
        vec2+=unroll;
    }

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        vec1+=1;
        vec2+=1;
    }

    return r10/n - r09*r08/n/n;
}

double covariance_loop_08_C(double vec1[], double vec2[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10;

    r08 = 0;
    r09 = 0;
    r10 = 0;
    while(n_unroll--){
        r00 = *(vec1);
        r01 = *(vec2);
        r02 = *(vec1+1);
        r03 = *(vec2+1);
        r04 = *(vec1+2);
        r05 = *(vec2+2);
        r06 = *(vec1+3);
        r07 = *(vec2+3);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        r00 = *(vec1+4);
        r01 = *(vec2+4);
        r02 = *(vec1+5);
        r03 = *(vec2+5);
        r04 = *(vec1+6);
        r05 = *(vec2+6);
        r06 = *(vec1+7);
        r07 = *(vec2+7);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        vec1+=unroll;
        vec2+=unroll;        
    }

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        vec1+=1;
        vec2+=1;
    }

    return r10/n - r09*r08/n/n;    
}

double covariance_loop_16_C(double vec1[], double vec2[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10;

    r08 = 0;
    r09 = 0;
    r10 = 0;
    while(n_unroll--){
        r00 = *(vec1);
        r01 = *(vec2);
        r02 = *(vec1+1);
        r03 = *(vec2+1);
        r04 = *(vec1+2);
        r05 = *(vec2+2);
        r06 = *(vec1+3);
        r07 = *(vec2+3);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        r00 = *(vec1+4);
        r01 = *(vec2+4);
        r02 = *(vec1+5);
        r03 = *(vec2+5);
        r04 = *(vec1+6);
        r05 = *(vec2+6);
        r06 = *(vec1+7);
        r07 = *(vec2+7);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        r00 = *(vec1+8);
        r01 = *(vec2+8);
        r02 = *(vec1+9);
        r03 = *(vec2+9);
        r04 = *(vec1+10);
        r05 = *(vec2+10);
        r06 = *(vec1+11);
        r07 = *(vec2+11);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        r00 = *(vec1+12);
        r01 = *(vec2+12);
        r02 = *(vec1+13);
        r03 = *(vec2+13);
        r04 = *(vec1+14);
        r05 = *(vec2+14);
        r06 = *(vec1+15);
        r07 = *(vec2+15);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        r08 += r02;
        r09 += r03;
        r10 += r02 * r03;

        r08 += r04;
        r09 += r05;
        r10 += r04 * r05;

        r08 += r06;
        r09 += r07;
        r10 += r06 * r07;

        vec1+=unroll;
        vec2+=unroll;        
    }

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        r08 += r00;
        r09 += r01;
        r10 += r00 * r01;

        vec1+=1;
        vec2+=1;
    }

    return r10/n - r09*r08/n/n;        
}

double covariance_loop_02x_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[vec1]), %%xmm0 \n\t"
            "movupd 0*8(%[vec2]), %%xmm1 \n\t"
            "\n\t"
            "vaddpd      %%xmm0,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm1,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm0,  %%xmm1,  %%xmm1  \n\t"
            "vaddpd      %%xmm1,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "subq $-2*8, %[vec1] \n\t"
            "subq $-2*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }

    if (n_remain>0){
        __asm__ __volatile__ (
            "movsd 0*8(%[vec1]), %%xmm0 \n\t"
            "movsd 0*8(%[vec2]), %%xmm1 \n\t"
            "\n\t"
            "vaddpd      %%xmm0,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm1,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm0,  %%xmm1,  %%xmm1  \n\t"
            "vaddpd      %%xmm1,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "subq $-1*8, %[vec1] \n\t"
            "subq $-1*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }

    __asm__ __volatile__ (
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
		);

        return tmp/n - avg1*avg2/n/n;
}

double covariance_loop_04x_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[vec1]), %%xmm0 \n\t"
            "movupd 0*8(%[vec2]), %%xmm1 \n\t"
            "\n\t"
            "vaddpd      %%xmm0,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm1,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm0,  %%xmm1,  %%xmm1  \n\t"
            "vaddpd      %%xmm1,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "movupd 2*8(%[vec1]), %%xmm2 \n\t"
            "movupd 2*8(%[vec2]), %%xmm3 \n\t"
            "\n\t"
            "vaddpd      %%xmm2,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm3,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm2,  %%xmm3,  %%xmm3  \n\t"
            "vaddpd      %%xmm3,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "subq $-4*8, %[vec1] \n\t"
            "subq $-4*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;    
}

double covariance_loop_04y_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%ymm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%ymm1 \n\t"
            "\n\t"
            "vaddpd      %%ymm0,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm1,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm0,  %%ymm1,  %%ymm1  \n\t"
            "vaddpd      %%ymm1,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-4*8, %[vec1] \n\t"
            "subq $-4*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }

    __asm__ __volatile__ (
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;    
}

double covariance_loop_08x_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;
    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[vec1]), %%xmm0 \n\t"
            "movupd 0*8(%[vec2]), %%xmm1 \n\t"
            "\n\t"
            "vaddpd      %%xmm0,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm1,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm0,  %%xmm1,  %%xmm1  \n\t"
            "vaddpd      %%xmm1,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "movupd 2*8(%[vec1]), %%xmm2 \n\t"
            "movupd 2*8(%[vec2]), %%xmm3 \n\t"
            "\n\t"
            "vaddpd      %%xmm2,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm3,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm2,  %%xmm3,  %%xmm3  \n\t"
            "vaddpd      %%xmm3,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "movupd 4*8(%[vec1]), %%xmm4 \n\t"
            "movupd 4*8(%[vec2]), %%xmm5 \n\t"
            "\n\t"
            "vaddpd      %%xmm4,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm5,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm4,  %%xmm5,  %%xmm5  \n\t"
            "vaddpd      %%xmm5,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "movupd 6*8(%[vec1]), %%xmm6 \n\t"
            "movupd 6*8(%[vec2]), %%xmm7 \n\t"
            "\n\t"
            "vaddpd      %%xmm6,  %%xmm13, %%xmm13 \n\t"
            "vaddpd      %%xmm7,  %%xmm14, %%xmm14 \n\t"
            "\n\t"
            "vmulpd      %%xmm6,  %%xmm7,  %%xmm7  \n\t"
            "vaddpd      %%xmm7,  %%xmm15, %%xmm15 \n\t"
            "\n\t"
            "subq $-8*8, %[vec1] \n\t"
            "subq $-8*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;        
}

double covariance_loop_08y_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%ymm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%ymm1 \n\t"
            "\n\t"
            "vaddpd      %%ymm0,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm1,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm0,  %%ymm1,  %%ymm1  \n\t"
            "vaddpd      %%ymm1,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 4*8(%[vec1]), %%ymm2 \n\t"
            "vmovupd 4*8(%[vec2]), %%ymm3 \n\t"
            "\n\t"
            "vaddpd      %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm3,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm2,  %%ymm3,  %%ymm3  \n\t"
            "vaddpd      %%ymm3,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-8*8, %[vec1] \n\t"
            "subq $-8*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm2  \n\t"
        "PSRLDQ $8,      %%xmm2  \n\t"
        "addpd  %%xmm2,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm3  \n\t"
        "PSRLDQ $8,      %%xmm3  \n\t"
        "addpd  %%xmm3,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;
}

double covariance_loop_08z_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    double r00, r01;

    __asm__ __volatile__ (
        "VXORPD  %%zmm13, %%zmm13, %%zmm13 \n\t"
        "VXORPD  %%zmm14, %%zmm14, %%zmm14 \n\t"
        "VXORPD  %%zmm15, %%zmm15, %%zmm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%zmm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%zmm1 \n\t"
            "\n\t"
            "vaddpd      %%zmm0,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm1,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm0,  %%zmm1,  %%zmm1  \n\t"
            "vaddpd      %%zmm1,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "subq $-8*8, %[vec1] \n\t"
            "subq $-8*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }

    __asm__ __volatile__ (
        "VEXTRACTF64X4  $0,  %%zmm13, %%ymm0  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm13, %%ymm1  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm14, %%ymm2  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm14, %%ymm3  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm4  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm5  \n\t"
        "\n\t"
        "vaddpd %%ymm0, %%ymm1, %%ymm13 \n\t"
        "vaddpd %%ymm2, %%ymm3, %%ymm14 \n\t"
        "vaddpd %%ymm4, %%ymm5, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;    
    
}

double covariance_loop_16y_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;
    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%ymm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%ymm1 \n\t"
            "\n\t"
            "vaddpd      %%ymm0,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm1,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm0,  %%ymm1,  %%ymm1  \n\t"
            "vaddpd      %%ymm1,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 4*8(%[vec1]), %%ymm2 \n\t"
            "vmovupd 4*8(%[vec2]), %%ymm3 \n\t"
            "\n\t"
            "vaddpd      %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm3,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm2,  %%ymm3,  %%ymm3  \n\t"
            "vaddpd      %%ymm3,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 8*8(%[vec1]), %%ymm4 \n\t"
            "vmovupd 8*8(%[vec2]), %%ymm5 \n\t"
            "\n\t"
            "vaddpd      %%ymm4,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm5,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm4,  %%ymm5,  %%ymm5  \n\t"
            "vaddpd      %%ymm5,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 12*8(%[vec1]), %%ymm6 \n\t"
            "vmovupd 12*8(%[vec2]), %%ymm7 \n\t"
            "\n\t"
            "vaddpd      %%ymm6,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm7,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm6,  %%ymm7,  %%ymm7  \n\t"
            "vaddpd      %%ymm7,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-16*8, %[vec1] \n\t"
            "subq $-16*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;            
}

double covariance_loop_16z_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;

    double r00, r01;

    __asm__ __volatile__ (
        "VXORPD %%zmm10, %%zmm10, %%zmm10 \n\t"
        "VXORPD %%zmm11, %%zmm11, %%zmm11 \n\t"
        "VXORPD %%zmm12, %%zmm12, %%zmm12 \n\t"
        "VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
        "VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
        "VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%zmm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%zmm1 \n\t"
            "\n\t"
            "vaddpd      %%zmm0,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm1,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm0,  %%zmm1,  %%zmm1  \n\t"
            "vaddpd      %%zmm1,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vmovupd 8*8(%[vec1]), %%zmm2 \n\t"
            "vmovupd 8*8(%[vec2]), %%zmm3 \n\t"
            "\n\t"
            "vaddpd      %%zmm2,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm3,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm2,  %%zmm3,  %%zmm3  \n\t"
            "vaddpd      %%zmm3,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "subq $-16*8, %[vec1] \n\t"
            "subq $-16*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "VEXTRACTF64X4  $0,  %%zmm13, %%ymm0  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm13, %%ymm1  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm14, %%ymm2  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm14, %%ymm3  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm4  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm5  \n\t"
        "\n\t"
        "vaddpd %%ymm0, %%ymm1, %%ymm13 \n\t"
        "vaddpd %%ymm2, %%ymm3, %%ymm14 \n\t"
        "vaddpd %%ymm4, %%ymm5, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm2  \n\t"
        "PSRLDQ $8,      %%xmm2  \n\t"
        "addpd  %%xmm2,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm3  \n\t"
        "PSRLDQ $8,      %%xmm3  \n\t"
        "addpd  %%xmm3,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;    
}

double covariance_loop_32y_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;
    double r00, r01;

    __asm__ __volatile__ (
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%ymm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%ymm1 \n\t"
            "\n\t"
            "vaddpd      %%ymm0,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm1,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm0,  %%ymm1,  %%ymm1  \n\t"
            "vaddpd      %%ymm1,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 4*8(%[vec1]), %%ymm2 \n\t"
            "vmovupd 4*8(%[vec2]), %%ymm3 \n\t"
            "\n\t"
            "vaddpd      %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm3,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm2,  %%ymm3,  %%ymm3  \n\t"
            "vaddpd      %%ymm3,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 8*8(%[vec1]), %%ymm4 \n\t"
            "vmovupd 8*8(%[vec2]), %%ymm5 \n\t"
            "\n\t"
            "vaddpd      %%ymm4,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm5,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm4,  %%ymm5,  %%ymm5  \n\t"
            "vaddpd      %%ymm5,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 12*8(%[vec1]), %%ymm6 \n\t"
            "vmovupd 12*8(%[vec2]), %%ymm7 \n\t"
            "\n\t"
            "vaddpd      %%ymm6,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm7,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm6,  %%ymm7,  %%ymm7  \n\t"
            "vaddpd      %%ymm7,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 16*8(%[vec1]), %%ymm0 \n\t"
            "vmovupd 16*8(%[vec2]), %%ymm1 \n\t"
            "\n\t"
            "vaddpd      %%ymm0,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm1,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm0,  %%ymm1,  %%ymm1  \n\t"
            "vaddpd      %%ymm1,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 20*8(%[vec1]), %%ymm2 \n\t"
            "vmovupd 20*8(%[vec2]), %%ymm3 \n\t"
            "\n\t"
            "vaddpd      %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm3,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm2,  %%ymm3,  %%ymm3  \n\t"
            "vaddpd      %%ymm3,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 24*8(%[vec1]), %%ymm4 \n\t"
            "vmovupd 24*8(%[vec2]), %%ymm5 \n\t"
            "\n\t"
            "vaddpd      %%ymm4,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm5,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm4,  %%ymm5,  %%ymm5  \n\t"
            "vaddpd      %%ymm5,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 28*8(%[vec1]), %%ymm6 \n\t"
            "vmovupd 28*8(%[vec2]), %%ymm7 \n\t"
            "\n\t"
            "vaddpd      %%ymm6,  %%ymm13, %%ymm13 \n\t"
            "vaddpd      %%ymm7,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmulpd      %%ymm6,  %%ymm7,  %%ymm7  \n\t"
            "vaddpd      %%ymm7,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-32*8, %[vec1] \n\t"
            "subq $-32*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;            
    
}

double covariance_loop_32z_A(double vec1[], double vec2[], int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll;
    double avg1, avg2, tmp;
    double r00, r01;

    __asm__ __volatile__ (
        "VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
        "VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
        "VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
        :
        :
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[vec1]), %%zmm0 \n\t"
            "vmovupd 0*8(%[vec2]), %%zmm1 \n\t"
            "\n\t"
            "vaddpd      %%zmm0,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm1,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm0,  %%zmm1,  %%zmm1  \n\t"
            "vaddpd      %%zmm1,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vmovupd 8*8(%[vec1]), %%zmm2 \n\t"
            "vmovupd 8*8(%[vec2]), %%zmm3 \n\t"
            "\n\t"
            "vaddpd      %%zmm2,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm3,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm2,  %%zmm3,  %%zmm3  \n\t"
            "vaddpd      %%zmm3,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vmovupd 16*8(%[vec1]), %%zmm4 \n\t"
            "vmovupd 16*8(%[vec2]), %%zmm5 \n\t"
            "\n\t"
            "vaddpd      %%zmm4,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm5,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm4,  %%zmm5,  %%zmm5  \n\t"
            "vaddpd      %%zmm5,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vmovupd 24*8(%[vec1]), %%zmm6 \n\t"
            "vmovupd 24*8(%[vec2]), %%zmm7 \n\t"
            "\n\t"
            "vaddpd      %%zmm6,  %%zmm13, %%zmm13 \n\t"
            "vaddpd      %%zmm7,  %%zmm14, %%zmm14 \n\t"
            "\n\t"
            "vmulpd      %%zmm6,  %%zmm7,  %%zmm7  \n\t"
            "vaddpd      %%zmm7,  %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "subq $-32*8, %[vec1] \n\t"
            "subq $-32*8, %[vec2] \n\t"
            :[vec1]"=r"(vec1), [vec2]"=r"(vec2)
            :       "0"(vec1),        "1"(vec2)
        );
    }
    
    __asm__ __volatile__ (
        "VEXTRACTF64X4  $0,  %%zmm13, %%ymm0  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm13, %%ymm1  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm14, %%ymm2  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm14, %%ymm3  \n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm4  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm5  \n\t"
        "\n\t"
        "vaddpd %%ymm0, %%ymm1, %%ymm13 \n\t"
        "vaddpd %%ymm2, %%ymm3, %%ymm14 \n\t"
        "vaddpd %%ymm4, %%ymm5, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3  \n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm4  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm5  \n\t"
        "\n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm13 \n\t"
        "vaddpd %%xmm2, %%xmm3, %%xmm14 \n\t"
        "vaddpd %%xmm4, %%xmm5, %%xmm15 \n\t"
        "\n\t"
        "movupd %%xmm13, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm13 \n\t"
        "movsd  %%xmm13, %[a1]    \n\t"
        "\n\t"
        "movupd %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm14 \n\t"
        "movsd  %%xmm14, %[a2]    \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm1  \n\t"
        "PSRLDQ $8,      %%xmm1  \n\t"
        "addpd  %%xmm1,  %%xmm15 \n\t"
        "movsd  %%xmm15, %[t]    \n\t"
        :[a1]"=m"(avg1), [a2]"=m"(avg2), [t]"=m"(tmp)
        :
	);

    while(n_remain--){
        r00 = *(vec1);
        r01 = *(vec2);

        avg1 += r00;
        avg2 += r01;

        tmp += r00*r01;
        vec1++;
        vec2++;
    }

    return tmp/n - avg1*avg2/n/n;            
    
}
