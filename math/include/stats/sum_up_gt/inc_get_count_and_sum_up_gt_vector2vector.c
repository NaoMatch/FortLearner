#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

void count_and_sum_up_gt_vector2vector_01_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=1, n_unroll=n/unroll, n_remain=n%unroll;

    double  r00, r01, r02;
    int64_t r03, r04;

    while(n_unroll--){
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        x_vals+=unroll;
        thr_vals+=unroll;
        sum_vals+=unroll;
        cnt_vals+=unroll;
    }
}

void count_and_sum_up_gt_vector2vector_02_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;

    double  r00, r01, r02;
    int64_t r03, r04;
    double  r05, r06, r07;
    int64_t r08, r09;

    while(n_unroll--){
        // ---------------------------------------------------
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+1);
        r06 = *(thr_vals+1);
        r07 = *(sum_vals+1);
        r08 = *(cnt_vals+1);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+1) = r07;
        *(cnt_vals+1) = r08;

        x_vals+=unroll;
        thr_vals+=unroll;
        sum_vals+=unroll;
        cnt_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        x_vals+=1;
        thr_vals+=1;
        sum_vals+=1;
        cnt_vals+=1;
    }
}

void count_and_sum_up_gt_vector2vector_04_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    double  r00, r01, r02;
    int64_t r03, r04;
    double  r05, r06, r07;
    int64_t r08, r09;
    double  r10, r11, r12;
    int64_t r13, r14;

    while(n_unroll--){
        // ---------------------------------------------------
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+1);
        r06 = *(thr_vals+1);
        r07 = *(sum_vals+1);
        r08 = *(cnt_vals+1);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+1) = r07;
        *(cnt_vals+1) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+2);
        r11 = *(thr_vals+2);
        r12 = *(sum_vals+2);
        r13 = *(cnt_vals+2);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+2) = r12;
        *(cnt_vals+2) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+3);
        r01 = *(thr_vals+3);
        r02 = *(sum_vals+3);
        r03 = *(cnt_vals+3);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+3) = r02;
        *(cnt_vals+3) = r03;

        x_vals+=unroll;
        thr_vals+=unroll;
        sum_vals+=unroll;
        cnt_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        x_vals+=1;
        thr_vals+=1;
        sum_vals+=1;
        cnt_vals+=1;
    }
}

void count_and_sum_up_gt_vector2vector_08_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    double  r00, r01, r02;
    int64_t r03, r04;
    double  r05, r06, r07;
    int64_t r08, r09;
    double  r10, r11, r12;
    int64_t r13, r14;

    while(n_unroll--){
        // ---------------------------------------------------
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+1);
        r06 = *(thr_vals+1);
        r07 = *(sum_vals+1);
        r08 = *(cnt_vals+1);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+1) = r07;
        *(cnt_vals+1) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+2);
        r11 = *(thr_vals+2);
        r12 = *(sum_vals+2);
        r13 = *(cnt_vals+2);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+2) = r12;
        *(cnt_vals+2) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+3);
        r01 = *(thr_vals+3);
        r02 = *(sum_vals+3);
        r03 = *(cnt_vals+3);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+3) = r02;
        *(cnt_vals+3) = r03;


        // ---------------------------------------------------
        r05 = *(x_vals+4);
        r06 = *(thr_vals+4);
        r07 = *(sum_vals+4);
        r08 = *(cnt_vals+4);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+4) = r07;
        *(cnt_vals+4) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+5);
        r11 = *(thr_vals+5);
        r12 = *(sum_vals+5);
        r13 = *(cnt_vals+5);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+5) = r12;
        *(cnt_vals+5) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+6);
        r01 = *(thr_vals+6);
        r02 = *(sum_vals+6);
        r03 = *(cnt_vals+6);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+6) = r02;
        *(cnt_vals+6) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+7);
        r06 = *(thr_vals+7);
        r07 = *(sum_vals+7);
        r08 = *(cnt_vals+7);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+7) = r07;
        *(cnt_vals+7) = r08;


        x_vals+=unroll;
        thr_vals+=unroll;
        sum_vals+=unroll;
        cnt_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        x_vals+=1;
        thr_vals+=1;
        sum_vals+=1;
        cnt_vals+=1;
    }    
}

void count_and_sum_up_gt_vector2vector_16_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    double  r00, r01, r02;
    int64_t r03, r04;
    double  r05, r06, r07;
    int64_t r08, r09;
    double  r10, r11, r12;
    int64_t r13, r14;

    while(n_unroll--){
        // ---------------------------------------------------
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+1);
        r06 = *(thr_vals+1);
        r07 = *(sum_vals+1);
        r08 = *(cnt_vals+1);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+1) = r07;
        *(cnt_vals+1) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+2);
        r11 = *(thr_vals+2);
        r12 = *(sum_vals+2);
        r13 = *(cnt_vals+2);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+2) = r12;
        *(cnt_vals+2) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+3);
        r01 = *(thr_vals+3);
        r02 = *(sum_vals+3);
        r03 = *(cnt_vals+3);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+3) = r02;
        *(cnt_vals+3) = r03;


        // ---------------------------------------------------
        r05 = *(x_vals+4);
        r06 = *(thr_vals+4);
        r07 = *(sum_vals+4);
        r08 = *(cnt_vals+4);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+4) = r07;
        *(cnt_vals+4) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+5);
        r11 = *(thr_vals+5);
        r12 = *(sum_vals+5);
        r13 = *(cnt_vals+5);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+5) = r12;
        *(cnt_vals+5) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+6);
        r01 = *(thr_vals+6);
        r02 = *(sum_vals+6);
        r03 = *(cnt_vals+6);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+6) = r02;
        *(cnt_vals+6) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+7);
        r06 = *(thr_vals+7);
        r07 = *(sum_vals+7);
        r08 = *(cnt_vals+7);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+7) = r07;
        *(cnt_vals+7) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+8);
        r11 = *(thr_vals+8);
        r12 = *(sum_vals+8);
        r13 = *(cnt_vals+8);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+8) = r12;
        *(cnt_vals+8) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+9);
        r01 = *(thr_vals+9);
        r02 = *(sum_vals+9);
        r03 = *(cnt_vals+9);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+9) = r02;
        *(cnt_vals+9) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+10);
        r06 = *(thr_vals+10);
        r07 = *(sum_vals+10);
        r08 = *(cnt_vals+10);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+10) = r07;
        *(cnt_vals+10) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+11);
        r11 = *(thr_vals+11);
        r12 = *(sum_vals+11);
        r13 = *(cnt_vals+11);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+11) = r12;
        *(cnt_vals+11) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+12);
        r01 = *(thr_vals+12);
        r02 = *(sum_vals+12);
        r03 = *(cnt_vals+12);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+12) = r02;
        *(cnt_vals+12) = r03;

        // ---------------------------------------------------
        r05 = *(x_vals+13);
        r06 = *(thr_vals+13);
        r07 = *(sum_vals+13);
        r08 = *(cnt_vals+13);

        r09 = r05 > r06;

        r07 += r09 * y_val;
        r08 += r09;

        *(sum_vals+13) = r07;
        *(cnt_vals+13) = r08;

        // ---------------------------------------------------
        r10 = *(x_vals+14);
        r11 = *(thr_vals+14);
        r12 = *(sum_vals+14);
        r13 = *(cnt_vals+14);

        r14 = r10 > r11;

        r12 += r14 * y_val;
        r13 += r14;

        *(sum_vals+14) = r12;
        *(cnt_vals+14) = r13;

        // ---------------------------------------------------
        r00 = *(x_vals+15);
        r01 = *(thr_vals+15);
        r02 = *(sum_vals+15);
        r03 = *(cnt_vals+15);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals+15) = r02;
        *(cnt_vals+15) = r03;


        x_vals+=unroll;
        thr_vals+=unroll;
        sum_vals+=unroll;
        cnt_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(x_vals);
        r01 = *(thr_vals);
        r02 = *(sum_vals);
        r03 = *(cnt_vals);

        r04 = r00 > r01;

        r02 += r04 * y_val;
        r03 += r04;

        *(sum_vals) = r02;
        *(cnt_vals) = r03;

        x_vals+=1;
        thr_vals+=1;
        sum_vals+=1;
        cnt_vals+=1;
    }        
}



void count_and_sum_up_gt_vector2vector_02x_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[s]), %%xmm0          \n\t"
            "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
            "movupd 0*8(%[t]), %%xmm2          \n\t"
            "movupd 0*8(%[x]), %%xmm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
            "\n\t"
            "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
            "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
            "\n\t"
            "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
            "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
            "\n\t"
            "movupd    %%xmm0, 0*8(%[s])       \n\t"
            "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
            "\n\t"
            "subq $-2*8, %[s]                  \n\t"
            "subq $-2*8, %[c]                  \n\t"
            "subq $-2*8, %[t]                  \n\t"
            "subq $-2*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }
}

void count_and_sum_up_gt_vector2vector_04x_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[s]), %%xmm0          \n\t"
            "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
            "movupd 0*8(%[t]), %%xmm2          \n\t"
            "movupd 0*8(%[x]), %%xmm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
            "\n\t"
            "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
            "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
            "\n\t"
            "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
            "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
            "\n\t"
            "movupd    %%xmm0, 0*8(%[s])       \n\t"
            "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
            "\n\t"
            "movupd 2*8(%[s]), %%xmm4          \n\t"
            "MOVDQU 2*8(%[c]), %%xmm5          \n\t"
            "movupd 2*8(%[t]), %%xmm6          \n\t"
            "movupd 2*8(%[x]), %%xmm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%xmm6, %%xmm7,  %%xmm7 \n\t"
            "\n\t"
            "vandpd    %%xmm7, %%xmm14, %%xmm6 \n\t"
            "VPAND     %%xmm7, %%xmm15, %%xmm7 \n\t"
            "\n\t"
            "vaddpd    %%xmm6, %%xmm4,  %%xmm4 \n\t"
            "vPADDQ    %%xmm7, %%xmm5,  %%xmm5 \n\t"
            "\n\t"
            "movupd    %%xmm4, 2*8(%[s])       \n\t"
            "MOVDQU    %%xmm5, 2*8(%[c])       \n\t"
            "\n\t"
            "subq $-4*8, %[s]                  \n\t"
            "subq $-4*8, %[c]                  \n\t"
            "subq $-4*8, %[t]                  \n\t"
            "subq $-4*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }
}

void count_and_sum_up_gt_vector2vector_04y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
            "vmovupd 0*8(%[t]), %%ymm2          \n\t"
            "vmovupd 0*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
            "\n\t"
            "subq $-4*8, %[s]                  \n\t"
            "subq $-4*8, %[c]                  \n\t"
            "subq $-4*8, %[t]                  \n\t"
            "subq $-4*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }    

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }
}

void count_and_sum_up_gt_vector2vector_08y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
            "vmovupd 0*8(%[t]), %%ymm2          \n\t"
            "vmovupd 0*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 4*8(%[s]), %%ymm4          \n\t"
            "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
            "vmovupd 4*8(%[t]), %%ymm6          \n\t"
            "vmovupd 4*8(%[x]), %%ymm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
            "\n\t"
            "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
            "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
            "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
            "\n\t"
            "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
            "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
            "\n\t"
            "subq $-8*8, %[s]                  \n\t"
            "subq $-8*8, %[c]                  \n\t"
            "subq $-8*8, %[t]                  \n\t"
            "subq $-8*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }    
}

void count_and_sum_up_gt_vector2vector_08z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%zmm15 \n\t"
        "VBROADCASTSD %[y], %%zmm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%zmm0          \n\t"
            "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
            "vmovupd 0*8(%[t]), %%zmm2          \n\t"
            "vmovupd 0*8(%[x]), %%zmm3          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
            "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
            "\n\t"
            "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
            "\n\t"
            "subq $-8*8, %[s]                  \n\t"
            "subq $-8*8, %[c]                  \n\t"
            "subq $-8*8, %[t]                  \n\t"
            "subq $-8*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }    
}

void count_and_sum_up_gt_vector2vector_16y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
            "vmovupd 0*8(%[t]), %%ymm2          \n\t"
            "vmovupd 0*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 4*8(%[s]), %%ymm4          \n\t"
            "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
            "vmovupd 4*8(%[t]), %%ymm6          \n\t"
            "vmovupd 4*8(%[x]), %%ymm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
            "\n\t"
            "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
            "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
            "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
            "\n\t"
            "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
            "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 8*8(%[s]), %%ymm8          \n\t"
            "vMOVDQU 8*8(%[c]), %%ymm9          \n\t"
            "vmovupd 8*8(%[t]), %%ymm10          \n\t"
            "vmovupd 8*8(%[x]), %%ymm11          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm10, %%ymm11,  %%ymm11 \n\t"
            "\n\t"
            "vandpd    %%ymm11, %%ymm14, %%ymm10 \n\t"
            "VPAND     %%ymm11, %%ymm15, %%ymm11 \n\t"
            "\n\t"
            "vaddpd    %%ymm10, %%ymm8,  %%ymm8 \n\t"
            "vPADDQ    %%ymm11, %%ymm9,  %%ymm9 \n\t"
            "\n\t"
            "vmovupd    %%ymm8, 8*8(%[s])       \n\t"
            "vMOVDQU    %%ymm9, 8*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 12*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 12*8(%[c]), %%ymm1          \n\t"
            "vmovupd 12*8(%[t]), %%ymm2          \n\t"
            "vmovupd 12*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 12*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 12*8(%[c])       \n\t"
            "\n\t"
            "subq $-16*8, %[s]                  \n\t"
            "subq $-16*8, %[c]                  \n\t"
            "subq $-16*8, %[t]                  \n\t"
            "subq $-16*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 4*8(%[s]), %%ymm4          \n\t"
                "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
                "vmovupd 4*8(%[t]), %%ymm6          \n\t"
                "vmovupd 4*8(%[x]), %%ymm7          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
                "\n\t"
                "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
                "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
                "\n\t"
                "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
                "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
                "\n\t"
                "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
                "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
                "\n\t"
                "subq $-8*8, %[s]                  \n\t"
                "subq $-8*8, %[c]                  \n\t"
                "subq $-8*8, %[t]                  \n\t"
                "subq $-8*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }        
}

void count_and_sum_up_gt_vector2vector_16z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%zmm15 \n\t"
        "VBROADCASTSD %[y], %%zmm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%zmm0          \n\t"
            "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
            "vmovupd 0*8(%[t]), %%zmm2          \n\t"
            "vmovupd 0*8(%[x]), %%zmm3          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
            "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
            "\n\t"
            "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 8*8(%[s]), %%zmm4          \n\t"
            "VMOVDQU64 8*8(%[c]), %%zmm5        \n\t"
            "vmovupd 8*8(%[t]), %%zmm6          \n\t"
            "vmovupd 8*8(%[x]), %%zmm7          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm6, %%zmm7,  %%k2 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm4,  %%zmm4 %{%%k2%} \n\t"
            "vPADDQ    %%zmm15, %%zmm5,  %%zmm5 %{%%k2%} \n\t"
            "\n\t"
            "vmovupd    %%zmm4, 8*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm5, 8*8(%[c])       \n\t"
            "\n\t"
            "subq $-16*8, %[s]                  \n\t"
            "subq $-16*8, %[c]                  \n\t"
            "subq $-16*8, %[t]                  \n\t"
            "subq $-16*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%zmm0          \n\t"
                "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
                "vmovupd 0*8(%[t]), %%zmm2          \n\t"
                "vmovupd 0*8(%[x]), %%zmm3          \n\t"
                "\n\t"
                "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
                "\n\t"
                "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
                "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
                "\n\t"
                "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
                "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-8*8, %[s]                  \n\t"
                "subq $-8*8, %[c]                  \n\t"
                "subq $-8*8, %[t]                  \n\t"
                "subq $-8*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }    
}

void count_and_sum_up_gt_vector2vector_32y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%ymm15 \n\t"
        "VBROADCASTSD %[y], %%ymm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
            "vmovupd 0*8(%[t]), %%ymm2          \n\t"
            "vmovupd 0*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 4*8(%[s]), %%ymm4          \n\t"
            "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
            "vmovupd 4*8(%[t]), %%ymm6          \n\t"
            "vmovupd 4*8(%[x]), %%ymm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
            "\n\t"
            "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
            "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
            "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
            "\n\t"
            "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
            "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 8*8(%[s]), %%ymm8          \n\t"
            "vMOVDQU 8*8(%[c]), %%ymm9          \n\t"
            "vmovupd 8*8(%[t]), %%ymm10          \n\t"
            "vmovupd 8*8(%[x]), %%ymm11          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm10, %%ymm11,  %%ymm11 \n\t"
            "\n\t"
            "vandpd    %%ymm11, %%ymm14, %%ymm10 \n\t"
            "VPAND     %%ymm11, %%ymm15, %%ymm11 \n\t"
            "\n\t"
            "vaddpd    %%ymm10, %%ymm8,  %%ymm8 \n\t"
            "vPADDQ    %%ymm11, %%ymm9,  %%ymm9 \n\t"
            "\n\t"
            "vmovupd    %%ymm8, 8*8(%[s])       \n\t"
            "vMOVDQU    %%ymm9, 8*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 12*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 12*8(%[c]), %%ymm1          \n\t"
            "vmovupd 12*8(%[t]), %%ymm2          \n\t"
            "vmovupd 12*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 12*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 12*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 16*8(%[s]), %%ymm4          \n\t"
            "vMOVDQU 16*8(%[c]), %%ymm5          \n\t"
            "vmovupd 16*8(%[t]), %%ymm6          \n\t"
            "vmovupd 16*8(%[x]), %%ymm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
            "\n\t"
            "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
            "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
            "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
            "\n\t"
            "vmovupd    %%ymm4, 16*8(%[s])       \n\t"
            "vMOVDQU    %%ymm5, 16*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 20*8(%[s]), %%ymm8          \n\t"
            "vMOVDQU 20*8(%[c]), %%ymm9          \n\t"
            "vmovupd 20*8(%[t]), %%ymm10          \n\t"
            "vmovupd 20*8(%[x]), %%ymm11          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm10, %%ymm11,  %%ymm11 \n\t"
            "\n\t"
            "vandpd    %%ymm11, %%ymm14, %%ymm10 \n\t"
            "VPAND     %%ymm11, %%ymm15, %%ymm11 \n\t"
            "\n\t"
            "vaddpd    %%ymm10, %%ymm8,  %%ymm8 \n\t"
            "vPADDQ    %%ymm11, %%ymm9,  %%ymm9 \n\t"
            "\n\t"
            "vmovupd    %%ymm8, 20*8(%[s])       \n\t"
            "vMOVDQU    %%ymm9, 20*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 24*8(%[s]), %%ymm0          \n\t"
            "vMOVDQU 24*8(%[c]), %%ymm1          \n\t"
            "vmovupd 24*8(%[t]), %%ymm2          \n\t"
            "vmovupd 24*8(%[x]), %%ymm3          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
            "\n\t"
            "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
            "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
            "\n\t"
            "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
            "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
            "\n\t"
            "vmovupd    %%ymm0, 24*8(%[s])       \n\t"
            "vMOVDQU    %%ymm1, 24*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 28*8(%[s]), %%ymm4          \n\t"
            "vMOVDQU 28*8(%[c]), %%ymm5          \n\t"
            "vmovupd 28*8(%[t]), %%ymm6          \n\t"
            "vmovupd 28*8(%[x]), %%ymm7          \n\t"
            "\n\t"
            "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
            "\n\t"
            "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
            "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
            "\n\t"
            "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
            "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
            "\n\t"
            "vmovupd    %%ymm4, 28*8(%[s])       \n\t"
            "vMOVDQU    %%ymm5, 28*8(%[c])       \n\t"
            "\n\t"
            "subq $-32*8, %[s]                  \n\t"
            "subq $-32*8, %[c]                  \n\t"
            "subq $-32*8, %[t]                  \n\t"
            "subq $-32*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&16){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 4*8(%[s]), %%ymm4          \n\t"
                "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
                "vmovupd 4*8(%[t]), %%ymm6          \n\t"
                "vmovupd 4*8(%[x]), %%ymm7          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
                "\n\t"
                "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
                "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
                "\n\t"
                "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
                "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
                "\n\t"
                "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
                "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 8*8(%[s]), %%ymm8          \n\t"
                "vMOVDQU 8*8(%[c]), %%ymm9          \n\t"
                "vmovupd 8*8(%[t]), %%ymm10          \n\t"
                "vmovupd 8*8(%[x]), %%ymm11          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm10, %%ymm11,  %%ymm11 \n\t"
                "\n\t"
                "vandpd    %%ymm11, %%ymm14, %%ymm10 \n\t"
                "VPAND     %%ymm11, %%ymm15, %%ymm11 \n\t"
                "\n\t"
                "vaddpd    %%ymm10, %%ymm8,  %%ymm8 \n\t"
                "vPADDQ    %%ymm11, %%ymm9,  %%ymm9 \n\t"
                "\n\t"
                "vmovupd    %%ymm8, 8*8(%[s])       \n\t"
                "vMOVDQU    %%ymm9, 8*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 12*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 12*8(%[c]), %%ymm1          \n\t"
                "vmovupd 12*8(%[t]), %%ymm2          \n\t"
                "vmovupd 12*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 12*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 12*8(%[c])       \n\t"
                "\n\t"
                "subq $-16*8, %[s]                  \n\t"
                "subq $-16*8, %[c]                  \n\t"
                "subq $-16*8, %[t]                  \n\t"
                "subq $-16*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 4*8(%[s]), %%ymm4          \n\t"
                "vMOVDQU 4*8(%[c]), %%ymm5          \n\t"
                "vmovupd 4*8(%[t]), %%ymm6          \n\t"
                "vmovupd 4*8(%[x]), %%ymm7          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm6, %%ymm7,  %%ymm7 \n\t"
                "\n\t"
                "vandpd    %%ymm7, %%ymm14, %%ymm6 \n\t"
                "VPAND     %%ymm7, %%ymm15, %%ymm7 \n\t"
                "\n\t"
                "vaddpd    %%ymm6, %%ymm4,  %%ymm4 \n\t"
                "vPADDQ    %%ymm7, %%ymm5,  %%ymm5 \n\t"
                "\n\t"
                "vmovupd    %%ymm4, 4*8(%[s])       \n\t"
                "vMOVDQU    %%ymm5, 4*8(%[c])       \n\t"
                "\n\t"
                "subq $-8*8, %[s]                  \n\t"
                "subq $-8*8, %[c]                  \n\t"
                "subq $-8*8, %[t]                  \n\t"
                "subq $-8*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }        
}

void count_and_sum_up_gt_vector2vector_32z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll, one=1;
    double y_copy = y_val;

    __asm__ __volatile__ (
        "VPBROADCASTQ %[o], %%zmm15 \n\t"
        "VBROADCASTSD %[y], %%zmm14 \n\t"
        :[o]"=r"(one), [y]"=x"(y_copy)
        :"0"(one)
    );

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[s]), %%zmm0          \n\t"
            "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
            "vmovupd 0*8(%[t]), %%zmm2          \n\t"
            "vmovupd 0*8(%[x]), %%zmm3          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
            "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
            "\n\t"
            "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 8*8(%[s]), %%zmm4          \n\t"
            "VMOVDQU64 8*8(%[c]), %%zmm5        \n\t"
            "vmovupd 8*8(%[t]), %%zmm6          \n\t"
            "vmovupd 8*8(%[x]), %%zmm7          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm6, %%zmm7,  %%k2 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm4,  %%zmm4 %{%%k2%} \n\t"
            "vPADDQ    %%zmm15, %%zmm5,  %%zmm5 %{%%k2%} \n\t"
            "\n\t"
            "vmovupd    %%zmm4, 8*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm5, 8*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 16*8(%[s]), %%zmm8          \n\t"
            "VMOVDQU64 16*8(%[c]), %%zmm9        \n\t"
            "vmovupd 16*8(%[t]), %%zmm10          \n\t"
            "vmovupd 16*8(%[x]), %%zmm11          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm10, %%zmm11,  %%k1 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm8,  %%zmm8 %{%%k1%} \n\t"
            "vPADDQ    %%zmm15, %%zmm9,  %%zmm9 %{%%k1%} \n\t"
            "\n\t"
            "vmovupd    %%zmm8, 16*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm9, 16*8(%[c])       \n\t"
            "\n\t"
            "vmovupd 24*8(%[s]), %%zmm0          \n\t"
            "VMOVDQU64 24*8(%[c]), %%zmm1        \n\t"
            "vmovupd 24*8(%[t]), %%zmm2          \n\t"
            "vmovupd 24*8(%[x]), %%zmm3          \n\t"
            "\n\t"
            "VCMPGTPD %%zmm2, %%zmm3,  %%k2 \n\t"
            "\n\t"
            "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k2%} \n\t"
            "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k2%} \n\t"
            "\n\t"
            "vmovupd    %%zmm0, 24*8(%[s])       \n\t"
            "VMOVDQU64  %%zmm1, 24*8(%[c])       \n\t"
            "\n\t"
            "subq $-32*8, %[s]                  \n\t"
            "subq $-32*8, %[c]                  \n\t"
            "subq $-32*8, %[t]                  \n\t"
            "subq $-32*8, %[x]                  \n\t"
            :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
            :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&16){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%zmm0          \n\t"
                "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
                "vmovupd 0*8(%[t]), %%zmm2          \n\t"
                "vmovupd 0*8(%[x]), %%zmm3          \n\t"
                "\n\t"
                "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
                "\n\t"
                "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
                "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
                "\n\t"
                "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
                "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "vmovupd 8*8(%[s]), %%zmm4          \n\t"
                "VMOVDQU64 8*8(%[c]), %%zmm5        \n\t"
                "vmovupd 8*8(%[t]), %%zmm6          \n\t"
                "vmovupd 8*8(%[x]), %%zmm7          \n\t"
                "\n\t"
                "VCMPGTPD %%zmm6, %%zmm7,  %%k2 \n\t"
                "\n\t"
                "vaddpd    %%zmm14, %%zmm4,  %%zmm4 %{%%k2%} \n\t"
                "vPADDQ    %%zmm15, %%zmm5,  %%zmm5 %{%%k2%} \n\t"
                "\n\t"
                "vmovupd    %%zmm4, 8*8(%[s])       \n\t"
                "VMOVDQU64  %%zmm5, 8*8(%[c])       \n\t"
                "\n\t"
                "subq $-16*8, %[s]                  \n\t"
                "subq $-16*8, %[c]                  \n\t"
                "subq $-16*8, %[t]                  \n\t"
                "subq $-16*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%zmm0          \n\t"
                "VMOVDQU64 0*8(%[c]), %%zmm1        \n\t"
                "vmovupd 0*8(%[t]), %%zmm2          \n\t"
                "vmovupd 0*8(%[x]), %%zmm3          \n\t"
                "\n\t"
                "VCMPGTPD %%zmm2, %%zmm3,  %%k1 \n\t"
                "\n\t"
                "vaddpd    %%zmm14, %%zmm0,  %%zmm0 %{%%k1%} \n\t"
                "vPADDQ    %%zmm15, %%zmm1,  %%zmm1 %{%%k1%} \n\t"
                "\n\t"
                "vmovupd    %%zmm0, 0*8(%[s])       \n\t"
                "VMOVDQU64  %%zmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-8*8, %[s]                  \n\t"
                "subq $-8*8, %[c]                  \n\t"
                "subq $-8*8, %[t]                  \n\t"
                "subq $-8*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[s]), %%ymm0          \n\t"
                "vMOVDQU 0*8(%[c]), %%ymm1          \n\t"
                "vmovupd 0*8(%[t]), %%ymm2          \n\t"
                "vmovupd 0*8(%[x]), %%ymm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%ymm2, %%ymm3,  %%ymm3 \n\t"
                "\n\t"
                "vandpd    %%ymm3, %%ymm14, %%ymm2 \n\t"
                "VPAND     %%ymm3, %%ymm15, %%ymm3 \n\t"
                "\n\t"
                "vaddpd    %%ymm2, %%ymm0,  %%ymm0 \n\t"
                "vPADDQ    %%ymm3, %%ymm1,  %%ymm1 \n\t"
                "\n\t"
                "vmovupd    %%ymm0, 0*8(%[s])       \n\t"
                "vMOVDQU    %%ymm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-4*8, %[s]                  \n\t"
                "subq $-4*8, %[c]                  \n\t"
                "subq $-4*8, %[t]                  \n\t"
                "subq $-4*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[s]), %%xmm0          \n\t"
                "MOVDQU 0*8(%[c]), %%xmm1          \n\t"
                "movupd 0*8(%[t]), %%xmm2          \n\t"
                "movupd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movupd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVDQU    %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-2*8, %[s]                  \n\t"
                "subq $-2*8, %[c]                  \n\t"
                "subq $-2*8, %[t]                  \n\t"
                "subq $-2*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[s]), %%xmm0          \n\t"
                "MOVQ  0*8(%[c]), %%xmm1          \n\t"
                "movsd 0*8(%[t]), %%xmm2          \n\t"
                "movsd 0*8(%[x]), %%xmm3          \n\t"
                "\n\t"
                "VCMPGTPD  %%xmm2, %%xmm3,  %%xmm3 \n\t"
                "\n\t"
                "vandpd    %%xmm3, %%xmm14, %%xmm2 \n\t"
                "VPAND     %%xmm3, %%xmm15, %%xmm3 \n\t"
                "\n\t"
                "vaddpd    %%xmm2, %%xmm0,  %%xmm0 \n\t"
                "vPADDQ    %%xmm3, %%xmm1,  %%xmm1 \n\t"
                "\n\t"
                "movsd    %%xmm0, 0*8(%[s])       \n\t"
                "MOVQ     %%xmm1, 0*8(%[c])       \n\t"
                "\n\t"
                "subq $-1*8, %[s]                  \n\t"
                "subq $-1*8, %[c]                  \n\t"
                "subq $-1*8, %[t]                  \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x_vals), [t]"=r"(thr_vals), [s]"=r"(sum_vals), [c]"=r"(cnt_vals)
                :    "0"(x_vals),     "1"(thr_vals),     "2"(sum_vals),     "3"(cnt_vals)
            );
        }
    }    
}
