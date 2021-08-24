#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111


double maxval_r8__(double x, double y){
	if (x>y){
        return x;
    }else{
        return y;
    }
}


double minval_r8__(double x, double y){
	if (x<y){
        return x;
    }else{
        return y;
    }
}


void get_minmax_vector2vector_01_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=1, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;

    while(n_unroll--){
        r00 = *(vals);

        r01 = *(min_vals);
        r02 = *(max_vals);

        r01 = minval_r8__(r00, r01);
        r02 = maxval_r8__(r00, r02);

        *(min_vals)=r01;
        *(max_vals)=r02;

        vals+=unroll;
        min_vals+=unroll;
        max_vals+=unroll;
    }
}


void get_minmax_vector2vector_02_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;

    while(n_unroll--){
        r00 = *(vals);
        r01 = *(vals+1);

        r02 = *(min_vals);
        r03 = *(min_vals+1);
        r04 = *(max_vals);
        r05 = *(max_vals+1);

        r02 = minval_r8__(r00, r02);
        r03 = minval_r8__(r01, r03);
        r04 = maxval_r8__(r00, r04);
        r05 = maxval_r8__(r01, r05);

        *(min_vals)  =r02;
        *(min_vals+1)=r03;
        *(max_vals)  =r04;
        *(max_vals+1)=r05;

        vals+=unroll;
        min_vals+=unroll;
        max_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(vals);

        r01 = *(min_vals);
        r02 = *(max_vals);

        r01 = minval_r8__(r00, r01);
        r02 = maxval_r8__(r00, r02);

        *(min_vals)=r01;
        *(max_vals)=r02;

        vals+=1;
        min_vals+=1;
        max_vals+=1;
    }

}


void get_minmax_vector2vector_04_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    while(n_unroll--){
        r00 = *(vals);
        r01 = *(vals+1);
        r02 = *(vals+2);
        r03 = *(vals+3);

        r04 = *(min_vals);
        r05 = *(min_vals+1);
        r06 = *(min_vals+2);
        r07 = *(min_vals+3);
        r08 = *(max_vals);
        r09 = *(max_vals+1);
        r10 = *(max_vals+2);
        r11 = *(max_vals+3);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals) = r04;
        *(min_vals+1) = r05;
        *(min_vals+2) = r06;
        *(min_vals+3) = r07;
        *(max_vals) = r08;
        *(max_vals+1) = r09;
        *(max_vals+2) = r10;
        *(max_vals+3) = r11;

        vals+=unroll;
        min_vals+=unroll;
        max_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(vals);

        r01 = *(min_vals);
        r02 = *(max_vals);

        r01 = minval_r8__(r00, r01);
        r02 = maxval_r8__(r00, r02);

        *(min_vals)=r01;
        *(max_vals)=r02;

        vals+=1;
        min_vals+=1;
        max_vals+=1;
    }
}


void get_minmax_vector2vector_08_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    while(n_unroll--){
        r00 = *(vals);
        r01 = *(vals+1);
        r02 = *(vals+2);
        r03 = *(vals+3);

        r04 = *(min_vals);
        r05 = *(min_vals+1);
        r06 = *(min_vals+2);
        r07 = *(min_vals+3);
        r08 = *(max_vals);
        r09 = *(max_vals+1);
        r10 = *(max_vals+2);
        r11 = *(max_vals+3);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals) = r04;
        *(min_vals+1) = r05;
        *(min_vals+2) = r06;
        *(min_vals+3) = r07;
        *(max_vals) = r08;
        *(max_vals+1) = r09;
        *(max_vals+2) = r10;
        *(max_vals+3) = r11;

        r00 = *(vals+4);
        r01 = *(vals+5);
        r02 = *(vals+6);
        r03 = *(vals+7);

        r04 = *(min_vals+4);
        r05 = *(min_vals+5);
        r06 = *(min_vals+6);
        r07 = *(min_vals+7);
        r08 = *(max_vals+4);
        r09 = *(max_vals+5);
        r10 = *(max_vals+6);
        r11 = *(max_vals+7);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals+4) = r04;
        *(min_vals+5) = r05;
        *(min_vals+6) = r06;
        *(min_vals+7) = r07;
        *(max_vals+4) = r08;
        *(max_vals+5) = r09;
        *(max_vals+6) = r10;
        *(max_vals+7) = r11;

        vals+=unroll;
        min_vals+=unroll;
        max_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(vals);

        r01 = *(min_vals);
        r02 = *(max_vals);

        r01 = minval_r8__(r00, r01);
        r02 = maxval_r8__(r00, r02);

        *(min_vals)=r01;
        *(max_vals)=r02;

        vals+=1;
        min_vals+=1;
        max_vals+=1;
    }

}


void get_minmax_vector2vector_16_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    while(n_unroll--){
        r00 = *(vals);
        r01 = *(vals+1);
        r02 = *(vals+2);
        r03 = *(vals+3);

        r04 = *(min_vals);
        r05 = *(min_vals+1);
        r06 = *(min_vals+2);
        r07 = *(min_vals+3);
        r08 = *(max_vals);
        r09 = *(max_vals+1);
        r10 = *(max_vals+2);
        r11 = *(max_vals+3);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals) = r04;
        *(min_vals+1) = r05;
        *(min_vals+2) = r06;
        *(min_vals+3) = r07;
        *(max_vals) = r08;
        *(max_vals+1) = r09;
        *(max_vals+2) = r10;
        *(max_vals+3) = r11;

        r00 = *(vals+4);
        r01 = *(vals+5);
        r02 = *(vals+6);
        r03 = *(vals+7);

        r04 = *(min_vals+4);
        r05 = *(min_vals+5);
        r06 = *(min_vals+6);
        r07 = *(min_vals+7);
        r08 = *(max_vals+4);
        r09 = *(max_vals+5);
        r10 = *(max_vals+6);
        r11 = *(max_vals+7);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals+4) = r04;
        *(min_vals+5) = r05;
        *(min_vals+6) = r06;
        *(min_vals+7) = r07;
        *(max_vals+4) = r08;
        *(max_vals+5) = r09;
        *(max_vals+6) = r10;
        *(max_vals+7) = r11;

        r00 = *(vals+8);
        r01 = *(vals+9);
        r02 = *(vals+10);
        r03 = *(vals+11);

        r04 = *(min_vals+8);
        r05 = *(min_vals+9);
        r06 = *(min_vals+10);
        r07 = *(min_vals+11);
        r08 = *(max_vals+8);
        r09 = *(max_vals+9);
        r10 = *(max_vals+10);
        r11 = *(max_vals+11);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals+8) = r04;
        *(min_vals+9) = r05;
        *(min_vals+10) = r06;
        *(min_vals+11) = r07;
        *(max_vals+8) = r08;
        *(max_vals+9) = r09;
        *(max_vals+10) = r10;
        *(max_vals+11) = r11;

        r00 = *(vals+12);
        r01 = *(vals+13);
        r02 = *(vals+14);
        r03 = *(vals+15);

        r04 = *(min_vals+12);
        r05 = *(min_vals+13);
        r06 = *(min_vals+14);
        r07 = *(min_vals+15);
        r08 = *(max_vals+12);
        r09 = *(max_vals+13);
        r10 = *(max_vals+14);
        r11 = *(max_vals+15);

        r04 = minval_r8__(r00, r04);
        r05 = minval_r8__(r01, r05);
        r06 = minval_r8__(r02, r06);
        r07 = minval_r8__(r03, r07);
        r08 = maxval_r8__(r00, r08);
        r09 = maxval_r8__(r01, r09);
        r10 = maxval_r8__(r02, r10);
        r11 = maxval_r8__(r03, r11);

        *(min_vals+12) = r04;
        *(min_vals+13) = r05;
        *(min_vals+14) = r06;
        *(min_vals+15) = r07;
        *(max_vals+12) = r08;
        *(max_vals+13) = r09;
        *(max_vals+14) = r10;
        *(max_vals+15) = r11;

        vals+=unroll;
        min_vals+=unroll;
        max_vals+=unroll;
    }

    while(n_remain--){
        r00 = *(vals);

        r01 = *(min_vals);
        r02 = *(max_vals);

        r01 = minval_r8__(r00, r01);
        r02 = maxval_r8__(r00, r02);

        *(min_vals)=r01;
        *(max_vals)=r02;

        vals+=1;
        min_vals+=1;
        max_vals+=1;
    }

}


void get_minmax_vector2vector_02x_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=2, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[val]), %%xmm0 \n\t"
            "movupd 0*8(%[min]), %%xmm1 \n\t"
            "movupd 0*8(%[max]), %%xmm2 \n\t"
            "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
            "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
            "movupd %%xmm1, 0*8(%[min]) \n\t"
            "movupd %%xmm2, 0*8(%[max]) \n\t"
            "subq $-2*8, %[val] \n\t"
            "subq $-2*8, %[min] \n\t"
            "subq $-2*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }

}


void get_minmax_vector2vector_04x_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "movupd 0*8(%[val]), %%xmm0 \n\t"
            "movupd 0*8(%[min]), %%xmm1 \n\t"
            "movupd 0*8(%[max]), %%xmm2 \n\t"
            "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
            "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
            "movupd %%xmm1, 0*8(%[min]) \n\t"
            "movupd %%xmm2, 0*8(%[max]) \n\t"
            "movupd 2*8(%[val]), %%xmm3 \n\t"
            "movupd 2*8(%[min]), %%xmm4 \n\t"
            "movupd 2*8(%[max]), %%xmm5 \n\t"
            "vminpd %%xmm3, %%xmm4, %%xmm4 \n\t"
            "vmaxpd %%xmm3, %%xmm5, %%xmm5 \n\t"
            "movupd %%xmm4, 2*8(%[min]) \n\t"
            "movupd %%xmm5, 2*8(%[max]) \n\t"
            "subq $-4*8, %[val] \n\t"
            "subq $-4*8, %[min] \n\t"
            "subq $-4*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }
}


void get_minmax_vector2vector_04y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=4, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%ymm0 \n\t"
            "vmovupd 0*8(%[min]), %%ymm1 \n\t"
            "vmovupd 0*8(%[max]), %%ymm2 \n\t"
            "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
            "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vmovupd %%ymm1, 0*8(%[min]) \n\t"
            "vmovupd %%ymm2, 0*8(%[max]) \n\t"
            "subq $-4*8, %[val] \n\t"
            "subq $-4*8, %[min] \n\t"
            "subq $-4*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }
}


void get_minmax_vector2vector_08y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%ymm0 \n\t"
            "vmovupd 0*8(%[min]), %%ymm1 \n\t"
            "vmovupd 0*8(%[max]), %%ymm2 \n\t"
            "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
            "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vmovupd %%ymm1, 0*8(%[min]) \n\t"
            "vmovupd %%ymm2, 0*8(%[max]) \n\t"
            "vmovupd 4*8(%[val]), %%ymm3 \n\t"
            "vmovupd 4*8(%[min]), %%ymm4 \n\t"
            "vmovupd 4*8(%[max]), %%ymm5 \n\t"
            "vminpd %%ymm3, %%ymm4, %%ymm4 \n\t"
            "vmaxpd %%ymm3, %%ymm5, %%ymm5 \n\t"
            "vmovupd %%ymm4, 4*8(%[min]) \n\t"
            "vmovupd %%ymm5, 4*8(%[max]) \n\t"
            "subq $-8*8, %[val] \n\t"
            "subq $-8*8, %[min] \n\t"
            "subq $-8*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%ymm0 \n\t"
                "vmovupd 0*8(%[min]), %%ymm1 \n\t"
                "vmovupd 0*8(%[max]), %%ymm2 \n\t"
                "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
                "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vmovupd %%ymm1, 0*8(%[min]) \n\t"
                "vmovupd %%ymm2, 0*8(%[max]) \n\t"
                "subq $-4*8, %[val] \n\t"
                "subq $-4*8, %[min] \n\t"
                "subq $-4*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }
}


void get_minmax_vector2vector_08z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=8, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%zmm0 \n\t"
            "vmovupd 0*8(%[min]), %%zmm1 \n\t"
            "vmovupd 0*8(%[max]), %%zmm2 \n\t"
            "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
            "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vmovupd %%zmm1, 0*8(%[min]) \n\t"
            "vmovupd %%zmm2, 0*8(%[max]) \n\t"
            "subq $-8*8, %[val] \n\t"
            "subq $-8*8, %[min] \n\t"
            "subq $-8*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }


    if(n_remain>0){
        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%ymm0 \n\t"
                "vmovupd 0*8(%[min]), %%ymm1 \n\t"
                "vmovupd 0*8(%[max]), %%ymm2 \n\t"
                "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
                "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vmovupd %%ymm1, 0*8(%[min]) \n\t"
                "vmovupd %%ymm2, 0*8(%[max]) \n\t"
                "subq $-4*8, %[val] \n\t"
                "subq $-4*8, %[min] \n\t"
                "subq $-4*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }

}


void get_minmax_vector2vector_16y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%ymm0 \n\t"
            "vmovupd 0*8(%[min]), %%ymm1 \n\t"
            "vmovupd 0*8(%[max]), %%ymm2 \n\t"
            "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
            "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vmovupd %%ymm1, 0*8(%[min]) \n\t"
            "vmovupd %%ymm2, 0*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 4*8(%[val]), %%ymm3 \n\t"
            "vmovupd 4*8(%[min]), %%ymm4 \n\t"
            "vmovupd 4*8(%[max]), %%ymm5 \n\t"
            "vminpd %%ymm3, %%ymm4, %%ymm4 \n\t"
            "vmaxpd %%ymm3, %%ymm5, %%ymm5 \n\t"
            "vmovupd %%ymm4, 4*8(%[min]) \n\t"
            "vmovupd %%ymm5, 4*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 8*8(%[val]), %%ymm6 \n\t"
            "vmovupd 8*8(%[min]), %%ymm7 \n\t"
            "vmovupd 8*8(%[max]), %%ymm8 \n\t"
            "vminpd %%ymm6, %%ymm7, %%ymm7 \n\t"
            "vmaxpd %%ymm6, %%ymm8, %%ymm8 \n\t"
            "vmovupd %%ymm7, 8*8(%[min]) \n\t"
            "vmovupd %%ymm8, 8*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 12*8(%[val]), %%ymm9 \n\t"
            "vmovupd 12*8(%[min]), %%ymm10 \n\t"
            "vmovupd 12*8(%[max]), %%ymm11 \n\t"
            "vminpd %%ymm9, %%ymm10, %%ymm10 \n\t"
            "vmaxpd %%ymm9, %%ymm11, %%ymm11 \n\t"
            "vmovupd %%ymm10, 12*8(%[min]) \n\t"
            "vmovupd %%ymm11, 12*8(%[max]) \n\t"
            "\n\t"
            "subq $-16*8, %[val] \n\t"
            "subq $-16*8, %[min] \n\t"
            "subq $-16*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%zmm0 \n\t"
                "vmovupd 0*8(%[min]), %%zmm1 \n\t"
                "vmovupd 0*8(%[max]), %%zmm2 \n\t"
                "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
                "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                "vmovupd %%zmm1, 0*8(%[min]) \n\t"
                "vmovupd %%zmm2, 0*8(%[max]) \n\t"
                "subq $-8*8, %[val] \n\t"
                "subq $-8*8, %[min] \n\t"
                "subq $-8*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%ymm0 \n\t"
                "vmovupd 0*8(%[min]), %%ymm1 \n\t"
                "vmovupd 0*8(%[max]), %%ymm2 \n\t"
                "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
                "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vmovupd %%ymm1, 0*8(%[min]) \n\t"
                "vmovupd %%ymm2, 0*8(%[max]) \n\t"
                "subq $-4*8, %[val] \n\t"
                "subq $-4*8, %[min] \n\t"
                "subq $-4*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }

}


void get_minmax_vector2vector_16z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=16, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%zmm0 \n\t"
            "vmovupd 0*8(%[min]), %%zmm1 \n\t"
            "vmovupd 0*8(%[max]), %%zmm2 \n\t"
            "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
            "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vmovupd %%zmm1, 0*8(%[min]) \n\t"
            "vmovupd %%zmm2, 0*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 8*8(%[val]), %%zmm3 \n\t"
            "vmovupd 8*8(%[min]), %%zmm4 \n\t"
            "vmovupd 8*8(%[max]), %%zmm5 \n\t"
            "vminpd %%zmm3, %%zmm4, %%zmm4 \n\t"
            "vmaxpd %%zmm3, %%zmm5, %%zmm5 \n\t"
            "vmovupd %%zmm4, 8*8(%[min]) \n\t"
            "vmovupd %%zmm5, 8*8(%[max]) \n\t"
            "\n\t"
            "subq $-16*8, %[val] \n\t"
            "subq $-16*8, %[min] \n\t"
            "subq $-16*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%zmm0 \n\t"
                "vmovupd 0*8(%[min]), %%zmm1 \n\t"
                "vmovupd 0*8(%[max]), %%zmm2 \n\t"
                "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
                "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                "vmovupd %%zmm1, 0*8(%[min]) \n\t"
                "vmovupd %%zmm2, 0*8(%[max]) \n\t"
                "subq $-8*8, %[val] \n\t"
                "subq $-8*8, %[min] \n\t"
                "subq $-8*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%ymm0 \n\t"
                "vmovupd 0*8(%[min]), %%ymm1 \n\t"
                "vmovupd 0*8(%[max]), %%ymm2 \n\t"
                "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
                "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vmovupd %%ymm1, 0*8(%[min]) \n\t"
                "vmovupd %%ymm2, 0*8(%[max]) \n\t"
                "subq $-4*8, %[val] \n\t"
                "subq $-4*8, %[min] \n\t"
                "subq $-4*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }

}

void get_minmax_vector2vector_32z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n){
    int64_t unroll=32, n_unroll=n/unroll, n_remain=n%unroll;

    while(n_unroll--){
        __asm__ __volatile__ (
            "vmovupd 0*8(%[val]), %%zmm0 \n\t"
            "vmovupd 0*8(%[min]), %%zmm1 \n\t"
            "vmovupd 0*8(%[max]), %%zmm2 \n\t"
            "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
            "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vmovupd %%zmm1, 0*8(%[min]) \n\t"
            "vmovupd %%zmm2, 0*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 8*8(%[val]), %%zmm3 \n\t"
            "vmovupd 8*8(%[min]), %%zmm4 \n\t"
            "vmovupd 8*8(%[max]), %%zmm5 \n\t"
            "vminpd %%zmm3, %%zmm4, %%zmm4 \n\t"
            "vmaxpd %%zmm3, %%zmm5, %%zmm5 \n\t"
            "vmovupd %%zmm4, 8*8(%[min]) \n\t"
            "vmovupd %%zmm5, 8*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 16*8(%[val]), %%zmm6 \n\t"
            "vmovupd 16*8(%[min]), %%zmm7 \n\t"
            "vmovupd 16*8(%[max]), %%zmm8 \n\t"
            "vminpd %%zmm6, %%zmm7, %%zmm7 \n\t"
            "vmaxpd %%zmm6, %%zmm8, %%zmm8 \n\t"
            "vmovupd %%zmm7, 16*8(%[min]) \n\t"
            "vmovupd %%zmm8, 16*8(%[max]) \n\t"
            "\n\t"
            "vmovupd 24*8(%[val]), %%zmm9 \n\t"
            "vmovupd 24*8(%[min]), %%zmm10 \n\t"
            "vmovupd 24*8(%[max]), %%zmm11 \n\t"
            "vminpd %%zmm9, %%zmm10, %%zmm10 \n\t"
            "vmaxpd %%zmm9, %%zmm11, %%zmm11 \n\t"
            "vmovupd %%zmm10, 24*8(%[min]) \n\t"
            "vmovupd %%zmm11, 24*8(%[max]) \n\t"
            "\n\t"
            "subq $-32*8, %[val] \n\t"
            "subq $-32*8, %[min] \n\t"
            "subq $-32*8, %[max] \n\t"
            :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
            :"0"(vals), "1"(min_vals), "2"(max_vals)
        );
    }

    if(n_remain>0){
        if(n_remain&16){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%zmm0 \n\t"
                "vmovupd 0*8(%[min]), %%zmm1 \n\t"
                "vmovupd 0*8(%[max]), %%zmm2 \n\t"
                "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
                "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                "vmovupd %%zmm1, 0*8(%[min]) \n\t"
                "vmovupd %%zmm2, 0*8(%[max]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[val]), %%zmm3 \n\t"
                "vmovupd 8*8(%[min]), %%zmm4 \n\t"
                "vmovupd 8*8(%[max]), %%zmm5 \n\t"
                "vminpd %%zmm3, %%zmm4, %%zmm4 \n\t"
                "vmaxpd %%zmm3, %%zmm5, %%zmm5 \n\t"
                "vmovupd %%zmm4, 8*8(%[min]) \n\t"
                "vmovupd %%zmm5, 8*8(%[max]) \n\t"
                "\n\t"
                "subq $-16*8, %[val] \n\t"
                "subq $-16*8, %[min] \n\t"
                "subq $-16*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&8){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%zmm0 \n\t"
                "vmovupd 0*8(%[min]), %%zmm1 \n\t"
                "vmovupd 0*8(%[max]), %%zmm2 \n\t"
                "vminpd %%zmm0, %%zmm1, %%zmm1 \n\t"
                "vmaxpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                "vmovupd %%zmm1, 0*8(%[min]) \n\t"
                "vmovupd %%zmm2, 0*8(%[max]) \n\t"
                "subq $-8*8, %[val] \n\t"
                "subq $-8*8, %[min] \n\t"
                "subq $-8*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&4){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[val]), %%ymm0 \n\t"
                "vmovupd 0*8(%[min]), %%ymm1 \n\t"
                "vmovupd 0*8(%[max]), %%ymm2 \n\t"
                "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
                "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vmovupd %%ymm1, 0*8(%[min]) \n\t"
                "vmovupd %%ymm2, 0*8(%[max]) \n\t"
                "subq $-4*8, %[val] \n\t"
                "subq $-4*8, %[min] \n\t"
                "subq $-4*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&2){
            __asm__ __volatile__ (
                "movupd 0*8(%[val]), %%xmm0 \n\t"
                "movupd 0*8(%[min]), %%xmm1 \n\t"
                "movupd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movupd %%xmm1, 0*8(%[min]) \n\t"
                "movupd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-2*8, %[val] \n\t"
                "subq $-2*8, %[min] \n\t"
                "subq $-2*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }

        if(n_remain&1){
            __asm__ __volatile__ (
                "movsd 0*8(%[val]), %%xmm0 \n\t"
                "movsd 0*8(%[min]), %%xmm1 \n\t"
                "movsd 0*8(%[max]), %%xmm2 \n\t"
                "vminpd %%xmm0, %%xmm1, %%xmm1 \n\t"
                "vmaxpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "movsd %%xmm1, 0*8(%[min]) \n\t"
                "movsd %%xmm2, 0*8(%[max]) \n\t"
                "subq $-1*8, %[val] \n\t"
                "subq $-1*8, %[min] \n\t"
                "subq $-1*8, %[max] \n\t"
                :[val]"=r"(vals), [min]"=r"(min_vals), [max]"=r"(max_vals)
                :"0"(vals), "1"(min_vals), "2"(max_vals)
            );
        }
    }
}