#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
double maxval_r8(double x, double y){
	if (x > y){
		return x;
	}else{
		return y;
	}
}

double minval_r8(double x, double y){
	if (x < y){
		return x;
	}else{
		return y;
	}
}

int64_t maxval_i8(int64_t x, int64_t y){
	if (x > y){
		return x;
	}else{
		return y;
	}
}

int64_t minval_i8(int64_t x, int64_t y){
	if (x < y){
		return x;
	}else{
		return y;
	}
}

void minmax_loop_c_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	int64_t i;
    double tmp;
	for(i=0; i<n; i++){
        tmp = x[i];
		*min = minval_r8(tmp, *min);
		*max = maxval_r8(tmp, *max);
	}
}

void minmax_loop_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t i;
    int64_t tmp;
	for(i=0; i<n; i++){
        tmp = x[i];
		*min = minval_i8(tmp, *min);
		*max = maxval_i8(tmp, *max);
	}
}

void minmax_loop_unroll_04_c_r8(double *min, double *max, double x[], int64_t n){
    if(n<4){
        minmax_loop_c_r8(min, max, x, n);
        return;
    }  

    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	int64_t i, n_unroll;
    double r00, r01, r02, r03;
    double r04, r05;

    n_unroll=n>>2;
    r04 = HUGE_VAL;
    r05 = -HUGE_VAL;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);

		r04 = minval_r8(r00, r04);
		r04 = minval_r8(r01, r04);
		r04 = minval_r8(r02, r04);
		r04 = minval_r8(r03, r04);

		r05 = maxval_r8(r00, r05);
		r05 = maxval_r8(r01, r05);
		r05 = maxval_r8(r02, r05);
		r05 = maxval_r8(r03, r05);
        x+=4;
	}

    n_unroll=n%4;
    while(n_unroll--){
        r00 = *(x);
		r04 = minval_r8(r00, r04);
		r05 = maxval_r8(r00, r05);
        x+=1;
    }

    *min = r04;
    *max = r05;
}

void minmax_loop_unroll_04_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    if(n<4){
        minmax_loop_c_i8(min, max, x, n);
        return;
    } 
    
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t i, n_unroll;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05;

    n_unroll=n>>2;
    r04 = INT64_MAX;
    r05 = INT64_MIN;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);

		r04 = minval_i8(r00, r04);
		r04 = minval_i8(r01, r04);
		r04 = minval_i8(r02, r04);
		r04 = minval_i8(r03, r04);

		r05 = maxval_i8(r00, r05);
		r05 = maxval_i8(r01, r05);
		r05 = maxval_i8(r02, r05);
		r05 = maxval_i8(r03, r05);
        x+=4;
	}

    n_unroll=n%4;
    while(n_unroll--){
        r00 = *(x);
		r04 = minval_i8(r00, r04);
		r05 = maxval_i8(r00, r05);
        x+=1;
    }

    *min = r04;
    *max = r05;
}

void minmax_loop_unroll_08_c_r8(double *min, double *max, double x[], int64_t n){
    if(n<8){
        minmax_loop_c_r8(min, max, x, n);
        return;
    }

    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	int64_t i, n_unroll;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    n_unroll=n>>3;
    r14 = HUGE_VAL;
    r15 = -HUGE_VAL;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);

		r14 = minval_r8(r00, r14);
		r14 = minval_r8(r01, r14);
		r14 = minval_r8(r02, r14);
		r14 = minval_r8(r03, r14);
		r14 = minval_r8(r04, r14);
		r14 = minval_r8(r05, r14);
		r14 = minval_r8(r06, r14);
		r14 = minval_r8(r07, r14);

		r15 = maxval_r8(r00, r15);
		r15 = maxval_r8(r01, r15);
		r15 = maxval_r8(r02, r15);
		r15 = maxval_r8(r03, r15);
		r15 = maxval_r8(r04, r15);
		r15 = maxval_r8(r05, r15);
		r15 = maxval_r8(r06, r15);
		r15 = maxval_r8(r07, r15);
        x+=8;
	}

    n_unroll=n%8;
    while(n_unroll--){
        r00 = *(x);
		r14 = minval_r8(r00, r14);
		r15 = maxval_r8(r00, r15);
        x+=1;
    }

    *min = r14;
    *max = r15;
}

void minmax_loop_unroll_08_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    if(n<8){
        minmax_loop_c_i8(min, max, x, n);
        return;
    }

    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t i, n_unroll;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    n_unroll=n>>2;
    r04 = INT64_MAX;
    r05 = INT64_MIN;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);

		r14 = minval_i8(r00, r14);
		r14 = minval_i8(r01, r14);
		r14 = minval_i8(r02, r14);
		r14 = minval_i8(r03, r14);
		r14 = minval_i8(r04, r14);
		r14 = minval_i8(r05, r14);
		r14 = minval_i8(r06, r14);
		r14 = minval_i8(r07, r14);

		r15 = maxval_i8(r00, r15);
		r15 = maxval_i8(r01, r15);
		r15 = maxval_i8(r02, r15);
		r15 = maxval_i8(r03, r15);
		r15 = maxval_i8(r04, r15);
		r15 = maxval_i8(r05, r15);
		r15 = maxval_i8(r06, r15);
		r15 = maxval_i8(r07, r15);
        x+=8;
	}

    n_unroll=n%8;
    while(n_unroll--){
        r00 = *(x);
		r14 = minval_i8(r00, r14);
		r15 = maxval_i8(r00, r15);
        x+=1;
    }

    *min = r14;
    *max = r15;
}

void minmax_loop_unroll_14_c_r8(double *min, double *max, double x[], int64_t n){
    if(n<14){
        minmax_loop_c_r8(min, max, x, n);
        return;
    }

    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	int64_t i, n_unroll;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    n_unroll=n/14;
    r14 = HUGE_VAL;
    r15 = -HUGE_VAL;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);
        r08 = *(x+8);
        r09 = *(x+9);
        r10 = *(x+10);
        r11 = *(x+11);
        r12 = *(x+12);
        r13 = *(x+13);

		r14 = minval_r8(r00, r14);
		r14 = minval_r8(r01, r14);
		r14 = minval_r8(r02, r14);
		r14 = minval_r8(r03, r14);
		r14 = minval_r8(r04, r14);
		r14 = minval_r8(r05, r14);
		r14 = minval_r8(r06, r14);
		r14 = minval_r8(r07, r14);
		r14 = minval_r8(r08, r14);
		r14 = minval_r8(r09, r14);
		r14 = minval_r8(r10, r14);
		r14 = minval_r8(r11, r14);
		r14 = minval_r8(r12, r14);
		r14 = minval_r8(r13, r14);

		r15 = maxval_r8(r00, r15);
		r15 = maxval_r8(r01, r15);
		r15 = maxval_r8(r02, r15);
		r15 = maxval_r8(r03, r15);
		r15 = maxval_r8(r04, r15);
		r15 = maxval_r8(r05, r15);
		r15 = maxval_r8(r06, r15);
		r15 = maxval_r8(r07, r15);
		r15 = maxval_r8(r08, r15);
		r15 = maxval_r8(r09, r15);
		r15 = maxval_r8(r10, r15);
		r15 = maxval_r8(r11, r15);
		r15 = maxval_r8(r12, r15);
		r15 = maxval_r8(r13, r15);
        x+=14;
	}

    n_unroll=n%14;
    while(n_unroll--){
        r00 = *(x);
		r14 = minval_r8(r00, r14);
		r15 = maxval_r8(r00, r15);
        x+=1;
    }

    *min = r14;
    *max = r15;
}

void minmax_loop_unroll_14_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    if(n<14){
        minmax_loop_c_i8(min, max, x, n);
        return;
    }

    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t i, n_unroll;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    n_unroll=n/14;
    r04 = INT64_MAX;
    r05 = INT64_MIN;

	while(n_unroll--){
        r00 = *(x);
        r01 = *(x+1);
        r02 = *(x+2);
        r03 = *(x+3);
        r04 = *(x+4);
        r05 = *(x+5);
        r06 = *(x+6);
        r07 = *(x+7);
        r08 = *(x+8);
        r09 = *(x+9);
        r10 = *(x+10);
        r11 = *(x+11);
        r12 = *(x+12);
        r13 = *(x+13);

		r14 = minval_i8(r00, r14);
		r14 = minval_i8(r01, r14);
		r14 = minval_i8(r02, r14);
		r14 = minval_i8(r03, r14);
		r14 = minval_i8(r04, r14);
		r14 = minval_i8(r05, r14);
		r14 = minval_i8(r06, r14);
		r14 = minval_i8(r07, r14);
		r14 = minval_i8(r08, r14);
		r14 = minval_i8(r09, r14);
		r14 = minval_i8(r10, r14);
		r14 = minval_i8(r11, r14);
		r14 = minval_i8(r12, r14);
		r14 = minval_i8(r13, r14);

		r15 = maxval_i8(r00, r15);
		r15 = maxval_i8(r01, r15);
		r15 = maxval_i8(r02, r15);
		r15 = maxval_i8(r03, r15);
		r15 = maxval_i8(r04, r15);
		r15 = maxval_i8(r05, r15);
		r15 = maxval_i8(r06, r15);
		r15 = maxval_i8(r07, r15);
		r15 = maxval_i8(r08, r15);
		r15 = maxval_i8(r09, r15);
		r15 = maxval_i8(r10, r15);
		r15 = maxval_i8(r11, r15);
		r15 = maxval_i8(r12, r15);
		r15 = maxval_i8(r13, r15);
        x+=14;
	}

    n_unroll=n%14;
    while(n_unroll--){
        r00 = *(x);
		r14 = minval_i8(r00, r14);
		r15 = maxval_i8(r00, r15);
        x+=1;
    }

    *min = r14;
    *max = r15;
}

void minmax_loop_unroll_02_ams_r8(double *min, double *max, double x[], int64_t n){
	if (n<2){
		*min=x[0];
        *max=x[0];
        return;
	}

	int64_t n_unroll=(n>>1), n_mod, i;
    double value_min, value_max;
    value_min = HUGE_VAL;
    value_max = - HUGE_VAL;

    __asm__ __volatile__ (
        "movupd  0*8(%[x]), %%xmm0 \n\t"
        "VBROADCASTSD %[v_max], %%ymm1 \n\t"
        "VBROADCASTSD %[v_min], %%ymm2 \n\t"
        "subq $-2*8, %[x] \n\t"
        :[x]"=r"(x), [v_max]"=m"(value_max), [v_min]"=m"(value_min)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "maxpd %%xmm0, %%xmm1 \n\t"
            "minpd %%xmm0, %%xmm2 \n\t"
            "movupd  0*8(%[x]), %%xmm0 \n\t"
            "\n\t"
            "subq $-2*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
		"maxpd            %%xmm0, %%xmm1 \n\t"
		"minpd            %%xmm0, %%xmm2 \n\t"
		"\n\t"
        "VBROADCASTSD %[v_min], %%ymm4 \n\t"
        "minpd %%xmm2, %%xmm4          \n\t"
        "PSRLDQ $1*8,  %%xmm2          \n\t"
        "minpd %%xmm2, %%xmm4          \n\t"
        "movsd %%xmm4, %[v_min]        \n\t"
		"\n\t"
        "VBROADCASTSD %[v_max], %%ymm3 \n\t"
        "maxpd %%xmm1, %%xmm3          \n\t"
        "PSRLDQ $1*8,  %%xmm1          \n\t"
        "maxpd %%xmm1, %%xmm3          \n\t"
        "movsd %%xmm3, %[v_max]        \n\t"
		"\n\t"
        :[x]"=r"(x), [v_max]"=m"(value_max), [v_min]"=m"(value_min)
        :"0"(x)
	);

    double tmp=*(x);
	*min = minval_r8(value_min, tmp);
	*max = maxval_r8(value_max, tmp);
}

void minmax_loop_unroll_02_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<2){
		*min=x[0];
        *max=x[0];
        return;
	}

	int64_t n_unroll=(n>>1), n_mod, i;
    int64_t value_min, value_max;
    value_min = INT64_MAX;
    value_max = INT64_MIN;

    __asm__ __volatile__ (
        "MOVDQU   0*8(%[x]), %%xmm0 \n\t"
        "VBROADCASTSD %[v_max], %%ymm1 \n\t"
        "VBROADCASTSD %[v_min], %%ymm2 \n\t"
        "subq $-2*8, %[x] \n\t"
        :[x]"=r"(x), [v_max]"=m"(value_max), [v_min]"=m"(value_min)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
            "VPMINSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
            "MOVDQU   0*8(%[x]), %%xmm0 \n\t"
            "\n\t"
            "subq $-2*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
        "VPMAXSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
        "VPMINSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
		"\n\t"
        "VBROADCASTSD %[v_max], %%ymm3  \n\t"
        "VPMAXSQ %%xmm1, %%xmm3, %%xmm3 \n\t"
        "PSRLDQ $1*8,  %%xmm1           \n\t"
        "VPMAXSQ %%xmm1, %%xmm3, %%xmm3 \n\t"
        "movq %%xmm3, %[v_max]          \n\t"
		"\n\t"
        "VBROADCASTSD %[v_min], %%ymm4  \n\t"
        "VPMINSQ %%xmm2, %%xmm4, %%xmm4 \n\t"
        "PSRLDQ $1*8,  %%xmm2           \n\t"
        "VPMINSQ %%xmm2, %%xmm4, %%xmm4 \n\t"
        "movq %%xmm4, %[v_min]          \n\t"
		"\n\t"
        :[x]"=r"(x), [v_max]"=m"(value_max), [v_min]"=m"(value_min)
        :"0"(x)
	);

    int64_t tmp=*(x);
	*min = minval_i8(value_min, tmp);
	*max = maxval_i8(value_max, tmp);
}

void minmax_loop_unroll_04_ams_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	double value_min, value_max;

	int64_t n_unroll=(n>>2), n_mod, i;
	
	if (n<=4){
		minmax_loop_c_r8(min, max, x, n);
        return;
	}

	n_mod = n%4;
    for (i=n-n_mod; i<n; i++){
        *min = minval_r8(x[i], *min);
        *max = maxval_r8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  0*8(%[x]), %%ymm1 \n\t"
        "vmovupd  0*8(%[x]), %%ymm2 \n\t"
        "subq $-4*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "vmaxpd %%ymm1, %%ymm0, %%ymm1 \n\t"
            "vminpd %%ymm2, %%ymm0, %%ymm2 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "subq $-4*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
		"vmaxpd            %%ymm1, %%ymm0, %%ymm1 \n\t"
		"vminpd            %%ymm2, %%ymm0, %%ymm2 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm1, %%ymm1, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm1, %%ymm1 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm1, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm1 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"vperm2f128 $0x01, %%ymm1, %%ymm1, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm1, %%ymm1 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm1, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm1 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movsd             %%xmm1, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm2 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm2 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movsd             %%xmm2, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_r8(value_min, *min);
	*max = maxval_r8(value_max, *max);
}

void minmax_loop_unroll_04_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t value_min, value_max;

	int64_t n_unroll=(n>>2), n_mod, i;
	
	if (n<4){
		minmax_loop_c_i8(min, max, x, n);
        return;
	}

	n_mod = n%4;
    for (i=n-n_mod; i<n; i++){
        *min = minval_i8(x[i], *min);
        *max = maxval_i8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm2 \n\t"
        "subq $-4*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%ymm1, %%ymm0, %%ymm1 \n\t"
            "VPMINSQ %%ymm2, %%ymm0, %%ymm2 \n\t"
            "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "subq $-4*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
		"VPMAXSQ           %%ymm1, %%ymm0, %%ymm1 \n\t"
		"VPMINSQ           %%ymm2, %%ymm0, %%ymm2 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm1, %%ymm1, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMAXSQ           %%ymm0, %%ymm1, %%ymm1 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm1, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"VPMAXSQ           %%xmm0, %%xmm1, %%xmm1 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movq              %%xmm1, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMINSQ           %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"VPMINSQ           %%xmm0, %%xmm2, %%xmm2 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movq              %%xmm2, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_i8(value_min, *min);
	*max = maxval_i8(value_max, *max);
}

void minmax_loop_unroll_08_ams_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	double value_min, value_max;

	int64_t n_unroll=(n>>3), n_mod, i;
	
	if (n<8){
		minmax_loop_c_r8(min, max, x, n);
        return;
	}

	n_mod = n%8;
    for (i=n-n_mod; i<n; i++){
        *min = minval_r8(x[i], *min);
        *max = maxval_r8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  4*8(%[x]), %%ymm1 \n\t"
        "vmovupd  0*8(%[x]), %%ymm2 \n\t"
        "vmovupd  0*8(%[x]), %%ymm3 \n\t"
        "subq $-8*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "vmaxpd %%ymm2, %%ymm0, %%ymm2 \n\t"
            "vminpd %%ymm3, %%ymm0, %%ymm3 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "vmaxpd %%ymm2, %%ymm1, %%ymm2 \n\t"
            "vminpd %%ymm3, %%ymm1, %%ymm3 \n\t"
            "vmovupd  4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
		"vmaxpd            %%ymm2, %%ymm0, %%ymm2 \n\t"
		"vminpd            %%ymm3, %%ymm0, %%ymm3 \n\t"
		"\n\t"
		"vmaxpd            %%ymm2, %%ymm1, %%ymm2 \n\t"
		"vminpd            %%ymm3, %%ymm1, %%ymm3 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm2 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movsd             %%xmm2, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm3, %%ymm3, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm3, %%ymm3 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm3, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm3 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movsd             %%xmm3, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_r8(value_min, *min);
	*max = maxval_r8(value_max, *max);
}

void minmax_loop_unroll_08_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t value_min, value_max;

	int64_t n_unroll=(n>>3), n_mod, i;
	
	if (n<8){
		minmax_loop_c_i8(min, max, x, n);
        return;
	}

	n_mod = n%8;
    for (i=n-n_mod; i<n; i++){
        *min = minval_i8(x[i], *min);
        *max = maxval_i8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  4*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm2 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm3 \n\t"
        "subq $-8*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%ymm2, %%ymm0, %%ymm2 \n\t"
            "VPMINSQ %%ymm3, %%ymm0, %%ymm3 \n\t"
            "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm2, %%ymm1, %%ymm2 \n\t"
            "VPMINSQ %%ymm3, %%ymm1, %%ymm3 \n\t"
            "vMOVDQU   4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
		"\n\t"
		"VPMAXSQ           %%ymm2, %%ymm0, %%ymm2 \n\t"
		"VPMINSQ           %%ymm3, %%ymm0, %%ymm3 \n\t"
		"\n\t"
		"VPMAXSQ           %%ymm2, %%ymm1, %%ymm2 \n\t"
		"VPMINSQ           %%ymm3, %%ymm1, %%ymm3 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMAXSQ           %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"VPMAXSQ           %%xmm0, %%xmm2, %%xmm2 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movq              %%xmm2, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm3, %%ymm3, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMINSQ           %%ymm0, %%ymm3, %%ymm3 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm3, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"VPMINSQ           %%xmm0, %%xmm3, %%xmm3 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movq              %%xmm3, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_i8(value_min, *min);
	*max = maxval_i8(value_max, *max);
}

void minmax_loop_unroll_16_ams_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	double value_min, value_max;

	int64_t n_unroll=(n>>4), n_mod, i;
	
	if (n<=16){
		minmax_loop_c_r8(min, max, x, n);
        return;
	}

	n_mod = n%16;
    for (i=n-n_mod; i<n; i++){
        *min = minval_r8(x[i], *min);
        *max = maxval_r8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  4*8(%[x]), %%ymm1 \n\t"
        "vmovupd  8*8(%[x]), %%ymm2 \n\t"
        "vmovupd 12*8(%[x]), %%ymm3 \n\t"
        "vmovupd  0*8(%[x]), %%ymm4 \n\t"
        "vmovupd  0*8(%[x]), %%ymm5 \n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "vmaxpd %%ymm4, %%ymm0, %%ymm4 \n\t"
            "vminpd %%ymm5, %%ymm0, %%ymm5 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "vmaxpd %%ymm4, %%ymm1, %%ymm4 \n\t"
            "vminpd %%ymm5, %%ymm1, %%ymm5 \n\t"
            "vmovupd  4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "vmaxpd %%ymm4, %%ymm2, %%ymm4 \n\t"
            "vminpd %%ymm5, %%ymm2, %%ymm5 \n\t"
            "vmovupd  8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "vmaxpd %%ymm4, %%ymm3, %%ymm4 \n\t"
            "vminpd %%ymm5, %%ymm3, %%ymm5 \n\t"
            "vmovupd 12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "vmaxpd %%ymm4, %%ymm0, %%ymm4 \n\t"
        "vminpd %%ymm5, %%ymm0, %%ymm5 \n\t"
        "\n\t"
        "vmaxpd %%ymm4, %%ymm1, %%ymm4 \n\t"
        "vminpd %%ymm5, %%ymm1, %%ymm5 \n\t"
        "\n\t"
        "vmaxpd %%ymm4, %%ymm2, %%ymm4 \n\t"
        "vminpd %%ymm5, %%ymm2, %%ymm5 \n\t"
        "\n\t"
        "vmaxpd %%ymm4, %%ymm3, %%ymm4 \n\t"
        "vminpd %%ymm5, %%ymm3, %%ymm5 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm4, %%ymm4, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm4, %%ymm4 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm4, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm4 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movsd             %%xmm4, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm5, %%ymm5, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm5, %%ymm5 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm5, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm5 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movsd             %%xmm5, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_r8(value_min, *min);
	*max = maxval_r8(value_max, *max);
}

void minmax_loop_unroll_16_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t value_min, value_max;

	int64_t n_unroll=(n>>4), n_mod, i;
	
	if (n<16){
		minmax_loop_c_i8(min, max, x, n);
        return;
	}

	n_mod = n%16;
    for (i=n-n_mod; i<n; i++){
        *min = minval_i8(x[i], *min);
        *max = maxval_i8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  4*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  8*8(%[x]), %%ymm2 \n\t"
        "vMOVDQU 12*8(%[x]), %%ymm3 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm4 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm5 \n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%ymm4, %%ymm0, %%ymm4 \n\t"
            "VPMINSQ %%ymm5, %%ymm0, %%ymm5 \n\t"
            "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm4, %%ymm1, %%ymm4 \n\t"
            "VPMINSQ %%ymm5, %%ymm1, %%ymm5 \n\t"
            "vMOVDQU   4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm4, %%ymm2, %%ymm4 \n\t"
            "VPMINSQ %%ymm5, %%ymm2, %%ymm5 \n\t"
            "vMOVDQU   8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm4, %%ymm3, %%ymm4 \n\t"
            "VPMINSQ %%ymm5, %%ymm3, %%ymm5 \n\t"
            "vMOVDQU  12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "VPMAXSQ %%ymm4, %%ymm0, %%ymm4 \n\t"
        "VPMINSQ %%ymm5, %%ymm0, %%ymm5 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm4, %%ymm1, %%ymm4 \n\t"
        "VPMINSQ %%ymm5, %%ymm1, %%ymm5 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm4, %%ymm2, %%ymm4 \n\t"
        "VPMINSQ %%ymm5, %%ymm2, %%ymm5 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm4, %%ymm3, %%ymm4 \n\t"
        "VPMINSQ %%ymm5, %%ymm3, %%ymm5 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm4, %%ymm4, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMAXSQ           %%ymm0, %%ymm4, %%ymm4 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm4, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"VPMAXSQ           %%xmm0, %%xmm4, %%xmm4 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movq              %%xmm4, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm5, %%ymm5, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMINSQ           %%ymm0, %%ymm5, %%ymm5 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm5, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"VPMINSQ           %%xmm0, %%xmm5, %%xmm5 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movq              %%xmm5, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_i8(value_min, *min);
	*max = maxval_i8(value_max, *max);
}

void minmax_loop_unroll_32_ams_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	double value_min, value_max;

	int64_t n_unroll=(n>>5), n_mod, i;
	
	if (n<=32){
		minmax_loop_c_r8(min, max, x, n);
        return;
	}

	n_mod = n%32;
    for (i=n-n_mod; i<n; i++){
        *min = minval_r8(x[i], *min);
        *max = maxval_r8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  4*8(%[x]), %%ymm1 \n\t"
        "vmovupd  8*8(%[x]), %%ymm2 \n\t"
        "vmovupd 12*8(%[x]), %%ymm3 \n\t"
        "vmovupd 16*8(%[x]), %%ymm4 \n\t"
        "vmovupd 20*8(%[x]), %%ymm5 \n\t"
        "vmovupd 24*8(%[x]), %%ymm6 \n\t"
        "vmovupd 28*8(%[x]), %%ymm7 \n\t"
        "vmovupd  0*8(%[x]), %%ymm8 \n\t"
        "vmovupd  0*8(%[x]), %%ymm9 \n\t"
        "subq $-32*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "vmaxpd %%ymm8, %%ymm0, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm0, %%ymm9 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm1, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm1, %%ymm9 \n\t"
            "vmovupd  4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm2, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm2, %%ymm9 \n\t"
            "vmovupd  8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm3, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm3, %%ymm9 \n\t"
            "vmovupd 12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm4, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm4, %%ymm9 \n\t"
            "vmovupd 16*8(%[x]), %%ymm4 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm5, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm5, %%ymm9 \n\t"
            "vmovupd 20*8(%[x]), %%ymm5 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm6, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm6, %%ymm9 \n\t"
            "vmovupd 24*8(%[x]), %%ymm6 \n\t"
            "\n\t"
            "vmaxpd %%ymm8, %%ymm7, %%ymm8 \n\t"
            "vminpd %%ymm9, %%ymm7, %%ymm9 \n\t"
            "vmovupd 28*8(%[x]), %%ymm7 \n\t"
            "\n\t"
            "subq $-32*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "vmaxpd %%ymm8, %%ymm0, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm0, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm1, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm1, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm2, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm2, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm3, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm3, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm4, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm4, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm5, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm5, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm6, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm6, %%ymm9 \n\t"
        "\n\t"
        "vmaxpd %%ymm8, %%ymm7, %%ymm8 \n\t"
        "vminpd %%ymm9, %%ymm7, %%ymm9 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm8, %%ymm8, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm8, %%ymm8 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm8, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm8 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movsd             %%xmm8, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm9, %%ymm9, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm9, %%ymm9 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm9, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm9 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movsd             %%xmm9, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_r8(value_min, *min);
	*max = maxval_r8(value_max, *max);
}

void minmax_loop_unroll_32_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t value_min, value_max;

	int64_t n_unroll=(n>>5), n_mod, i;
	
	if (n<32){
		minmax_loop_c_i8(min, max, x, n);
        return;
	}

	n_mod = n%32;
    for (i=n-n_mod; i<n; i++){
        *min = minval_i8(x[i], *min);
        *max = maxval_i8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  4*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  8*8(%[x]), %%ymm2 \n\t"
        "vMOVDQU 12*8(%[x]), %%ymm3 \n\t"
        "vMOVDQU 16*8(%[x]), %%ymm4 \n\t"
        "vMOVDQU 20*8(%[x]), %%ymm5 \n\t"
        "vMOVDQU 24*8(%[x]), %%ymm6 \n\t"
        "vMOVDQU 28*8(%[x]), %%ymm7 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm8 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm9 \n\t"
        "subq $-32*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm0, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm0, %%ymm9 \n\t"
            "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm1, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm1, %%ymm9 \n\t"
            "vMOVDQU   4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm2, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm2, %%ymm9 \n\t"
            "vMOVDQU   8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm3, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm3, %%ymm9 \n\t"
            "vMOVDQU  12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm4, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm4, %%ymm9 \n\t"
            "vMOVDQU  16*8(%[x]), %%ymm4 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm5, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm5, %%ymm9 \n\t"
            "vMOVDQU  20*8(%[x]), %%ymm5 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm6, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm6, %%ymm9 \n\t"
            "vMOVDQU  24*8(%[x]), %%ymm6 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm8, %%ymm7, %%ymm8 \n\t"
            "VPMINSQ %%ymm9, %%ymm7, %%ymm9 \n\t"
            "vMOVDQU  28*8(%[x]), %%ymm7 \n\t"
            "\n\t"
            "subq $-32*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm0, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm0, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm1, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm1, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm2, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm2, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm3, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm3, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm4, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm4, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm5, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm5, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm6, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm6, %%ymm9 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm8, %%ymm7, %%ymm8 \n\t"
        "VPMINSQ %%ymm9, %%ymm7, %%ymm9 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm8, %%ymm8, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMAXSQ           %%ymm0, %%ymm8, %%ymm8 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm8, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"VPMAXSQ           %%xmm0, %%xmm8, %%xmm8 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movq              %%xmm8, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm9, %%ymm9, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMINSQ           %%ymm0, %%ymm9, %%ymm9 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm9, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"VPMINSQ           %%xmm0, %%xmm9, %%xmm9 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movq              %%xmm9, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_i8(value_min, *min);
	*max = maxval_i8(value_max, *max);
}

void minmax_loop_unroll_64_ams_r8(double *min, double *max, double x[], int64_t n){
    *min =   HUGE_VAL;
    *max = - HUGE_VAL;

	double value_min, value_max;

	int64_t n_unroll=(n>>6), n_mod, i;
	
	if (n<64){
		minmax_loop_c_r8(min, max, x, n);
        return;
	}

	n_mod = n/64;
    for (i=n-n_mod; i<n; i++){
        *min = minval_r8(x[i], *min);
        *max = maxval_r8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  4*8(%[x]), %%ymm1 \n\t"
        "vmovupd  8*8(%[x]), %%ymm2 \n\t"
        "vmovupd 12*8(%[x]), %%ymm3 \n\t"
        "vmovupd 16*8(%[x]), %%ymm4 \n\t"
        "vmovupd 20*8(%[x]), %%ymm5 \n\t"
        "vmovupd 24*8(%[x]), %%ymm6 \n\t"
        "vmovupd 28*8(%[x]), %%ymm7 \n\t"
        "vmovupd 32*8(%[x]), %%ymm8 \n\t"
        "vmovupd 36*8(%[x]), %%ymm9 \n\t"
        "vmovupd 40*8(%[x]), %%ymm10 \n\t"
        "vmovupd 44*8(%[x]), %%ymm11 \n\t"
        "vmovupd 48*8(%[x]), %%ymm12 \n\t"
        "vmovupd 52*8(%[x]), %%ymm13 \n\t"
        "vmovupd 56*8(%[x]), %%ymm14 \n\t"
        "vmovupd 60*8(%[x]), %%ymm15 \n\t"
        "vmovupd  0*8(%[x]), %%ymm16 \n\t"
        "vmovupd  0*8(%[x]), %%ymm17 \n\t"
        "subq $-64*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "vmaxpd %%ymm16, %%ymm0, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm0, %%ymm17 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm1, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm1, %%ymm17 \n\t"
            "vmovupd  4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm2, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm2, %%ymm17 \n\t"
            "vmovupd  8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm3, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm3, %%ymm17 \n\t"
            "vmovupd 12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm4, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm4, %%ymm17 \n\t"
            "vmovupd 16*8(%[x]), %%ymm4 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm5, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm5, %%ymm17 \n\t"
            "vmovupd 20*8(%[x]), %%ymm5 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm6, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm6, %%ymm17 \n\t"
            "vmovupd 24*8(%[x]), %%ymm6 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm7, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm7, %%ymm17 \n\t"
            "vmovupd 28*8(%[x]), %%ymm7 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm8, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm8, %%ymm17 \n\t"
            "vmovupd 32*8(%[x]), %%ymm8 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm9, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm9, %%ymm17 \n\t"
            "vmovupd 36*8(%[x]), %%ymm9 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm10, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm10, %%ymm17 \n\t"
            "vmovupd 40*8(%[x]), %%ymm10 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm11, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm11, %%ymm17 \n\t"
            "vmovupd 44*8(%[x]), %%ymm11 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm12, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm12, %%ymm17 \n\t"
            "vmovupd 48*8(%[x]), %%ymm12 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm13, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm13, %%ymm17 \n\t"
            "vmovupd 52*8(%[x]), %%ymm13 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm14, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm14, %%ymm17 \n\t"
            "vmovupd 56*8(%[x]), %%ymm14 \n\t"
            "\n\t"
            "vmaxpd %%ymm16, %%ymm15, %%ymm16 \n\t"
            "vminpd %%ymm17, %%ymm15, %%ymm17 \n\t"
            "vmovupd 60*8(%[x]), %%ymm15 \n\t"
            "\n\t"
            "subq $-64*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "vmaxpd %%ymm16, %%ymm0, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm0, %%ymm17 \n\t"
    
        "\n\t"
        "vmaxpd %%ymm16, %%ymm1, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm1, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm2, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm2, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm3, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm3, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm4, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm4, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm5, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm5, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm6, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm6, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm7, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm7, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm8, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm8, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm9, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm9, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm10, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm10, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm11, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm11, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm12, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm12, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm13, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm13, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm14, %%ymm16 \n\t"
        "vminpd %%ymm17, %%ymm14, %%ymm17 \n\t"
        "\n\t"
        "vmaxpd %%ymm16, %%ymm15, %%ymm1 \n\t"
        "vminpd %%ymm17, %%ymm15, %%ymm2 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm1, %%ymm1, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vmaxpd            %%ymm0, %%ymm1, %%ymm1 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm1, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"maxpd             %%xmm0, %%xmm1 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movsd             %%xmm1, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vminpd            %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"minpd             %%xmm0, %%xmm2 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movsd             %%xmm2, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_r8(value_min, *min);
	*max = maxval_r8(value_max, *max);
}

void minmax_loop_unroll_64_ams_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    *min = INT64_MAX;
    *max = INT64_MIN;

	int64_t value_min, value_max;

	int64_t n_unroll=(n/56), n_mod, i;
	
	if (n<56){
		minmax_loop_c_i8(min, max, x, n);
        return;
	}

	n_mod = n%56;
    for (i=n-n_mod; i<n; i++){
        *min = minval_i8(x[i], *min);
        *max = maxval_i8(x[i], *max);
    }

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  4*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  8*8(%[x]), %%ymm2 \n\t"
        "vMOVDQU 12*8(%[x]), %%ymm3 \n\t"
        "vMOVDQU 16*8(%[x]), %%ymm4 \n\t"
        "vMOVDQU 20*8(%[x]), %%ymm5 \n\t"
        "vMOVDQU 24*8(%[x]), %%ymm6 \n\t"
        "vMOVDQU 28*8(%[x]), %%ymm7 \n\t"
        "vMOVDQU 32*8(%[x]), %%ymm8 \n\t"
        "vMOVDQU 36*8(%[x]), %%ymm9 \n\t"
        "vMOVDQU 40*8(%[x]), %%ymm10 \n\t"
        "vMOVDQU 44*8(%[x]), %%ymm11 \n\t"
        "vMOVDQU 48*8(%[x]), %%ymm12 \n\t"
        "vMOVDQU 52*8(%[x]), %%ymm13 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm14 \n\t"
        "vMOVDQU  0*8(%[x]), %%ymm15 \n\t"
        "subq $-56*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );
    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm0, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm0, %%ymm15 \n\t"
            "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm1, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm1, %%ymm15 \n\t"
            "vMOVDQU   4*8(%[x]), %%ymm1 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm2, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm2, %%ymm15 \n\t"
            "vMOVDQU   8*8(%[x]), %%ymm2 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm3, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm3, %%ymm15 \n\t"
            "vMOVDQU  12*8(%[x]), %%ymm3 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm4, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm4, %%ymm15 \n\t"
            "vMOVDQU  16*8(%[x]), %%ymm4 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm5, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm5, %%ymm15 \n\t"
            "vMOVDQU  20*8(%[x]), %%ymm5 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm6, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm6, %%ymm15 \n\t"
            "vMOVDQU  24*8(%[x]), %%ymm6 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm7, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm7, %%ymm15 \n\t"
            "vMOVDQU  28*8(%[x]), %%ymm7 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm8, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm8, %%ymm15 \n\t"
            "vMOVDQU  32*8(%[x]), %%ymm8 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm9, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm9, %%ymm15 \n\t"
            "vMOVDQU  36*8(%[x]), %%ymm9 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm10, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm10, %%ymm15 \n\t"
            "vMOVDQU  40*8(%[x]), %%ymm10 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm11, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm11, %%ymm15 \n\t"
            "vMOVDQU  44*8(%[x]), %%ymm11 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm12, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm12, %%ymm15 \n\t"
            "vMOVDQU  48*8(%[x]), %%ymm12 \n\t"
            "\n\t"
            "VPMAXSQ %%ymm14, %%ymm13, %%ymm14 \n\t"
            "VPMINSQ %%ymm15, %%ymm13, %%ymm15 \n\t"
            "vMOVDQU  52*8(%[x]), %%ymm13 \n\t"
            "\n\t"
            "subq $-56*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

	__asm__ __volatile__(
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm0, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm0, %%ymm15 \n\t"
    
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm1, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm1, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm2, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm2, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm3, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm3, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm4, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm4, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm5, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm5, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm6, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm6, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm7, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm7, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm8, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm8, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm9, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm9, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm10, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm10, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm11, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm11, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm12, %%ymm14 \n\t"
        "VPMINSQ %%ymm15, %%ymm12, %%ymm15 \n\t"
        "\n\t"
        "VPMAXSQ %%ymm14, %%ymm13, %%ymm1 \n\t"
        "VPMINSQ %%ymm15, %%ymm13, %%ymm2 \n\t"
		"\n\t"
		"vperm2f128 $0x01, %%ymm1, %%ymm1, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMAXSQ           %%ymm0, %%ymm1, %%ymm1 \n\t" // return   |max(a,c)|max(b,d)|max(a,c)|max(b,d)| 
		"shufpd $0x01,     %%xmm1, %%xmm0 \n\t"         // exchange |max(a,c)|max(b,d)| -> |max(b,d)|max(a,c)|
		"VPMAXSQ           %%xmm0, %%xmm1, %%xmm1 \n\t"         // return   |max(a,b,c,d)|max(a,b,c,d)|
		"movq              %%xmm1, %[v_max]   \n\t"         // return   max(a,b,c,d)
		"\n\t"
		"vperm2f128 $0x01, %%ymm2, %%ymm2, %%ymm0 \n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"VPMINSQ           %%ymm0, %%ymm2, %%ymm2 \n\t" // return   |min(a,c)|min(b,d)|min(a,c)|min(b,d)| 
		"shufpd $0x01,     %%xmm2, %%xmm0 \n\t"         // exchange |min(a,c)|min(b,d)| -> |min(b,d)|min(a,c)|
		"VPMINSQ           %%xmm0, %%xmm2, %%xmm2 \n\t"         // return   |min(a,b,c,d)|min(a,b,c,d)|
		"movq              %%xmm2, %[v_min]   \n\t"         // return   min(a,b,c,d)
		"\n\t"
		:[v_max]"=m"(value_max), [v_min]"=m"(value_min)
	);
	*min = minval_i8(value_min, *min);
	*max = maxval_i8(value_max, *max);
}

