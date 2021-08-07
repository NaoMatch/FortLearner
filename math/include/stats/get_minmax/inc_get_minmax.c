#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double maxval_r8(double x, double y){
	if (x>y){
        return x;
    }else{
        return y;
    }
}

double minval_r8(double x, double y){
	if (x<y){
        return x;
    }else{
        return y;
    }
}

int64_t maxval_i8(int64_t x, int64_t y){
	if (x>y){
        return x;
    }else{
        return y;
    }
}

int64_t minval_i8(int64_t x, int64_t y){
	if (x<y){
        return x;
    }else{
        return y;
    }
}

// double maxval_r8(double x, double y){
// 	return x>y ? x: y;
// }

// double minval_r8(double x, double y){
// 	return x>y ? y: x;
// }

// int64_t maxval_i8(int64_t x, int64_t y){
// 	return x>y ? x: y;
// }

// int64_t minval_i8(int64_t x, int64_t y){
// 	return x>y ? y: x;
// }

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_c_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t i;
    double tmp;
	for(i=0; i<n; i++){
        tmp = x[i];
		tmp_min = minval_r8(tmp, tmp_min);
		tmp_max = maxval_r8(tmp, tmp_max);
	}
    *min = tmp_min;
    *max = tmp_max;
}

void get_minmax_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t i;
    int64_t tmp;
	for(i=0; i<n; i++){
        tmp = x[i];
		tmp_min = minval_i8(tmp, tmp_min);
		tmp_max = maxval_i8(tmp, tmp_max);
	}
    *min = tmp_min;
    *max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_02_c_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t unroll_size=2, n_unroll=(n>>1), n_remain=n%unroll_size;
    double r00, r01;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_r8(r00, tmp_min);
		tmp_max = maxval_r8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_r8(r01, tmp_min);
		tmp_max = maxval_r8(r01, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        r00 = *(x);
        tmp_min = minval_r8(r00, tmp_min);
        tmp_max = maxval_r8(r00, tmp_max);
    }
    *min = tmp_min;
    *max = tmp_max;
}

void get_minmax_unroll_02_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t unroll_size=2, n_unroll=(n>>1), n_remain=n%unroll_size;
    int64_t r00, r01;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_i8(r00, tmp_min);
		tmp_max = maxval_i8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_i8(r01, tmp_min);
		tmp_max = maxval_i8(r01, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        r00 = *(x);
        tmp_min = minval_i8(r00, tmp_min);
        tmp_max = maxval_i8(r00, tmp_max);
    }
    *min = tmp_min;
    *max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_04_c_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t unroll_size=4, n_unroll=(n>>2), n_remain=n%unroll_size;
    double r00, r01, r02, r03;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_r8(r00, tmp_min);
		tmp_max = maxval_r8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_r8(r01, tmp_min);
		tmp_max = maxval_r8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_r8(r02, tmp_min);
		tmp_max = maxval_r8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_r8(r03, tmp_min);
		tmp_max = maxval_r8(r03, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_r8(r00, tmp_min);
            tmp_max = maxval_r8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

void get_minmax_unroll_04_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t unroll_size=4, n_unroll=(n>>2), n_remain=n%unroll_size;
    int64_t r00, r01, r02, r03;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_i8(r00, tmp_min);
		tmp_max = maxval_i8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_i8(r01, tmp_min);
		tmp_max = maxval_i8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_i8(r02, tmp_min);
		tmp_max = maxval_i8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_i8(r03, tmp_min);
		tmp_max = maxval_i8(r03, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_i8(r00, tmp_min);
            tmp_max = maxval_i8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_08_c_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t unroll_size=8, n_unroll=(n>>3), n_remain=n%unroll_size;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_r8(r00, tmp_min);
		tmp_max = maxval_r8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_r8(r01, tmp_min);
		tmp_max = maxval_r8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_r8(r02, tmp_min);
		tmp_max = maxval_r8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_r8(r03, tmp_min);
		tmp_max = maxval_r8(r03, tmp_max);

        r04 = *(x+4);
		tmp_min = minval_r8(r04, tmp_min);
		tmp_max = maxval_r8(r04, tmp_max);

        r05 = *(x+5);
		tmp_min = minval_r8(r05, tmp_min);
		tmp_max = maxval_r8(r05, tmp_max);

        r06 = *(x+6);
		tmp_min = minval_r8(r06, tmp_min);
		tmp_max = maxval_r8(r06, tmp_max);

        r07 = *(x+7);
		tmp_min = minval_r8(r07, tmp_min);
		tmp_max = maxval_r8(r07, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_r8(r00, tmp_min);
            tmp_max = maxval_r8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

void get_minmax_unroll_08_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t unroll_size=8, n_unroll=(n>>3), n_remain=n%unroll_size;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06, r07;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_i8(r00, tmp_min);
		tmp_max = maxval_i8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_i8(r01, tmp_min);
		tmp_max = maxval_i8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_i8(r02, tmp_min);
		tmp_max = maxval_i8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_i8(r03, tmp_min);
		tmp_max = maxval_i8(r03, tmp_max);

        r04 = *(x+4);
		tmp_min = minval_i8(r04, tmp_min);
		tmp_max = maxval_i8(r04, tmp_max);

        r05 = *(x+5);
		tmp_min = minval_i8(r05, tmp_min);
		tmp_max = maxval_i8(r05, tmp_max);

        r06 = *(x+6);
		tmp_min = minval_i8(r06, tmp_min);
		tmp_max = maxval_i8(r06, tmp_max);

        r07 = *(x+7);
		tmp_min = minval_i8(r07, tmp_min);
		tmp_max = maxval_i8(r07, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_i8(r00, tmp_min);
            tmp_max = maxval_i8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_16_c_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t unroll_size=16, n_unroll=(n>>4), n_remain=n%unroll_size;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_r8(r00, tmp_min);
		tmp_max = maxval_r8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_r8(r01, tmp_min);
		tmp_max = maxval_r8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_r8(r02, tmp_min);
		tmp_max = maxval_r8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_r8(r03, tmp_min);
		tmp_max = maxval_r8(r03, tmp_max);

        r04 = *(x+4);
		tmp_min = minval_r8(r04, tmp_min);
		tmp_max = maxval_r8(r04, tmp_max);

        r05 = *(x+5);
		tmp_min = minval_r8(r05, tmp_min);
		tmp_max = maxval_r8(r05, tmp_max);

        r06 = *(x+6);
		tmp_min = minval_r8(r06, tmp_min);
		tmp_max = maxval_r8(r06, tmp_max);

        r07 = *(x+7);
		tmp_min = minval_r8(r07, tmp_min);
		tmp_max = maxval_r8(r07, tmp_max);

        r08 = *(x+8);
		tmp_min = minval_r8(r08, tmp_min);
		tmp_max = maxval_r8(r08, tmp_max);

        r09 = *(x+9);
		tmp_min = minval_r8(r09, tmp_min);
		tmp_max = maxval_r8(r09, tmp_max);

        r10 = *(x+10);
		tmp_min = minval_r8(r10, tmp_min);
		tmp_max = maxval_r8(r10, tmp_max);

        r11 = *(x+11);
		tmp_min = minval_r8(r11, tmp_min);
		tmp_max = maxval_r8(r11, tmp_max);

        r12 = *(x+12);
		tmp_min = minval_r8(r12, tmp_min);
		tmp_max = maxval_r8(r12, tmp_max);

        r13 = *(x+13);
		tmp_min = minval_r8(r13, tmp_min);
		tmp_max = maxval_r8(r13, tmp_max);

        r14 = *(x+14);
		tmp_min = minval_r8(r14, tmp_min);
		tmp_max = maxval_r8(r14, tmp_max);

        r15 = *(x+15);
		tmp_min = minval_r8(r15, tmp_min);
		tmp_max = maxval_r8(r15, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_r8(r00, tmp_min);
            tmp_max = maxval_r8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

void get_minmax_unroll_16_c_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t unroll_size=16, n_unroll=(n>>4), n_remain=n%unroll_size;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06, r07;
    int64_t r08, r09, r10, r11;
    int64_t r12, r13, r14, r15;
	while(n_unroll--){
        r00 = *(x);
		tmp_min = minval_i8(r00, tmp_min);
		tmp_max = maxval_i8(r00, tmp_max);

        r01 = *(x+1);
		tmp_min = minval_i8(r01, tmp_min);
		tmp_max = maxval_i8(r01, tmp_max);

        r02 = *(x+2);
		tmp_min = minval_i8(r02, tmp_min);
		tmp_max = maxval_i8(r02, tmp_max);

        r03 = *(x+3);
		tmp_min = minval_i8(r03, tmp_min);
		tmp_max = maxval_i8(r03, tmp_max);

        r04 = *(x+4);
		tmp_min = minval_i8(r04, tmp_min);
		tmp_max = maxval_i8(r04, tmp_max);

        r05 = *(x+5);
		tmp_min = minval_i8(r05, tmp_min);
		tmp_max = maxval_i8(r05, tmp_max);

        r06 = *(x+6);
		tmp_min = minval_i8(r06, tmp_min);
		tmp_max = maxval_i8(r06, tmp_max);

        r07 = *(x+7);
		tmp_min = minval_i8(r07, tmp_min);
		tmp_max = maxval_i8(r07, tmp_max);

        r08 = *(x+8);
		tmp_min = minval_i8(r08, tmp_min);
		tmp_max = maxval_i8(r08, tmp_max);

        r09 = *(x+9);
		tmp_min = minval_i8(r09, tmp_min);
		tmp_max = maxval_i8(r09, tmp_max);

        r10 = *(x+10);
		tmp_min = minval_i8(r10, tmp_min);
		tmp_max = maxval_i8(r10, tmp_max);

        r11 = *(x+11);
		tmp_min = minval_i8(r11, tmp_min);
		tmp_max = maxval_i8(r11, tmp_max);

        r12 = *(x+12);
		tmp_min = minval_i8(r12, tmp_min);
		tmp_max = maxval_i8(r12, tmp_max);

        r13 = *(x+13);
		tmp_min = minval_i8(r13, tmp_min);
		tmp_max = maxval_i8(r13, tmp_max);

        r14 = *(x+14);
		tmp_min = minval_i8(r14, tmp_min);
		tmp_max = maxval_i8(r14, tmp_max);

        r15 = *(x+15);
		tmp_min = minval_i8(r15, tmp_min);
		tmp_max = maxval_i8(r15, tmp_max);
        x+=unroll_size;
	}

    if (n_remain>0){
        while(n_remain--){
            r00 = *(x);
            tmp_min = minval_i8(r00, tmp_min);
            tmp_max = maxval_i8(r00, tmp_max);
            x+=1;
        }
    }
    *min = tmp_min;
    *max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_a_r8(double *min, double *max, double x[], int64_t n){
    double tmp_min, tmp_max;
	int64_t n_unroll=n;

    __asm__ __volatile__ (
        "movsd 0*8(%[x]), %%xmm0 \n\t"
        "movsd %%xmm0,    %%xmm1 \n\t"
        "movsd %%xmm0,    %%xmm2 \n\t"
        "subq $-1*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "minpd %%xmm0, %%xmm1 \n\t"
            "maxpd %%xmm0, %%xmm2 \n\t"
            "movsd  0*8(%[x]), %%xmm0 \n\t"
            "\n\t"
            "subq $-1*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "\n\t"
        "minpd %%xmm0, %%xmm1 \n\t"
        "maxpd %%xmm0, %%xmm2 \n\t"
        ::
    );

	__asm__ __volatile__(
		"\n\t"
		"minpd %%xmm0, %%xmm1 \n\t"
		"maxpd %%xmm0, %%xmm2 \n\t"
		"\n\t"
        "movsd  %%xmm1, %[min] \n\t"
		"\n\t"
        "movsd  %%xmm2, %[max] \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
    int64_t tmp_min, tmp_max;
	int64_t n_unroll=n;

    __asm__ __volatile__ (
        "MOVQ 0*8(%[x]), %%xmm0 \n\t"
        "MOVQ %%xmm0,    %%xmm1 \n\t"
        "MOVQ %%xmm0,    %%xmm2 \n\t"
        "subq $-1*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
            "VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
            "MOVQ  0*8(%[x]), %%xmm0 \n\t"
            "\n\t"
            "subq $-1*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "\n\t"
        "VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
        "VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
        ::
    );

	__asm__ __volatile__(
		"\n\t"
		"VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
		"VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
		"\n\t"
        "MOVQ  %%xmm1, %[min] \n\t"
		"\n\t"
        "MOVQ  %%xmm2, %[max] \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_02_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<2){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min =   HUGE_VAL;
    double tmp_max = - HUGE_VAL;

	int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "movupd 0*8(%[x]), %%xmm0 \n\t"
        "movupd %%xmm0,    %%xmm1 \n\t"
        "movupd %%xmm0,    %%xmm2 \n\t"
        "subq $-2*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "minpd  %%xmm0,    %%xmm1 \n\t"
            "maxpd  %%xmm0,    %%xmm2 \n\t"
            "movupd 0*8(%[x]), %%xmm0 \n\t"
            "\n\t"
            "subq $-2*8, %[x]         \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "\n\t"
        "minpd %%xmm0, %%xmm1 \n\t"
        "maxpd %%xmm0, %%xmm2 \n\t"
        ::
    );

	__asm__ __volatile__(
		"\n\t"
		"minpd  %%xmm0, %%xmm1 \n\t"
		"maxpd  %%xmm0, %%xmm2 \n\t"
		"\n\t"
        "movupd %%xmm1, %%xmm3 \n\t"
        "PSRLDQ $1*8,   %%xmm3 \n\t"
        "minpd  %%xmm3, %%xmm1 \n\t"
		"\n\t"
        "movupd %%xmm2, %%xmm4 \n\t"
        "PSRLDQ $1*8,   %%xmm4 \n\t"
        "maxpd  %%xmm4, %%xmm2 \n\t"
		"\n\t"
        ::
	);

    if (n_remain&1){
        __asm__ __volatile__ (
            "movsd  0*8(%[x]), %%xmm15 \n\t"
            "minpd  %%xmm15,   %%xmm1  \n\t"
            "maxpd  %%xmm15,   %%xmm2  \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
        n_remain-=1;
    }

	__asm__ __volatile__(
		"\n\t"
        "movsd %%xmm1, %[min] \n\t"
        "movsd %%xmm2, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_02_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<2){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min = INT64_MAX;
    int64_t tmp_max = INT64_MIN;

	int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "MOVDQU 0*8(%[x]), %%xmm0 \n\t"
        "MOVDQU %%xmm0,    %%xmm1 \n\t"
        "MOVDQU %%xmm0,    %%xmm2 \n\t"
        "subq $-2*8, %[x]         \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "\n\t"
            "VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
            "VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
            "MOVDQU  0*8(%[x]), %%xmm0      \n\t"
            "\n\t"
            "subq $-2*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "\n\t"
        "VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
        "VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
        ::
    );

	__asm__ __volatile__(
		"\n\t"
		"VPMINSQ %%xmm0, %%xmm1, %%xmm1 \n\t"
		"VPMAXSQ %%xmm0, %%xmm2, %%xmm2 \n\t"
		"\n\t"
        "MOVDQU %%xmm1, %%xmm3          \n\t"
        "PSRLDQ $1*8,   %%xmm3          \n\t"
        "VPMINSQ %%xmm3, %%xmm1, %%xmm1 \n\t"
		"\n\t"
        "MOVDQU %%xmm2, %%xmm4          \n\t"
        "PSRLDQ $1*8,   %%xmm4          \n\t"
        "VPMAXSQ %%xmm4, %%xmm2, %%xmm2 \n\t"
		"\n\t"
        ::
	);

    if (n_remain&1){
        __asm__ __volatile__ (
            "MOVQ    0*8(%[x]), %%xmm15       \n\t"
            "VPMINSQ %%xmm15, %%xmm1, %%xmm1  \n\t"
            "VPMAXSQ %%xmm15, %%xmm2, %%xmm2  \n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
        n_remain-=1;
    }

	__asm__ __volatile__(
		"\n\t"
        "MOVQ   %%xmm1, %[min] \n\t"
        "MOVQ   %%xmm2, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_04_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<4){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd 0*8(%[x]), %%ymm0 \n\t"
        "vmovupd %%ymm0,    %%ymm1 \n\t"
        "vmovupd %%ymm0,    %%ymm2 \n\t"
        "subq $-4*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
            "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0    \n\t"
            "\n\t"
            "subq $-4*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "\n\t"
        "vminpd %%ymm0, %%ymm1, %%ymm1 \n\t"
        "vmaxpd %%ymm0, %%ymm2, %%ymm2 \n\t"
        ::
    );

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm1      \n\t"
    //         "maxpd  %%xmm0, %%xmm2      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm1, %%xmm3  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm1, %%xmm4  \n\t"
        "minpd   %%xmm4, %%xmm3 \n\t"
        "vmovupd %%xmm3, %%xmm4 \n\t"
        "PSRLDQ  $1*8,   %%xmm4 \n\t"
        "minpd   %%xmm4, %%xmm3 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm2, %%xmm5  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm2, %%xmm6  \n\t"
        "maxpd   %%xmm6, %%xmm5 \n\t"
        "vmovupd %%xmm5, %%xmm6 \n\t"
        "PSRLDQ  $1*8,   %%xmm6 \n\t"
        "maxpd   %%xmm6, %%xmm5 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15,     %%xmm3  \n\t"
    //         "maxpd %%xmm15,     %%xmm5  \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm3  \n\t"
                "maxpd %%xmm15,     %%xmm5  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
		"\n\t"
        "movsd  %%xmm3, %[min] \n\t"
        "movsd  %%xmm5, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_04_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<4){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vMOVDQU 0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU %%ymm0,    %%ymm1 \n\t"
        "vMOVDQU %%ymm0,    %%ymm2 \n\t"
        "subq $-4*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%ymm0, %%ymm1, %%ymm1 \n\t"
            "vPMAXSQ %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vMOVDQU  0*8(%[x]), %%ymm0     \n\t"
            "\n\t"
            "subq $-4*8, %[x]               \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "vPMINSQ %%ymm0, %%ymm1, %%ymm1 \n\t"
        "vPMAXSQ %%ymm0, %%ymm2, %%ymm2 \n\t"
        ::
    );

    // if (n_remain&2){
    //     printf("do1 \n");
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]), %%xmm0      \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm1, %%xmm1 \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm2, %%xmm2 \n\t"
    //         "subq $-2*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
        "VEXTRACTI64X2   $0,  %%ymm1, %%xmm3 \n\t"
        "VEXTRACTI64X2   $1,  %%ymm1, %%xmm4 \n\t"
        "vPMINSQ %%xmm4, %%xmm3, %%xmm3      \n\t"
        "MOVDQU  %%xmm3, %%xmm4              \n\t"
        "PSRLDQ  $1*8,   %%xmm4              \n\t"
        "vPMINSQ %%xmm4, %%xmm3, %%xmm3      \n\t"
		"\n\t"
        "VEXTRACTI64X2   $0,  %%ymm2, %%xmm5 \n\t"
        "VEXTRACTI64X2   $1,  %%ymm2, %%xmm6 \n\t"
        "vPMAXSQ %%xmm6, %%xmm5, %%xmm5      \n\t"
        "MOVDQU  %%xmm5, %%xmm6              \n\t"
        "PSRLDQ  $1*8,   %%xmm6              \n\t"
        "vPMAXSQ %%xmm6, %%xmm5, %%xmm5      \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     printf("do2 \n");
    //     __asm__ __volatile__ (
    //         "MOVQ   0*8(%[x]), %%xmm15       \n\t"
    //         "vPMINSQ %%xmm15, %%xmm3, %%xmm3 \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm5, %%xmm5 \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm3, %%xmm3 \n\t"
                "vPMAXSQ %%xmm15, %%xmm5, %%xmm5 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ %%xmm3, %[min] \n\t"
        "MOVQ %%xmm5, %[max] \n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_08_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<8){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd 0*8(%[x]), %%ymm0 \n\t"
        "vmovupd 4*8(%[x]), %%ymm1 \n\t"
        "\n\t"
        "vmovupd %%ymm0,    %%ymm2 \n\t"
        "vmovupd %%ymm0,    %%ymm3 \n\t"
        "\n\t"
        "vmovupd %%ymm1,    %%ymm4 \n\t"
        "vmovupd %%ymm1,    %%ymm5 \n\t"
        "subq $-8*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vmaxpd %%ymm0, %%ymm3, %%ymm3 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0    \n\t"
            "\n\t"
            "vminpd %%ymm1, %%ymm4, %%ymm4 \n\t"
            "vmaxpd %%ymm1, %%ymm5, %%ymm5 \n\t"
            "vmovupd  4*8(%[x]), %%ymm1    \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vminpd %%ymm0, %%ymm2, %%ymm2 \n\t"
        "vmaxpd %%ymm0, %%ymm3, %%ymm3 \n\t"
        "\n\t"
        "vminpd %%ymm1, %%ymm4, %%ymm4 \n\t"
        "vmaxpd %%ymm1, %%ymm5, %%ymm5 \n\t"
        ::
    );

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0    \n\t"
    //         "vminpd  %%ymm0, %%ymm2, %%ymm2 \n\t"
    //         "vmaxpd  %%ymm0, %%ymm3, %%ymm3 \n\t"
    //         "subq $-4*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm2      \n\t"
    //         "maxpd  %%xmm0, %%xmm3      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vminpd %%ymm2, %%ymm4, %%ymm4 \n\t"
		"vmaxpd %%ymm3, %%ymm5, %%ymm5 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm4, %%xmm6  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm4, %%xmm7  \n\t"
        "minpd   %%xmm7, %%xmm6 \n\t"
        "vmovupd %%xmm6, %%xmm7 \n\t"
        "PSRLDQ  $1*8,   %%xmm7 \n\t"
        "minpd   %%xmm7, %%xmm6 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm5, %%xmm8  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm5, %%xmm9  \n\t"
        "maxpd   %%xmm9, %%xmm8 \n\t"
        "vmovupd %%xmm8, %%xmm9 \n\t"
        "PSRLDQ  $1*8,   %%xmm9 \n\t"
        "maxpd   %%xmm9, %%xmm8 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm6      \n\t"
    //         "maxpd %%xmm15, %%xmm8      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm6  \n\t"
                "maxpd %%xmm15,     %%xmm8  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm6, %[min] \n\t"
        "movsd  %%xmm8, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_08_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<8){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vMOVDQU 0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU 4*8(%[x]), %%ymm1 \n\t"
        "\n\t"
        "vMOVDQU %%ymm0,    %%ymm2 \n\t"
        "vMOVDQU %%ymm0,    %%ymm3 \n\t"
        "\n\t"
        "vMOVDQU %%ymm1,    %%ymm4 \n\t"
        "vMOVDQU %%ymm1,    %%ymm5 \n\t"
        "subq $-8*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%ymm0, %%ymm2, %%ymm2 \n\t"
            "vPMAXSQ %%ymm0, %%ymm3, %%ymm3 \n\t"
            "vMOVDQU  0*8(%[x]), %%ymm0     \n\t"
            "\n\t"
            "vPMINSQ %%ymm1, %%ymm4, %%ymm4 \n\t"
            "vPMAXSQ %%ymm1, %%ymm5, %%ymm5 \n\t"
            "vMOVDQU  4*8(%[x]), %%ymm1     \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vPMINSQ %%ymm0, %%ymm2, %%ymm2 \n\t"
        "vPMAXSQ %%ymm0, %%ymm3, %%ymm3 \n\t"
        "\n\t"
        "vPMINSQ %%ymm1, %%ymm4, %%ymm4 \n\t"
        "vPMAXSQ %%ymm1, %%ymm5, %%ymm5 \n\t"
        ::
    );

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%ymm0     \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm2, %%ymm2 \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm3, %%ymm3 \n\t"
    //         "subq $-4*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]), %%xmm0      \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm2, %%xmm2 \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm3, %%xmm3 \n\t"
    //         "subq $-2*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vPMINSQ %%ymm2, %%ymm4, %%ymm4 \n\t"
		"vPMAXSQ %%ymm3, %%ymm5, %%ymm5 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm4, %%xmm6  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm4, %%xmm7  \n\t"
        "vPMINSQ %%xmm7, %%xmm6, %%xmm6      \n\t"
        "vMOVDQU %%xmm6, %%xmm7              \n\t"
        "PSRLDQ  $1*8,   %%xmm7              \n\t"
        "vPMINSQ %%xmm7, %%xmm6, %%xmm6      \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm5, %%xmm8  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm5, %%xmm9  \n\t"
        "vPMAXSQ %%xmm9, %%xmm8, %%xmm8      \n\t"
        "vMOVDQU %%xmm8, %%xmm9              \n\t"
        "PSRLDQ  $1*8,   %%xmm9              \n\t"
        "vPMAXSQ %%xmm9, %%xmm8, %%xmm8      \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "MOVQ   0*8(%[x]), %%xmm15       \n\t"
    //         "vPMINSQ %%xmm15, %%xmm6, %%xmm6 \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm8, %%xmm8 \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm6, %%xmm6 \n\t"
                "vPMAXSQ %%xmm15, %%xmm8, %%xmm8 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ    %%xmm6, %[min]              \n\t"
        "MOVQ    %%xmm8, %[max]              \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_08z_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<8){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd 0*8(%[x]), %%zmm0 \n\t"
        "\n\t"
        "vmovupd %%zmm0,    %%zmm2 \n\t"
        "vmovupd %%zmm0,    %%zmm3 \n\t"
        "subq $-8*8, %[x]          \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vmaxpd %%zmm0, %%zmm3, %%zmm3 \n\t"
            "vmovupd  0*8(%[x]), %%zmm0    \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vminpd %%zmm0, %%zmm2, %%zmm2 \n\t"
        "vmaxpd %%zmm0, %%zmm3, %%zmm3 \n\t"
        ::
    );

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0 \n\t"
    //         "vminpd  %%ymm0, %%ymm2, %%ymm2      \n\t"
    //         "vmaxpd  %%ymm0, %%ymm3, %%ymm3      \n\t"
    //         "subq $-4*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm2      \n\t"
    //         "maxpd  %%xmm0, %%xmm3      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm2, %%ymm12 \n\t"
        "VEXTRACTF64X4  $1,  %%zmm2, %%ymm13 \n\t"
        "vminpd %%ymm13, %%ymm12, %%ymm12    \n\t"
        "VEXTRACTF64X2  $0,  %%ymm12, %%xmm6  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm12, %%xmm7  \n\t"
        "minpd %%xmm7, %%xmm6 \n\t"
        "vmovupd %%xmm6, %%xmm7 \n\t"
        "PSRLDQ $1*8,   %%xmm7 \n\t"
        "minpd  %%xmm7, %%xmm6 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm3, %%ymm14 \n\t"
        "VEXTRACTF64X4  $1,  %%zmm3, %%ymm15 \n\t"
        "vmaxpd %%ymm15, %%ymm14, %%ymm14    \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm8  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm9  \n\t"
        "maxpd %%xmm9, %%xmm8 \n\t"
        "vmovupd %%xmm8, %%xmm9 \n\t"
        "PSRLDQ $1*8,   %%xmm9 \n\t"
        "maxpd  %%xmm9, %%xmm8 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm6      \n\t"
    //         "maxpd %%xmm15, %%xmm8      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm6  \n\t"
                "maxpd %%xmm15,     %%xmm8  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm6, %[min] \n\t"
        "movsd  %%xmm8, %[max] \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_08z_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<8){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "VMOVDQU64 0*8(%[x]), %%zmm0 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm0,    %%zmm2 \n\t"
        "VMOVDQU64 %%zmm0,    %%zmm3 \n\t"
        "subq $-8*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vPMAXSQ %%zmm0, %%zmm3, %%zmm3 \n\t"
            "VMOVDQU64  0*8(%[x]), %%zmm0    \n\t"
            "\n\t"
            "subq $-8*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
        "vPMINSQ %%zmm0, %%zmm2, %%zmm2 \n\t"
        "vPMAXSQ %%zmm0, %%zmm3, %%zmm3 \n\t"
        ::
    );

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm2, %%ymm2      \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm3, %%ymm3      \n\t"
    //         "subq $-4*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]), %%xmm0 \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm2, %%xmm2      \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm3, %%xmm3      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm2, %%ymm12 \n\t"
        "VEXTRACTI64X4  $1,  %%zmm2, %%ymm13 \n\t"
        "vPMINSQ %%ymm13, %%ymm12, %%ymm12    \n\t"
        "VEXTRACTI64X2  $0,  %%ymm12, %%xmm6  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm12, %%xmm7  \n\t"
        "vPMINSQ %%xmm7, %%xmm6, %%xmm6 \n\t"
        "MOVDQU %%xmm6, %%xmm7 \n\t"
        "PSRLDQ $1*8,   %%xmm7 \n\t"
        "vPMINSQ  %%xmm7, %%xmm6, %%xmm6 \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm3,  %%ymm14 \n\t"
        "VEXTRACTI64X4  $1,  %%zmm3,  %%ymm15 \n\t"
        "vPMAXSQ    %%ymm15, %%ymm14, %%ymm14 \n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm8  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm9  \n\t"
        "vPMAXSQ    %%xmm9,  %%xmm8,  %%xmm8  \n\t"
        "MOVDQU     %%xmm8,  %%xmm9           \n\t"
        "PSRLDQ     $1*8,    %%xmm9           \n\t"
        "vPMAXSQ    %%xmm9,  %%xmm8,  %%xmm8  \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "MOVQ    0*8(%[x]),       %%xmm15 \n\t"
    //         "vPMINSQ %%xmm15, %%xmm6, %%xmm6  \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm8, %%xmm8  \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm6, %%xmm6 \n\t"
                "vPMAXSQ %%xmm15, %%xmm8, %%xmm8 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm6, %[min] \n\t"
        "MOVQ  %%xmm8, %[max] \n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}


// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_16_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<16){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%ymm0 \n\t"
        "vmovupd  4*8(%[x]), %%ymm1 \n\t"
        "vmovupd  8*8(%[x]), %%ymm2 \n\t"
        "vmovupd 12*8(%[x]), %%ymm3 \n\t"
        "\n\t"
        "vmovupd %%ymm0,    %%ymm4  \n\t"
        "vmovupd %%ymm0,    %%ymm5  \n\t"
        "\n\t"
        "vmovupd %%ymm1,    %%ymm6  \n\t"
        "vmovupd %%ymm1,    %%ymm7  \n\t"
        "\n\t"
        "vmovupd %%ymm2,    %%ymm8  \n\t"
        "vmovupd %%ymm2,    %%ymm9  \n\t"
        "\n\t"
        "vmovupd %%ymm3,    %%ymm10 \n\t"
        "vmovupd %%ymm3,    %%ymm11 \n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%ymm0, %%ymm4, %%ymm4   \n\t"
            "vmaxpd %%ymm0, %%ymm5, %%ymm5   \n\t"
            "vmovupd  0*8(%[x]), %%ymm0      \n\t"
            "\n\t"
            "vminpd %%ymm1, %%ymm6, %%ymm6   \n\t"
            "vmaxpd %%ymm1, %%ymm7, %%ymm7   \n\t"
            "vmovupd  4*8(%[x]), %%ymm1      \n\t"
            "\n\t"
            "vminpd %%ymm2, %%ymm8, %%ymm8   \n\t"
            "vmaxpd %%ymm2, %%ymm9, %%ymm9   \n\t"
            "vmovupd  8*8(%[x]), %%ymm2      \n\t"
            "\n\t"
            "vminpd %%ymm3, %%ymm10, %%ymm10 \n\t"
            "vmaxpd %%ymm3, %%ymm11, %%ymm11 \n\t"
            "vmovupd 12*8(%[x]), %%ymm3      \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vminpd %%ymm0, %%ymm4, %%ymm4   \n\t"
        "vmaxpd %%ymm0, %%ymm5, %%ymm5   \n\t"
        "\n\t"
        "vminpd %%ymm1, %%ymm6, %%ymm6   \n\t"
        "vmaxpd %%ymm1, %%ymm7, %%ymm7   \n\t"
        "\n\t"
        "vminpd %%ymm2, %%ymm8, %%ymm8   \n\t"
        "vmaxpd %%ymm2, %%ymm9, %%ymm9   \n\t"
        "\n\t"
        "vminpd %%ymm3, %%ymm10, %%ymm10 \n\t"
        "vmaxpd %%ymm3, %%ymm11, %%ymm11 \n\t"
        "\n\t"
        ::
    );

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0 \n\t"
    //         "vminpd  %%ymm0, %%ymm4, %%ymm4      \n\t"
    //         "vmaxpd  %%ymm0, %%ymm5, %%ymm5      \n\t"
    //         "\n\t"
    //         "vmovupd   4*8(%[x]), %%ymm1 \n\t"
    //         "vminpd  %%ymm1, %%ymm6, %%ymm6      \n\t"
    //         "vmaxpd  %%ymm1, %%ymm7, %%ymm7      \n\t"
    //         "subq $-8*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0 \n\t"
    //         "vminpd  %%ymm0, %%ymm4, %%ymm4      \n\t"
    //         "vmaxpd  %%ymm0, %%ymm5, %%ymm5      \n\t"
    //         "subq $-4*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm4      \n\t"
    //         "maxpd  %%xmm0, %%xmm5      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vminpd %%ymm4, %%ymm6, %%ymm6 \n\t"
		"vmaxpd %%ymm5, %%ymm7, %%ymm7 \n\t"
        "\n\t"
		"vminpd %%ymm6, %%ymm8, %%ymm8 \n\t"
		"vmaxpd %%ymm7, %%ymm9, %%ymm9 \n\t"
        "\n\t"
		"vminpd %%ymm8, %%ymm10, %%ymm10 \n\t"
		"vmaxpd %%ymm9, %%ymm11, %%ymm11 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm10, %%xmm12  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm10, %%xmm13  \n\t"
        "minpd %%xmm13, %%xmm12 \n\t"
        "vmovupd %%xmm12, %%xmm13 \n\t"
        "PSRLDQ $1*8,   %%xmm13 \n\t"
        "minpd  %%xmm13, %%xmm12 \n\t"
		"\n\t"
        "VEXTRACTF64X2  $0,  %%ymm11, %%xmm14  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm11, %%xmm15  \n\t"
        "maxpd %%xmm15, %%xmm14 \n\t"
        "vmovupd %%xmm14, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "maxpd  %%xmm15, %%xmm14 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm12      \n\t"
    //         "maxpd %%xmm15, %%xmm14      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15  \n\t"
                "minpd %%xmm15,     %%xmm12  \n\t"
                "maxpd %%xmm15,     %%xmm14  \n\t"
                "subq $-1*8, %[x]            \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm12, %[min] \n\t"
        "movsd  %%xmm14, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_16_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<16){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vMOVDQU  0*8(%[x]), %%ymm0 \n\t"
        "vMOVDQU  4*8(%[x]), %%ymm1 \n\t"
        "vMOVDQU  8*8(%[x]), %%ymm2 \n\t"
        "vMOVDQU 12*8(%[x]), %%ymm3 \n\t"
        "\n\t"
        "vMOVDQU %%ymm0,    %%ymm4  \n\t"
        "vMOVDQU %%ymm0,    %%ymm5  \n\t"
        "\n\t"
        "vMOVDQU %%ymm1,    %%ymm6  \n\t"
        "vMOVDQU %%ymm1,    %%ymm7  \n\t"
        "\n\t"
        "vMOVDQU %%ymm2,    %%ymm8  \n\t"
        "vMOVDQU %%ymm2,    %%ymm9  \n\t"
        "\n\t"
        "vMOVDQU %%ymm3,    %%ymm10 \n\t"
        "vMOVDQU %%ymm3,    %%ymm11 \n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%ymm0, %%ymm4, %%ymm4   \n\t"
            "vPMAXSQ %%ymm0, %%ymm5, %%ymm5   \n\t"
            "vMOVDQU  0*8(%[x]), %%ymm0      \n\t"
            "\n\t"
            "vPMINSQ %%ymm1, %%ymm6, %%ymm6   \n\t"
            "vPMAXSQ %%ymm1, %%ymm7, %%ymm7   \n\t"
            "vMOVDQU  4*8(%[x]), %%ymm2      \n\t"
            "\n\t"
            "vPMINSQ %%ymm2, %%ymm8, %%ymm8   \n\t"
            "vPMAXSQ %%ymm2, %%ymm9, %%ymm9   \n\t"
            "vMOVDQU  8*8(%[x]), %%ymm3      \n\t"
            "\n\t"
            "vPMINSQ %%ymm3, %%ymm10, %%ymm10 \n\t"
            "vPMAXSQ %%ymm3, %%ymm11, %%ymm11 \n\t"
            "vMOVDQU 12*8(%[x]), %%ymm4      \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vPMINSQ %%ymm0, %%ymm4, %%ymm4   \n\t"
        "vPMAXSQ %%ymm0, %%ymm5, %%ymm5   \n\t"
        "\n\t"
        "vPMINSQ %%ymm1, %%ymm6, %%ymm6   \n\t"
        "vPMAXSQ %%ymm1, %%ymm7, %%ymm7   \n\t"
        "\n\t"
        "vPMINSQ %%ymm2, %%ymm8, %%ymm8   \n\t"
        "vPMAXSQ %%ymm2, %%ymm9, %%ymm9   \n\t"
        "\n\t"
        "vPMINSQ %%ymm3, %%ymm10, %%ymm10 \n\t"
        "vPMAXSQ %%ymm3, %%ymm11, %%ymm11 \n\t"
        "\n\t"
        ::
    );

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
    //         "vMOVDQU   4*8(%[x]), %%ymm1 \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm4, %%ymm4      \n\t"
    //         "vPMAXSQ  %%ymm1, %%ymm5, %%ymm5      \n\t"
    //         "subq $-8*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%ymm0 \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm4, %%ymm4      \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm5, %%ymm5      \n\t"
    //         "subq $-4*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]), %%xmm0 \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm4, %%xmm4      \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm5, %%xmm5      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vPMINSQ %%ymm4, %%ymm6, %%ymm6 \n\t"
		"vPMAXSQ %%ymm5, %%ymm7, %%ymm7 \n\t"
        "\n\t"
		"vPMINSQ %%ymm6, %%ymm8, %%ymm8 \n\t"
		"vPMAXSQ %%ymm7, %%ymm9, %%ymm9 \n\t"
        "\n\t"
		"vPMINSQ %%ymm8, %%ymm10, %%ymm10 \n\t"
		"vPMAXSQ %%ymm9, %%ymm11, %%ymm11 \n\t"
		"\n\t"
        "VEXTRACTI64X2  $0,  %%ymm10, %%xmm12  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm10, %%xmm13  \n\t"
        "vPMINSQ %%xmm13, %%xmm12, %%xmm12 \n\t"
        "vMOVDQU %%xmm12, %%xmm13 \n\t"
        "PSRLDQ $1*8,   %%xmm13 \n\t"
        "vPMINSQ  %%xmm13, %%xmm12, %%xmm12 \n\t"
		"\n\t"
        "VEXTRACTI64X2  $0,  %%ymm11, %%xmm14  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm11, %%xmm15  \n\t"
        "vPMAXSQ %%xmm15, %%xmm14, %%xmm14 \n\t"
        "vMOVDQU %%xmm14, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "vPMAXSQ  %%xmm15, %%xmm14, %%xmm14 \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "MOVQ   0*8(%[x]), %%xmm15         \n\t"
    //         "vPMINSQ %%xmm15, %%xmm12, %%xmm12 \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm14, %%xmm14 \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm12, %%xmm12 \n\t"
                "vPMAXSQ %%xmm15, %%xmm14, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm12, %[min] \n\t"
        "movsd  %%xmm14, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}


// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_16z_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<16){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd 0*8(%[x]), %%zmm0 \n\t"
        "vmovupd 8*8(%[x]), %%zmm1 \n\t"
        "\n\t"
        "vmovupd %%zmm0,    %%zmm2 \n\t"
        "vmovupd %%zmm0,    %%zmm3 \n\t"
        "\n\t"
        "vmovupd %%zmm1,    %%zmm4 \n\t"
        "vmovupd %%zmm1,    %%zmm5 \n\t"
        "\n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vmaxpd %%zmm0, %%zmm3, %%zmm3 \n\t"
            "vmovupd  0*8(%[x]), %%zmm0    \n\t"
            "\n\t"
            "vminpd %%zmm1, %%zmm4, %%zmm4 \n\t"
            "vmaxpd %%zmm1, %%zmm5, %%zmm5 \n\t"
            "vmovupd  8*8(%[x]), %%zmm1    \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vminpd %%zmm0, %%zmm2, %%zmm2 \n\t"
        "vmaxpd %%zmm0, %%zmm3, %%zmm3 \n\t"
        "\n\t"
        "vminpd %%zmm1, %%zmm4, %%zmm4 \n\t"
        "vmaxpd %%zmm1, %%zmm5, %%zmm5 \n\t"
        "\n\t"
        "vminpd %%zmm4, %%zmm2, %%zmm2 \n\t"
        "vmaxpd %%zmm5, %%zmm3, %%zmm3 \n\t"
        ::
    );

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vminpd  %%zmm0, %%zmm2, %%zmm2 \n\t"
    //         "vmaxpd  %%zmm0, %%zmm3, %%zmm3 \n\t"
    //         "subq $-8*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0    \n\t"
    //         "vminpd  %%ymm0, %%ymm2, %%ymm2 \n\t"
    //         "vmaxpd  %%ymm0, %%ymm3, %%ymm3 \n\t"
    //         "subq $-4*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm2      \n\t"
    //         "maxpd  %%xmm0, %%xmm3      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm2, %%ymm12  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm2, %%ymm13  \n\t"
        "vminpd %%ymm13, %%ymm12, %%ymm12 \n\t"
        "VEXTRACTF64X2  $0,  %%ymm12, %%xmm6  \n\t"
        "VEXTRACTF64X2  $1,  %%ymm12, %%xmm7  \n\t"
        "minpd %%xmm7, %%xmm6 \n\t"
        "vmovupd %%xmm6, %%xmm7 \n\t"
        "PSRLDQ $1*8,   %%xmm7 \n\t"
        "minpd  %%xmm7, %%xmm6 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm3, %%ymm12  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm3, %%ymm13  \n\t"
        "vmaxpd  %%ymm13, %%ymm12, %%ymm12     \n\t"
        "VEXTRACTF64X2  $0,  %%ymm12, %%xmm8   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm12, %%xmm9   \n\t"
        "maxpd %%xmm9, %%xmm8   \n\t"
        "vmovupd %%xmm8, %%xmm9 \n\t"
        "PSRLDQ $1*8,   %%xmm9  \n\t"
        "maxpd  %%xmm9, %%xmm8  \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm6      \n\t"
    //         "maxpd %%xmm15, %%xmm8      \n\t"
    //         "subq $-1*8, %[x]          \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm6  \n\t"
                "maxpd %%xmm15,     %%xmm8  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm6, %[min] \n\t"
        "movsd  %%xmm8, %[max]  \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_16z_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<16){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "VMOVDQU64 0*8(%[x]), %%zmm0 \n\t"
        "VMOVDQU64 8*8(%[x]), %%zmm1 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm0,    %%zmm2 \n\t"
        "VMOVDQU64 %%zmm0,    %%zmm3 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm1,    %%zmm4 \n\t"
        "VMOVDQU64 %%zmm1,    %%zmm5 \n\t"
        "subq $-16*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%zmm0, %%zmm2, %%zmm2 \n\t"
            "vPMAXSQ %%zmm0, %%zmm3, %%zmm3 \n\t"
            "VMOVDQU64  0*8(%[x]), %%zmm0     \n\t"
            "\n\t"
            "vPMINSQ %%zmm1, %%zmm4, %%zmm4 \n\t"
            "vPMAXSQ %%zmm1, %%zmm5, %%zmm5 \n\t"
            "VMOVDQU64  8*8(%[x]), %%zmm1     \n\t"
            "\n\t"
            "subq $-16*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vPMINSQ %%zmm0, %%zmm2, %%zmm2 \n\t"
        "vPMAXSQ %%zmm0, %%zmm3, %%zmm3 \n\t"
        "\n\t"
        "vPMINSQ %%zmm1, %%zmm4, %%zmm4 \n\t"
        "vPMAXSQ %%zmm1, %%zmm5, %%zmm5 \n\t"
        ::
    );

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%zmm0    \n\t"
    //         "vPMINSQ  %%zmm0, %%zmm2, %%zmm2 \n\t"
    //         "vPMAXSQ  %%zmm0, %%zmm3, %%zmm3 \n\t"
    //         "subq $-8*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vMOVDQU   0*8(%[x]), %%ymm0     \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm2, %%ymm2 \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm3, %%ymm3 \n\t"
    //         "subq $-4*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]), %%xmm0      \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm2, %%xmm2 \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm3, %%xmm3 \n\t"
    //         "subq $-2*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vPMINSQ %%zmm2, %%zmm4, %%zmm4 \n\t"
		"vPMAXSQ %%zmm3, %%zmm5, %%zmm5 \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm4, %%ymm12  \n\t"
        "VEXTRACTI64X4  $1,  %%zmm4, %%ymm13  \n\t"
        "vPMINSQ %%ymm13, %%ymm12, %%ymm12     \n\t"
        "VEXTRACTI64X2  $0,  %%ymm12, %%xmm6  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm12, %%xmm7  \n\t"
        "vPMINSQ %%xmm7, %%xmm6, %%xmm6      \n\t"
        "vMOVDQU %%xmm6, %%xmm7              \n\t"
        "PSRLDQ  $1*8,   %%xmm7              \n\t"
        "vPMINSQ %%xmm7, %%xmm6, %%xmm6      \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm5, %%ymm14  \n\t"
        "VEXTRACTI64X4  $1,  %%zmm5, %%ymm15  \n\t"
        "vPMAXSQ %%ymm15, %%ymm14, %%ymm14     \n\t"
        "VEXTRACTI64X2  $0,  %%ymm5, %%xmm8  \n\t"
        "VEXTRACTI64X2  $1,  %%ymm5, %%xmm9  \n\t"
        "vPMAXSQ %%xmm9, %%xmm8, %%xmm8      \n\t"
        "vMOVDQU %%xmm8, %%xmm9              \n\t"
        "PSRLDQ  $1*8,   %%xmm9              \n\t"
        "vPMAXSQ %%xmm9, %%xmm8, %%xmm8      \n\t"
		"\n\t"
        :[x]"=r"(x), [min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :"0"(x)
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "MOVQ   0*8(%[x]), %%xmm15       \n\t"
    //         "vPMINSQ %%xmm15, %%xmm6, %%xmm6 \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm8, %%xmm8 \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm6, %%xmm6 \n\t"
                "vPMAXSQ %%xmm15, %%xmm8, %%xmm8 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ    %%xmm6, %[min]              \n\t"
        "MOVQ    %%xmm8, %[max]              \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_32_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<32){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%zmm0 \n\t"
        "vmovupd  8*8(%[x]), %%zmm1 \n\t"
        "vmovupd 16*8(%[x]), %%zmm2 \n\t"
        "vmovupd 24*8(%[x]), %%zmm3 \n\t"
        "\n\t"
        "vmovupd %%zmm0,    %%zmm4  \n\t"
        "vmovupd %%zmm0,    %%zmm5  \n\t"
        "\n\t"
        "vmovupd %%zmm1,    %%zmm6  \n\t"
        "vmovupd %%zmm1,    %%zmm7  \n\t"
        "\n\t"
        "vmovupd %%zmm2,    %%zmm8  \n\t"
        "vmovupd %%zmm2,    %%zmm9  \n\t"
        "\n\t"
        "vmovupd %%zmm3,    %%zmm10 \n\t"
        "vmovupd %%zmm3,    %%zmm11 \n\t"
        "subq $-32*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%zmm0, %%zmm4, %%zmm4   \n\t"
            "vmaxpd %%zmm0, %%zmm5, %%zmm5   \n\t"
            "vmovupd  0*8(%[x]), %%zmm0      \n\t"
            "\n\t"
            "vminpd %%zmm1, %%zmm6, %%zmm6   \n\t"
            "vmaxpd %%zmm1, %%zmm7, %%zmm7   \n\t"
            "vmovupd  8*8(%[x]), %%zmm1      \n\t"
            "\n\t"
            "vminpd %%zmm2, %%zmm8, %%zmm8   \n\t"
            "vmaxpd %%zmm2, %%zmm9, %%zmm9   \n\t"
            "vmovupd 16*8(%[x]), %%zmm2      \n\t"
            "\n\t"
            "vminpd %%zmm3, %%zmm10, %%zmm10 \n\t"
            "vmaxpd %%zmm3, %%zmm11, %%zmm11 \n\t"
            "vmovupd 24*8(%[x]), %%zmm3      \n\t"
            "\n\t"
            "subq $-32*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vminpd %%zmm0, %%zmm4, %%zmm4   \n\t"
        "vmaxpd %%zmm0, %%zmm5, %%zmm5   \n\t"
        "\n\t"
        "vminpd %%zmm1, %%zmm6, %%zmm6   \n\t"
        "vmaxpd %%zmm1, %%zmm7, %%zmm7   \n\t"
        "\n\t"
        "vminpd %%zmm2, %%zmm8, %%zmm8   \n\t"
        "vmaxpd %%zmm2, %%zmm9, %%zmm9   \n\t"
        "\n\t"
        "vminpd %%zmm3, %%zmm10, %%zmm10 \n\t"
        "vmaxpd %%zmm3, %%zmm11, %%zmm11 \n\t"
        ::
    );

    // if (n_remain&16){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vminpd  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vmaxpd  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "\n\t"
    //         "vmovupd   8*8(%[x]), %%zmm1    \n\t"
    //         "vminpd  %%zmm1, %%zmm6, %%zmm6 \n\t"
    //         "vmaxpd  %%zmm1, %%zmm7, %%zmm7 \n\t"
    //         "\n\t"
    //         "subq $-16*8, %[x]              \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=16;
    // }

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vminpd  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vmaxpd  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "subq $-8*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0    \n\t"
    //         "vminpd  %%ymm0, %%ymm4, %%ymm4 \n\t"
    //         "vmaxpd  %%ymm0, %%ymm5, %%ymm5 \n\t"
    //         "subq $-4*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm4      \n\t"
    //         "maxpd  %%xmm0, %%xmm5      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vminpd %%zmm4, %%zmm6, %%zmm6   \n\t"
		"vmaxpd %%zmm5, %%zmm7, %%zmm7   \n\t"
        "\n\t"
		"vminpd %%zmm6, %%zmm8, %%zmm8   \n\t"
		"vmaxpd %%zmm7, %%zmm9, %%zmm9   \n\t"
        "\n\t"
		"vminpd %%zmm8, %%zmm10, %%zmm10 \n\t"
		"vmaxpd %%zmm9, %%zmm11, %%zmm11 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm10, %%ymm12  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm10, %%ymm13  \n\t"
        "vminpd %%ymm13, %%ymm12, %%ymm12      \n\t"
        "VEXTRACTF64X2  $0,  %%ymm12, %%xmm0   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm12, %%xmm1   \n\t"
        "minpd %%xmm1, %%xmm0                  \n\t"
        "vmovupd %%xmm0, %%xmm1                \n\t"
        "PSRLDQ $1*8,   %%xmm1                 \n\t"
        "minpd  %%xmm1, %%xmm0                 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm11, %%ymm14  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm11, %%ymm15  \n\t"
        "vmaxpd %%ymm15, %%ymm14, %%ymm14      \n\t"
        "VEXTRACTF64X2  $0,  %%ymm14, %%xmm2   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm14, %%xmm3   \n\t"
        "maxpd %%xmm3, %%xmm2                  \n\t"
        "vmovupd %%xmm2, %%xmm3                \n\t"
        "PSRLDQ $1*8,   %%xmm3                 \n\t"
        "maxpd  %%xmm3, %%xmm2                 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm0      \n\t"
    //         "maxpd %%xmm15, %%xmm2      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm0  \n\t"
                "maxpd %%xmm15,     %%xmm2  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm0, %[min]                 \n\t"
        "movsd  %%xmm2, %[max]                 \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_32_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<32){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "VMOVDQU64   0*8(%[x]), %%zmm0 \n\t"
        "VMOVDQU64   8*8(%[x]), %%zmm1 \n\t"
        "VMOVDQU64  16*8(%[x]), %%zmm2 \n\t"
        "VMOVDQU64  24*8(%[x]), %%zmm3 \n\t"
        "\n\t"
        "VMOVDQU64  %%zmm0,    %%zmm4  \n\t"
        "VMOVDQU64  %%zmm0,    %%zmm5  \n\t"
        "\n\t"
        "VMOVDQU64  %%zmm1,    %%zmm6  \n\t"
        "VMOVDQU64  %%zmm1,    %%zmm7  \n\t"
        "\n\t"
        "VMOVDQU64  %%zmm2,    %%zmm8  \n\t"
        "VMOVDQU64  %%zmm2,    %%zmm9  \n\t"
        "\n\t"
        "VMOVDQU64  %%zmm3,    %%zmm10 \n\t"
        "VMOVDQU64  %%zmm3,    %%zmm11 \n\t"
        "subq $-32*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%zmm0, %%zmm4, %%zmm4   \n\t"
            "vPMAXSQ %%zmm0, %%zmm5, %%zmm5   \n\t"
            "VMOVDQU64   0*8(%[x]), %%zmm0    \n\t"
            "\n\t"
            "vPMINSQ %%zmm1, %%zmm6, %%zmm6   \n\t"
            "vPMAXSQ %%zmm1, %%zmm7, %%zmm7   \n\t"
            "VMOVDQU64   8*8(%[x]), %%zmm1    \n\t"
            "\n\t"
            "vPMINSQ %%zmm2, %%zmm8, %%zmm8   \n\t"
            "vPMAXSQ %%zmm2, %%zmm9, %%zmm9   \n\t"
            "VMOVDQU64  16*8(%[x]), %%zmm2    \n\t"
            "\n\t"
            "vPMINSQ %%zmm3, %%zmm10, %%zmm10 \n\t"
            "vPMAXSQ %%zmm3, %%zmm11, %%zmm11 \n\t"
            "VMOVDQU64  24*8(%[x]), %%zmm3    \n\t"
            "\n\t"
            "subq $-32*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
        "vPMINSQ %%zmm0, %%zmm4, %%zmm4   \n\t"
        "vPMAXSQ %%zmm0, %%zmm5, %%zmm5   \n\t"
        "\n\t"
        "vPMINSQ %%zmm1, %%zmm6, %%zmm6   \n\t"
        "vPMAXSQ %%zmm1, %%zmm7, %%zmm7   \n\t"
        "\n\t"
        "vPMINSQ %%zmm2, %%zmm8, %%zmm8   \n\t"
        "vPMAXSQ %%zmm2, %%zmm9, %%zmm9   \n\t"
        "\n\t"
        "vPMINSQ %%zmm3, %%zmm10, %%zmm10 \n\t"
        "vPMAXSQ %%zmm3, %%zmm11, %%zmm11 \n\t"
        ::
    );

    // if (n_remain&16){
    //     __asm__ __volatile__ (
    //         "VMOVDQU64    0*8(%[x]),  %%zmm0 \n\t"
    //         "vPMINSQ  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vPMAXSQ  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "\n\t"
    //         "VMOVDQU64    8*8(%[x]),  %%zmm1 \n\t"
    //         "vPMINSQ  %%zmm1, %%zmm6, %%zmm6 \n\t"
    //         "vPMAXSQ  %%zmm1, %%zmm7, %%zmm7 \n\t"
    //         "\n\t"
    //         "subq $-16*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=16;
    // }

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "VMOVDQU64    0*8(%[x]),  %%zmm0 \n\t"
    //         "vPMINSQ  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vPMAXSQ  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "subq $-8*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "VMOVDQU  0*8(%[x]),      %%ymm0 \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm4, %%ymm4 \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm5, %%ymm5 \n\t"
    //         "subq $-4*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "MOVDQU   0*8(%[x]),      %%xmm0 \n\t"
    //         "vPMINSQ  %%xmm0, %%xmm4, %%xmm4 \n\t"
    //         "vPMAXSQ  %%xmm0, %%xmm5, %%xmm5 \n\t"
    //         "subq $-2*8, %[x]                \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vPMINSQ %%zmm4, %%zmm6, %%zmm6   \n\t"
		"vPMAXSQ %%zmm5, %%zmm7, %%zmm7   \n\t"
        "\n\t"
		"vPMINSQ %%zmm6, %%zmm8, %%zmm8   \n\t"
		"vPMAXSQ %%zmm7, %%zmm9, %%zmm9   \n\t"
        "\n\t"
		"vPMINSQ %%zmm8, %%zmm10, %%zmm10 \n\t"
		"vPMAXSQ %%zmm9, %%zmm11, %%zmm11 \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm10, %%ymm12  \n\t"
        "VEXTRACTI64X4  $1,  %%zmm10, %%ymm13  \n\t"
        "vPMINSQ %%ymm13, %%ymm12, %%ymm12     \n\t"
        "VEXTRACTI64X2  $0,  %%ymm12, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm12, %%xmm1   \n\t"
        "vPMINSQ %%xmm1, %%xmm0, %%xmm0        \n\t"
        "vMOVDQU %%xmm0, %%xmm1                \n\t"
        "PSRLDQ  $1*8,   %%xmm1                \n\t"
        "vPMINSQ %%xmm1, %%xmm0, %%xmm0        \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm11, %%ymm14  \n\t"
        "VEXTRACTI64X4  $1,  %%zmm11, %%ymm15  \n\t"
        "vPMAXSQ %%ymm15, %%ymm14, %%ymm14     \n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm2   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm3   \n\t"
        "vPMAXSQ %%xmm3, %%xmm2, %%xmm2        \n\t"
        "vMOVDQU %%xmm2, %%xmm3                \n\t"
        "PSRLDQ  $1*8,   %%xmm3                \n\t"
        "vPMAXSQ %%xmm3, %%xmm2, %%xmm2        \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "MOVQ   0*8(%[x]), %%xmm15       \n\t"
    //         "vPMINSQ %%xmm15, %%xmm0, %%xmm0 \n\t"
    //         "vPMAXSQ %%xmm15, %%xmm2, %%xmm2 \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ   0*8(%[x]), %%xmm15       \n\t"
                "vPMINSQ %%xmm15, %%xmm0, %%xmm0 \n\t"
                "vPMAXSQ %%xmm15, %%xmm2, %%xmm2 \n\t"
                "subq $-1*8, %[x] \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm0, %[min] \n\t"
        "MOVQ  %%xmm2, %[max] \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
// -----------------------------------------------------------------------------------
void get_minmax_unroll_64_a_r8(double *min, double *max, double x[], int64_t n){
	if (n<64){
		get_minmax_c_r8(min, max, x, n);
        return;
	}

    double tmp_min, tmp_max;

	int64_t unroll_size=64, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vmovupd  0*8(%[x]), %%zmm0 \n\t"
        "vmovupd  8*8(%[x]), %%zmm1 \n\t"
        "vmovupd 16*8(%[x]), %%zmm2 \n\t"
        "vmovupd 24*8(%[x]), %%zmm3 \n\t"
        "vmovupd 32*8(%[x]), %%zmm4 \n\t"
        "vmovupd 40*8(%[x]), %%zmm5 \n\t"
        "vmovupd 48*8(%[x]), %%zmm6 \n\t"
        "vmovupd 56*8(%[x]), %%zmm7 \n\t"
        "\n\t"
        "vmovupd %%zmm0,    %%zmm8  \n\t"
        "vmovupd %%zmm0,    %%zmm9  \n\t"
        "\n\t"
        "vmovupd %%zmm1,    %%zmm10 \n\t"
        "vmovupd %%zmm1,    %%zmm11 \n\t"
        "\n\t"
        "vmovupd %%zmm2,    %%zmm12 \n\t"
        "vmovupd %%zmm2,    %%zmm13 \n\t"
        "\n\t"
        "vmovupd %%zmm3,    %%zmm14 \n\t"
        "vmovupd %%zmm3,    %%zmm15 \n\t"
        "subq $-64*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vminpd %%zmm0, %%zmm8, %%zmm8   \n\t"
            "vmaxpd %%zmm0, %%zmm9, %%zmm9   \n\t"
            "vmovupd  0*8(%[x]), %%zmm0      \n\t"
            "\n\t"
            "vminpd %%zmm1, %%zmm10, %%zmm10   \n\t"
            "vmaxpd %%zmm1, %%zmm11, %%zmm11   \n\t"
            "vmovupd  8*8(%[x]), %%zmm1      \n\t"
            "\n\t"
            "vminpd %%zmm2, %%zmm12, %%zmm12   \n\t"
            "vmaxpd %%zmm2, %%zmm13, %%zmm13   \n\t"
            "vmovupd 16*8(%[x]), %%zmm2      \n\t"
            "\n\t"
            "vminpd %%zmm3, %%zmm14, %%zmm14 \n\t"
            "vmaxpd %%zmm3, %%zmm15, %%zmm15 \n\t"
            "vmovupd 24*8(%[x]), %%zmm3      \n\t"
            "\n\t"
            "vminpd %%zmm4, %%zmm8, %%zmm8   \n\t"
            "vmaxpd %%zmm4, %%zmm9, %%zmm9   \n\t"
            "vmovupd 32*8(%[x]), %%zmm4      \n\t"
            "\n\t"
            "vminpd %%zmm5, %%zmm10, %%zmm10   \n\t"
            "vmaxpd %%zmm5, %%zmm11, %%zmm11   \n\t"
            "vmovupd 40*8(%[x]), %%zmm5      \n\t"
            "\n\t"
            "vminpd %%zmm6, %%zmm12, %%zmm12   \n\t"
            "vmaxpd %%zmm6, %%zmm13, %%zmm13   \n\t"
            "vmovupd 48*8(%[x]), %%zmm6      \n\t"
            "\n\t"
            "vminpd %%zmm7, %%zmm14, %%zmm14 \n\t"
            "vmaxpd %%zmm7, %%zmm15, %%zmm15 \n\t"
            "vmovupd 56*8(%[x]), %%zmm7      \n\t"
            "\n\t"
            "subq $-64*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }
    __asm__ __volatile__ (
            "vminpd %%zmm0, %%zmm8, %%zmm8   \n\t"
            "vmaxpd %%zmm0, %%zmm9, %%zmm9   \n\t"
            "\n\t"
            "vminpd %%zmm1, %%zmm10, %%zmm10   \n\t"
            "vmaxpd %%zmm1, %%zmm11, %%zmm11   \n\t"
            "\n\t"
            "vminpd %%zmm2, %%zmm12, %%zmm12   \n\t"
            "vmaxpd %%zmm2, %%zmm13, %%zmm13   \n\t"
            "\n\t"
            "vminpd %%zmm3, %%zmm14, %%zmm14 \n\t"
            "vmaxpd %%zmm3, %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vminpd %%zmm4, %%zmm8, %%zmm8   \n\t"
            "vmaxpd %%zmm4, %%zmm9, %%zmm9   \n\t"
            "\n\t"
            "vminpd %%zmm5, %%zmm10, %%zmm10   \n\t"
            "vmaxpd %%zmm5, %%zmm11, %%zmm11   \n\t"
            "\n\t"
            "vminpd %%zmm6, %%zmm12, %%zmm12   \n\t"
            "vmaxpd %%zmm6, %%zmm13, %%zmm13   \n\t"
            "\n\t"
            "vminpd %%zmm7, %%zmm14, %%zmm14 \n\t"
            "vmaxpd %%zmm7, %%zmm15, %%zmm15 \n\t"
        ::
    );

    // if (n_remain&16){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vminpd  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vmaxpd  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "\n\t"
    //         "vmovupd   8*8(%[x]), %%zmm1    \n\t"
    //         "vminpd  %%zmm1, %%zmm6, %%zmm6 \n\t"
    //         "vmaxpd  %%zmm1, %%zmm7, %%zmm7 \n\t"
    //         "\n\t"
    //         "subq $-16*8, %[x]              \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=16;
    // }

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vminpd  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vmaxpd  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "subq $-8*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0    \n\t"
    //         "vminpd  %%ymm0, %%ymm4, %%ymm4 \n\t"
    //         "vmaxpd  %%ymm0, %%ymm5, %%ymm5 \n\t"
    //         "subq $-4*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm4      \n\t"
    //         "maxpd  %%xmm0, %%xmm5      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vminpd %%zmm8, %%zmm10, %%zmm10   \n\t"
		"vmaxpd %%zmm9, %%zmm11, %%zmm11   \n\t"
        "\n\t"
		"vminpd %%zmm10, %%zmm12, %%zmm12   \n\t"
		"vmaxpd %%zmm11, %%zmm13, %%zmm13   \n\t"
        "\n\t"
		"vminpd %%zmm12, %%zmm14, %%zmm14 \n\t"
		"vmaxpd %%zmm13, %%zmm15, %%zmm15 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm14, %%ymm0   \n\t"
        "VEXTRACTF64X4  $1,  %%zmm14, %%ymm1   \n\t"
        "vminpd %%ymm1, %%ymm0, %%ymm0         \n\t"
        "VEXTRACTF64X2  $0,  %%ymm0, %%xmm2    \n\t"
        "VEXTRACTF64X2  $1,  %%ymm0, %%xmm3    \n\t"
        "minpd %%xmm3, %%xmm2                  \n\t"
        "vmovupd %%xmm2, %%xmm3                \n\t"
        "PSRLDQ $1*8,   %%xmm3                 \n\t"
        "minpd  %%xmm3, %%xmm2                 \n\t"
		"\n\t"
        "VEXTRACTF64X4  $0,  %%zmm15, %%ymm4  \n\t"
        "VEXTRACTF64X4  $1,  %%zmm15, %%ymm5  \n\t"
        "vmaxpd %%ymm5, %%ymm4, %%ymm4      \n\t"
        "VEXTRACTF64X2  $0,  %%ymm4, %%xmm6   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm4, %%xmm7   \n\t"
        "maxpd %%xmm7, %%xmm6                  \n\t"
        "vmovupd %%xmm6, %%xmm7                \n\t"
        "PSRLDQ $1*8,   %%xmm7                 \n\t"
        "maxpd  %%xmm7, %%xmm6                 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm0      \n\t"
    //         "maxpd %%xmm15, %%xmm2      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "minpd %%xmm15,     %%xmm2  \n\t"
                "maxpd %%xmm15,     %%xmm6  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm2, %[min]                 \n\t"
        "movsd  %%xmm6, %[max]                 \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}

void get_minmax_unroll_64_a_i8(int64_t *min, int64_t *max, int64_t x[], int64_t n){
	if (n<64){
		get_minmax_c_i8(min, max, x, n);
        return;
	}

    int64_t tmp_min, tmp_max;

	int64_t unroll_size=64, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "VMOVDQU64  0*8(%[x]), %%zmm0 \n\t"
        "VMOVDQU64  8*8(%[x]), %%zmm1 \n\t"
        "VMOVDQU64 16*8(%[x]), %%zmm2 \n\t"
        "VMOVDQU64 24*8(%[x]), %%zmm3 \n\t"
        "VMOVDQU64 32*8(%[x]), %%zmm4 \n\t"
        "VMOVDQU64 40*8(%[x]), %%zmm5 \n\t"
        "VMOVDQU64 48*8(%[x]), %%zmm6 \n\t"
        "VMOVDQU64 56*8(%[x]), %%zmm7 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm0,    %%zmm8  \n\t"
        "VMOVDQU64 %%zmm0,    %%zmm9  \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm1,    %%zmm10 \n\t"
        "VMOVDQU64 %%zmm1,    %%zmm11 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm2,    %%zmm12 \n\t"
        "VMOVDQU64 %%zmm2,    %%zmm13 \n\t"
        "\n\t"
        "VMOVDQU64 %%zmm3,    %%zmm14 \n\t"
        "VMOVDQU64 %%zmm3,    %%zmm15 \n\t"
        "subq $-64*8, %[x] \n\t"
        :[x]"=r"(x)
        :"0"(x)
    );

    n_unroll--;
    while ( n_unroll-- ){
        __asm__ __volatile__ (
            "vPMINSQ %%zmm0, %%zmm8, %%zmm8   \n\t"
            "vPMAXSQ %%zmm0, %%zmm9, %%zmm9   \n\t"
            "VMOVDQU64  0*8(%[x]), %%zmm0      \n\t"
            "\n\t"
            "vPMINSQ %%zmm1, %%zmm10, %%zmm10   \n\t"
            "vPMAXSQ %%zmm1, %%zmm11, %%zmm11   \n\t"
            "VMOVDQU64  8*8(%[x]), %%zmm1      \n\t"
            "\n\t"
            "vPMINSQ %%zmm2, %%zmm12, %%zmm12   \n\t"
            "vPMAXSQ %%zmm2, %%zmm13, %%zmm13   \n\t"
            "VMOVDQU64 16*8(%[x]), %%zmm2      \n\t"
            "\n\t"
            "vPMINSQ %%zmm3, %%zmm14, %%zmm14 \n\t"
            "vPMAXSQ %%zmm3, %%zmm15, %%zmm15 \n\t"
            "VMOVDQU64 24*8(%[x]), %%zmm3      \n\t"
            "\n\t"
            "vPMINSQ %%zmm4, %%zmm8, %%zmm8   \n\t"
            "vPMAXSQ %%zmm4, %%zmm9, %%zmm9   \n\t"
            "VMOVDQU64 32*8(%[x]), %%zmm4      \n\t"
            "\n\t"
            "vPMINSQ %%zmm5, %%zmm10, %%zmm10   \n\t"
            "vPMAXSQ %%zmm5, %%zmm11, %%zmm11   \n\t"
            "VMOVDQU64 40*8(%[x]), %%zmm5      \n\t"
            "\n\t"
            "vPMINSQ %%zmm6, %%zmm12, %%zmm12   \n\t"
            "vPMAXSQ %%zmm6, %%zmm13, %%zmm13   \n\t"
            "VMOVDQU64 48*8(%[x]), %%zmm6      \n\t"
            "\n\t"
            "vPMINSQ %%zmm7, %%zmm14, %%zmm14 \n\t"
            "vPMAXSQ %%zmm7, %%zmm15, %%zmm15 \n\t"
            "VMOVDQU64 56*8(%[x]), %%zmm7      \n\t"
            "\n\t"
            "subq $-64*8, %[x] \n\t"
            "\n\t"
            :[x]"=r"(x)
            :"0"(x)
        );
    }

    __asm__ __volatile__ (
            "vPMINSQ %%zmm0, %%zmm8, %%zmm8   \n\t"
            "vPMAXSQ %%zmm0, %%zmm9, %%zmm9   \n\t"
            "\n\t"
            "vPMINSQ %%zmm1, %%zmm10, %%zmm10   \n\t"
            "vPMAXSQ %%zmm1, %%zmm11, %%zmm11   \n\t"
            "\n\t"
            "vPMINSQ %%zmm2, %%zmm12, %%zmm12   \n\t"
            "vPMAXSQ %%zmm2, %%zmm13, %%zmm13   \n\t"
            "\n\t"
            "vPMINSQ %%zmm3, %%zmm14, %%zmm14 \n\t"
            "vPMAXSQ %%zmm3, %%zmm15, %%zmm15 \n\t"
            "\n\t"
            "vPMINSQ %%zmm4, %%zmm8, %%zmm8   \n\t"
            "vPMAXSQ %%zmm4, %%zmm9, %%zmm9   \n\t"
            "\n\t"
            "vPMINSQ %%zmm5, %%zmm10, %%zmm10   \n\t"
            "vPMAXSQ %%zmm5, %%zmm11, %%zmm11   \n\t"
            "\n\t"
            "vPMINSQ %%zmm6, %%zmm12, %%zmm12   \n\t"
            "vPMAXSQ %%zmm6, %%zmm13, %%zmm13   \n\t"
            "\n\t"
            "vPMINSQ %%zmm7, %%zmm14, %%zmm14 \n\t"
            "vPMAXSQ %%zmm7, %%zmm15, %%zmm15 \n\t"
        ::
    );

    // if (n_remain&16){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vPMINSQ  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vPMAXSQ  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "\n\t"
    //         "vmovupd   8*8(%[x]), %%zmm1    \n\t"
    //         "vPMINSQ  %%zmm1, %%zmm6, %%zmm6 \n\t"
    //         "vPMAXSQ  %%zmm1, %%zmm7, %%zmm7 \n\t"
    //         "\n\t"
    //         "subq $-16*8, %[x]              \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=16;
    // }

    // if (n_remain&8){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%zmm0    \n\t"
    //         "vPMINSQ  %%zmm0, %%zmm4, %%zmm4 \n\t"
    //         "vPMAXSQ  %%zmm0, %%zmm5, %%zmm5 \n\t"
    //         "subq $-8*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=8;
    // }

    // if (n_remain&4){
    //     __asm__ __volatile__ (
    //         "vmovupd   0*8(%[x]), %%ymm0    \n\t"
    //         "vPMINSQ  %%ymm0, %%ymm4, %%ymm4 \n\t"
    //         "vPMAXSQ  %%ymm0, %%ymm5, %%ymm5 \n\t"
    //         "subq $-4*8, %[x]               \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=4;
    // }

    // if (n_remain&2){
    //     __asm__ __volatile__ (
    //         "movupd   0*8(%[x]), %%xmm0 \n\t"
    //         "minpd  %%xmm0, %%xmm4      \n\t"
    //         "maxpd  %%xmm0, %%xmm5      \n\t"
    //         "subq $-2*8, %[x]           \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=2;
    // }

	__asm__ __volatile__(
		"\n\t"
		"vPMINSQ %%zmm8, %%zmm10, %%zmm10   \n\t"
		"vPMAXSQ %%zmm9, %%zmm11, %%zmm11   \n\t"
        "\n\t"
		"vPMINSQ %%zmm10, %%zmm12, %%zmm12   \n\t"
		"vPMAXSQ %%zmm11, %%zmm13, %%zmm13   \n\t"
        "\n\t"
		"vPMINSQ %%zmm12, %%zmm14, %%zmm14 \n\t"
		"vPMAXSQ %%zmm13, %%zmm15, %%zmm15 \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm14, %%ymm0   \n\t"
        "VEXTRACTI64X4  $1,  %%zmm14, %%ymm1   \n\t"
        "vPMINSQ %%ymm1, %%ymm0, %%ymm0         \n\t"
        "VEXTRACTI64X2  $0,  %%ymm0, %%xmm2    \n\t"
        "VEXTRACTI64X2  $1,  %%ymm0, %%xmm3    \n\t"
        "vPMINSQ %%xmm3, %%xmm2, %%xmm2                  \n\t"
        "vMOVDQU %%xmm2, %%xmm3                \n\t"
        "PSRLDQ $1*8,   %%xmm3                 \n\t"
        "vPMINSQ  %%xmm3, %%xmm2, %%xmm2                 \n\t"
		"\n\t"
        "VEXTRACTI64X4  $0,  %%zmm15, %%ymm4  \n\t"
        "VEXTRACTI64X4  $1,  %%zmm15, %%ymm5  \n\t"
        "vPMAXSQ %%ymm5, %%ymm4, %%ymm4      \n\t"
        "VEXTRACTI64X2  $0,  %%ymm4, %%xmm6   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm4, %%xmm7   \n\t"
        "vPMAXSQ %%xmm7, %%xmm6, %%xmm6                  \n\t"
        "vMOVDQU %%xmm6, %%xmm7                \n\t"
        "PSRLDQ $1*8,   %%xmm7                 \n\t"
        "vPMAXSQ  %%xmm7, %%xmm6, %%xmm6                 \n\t"
		"\n\t"
        ::
	);

    // if (n_remain&1){
    //     __asm__ __volatile__ (
    //         "movsd   0*8(%[x]), %%xmm15 \n\t"
    //         "minpd %%xmm15, %%xmm0      \n\t"
    //         "maxpd %%xmm15, %%xmm2      \n\t"
    //         :[x]"=r"(x)
    //         :"0"(x)
    //     );
    //     n_remain-=1;
    // }
    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm15 \n\t"
                "vPMINSQ %%xmm15, %%xmm2, %%xmm2  \n\t"
                "vPMAXSQ %%xmm15, %%xmm4, %%xmm4  \n\t"
                "subq $-1*8, %[x]           \n\t"
                :[x]"=r"(x)
                :"0"(x)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm2, %[min]                 \n\t"
        "MOVQ  %%xmm4, %[max]                 \n\t"
		"\n\t"
        :[min]"=m"(tmp_min), [max]"=m"(tmp_max)
        :
	);

	*min = tmp_min;
	*max = tmp_max;
}
