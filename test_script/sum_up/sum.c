#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <omp.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

int64_t sum_naive_i8_c_(int64_t x[], int64_t n){
    int64_t res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

double sum_naive_r8_c_(double x[], int64_t n){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

int64_t sum_unroll_02_i8_c_(int64_t x[], int64_t n){
    int64_t ymm0,ymm1,ymm14,ymm15;

	int64_t res=0;
	ymm14 = 0;
	ymm15 = 0;
	int64_t n_unroll=(n>>1);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm14 = ymm14 + ymm0;
		ymm15 = ymm15 + ymm1;
		x+=2;
	}

	ymm0  = *(x  );
	ymm15 = ymm15 + ymm0;
	return(ymm14 + ymm15);
}

double sum_unroll_02_r8_c_(double x[], int64_t n){
    double ymm0,ymm1,ymm14,ymm15;

	double res=0e0;
	ymm14 = 0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n>>1);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm14 = ymm14 + ymm0;
		ymm15 = ymm15 + ymm1;
		x+=2;
	}

	ymm0  = *(x  );
	ymm15 = ymm15 + ymm0;
	return(ymm14 + ymm15);
}

int64_t sum_unroll_04_i8_c_(int64_t x[], int64_t n){
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t res=0;
	ymm12 = 0;
	ymm13 = 0;
	ymm14 = 0;
	ymm15 = 0;
	int64_t n_unroll=(n>>2);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm2  = *(x+2);
		ymm3  = *(x+3);
		ymm12 = ymm12 + ymm0;
		ymm13 = ymm13 + ymm1;
		ymm14 = ymm14 + ymm2;
		ymm15 = ymm15 + ymm3;
		x+=4;
	}

	int64_t n_unroll_remain=(n%4);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm12 + ymm13 + ymm14 + ymm15);
}

double sum_unroll_04_r8_c_(double x[], int64_t n){
    double ymm0,ymm1,ymm2,ymm3;
    double ymm12,ymm13,ymm14,ymm15;

	double res=0e0;
	ymm12 = 0e0;
	ymm13 = 0e0;
	ymm14 = 0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n>>2);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm2  = *(x+2);
		ymm3  = *(x+3);
		ymm12 = ymm12 + ymm0;
		ymm13 = ymm13 + ymm1;
		ymm14 = ymm14 + ymm2;
		ymm15 = ymm15 + ymm3;
		x+=4;
	}

	int64_t n_unroll_remain=(n%4);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm12 + ymm13 + ymm14 + ymm15);
}

int64_t sum_unroll_08_i8_c_(int64_t x[], int64_t n){
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t res=0e0;
	ymm12 = 0e0;
	ymm13 = 0e0;
	ymm14 = 0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n>>3);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm2  = *(x+2);
		ymm3  = *(x+3);
		ymm4  = *(x+4);
		ymm5  = *(x+5);
		ymm6  = *(x+6);
		ymm7  = *(x+7);
		ymm8  = ymm0  + ymm4;
		ymm9  = ymm1  + ymm5;
		ymm10 = ymm2  + ymm6;
		ymm11 = ymm3  + ymm7;
		ymm12 = ymm12 + ymm8;
		ymm13 = ymm13 + ymm9;
		ymm14 = ymm14 + ymm10;
		ymm15 = ymm15 + ymm11;
		x+=8;
	}

	int64_t n_unroll_remain=(n%8);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm12 = ymm12 + ymm0;
		x+=1;
	}
	return(ymm12 + ymm13 + ymm14 + ymm15);
}

double sum_unroll_08_r8_c_(double x[], int64_t n){
    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	double res=0e0;
	ymm12 = 0e0;
	ymm13 = 0e0;
	ymm14 = 0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n>>3);
	while( n_unroll-- ){ 
		ymm0  = *(x  );
		ymm1  = *(x+1);
		ymm2  = *(x+2);
		ymm3  = *(x+3);
		ymm4  = *(x+4);
		ymm5  = *(x+5);
		ymm6  = *(x+6);
		ymm7  = *(x+7);
		ymm8  = ymm0  + ymm4;
		ymm9  = ymm1  + ymm5;
		ymm10 = ymm2  + ymm6;
		ymm11 = ymm3  + ymm7;
		ymm12 = ymm12 + ymm8;
		ymm13 = ymm13 + ymm9;
		ymm14 = ymm14 + ymm10;
		ymm15 = ymm15 + ymm11;
		x+=8;
	}

	int64_t n_unroll_remain=(n%8);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm12 = ymm12 + ymm0;
		x+=1;
	}
	return(ymm12 + ymm13 + ymm14 + ymm15);
}

int64_t sum_unroll_15_i8_c_(int64_t x[], int64_t n){
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t res=0;
	ymm15 = 0;
	int64_t n_unroll=(n/15);
	while( n_unroll-- ){ 
		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;
	}

	int64_t n_unroll_remain=(n%15);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm15);
}

double sum_unroll_15_r8_c_(double x[], int64_t n){
    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	double res=0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n/15);
	while( n_unroll-- ){ 
		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;
	}

	int64_t n_unroll_remain=(n%15);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm15);
}

int64_t sum_unroll_30_i8_c_(int64_t x[], int64_t n){
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t res=0;
	ymm15 = 0;
	int64_t n_unroll=(n/30);
	while( n_unroll-- ){ 
		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;

		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;
	}

	int64_t n_unroll_remain=(n%30);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm15);
}

double sum_unroll_30_r8_c_(double x[], int64_t n){
    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	double res=0e0;
	ymm15 = 0e0;
	int64_t n_unroll=(n/30);
	while( n_unroll-- ){ 
		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;

		ymm0   = *(x  );
		ymm1   = *(x+1);
		ymm2   = *(x+2);
		ymm3   = *(x+3);
		ymm4   = *(x+4);
		ymm5   = *(x+5);
		ymm6   = *(x+6);
		ymm7   = *(x+7);
		ymm8   = *(x+8);
		ymm9   = *(x+9);
		ymm10  = *(x+10);
		ymm11  = *(x+11);
		ymm12  = *(x+12);
		ymm13  = *(x+13);
		ymm14  = *(x+14);

		ymm15 = ymm15 + ymm0;
		ymm15 = ymm15 + ymm1;
		ymm15 = ymm15 + ymm2;
		ymm15 = ymm15 + ymm3;
		ymm15 = ymm15 + ymm4;
		ymm15 = ymm15 + ymm5;
		ymm15 = ymm15 + ymm6;
		ymm15 = ymm15 + ymm7;
		ymm15 = ymm15 + ymm8;
		ymm15 = ymm15 + ymm9;
		ymm15 = ymm15 + ymm10;
		ymm15 = ymm15 + ymm11;
		ymm15 = ymm15 + ymm12;
		ymm15 = ymm15 + ymm13;
		ymm15 = ymm15 + ymm14;
		x+=15;
	}

	int64_t n_unroll_remain=(n%30);
	while( n_unroll_remain-- ){ 
		ymm0  = *(x  );
		ymm15 = ymm15 + ymm0;
		x+=1;
	}
	return(ymm15);
}


// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
int64_t sum_assembl_i8_04_C_(int64_t x[], int64_t n){
	if (n<4){
		return(sum_naive_i8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm0,  %%ymm0,  %%ymm0  \n\t"
		"vpxor %%ymm1,  %%ymm1,  %%ymm1  \n\t"
		"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
		"vpxor %%ymm3,  %%ymm3,  %%ymm3  \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 2;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// ----------------------------------------------------------------------------------
	// Main Part
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
			"VPADDQ  %%ymm0 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-4*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
		"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
		"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
		::
	);

	// ----------------------------------------------------------------------------------
	// Remainder Part
	int64_t n_rem=n%unroll_step_size;
	if (n_rem & 2){
		__asm__ __volatile__ (
			"MOVDQU 0*8(%[x]), %%xmm2 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVQ   0*8(%[x]), %%xmm3         \n\t"
			"PADDQ  %%xmm3,    %%xmm2         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	int64_t result;
	__asm__ __volatile__ (
		"PADDQ  %%xmm0, %%xmm2 \n\t"
		"MOVDQU %%xmm2, %%xmm3 \n\t"
		"PSRLDQ $8,     %%xmm2 \n\t"
		"PADDQ  %%xmm2, %%xmm3 \n\t"
		"movq %%xmm3, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

double sum_assembl_r8_04_C_(double x[], int64_t n){
	if (n<4){
		return(sum_naive_r8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm0,  %%ymm0,  %%ymm0  \n\t"
		"vpxor %%ymm1,  %%ymm1,  %%ymm1  \n\t"
		"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
		"vpxor %%ymm3,  %%ymm3,  %%ymm3  \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 2;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vmovupd 0*8(%[x]), %%ymm0           \n\t"
			"vaddpd  %%ymm0 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-4*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
		"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
		"addpd               %%xmm1,  %%xmm0    \n\t" // sum
		::
	);

	int64_t n_rem=n%unroll_step_size;
	__asm__ __volatile__ (
		"pxor  %%xmm2, %%xmm2 \n\t"
		"pxor  %%xmm3, %%xmm3 \n\t"
		::
	);

	if (n_rem & 2){
		__asm__ __volatile__ (
			"movupd 0*8(%[x]), %%xmm2 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"movsd 0*8(%[x]), %%xmm3         \n\t"
			"addpd  %%xmm3,   %%xmm2         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	double result;
	__asm__ __volatile__ (
		"addpd  %%xmm0, %%xmm2 \n\t"
		"movupd %%xmm2, %%xmm3 \n\t"
		"PSRLDQ $8, %%xmm2     \n\t"
		"addpd %%xmm2, %%xmm3  \n\t"
		"movsd %%xmm3, %[r]   \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

int64_t sum_assembl_i8_08_C_(int64_t x[], int64_t n){
	if (n<8){
		return(sum_naive_i8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
		"vpxor %%ymm4,  %%ymm4,  %%ymm4  \n\t"
		"vpxor %%ymm5,  %%ymm5,  %%ymm5  \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 3;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// ----------------------------------------------------------------------------------
	// Main Part
	__asm__ __volatile__(
		"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
		"vMOVDQU 4*8(%[x]), %%ymm1           \n\t"
		"\n\t"
		"subq $-8*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm2  \n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU 4*8(%[x]), %%ymm1           \n\t"
			"VPADDQ  %%ymm2 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"VPADDQ         %%ymm0 ,   %%ymm1,  %%ymm2  \n\t"
		"VPADDQ         %%ymm2 ,   %%ymm15, %%ymm15 \n\t"		
		"VEXTRACTI128   $0,  %%ymm15, %%xmm0        \n\t" // extract lower  128 bits
		"VEXTRACTI128   $1,  %%ymm15, %%xmm1        \n\t" // extract higher 128 bits
		"PADDQ               %%xmm1,  %%xmm0        \n\t" // sum
		::
	);

	// ----------------------------------------------------------------------------------
	// Remainder Part
	int64_t n_rem=n%unroll_step_size;
	if (n_rem & 4){
		__asm__ __volatile__ (
			"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
			"paddq  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVQ   0*8(%[x]), %%xmm5         \n\t"
			"PADDQ  %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	int64_t result;
	__asm__ __volatile__ (
		"MOVDQU %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"PADDQ  %%xmm1, %%xmm0 \n\t"
		"movq %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

double sum_assembl_r8_08_C_(double x[], int64_t n){
	if (n<8){
		return(sum_naive_r8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
		"vpxor %%ymm4,  %%ymm4,  %%ymm4  \n\t"
		"vpxor %%ymm5,  %%ymm5,  %%ymm5  \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 3;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll
	__asm__ __volatile__(
		"\n\t"
		"vmovupd 0*8(%[x]), %%ymm0           \n\t"
		"vmovupd 4*8(%[x]), %%ymm1           \n\t"
		"\n\t"
		"subq $-8*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vaddpd  %%ymm0, %%ymm1, %%ymm2   \n\t"
			"vmovupd 0*8(%[x]), %%ymm0        \n\t"
			"vmovupd 4*8(%[x]), %%ymm1        \n\t"
			"vaddpd  %%ymm2, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"vaddpd         %%ymm0 ,   %%ymm1,  %%ymm2  \n\t"
		"vaddpd         %%ymm2 ,   %%ymm15, %%ymm15 \n\t"		
		"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
		"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
		"addpd               %%xmm1,  %%xmm0    \n\t" // sum
		::
	);

	int64_t n_rem=n%unroll_step_size;
	__asm__ __volatile__ (
		"pxor  %%xmm2, %%xmm2 \n\t"
		"pxor  %%xmm3, %%xmm3 \n\t"
		::
	);

	if (n_rem & 4){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"movupd 0*8(%[x]), %%xmm4 \n\t"
			"addpd  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVSD   0*8(%[x]), %%xmm5         \n\t"
			"addpd   %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	double result;
	__asm__ __volatile__ (
		"movupd %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"addpd  %%xmm1, %%xmm0 \n\t"
		"movsd %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

int64_t sum_assembl_i8_16_C_(int64_t x[], int64_t n){
	if (n<16){
		return(sum_naive_i8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
		"vpxor %%ymm4,  %%ymm4,  %%ymm4  \n\t"
		"vpxor %%ymm5,  %%ymm5,  %%ymm5  \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 4;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// ----------------------------------------------------------------------------------
	// Main Part
	__asm__ __volatile__(
		"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
		"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
		"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
		"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
		"\n\t"
		"subq $-16*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm4  \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"VPADDQ  %%ymm4 ,   %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm5  \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"VPADDQ  %%ymm5 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"VPADDQ         %%ymm0,   %%ymm1,  %%ymm4  \n\t"
		"VPADDQ         %%ymm2,   %%ymm3,  %%ymm5  \n\t"
		"VPADDQ         %%ymm4,   %%ymm14, %%ymm14 \n\t"		
		"VPADDQ         %%ymm5,   %%ymm15, %%ymm15 \n\t"		
		"VPADDQ         %%ymm14,  %%ymm15, %%ymm15 \n\t"		
		"VEXTRACTI128   $0,  %%ymm15, %%xmm0       \n\t" // extract lower  128 bits
		"VEXTRACTI128   $1,  %%ymm15, %%xmm1       \n\t" // extract higher 128 bits
		"PADDQ               %%xmm1,  %%xmm0       \n\t" // sum
		::
	);

	// ----------------------------------------------------------------------------------
	// Remainder Part
	int64_t n_rem=n%unroll_step_size;
	if (n_rem & 8){
		__asm__ __volatile__ (
			"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
			"vMOVDQU        4*8(%[x]), %%ymm4    \n\t"
			"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-8*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 4){
		__asm__ __volatile__ (
			"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
			"paddq  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVQ   0*8(%[x]), %%xmm5         \n\t"
			"PADDQ  %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	int64_t result;
	__asm__ __volatile__ (
		"MOVDQU %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"PADDQ  %%xmm1, %%xmm0 \n\t"
		"movq %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

double sum_assembl_r8_16_C_(double x[], int64_t n){
	if (n<16){
		return(sum_naive_r8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 4;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll
	__asm__ __volatile__(
		"\n\t"
		"vmovupd  0*8(%[x]), %%ymm0           \n\t"
		"vmovupd  4*8(%[x]), %%ymm1           \n\t"
		"vmovupd  8*8(%[x]), %%ymm2           \n\t"
		"vmovupd 12*8(%[x]), %%ymm3           \n\t"
		"\n\t"
		"subq $-16*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vaddpd  %%ymm0, %%ymm1, %%ymm4   \n\t"
			"vmovupd  0*8(%[x]), %%ymm0        \n\t"
			"vmovupd  4*8(%[x]), %%ymm1        \n\t"
			"vaddpd  %%ymm4, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"vaddpd  %%ymm2, %%ymm3, %%ymm5   \n\t"
			"vmovupd  8*8(%[x]), %%ymm2        \n\t"
			"vmovupd 12*8(%[x]), %%ymm3        \n\t"
			"vaddpd  %%ymm5, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"vaddpd         %%ymm0 ,   %%ymm1,  %%ymm4  \n\t"
		"vaddpd         %%ymm4 ,   %%ymm14, %%ymm14 \n\t"		
		"vaddpd         %%ymm2 ,   %%ymm3,  %%ymm5  \n\t"
		"vaddpd         %%ymm5 ,   %%ymm15, %%ymm15 \n\t"		
		"vaddpd         %%ymm14,   %%ymm15, %%ymm15 \n\t"		
		"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
		"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
		"addpd               %%xmm1,  %%xmm0    \n\t" // sum
		::
	);

	int64_t n_rem=n%unroll_step_size;
	__asm__ __volatile__ (
		"pxor  %%xmm2, %%xmm2 \n\t"
		"pxor  %%xmm3, %%xmm3 \n\t"
		::
	);

	if (n_rem & 8){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"vmovupd        4*8(%[x]), %%ymm4    \n\t"
			"vaddpd         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-8*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 4){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"movupd 0*8(%[x]), %%xmm4 \n\t"
			"addpd  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVSD   0*8(%[x]), %%xmm5         \n\t"
			"addpd   %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	double result;
	__asm__ __volatile__ (
		"movupd %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"addpd  %%xmm1, %%xmm0 \n\t"
		"movsd %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

int64_t sum_assembl_i8_32_C_(int64_t x[], int64_t n){
	if (n<32){
		return(sum_naive_i8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 5;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// ----------------------------------------------------------------------------------
	// Main Part
	__asm__ __volatile__(
		"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
		"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
		"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
		"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
		"vMOVDQU 16*8(%[x]), %%ymm4           \n\t"
		"vMOVDQU 20*8(%[x]), %%ymm5           \n\t"
		"vMOVDQU 24*8(%[x]), %%ymm6           \n\t"
		"vMOVDQU 28*8(%[x]), %%ymm7           \n\t"
		"\n\t"
		"subq $-32*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm8  \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"VPADDQ  %%ymm8 ,   %%ymm12, %%ymm12 \n\t"
			"\n\t"
			"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm9  \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"VPADDQ  %%ymm9 ,   %%ymm13, %%ymm13 \n\t"
			"\n\t"
			"VPADDQ  %%ymm4 ,   %%ymm5,  %%ymm10  \n\t"
			"vMOVDQU 16*8(%[x]), %%ymm4           \n\t"
			"vMOVDQU 20*8(%[x]), %%ymm5           \n\t"
			"VPADDQ  %%ymm10 ,   %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"VPADDQ  %%ymm6 ,   %%ymm7,  %%ymm11  \n\t"
			"vMOVDQU 24*8(%[x]), %%ymm6           \n\t"
			"vMOVDQU 28*8(%[x]), %%ymm7           \n\t"
			"VPADDQ  %%ymm11 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm8  \n\t"
		"VPADDQ  %%ymm8 ,   %%ymm12, %%ymm12 \n\t"
		"\n\t"
		"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm9  \n\t"
		"VPADDQ  %%ymm9 ,   %%ymm13, %%ymm13 \n\t"
		"\n\t"
		"VPADDQ  %%ymm4 ,   %%ymm5,  %%ymm10  \n\t"
		"VPADDQ  %%ymm10 ,   %%ymm14, %%ymm14 \n\t"
		"\n\t"
		"VPADDQ  %%ymm6 ,   %%ymm7,  %%ymm11  \n\t"
		"VPADDQ  %%ymm11 ,   %%ymm15, %%ymm15 \n\t"
		"\n\t"
		"VPADDQ  %%ymm12, %%ymm15, %%ymm15    \n\t"
		"VPADDQ  %%ymm13, %%ymm15, %%ymm15    \n\t"
		"VPADDQ  %%ymm14, %%ymm15, %%ymm15    \n\t"
		"VEXTRACTI128   $0,  %%ymm15, %%xmm0       \n\t" // extract lower  128 bits
		"VEXTRACTI128   $1,  %%ymm15, %%xmm1       \n\t" // extract higher 128 bits
		"PADDQ               %%xmm1,  %%xmm0       \n\t" // sum
		::
	);

	// ----------------------------------------------------------------------------------
	// Remainder Part
	int64_t n_rem=n%unroll_step_size;
	if (n_rem & 16){
		__asm__ __volatile__ (
			"vMOVDQU         0*8(%[x]), %%ymm3    \n\t"
			"vMOVDQU         4*8(%[x]), %%ymm4    \n\t"
			"vMOVDQU         8*8(%[x]), %%ymm5    \n\t"
			"vMOVDQU        12*8(%[x]), %%ymm6    \n\t"
			"\n\t"
			"VPADDQ         %%ymm6, %%ymm3, %%ymm3 \n\t"
			"VPADDQ         %%ymm5, %%ymm3, %%ymm3 \n\t"
			"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-16*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 8){
		__asm__ __volatile__ (
			"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
			"vMOVDQU        4*8(%[x]), %%ymm4    \n\t"
			"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-8*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 4){
		__asm__ __volatile__ (
			"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"paddq          %%xmm2, %%xmm1       \n\t"
			"paddq          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
			"paddq  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVQ   0*8(%[x]), %%xmm5         \n\t"
			"PADDQ  %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	int64_t result;
	__asm__ __volatile__ (
		"MOVDQU %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"PADDQ  %%xmm1, %%xmm0 \n\t"
		"movq %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

double sum_assembl_r8_32_C_(double x[], int64_t n){
	if (n<32){
		return(sum_naive_r8_c_(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_step_size;
	pow2 = 5;
	unroll_step_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll
	__asm__ __volatile__(
		"\n\t"
		"vmovupd  0*8(%[x]), %%ymm0           \n\t"
		"vmovupd  4*8(%[x]), %%ymm1           \n\t"
		"vmovupd  8*8(%[x]), %%ymm2           \n\t"
		"vmovupd 12*8(%[x]), %%ymm3           \n\t"
		"vmovupd 16*8(%[x]), %%ymm4           \n\t"
		"vmovupd 20*8(%[x]), %%ymm5           \n\t"
		"vmovupd 24*8(%[x]), %%ymm6           \n\t"
		"vmovupd 28*8(%[x]), %%ymm7           \n\t"
		"\n\t"
		"subq $-32*8, %[x]\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	n_unroll--;
	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vaddpd  %%ymm0, %%ymm1, %%ymm8   \n\t"
			"vmovupd  0*8(%[x]), %%ymm0        \n\t"
			"vmovupd  4*8(%[x]), %%ymm1        \n\t"
			"vaddpd  %%ymm8, %%ymm12, %%ymm12 \n\t"
			"\n\t"
			"vaddpd  %%ymm2, %%ymm3, %%ymm9   \n\t"
			"vmovupd  8*8(%[x]), %%ymm2        \n\t"
			"vmovupd 12*8(%[x]), %%ymm3        \n\t"
			"vaddpd  %%ymm9, %%ymm13, %%ymm13 \n\t"
			"\n\t"
			"vaddpd  %%ymm4, %%ymm5, %%ymm10   \n\t"
			"vmovupd 16*8(%[x]), %%ymm4        \n\t"
			"vmovupd 20*8(%[x]), %%ymm5        \n\t"
			"vaddpd  %%ymm10, %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"vaddpd  %%ymm6, %%ymm7, %%ymm11   \n\t"
			"vmovupd 24*8(%[x]), %%ymm6        \n\t"
			"vmovupd 28*8(%[x]), %%ymm7        \n\t"
			"vaddpd  %%ymm11, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	__asm__ __volatile__(
		"\n\t"
		"vaddpd  %%ymm0, %%ymm1,  %%ymm8   \n\t"
		"vaddpd  %%ymm8, %%ymm12, %%ymm12 \n\t"
		"\n\t"
		"vaddpd  %%ymm2, %%ymm3,  %%ymm9   \n\t"
		"vaddpd  %%ymm9, %%ymm13, %%ymm13 \n\t"
		"\n\t"
		"vaddpd  %%ymm4,  %%ymm5,  %%ymm10   \n\t"
		"vaddpd  %%ymm10, %%ymm14, %%ymm14 \n\t"
		"\n\t"
		"vaddpd  %%ymm6,  %%ymm7,  %%ymm11   \n\t"
		"vaddpd  %%ymm11, %%ymm15, %%ymm15 \n\t"
		"\n\t"
		"vaddpd	%%ymm12, %%ymm15, %%ymm15 \n\t"
		"vaddpd	%%ymm13, %%ymm15, %%ymm15 \n\t"
		"vaddpd	%%ymm14, %%ymm15, %%ymm15 \n\t"
		"\n\t"
		"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
		"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
		"addpd               %%xmm1,  %%xmm0    \n\t" // sum
		::
	);

	int64_t n_rem=n%unroll_step_size;
	__asm__ __volatile__ (
		"pxor  %%xmm2, %%xmm2 \n\t"
		"pxor  %%xmm3, %%xmm3 \n\t"
		::
	);

	if (n_rem & 16){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"vmovupd        4*8(%[x]), %%ymm4    \n\t"
			"vmovupd        8*8(%[x]), %%ymm5    \n\t"
			"vmovupd       12*8(%[x]), %%ymm6    \n\t"
			"vaddpd         %%ymm6, %%ymm3, %%ymm3 \n\t"
			"vaddpd         %%ymm5, %%ymm3, %%ymm3 \n\t"
			"vaddpd         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-16*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 8){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"vmovupd        4*8(%[x]), %%ymm4    \n\t"
			"vaddpd         %%ymm4, %%ymm3, %%ymm3 \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-8*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 4){
		__asm__ __volatile__ (
			"vmovupd        0*8(%[x]), %%ymm3    \n\t"
			"VEXTRACTF64X2  $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
			"addpd          %%xmm2, %%xmm1       \n\t"
			"addpd          %%xmm1, %%xmm0       \n\t"
			"subq   $-4*8, %[x]                  \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 2){
		__asm__ __volatile__ (
			"movupd 0*8(%[x]), %%xmm4 \n\t"
			"addpd  %%xmm4,    %%xmm0 \n\t"
			"subq   $-2*8, %[x]       \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	if (n_rem & 1){
		__asm__ __volatile__ (
			"MOVSD   0*8(%[x]), %%xmm5         \n\t"
			"addpd   %%xmm5,    %%xmm0         \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}

	double result;
	__asm__ __volatile__ (
		"movupd %%xmm0, %%xmm1 \n\t"
		"PSRLDQ $8,     %%xmm1 \n\t"
		"addpd  %%xmm1, %%xmm0 \n\t"
		"movsd %%xmm0, %[r]     \n\t"
		:[r]"=m"(result)
		:
	);

	return result;
}

// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
int64_t sum_up_04_i8(int64_t x[], int64_t n){
	#if _default
		int64_t ymm0,ymm1,ymm2,ymm3;
		int64_t ymm12,ymm13,ymm14,ymm15;

		int64_t res=0;
		ymm12 = 0;
		ymm13 = 0;
		ymm14 = 0;
		ymm15 = 0;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm12 = ymm12 + ymm0;
			ymm13 = ymm13 + ymm1;
			ymm14 = ymm14 + ymm2;
			ymm15 = ymm15 + ymm3;
			x+=4;
		}

		int64_t n_unroll_remain=(n%4);
		while( n_unroll_remain-- ){ 
			ymm0  = *(x  );
			ymm15 = ymm15 + ymm0;
			x+=1;
		}
		return(ymm12 + ymm13 + ymm14 + ymm15);
	#elif _x86_64
		if (n<4){
			return(sum_naive_i8_c_(x,n));
		}

		// Zero Clear
		__asm__ __volatile__(
			"vpxor %%ymm0,  %%ymm0,  %%ymm0  \n\t"
			"vpxor %%ymm1,  %%ymm1,  %%ymm1  \n\t"
			"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
			"vpxor %%ymm3,  %%ymm3,  %%ymm3  \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			::
		);

		int64_t n_unroll, pow2, unroll_step_size;
		pow2 = 2;
		unroll_step_size = pow(2, pow2);
		n_unroll = (n>>pow2);  // Unroll

		// ----------------------------------------------------------------------------------
		// Main Part
		while( n_unroll-- ){ 
			__asm__ __volatile__(
				"\n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
				"VPADDQ  %%ymm0 ,   %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-4*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
			::
		);

		// ----------------------------------------------------------------------------------
		// Remainder Part
		int64_t n_rem=n%unroll_step_size;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm2 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ   0*8(%[x]), %%xmm3         \n\t"
				"PADDQ  %%xmm3,    %%xmm2         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"PADDQ  %%xmm0, %%xmm2 \n\t"
			"MOVDQU %%xmm2, %%xmm3 \n\t"
			"PSRLDQ $8,     %%xmm2 \n\t"
			"PADDQ  %%xmm2, %%xmm3 \n\t"
			"movq %%xmm3, %[r]     \n\t"
			:[r]"=m"(result)
			:
		);

		return result;
	#endif
}

int64_t sum_up_08_i8(int64_t x[], int64_t n){
	#if _default
		int64_t ymm0,ymm1,ymm2,ymm3;
		int64_t ymm4,ymm5,ymm6,ymm7;
		int64_t ymm8,ymm9,ymm10,ymm11;
		int64_t ymm12,ymm13,ymm14,ymm15;

		int64_t res=0e0;
		ymm12 = 0e0;
		ymm13 = 0e0;
		ymm14 = 0e0;
		ymm15 = 0e0;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm4  = *(x+4);
			ymm5  = *(x+5);
			ymm6  = *(x+6);
			ymm7  = *(x+7);
			ymm8  = ymm0  + ymm4;
			ymm9  = ymm1  + ymm5;
			ymm10 = ymm2  + ymm6;
			ymm11 = ymm3  + ymm7;
			ymm12 = ymm12 + ymm8;
			ymm13 = ymm13 + ymm9;
			ymm14 = ymm14 + ymm10;
			ymm15 = ymm15 + ymm11;
			x+=8;
		}

		int64_t n_unroll_remain=(n%8);
		while( n_unroll_remain-- ){ 
			ymm0  = *(x  );
			ymm12 = ymm12 + ymm0;
			x+=1;
		}
		return(ymm12 + ymm13 + ymm14 + ymm15);
	#elif _x86_64
		if (n<8){
			return(sum_naive_i8_c_(x,n));
		}

		// Zero Clear
		__asm__ __volatile__(
			"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
			"vpxor %%ymm4,  %%ymm4,  %%ymm4  \n\t"
			"vpxor %%ymm5,  %%ymm5,  %%ymm5  \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			::
		);

		int64_t n_unroll, pow2, unroll_step_size;
		pow2 = 3;
		unroll_step_size = pow(2, pow2);
		n_unroll = (n>>pow2);  // Unroll

		// ----------------------------------------------------------------------------------
		// Main Part
		__asm__ __volatile__(
			"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU 4*8(%[x]), %%ymm1           \n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__(
				"\n\t"
				"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm2  \n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1           \n\t"
				"VPADDQ  %%ymm2 ,   %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-8*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ         %%ymm0 ,   %%ymm1,  %%ymm2  \n\t"
			"VPADDQ         %%ymm2 ,   %%ymm15, %%ymm15 \n\t"		
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0        \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1        \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0        \n\t" // sum
			::
		);

		// ----------------------------------------------------------------------------------
		// Remainder Part
		int64_t n_rem=n%unroll_step_size;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-4*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"paddq  %%xmm4,    %%xmm0 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ   0*8(%[x]), %%xmm5         \n\t"
				"PADDQ  %%xmm5,    %%xmm0         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"MOVDQU %%xmm0, %%xmm1 \n\t"
			"PSRLDQ $8,     %%xmm1 \n\t"
			"PADDQ  %%xmm1, %%xmm0 \n\t"
			"movq %%xmm0, %[r]     \n\t"
			:[r]"=m"(result)
			:
		);

		return result;
	#endif
}

int64_t sum_up_16_i8(int64_t x[], int64_t n){
	#if _default
		int64_t ymm0,ymm1,ymm2,ymm3;
		int64_t ymm4,ymm5,ymm6,ymm7;
		int64_t ymm8,ymm9,ymm10,ymm11;
		int64_t ymm12,ymm13,ymm14,ymm15;

		int64_t res=0;
		ymm15 = 0;
		int64_t n_unroll=(n/15);
		while( n_unroll-- ){ 
			ymm0   = *(x  );
			ymm1   = *(x+1);
			ymm2   = *(x+2);
			ymm3   = *(x+3);
			ymm4   = *(x+4);
			ymm5   = *(x+5);
			ymm6   = *(x+6);
			ymm7   = *(x+7);
			ymm8   = *(x+8);
			ymm9   = *(x+9);
			ymm10  = *(x+10);
			ymm11  = *(x+11);
			ymm12  = *(x+12);
			ymm13  = *(x+13);
			ymm14  = *(x+14);

			ymm15 = ymm15 + ymm0;
			ymm15 = ymm15 + ymm1;
			ymm15 = ymm15 + ymm2;
			ymm15 = ymm15 + ymm3;
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;
			ymm15 = ymm15 + ymm8;
			ymm15 = ymm15 + ymm9;
			ymm15 = ymm15 + ymm10;
			ymm15 = ymm15 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;
			ymm15 = ymm15 + ymm14;
			x+=15;
		}

		int64_t n_unroll_remain=(n%15);
		while( n_unroll_remain-- ){ 
			ymm0  = *(x  );
			ymm15 = ymm15 + ymm0;
			x+=1;
		}
		return(ymm15);
	#elif _x86_64
		if (n<16){
			return(sum_naive_i8_c_(x,n));
		}

		// Zero Clear
		__asm__ __volatile__(
			"vpxor %%ymm2,  %%ymm2,  %%ymm2  \n\t"
			"vpxor %%ymm4,  %%ymm4,  %%ymm4  \n\t"
			"vpxor %%ymm5,  %%ymm5,  %%ymm5  \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			::
		);

		int64_t n_unroll, pow2, unroll_step_size;
		pow2 = 4;
		unroll_step_size = pow(2, pow2);
		n_unroll = (n>>pow2);  // Unroll

		// ----------------------------------------------------------------------------------
		// Main Part
		__asm__ __volatile__(
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__(
				"\n\t"
				"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm4  \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
				"VPADDQ  %%ymm4 ,   %%ymm14, %%ymm14 \n\t"
				"\n\t"
				"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm5  \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
				"VPADDQ  %%ymm5 ,   %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-16*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ         %%ymm0,   %%ymm1,  %%ymm4  \n\t"
			"VPADDQ         %%ymm2,   %%ymm3,  %%ymm5  \n\t"
			"VPADDQ         %%ymm4,   %%ymm14, %%ymm14 \n\t"		
			"VPADDQ         %%ymm5,   %%ymm15, %%ymm15 \n\t"		
			"VPADDQ         %%ymm14,  %%ymm15, %%ymm15 \n\t"		
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0       \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1       \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0       \n\t" // sum
			::
		);

		// ----------------------------------------------------------------------------------
		// Remainder Part
		int64_t n_rem=n%unroll_step_size;
		if (n_rem & 8){
			__asm__ __volatile__ (
				"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
				"vMOVDQU        4*8(%[x]), %%ymm4    \n\t"
				"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-8*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-4*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"paddq  %%xmm4,    %%xmm0 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ   0*8(%[x]), %%xmm5         \n\t"
				"PADDQ  %%xmm5,    %%xmm0         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"MOVDQU %%xmm0, %%xmm1 \n\t"
			"PSRLDQ $8,     %%xmm1 \n\t"
			"PADDQ  %%xmm1, %%xmm0 \n\t"
			"movq %%xmm0, %[r]     \n\t"
			:[r]"=m"(result)
			:
		);

		return result;
	#endif
}

int64_t sum_up_32_i8(int64_t x[], int64_t n){
	#if _default
		int64_t ymm0,ymm1,ymm2,ymm3;
		int64_t ymm4,ymm5,ymm6,ymm7;
		int64_t ymm8,ymm9,ymm10,ymm11;
		int64_t ymm12,ymm13,ymm14,ymm15;

		int64_t res=0;
		ymm15 = 0;
		int64_t n_unroll=(n/30);
		while( n_unroll-- ){ 
			ymm0   = *(x  );
			ymm1   = *(x+1);
			ymm2   = *(x+2);
			ymm3   = *(x+3);
			ymm4   = *(x+4);
			ymm5   = *(x+5);
			ymm6   = *(x+6);
			ymm7   = *(x+7);
			ymm8   = *(x+8);
			ymm9   = *(x+9);
			ymm10  = *(x+10);
			ymm11  = *(x+11);
			ymm12  = *(x+12);
			ymm13  = *(x+13);
			ymm14  = *(x+14);

			ymm15 = ymm15 + ymm0;
			ymm15 = ymm15 + ymm1;
			ymm15 = ymm15 + ymm2;
			ymm15 = ymm15 + ymm3;
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;
			ymm15 = ymm15 + ymm8;
			ymm15 = ymm15 + ymm9;
			ymm15 = ymm15 + ymm10;
			ymm15 = ymm15 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;
			ymm15 = ymm15 + ymm14;
			x+=15;

			ymm0   = *(x  );
			ymm1   = *(x+1);
			ymm2   = *(x+2);
			ymm3   = *(x+3);
			ymm4   = *(x+4);
			ymm5   = *(x+5);
			ymm6   = *(x+6);
			ymm7   = *(x+7);
			ymm8   = *(x+8);
			ymm9   = *(x+9);
			ymm10  = *(x+10);
			ymm11  = *(x+11);
			ymm12  = *(x+12);
			ymm13  = *(x+13);
			ymm14  = *(x+14);

			ymm15 = ymm15 + ymm0;
			ymm15 = ymm15 + ymm1;
			ymm15 = ymm15 + ymm2;
			ymm15 = ymm15 + ymm3;
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;
			ymm15 = ymm15 + ymm8;
			ymm15 = ymm15 + ymm9;
			ymm15 = ymm15 + ymm10;
			ymm15 = ymm15 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;
			ymm15 = ymm15 + ymm14;
			x+=15;
		}

		int64_t n_unroll_remain=(n%30);
		while( n_unroll_remain-- ){ 
			ymm0  = *(x  );
			ymm15 = ymm15 + ymm0;
			x+=1;
		}
		return(ymm15);
	#elif _x86_64
		if (n<32){
			return(sum_naive_i8_c_(x,n));
		}

		// Zero Clear
		__asm__ __volatile__(
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			::
		);

		int64_t n_unroll, pow2, unroll_step_size;
		pow2 = 5;
		unroll_step_size = pow(2, pow2);
		n_unroll = (n>>pow2);  // Unroll

		// ----------------------------------------------------------------------------------
		// Main Part
		__asm__ __volatile__(
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"vMOVDQU 16*8(%[x]), %%ymm4           \n\t"
			"vMOVDQU 20*8(%[x]), %%ymm5           \n\t"
			"vMOVDQU 24*8(%[x]), %%ymm6           \n\t"
			"vMOVDQU 28*8(%[x]), %%ymm7           \n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__(
				"\n\t"
				"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm8  \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
				"VPADDQ  %%ymm8 ,   %%ymm12, %%ymm12 \n\t"
				"\n\t"
				"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm9  \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
				"VPADDQ  %%ymm9 ,   %%ymm13, %%ymm13 \n\t"
				"\n\t"
				"VPADDQ  %%ymm4 ,   %%ymm5,  %%ymm10  \n\t"
				"vMOVDQU 16*8(%[x]), %%ymm4           \n\t"
				"vMOVDQU 20*8(%[x]), %%ymm5           \n\t"
				"VPADDQ  %%ymm10 ,   %%ymm14, %%ymm14 \n\t"
				"\n\t"
				"VPADDQ  %%ymm6 ,   %%ymm7,  %%ymm11  \n\t"
				"vMOVDQU 24*8(%[x]), %%ymm6           \n\t"
				"vMOVDQU 28*8(%[x]), %%ymm7           \n\t"
				"VPADDQ  %%ymm11 ,   %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-32*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ  %%ymm0 ,   %%ymm1,  %%ymm8  \n\t"
			"VPADDQ  %%ymm8 ,   %%ymm12, %%ymm12 \n\t"
			"\n\t"
			"VPADDQ  %%ymm2 ,   %%ymm3,  %%ymm9  \n\t"
			"VPADDQ  %%ymm9 ,   %%ymm13, %%ymm13 \n\t"
			"\n\t"
			"VPADDQ  %%ymm4 ,   %%ymm5,  %%ymm10  \n\t"
			"VPADDQ  %%ymm10 ,   %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"VPADDQ  %%ymm6 ,   %%ymm7,  %%ymm11  \n\t"
			"VPADDQ  %%ymm11 ,   %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"VPADDQ  %%ymm12, %%ymm15, %%ymm15    \n\t"
			"VPADDQ  %%ymm13, %%ymm15, %%ymm15    \n\t"
			"VPADDQ  %%ymm14, %%ymm15, %%ymm15    \n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0       \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1       \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0       \n\t" // sum
			::
		);

		// ----------------------------------------------------------------------------------
		// Remainder Part
		int64_t n_rem=n%unroll_step_size;
		if (n_rem & 16){
			__asm__ __volatile__ (
				"vMOVDQU         0*8(%[x]), %%ymm3    \n\t"
				"vMOVDQU         4*8(%[x]), %%ymm4    \n\t"
				"vMOVDQU         8*8(%[x]), %%ymm5    \n\t"
				"vMOVDQU        12*8(%[x]), %%ymm6    \n\t"
				"\n\t"
				"VPADDQ         %%ymm6, %%ymm3, %%ymm3 \n\t"
				"VPADDQ         %%ymm5, %%ymm3, %%ymm3 \n\t"
				"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-16*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 8){
			__asm__ __volatile__ (
				"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
				"vMOVDQU        4*8(%[x]), %%ymm4    \n\t"
				"VPADDQ         %%ymm4, %%ymm3, %%ymm3 \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-8*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"vMOVDQU        0*8(%[x]), %%ymm3    \n\t"
				"VEXTRACTI128   $0,  %%ymm3, %%xmm1  \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm3, %%xmm2  \n\t" // extract higher 128 bits
				"paddq          %%xmm2, %%xmm1       \n\t"
				"paddq          %%xmm1, %%xmm0       \n\t"
				"subq   $-4*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"paddq  %%xmm4,    %%xmm0 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ   0*8(%[x]), %%xmm5         \n\t"
				"PADDQ  %%xmm5,    %%xmm0         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"MOVDQU %%xmm0, %%xmm1 \n\t"
			"PSRLDQ $8,     %%xmm1 \n\t"
			"PADDQ  %%xmm1, %%xmm0 \n\t"
			"movq %%xmm0, %[r]     \n\t"
			:[r]"=m"(result)
			:
		);

		return result;
	#endif
}


// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
void sum_up_matrix_naive_r8_C(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
	double tmp;
	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=0e0;
		for(ii=0; ii<n; ii++){
			tmp += x[ii+j0];
		}
		x_sum[jj]=tmp;
	}
}

void sum_up_matrix_naive_i8_C(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
	int64_t tmp;
	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=0e0;
		for(ii=0; ii<n; ii++){
			tmp += x[ii+j0];
		}
		x_sum[jj]=tmp;
	}
}

void sum_up_matrix_unroll_02_r8_C(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    double ymm0,ymm1,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm14 = ymm14 + ymm0;
			ymm15 = ymm15 + ymm1;
			x+=2;
		}
		ymm15 += ymm14;

		int64_t n_remain=n%2;
		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_02_i8_C(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    int64_t ymm0,ymm1,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm14 = ymm14 + ymm0;
			ymm15 = ymm15 + ymm1;
			x+=2;
		}
		ymm15 += ymm14;

		int64_t n_remain=n%2;
		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_04_r8_C(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    double ymm0,ymm1,ymm2,ymm3;
    double ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm12 = ymm12 + ymm0;
			ymm13 = ymm13 + ymm1;
			ymm14 = ymm14 + ymm2;
			ymm15 = ymm15 + ymm3;
			x+=4;
		}
		ymm15 += ymm14;
		ymm15 += ymm13;
		ymm15 += ymm12;

		int64_t n_remain=n%4;
		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_04_i8_C(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm12 = ymm12 + ymm0;
			ymm13 = ymm13 + ymm1;
			ymm14 = ymm14 + ymm2;
			ymm15 = ymm15 + ymm3;
			x+=4;
		}
		ymm15 += ymm14;
		ymm15 += ymm13;
		ymm15 += ymm12;

		int64_t n_remain=n%4;
		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_08_r8_C(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm4  = *(x+4);
			ymm5  = *(x+5);
			ymm6  = *(x+6);
			ymm7  = *(x+7);
			ymm8  = ymm8  + ymm0;
			ymm9  = ymm9  + ymm1;
			ymm10 = ymm10 + ymm2;
			ymm11 = ymm11 + ymm3;
			ymm12 = ymm12 + ymm4;
			ymm13 = ymm13 + ymm5;
			ymm14 = ymm14 + ymm6;
			ymm15 = ymm15 + ymm7;
			x+=8;
		}
		ymm15 += ymm14;
		ymm15 += ymm13;
		ymm15 += ymm12;
		ymm15 += ymm11;
		ymm15 += ymm10;
		ymm15 += ymm9;
		ymm15 += ymm8;

		int64_t n_remain=n%8;
		if(n_remain & 4){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			x+=4;
		}

		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_08_i8_C(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm4  = *(x+4);
			ymm5  = *(x+5);
			ymm6  = *(x+6);
			ymm7  = *(x+7);
			ymm8  = ymm8  + ymm0;
			ymm9  = ymm9  + ymm1;
			ymm10 = ymm10 + ymm2;
			ymm11 = ymm11 + ymm3;
			ymm12 = ymm12 + ymm4;
			ymm13 = ymm13 + ymm5;
			ymm14 = ymm14 + ymm6;
			ymm15 = ymm15 + ymm7;
			x+=8;
		}
		ymm15 += ymm14;
		ymm15 += ymm13;
		ymm15 += ymm12;
		ymm15 += ymm11;
		ymm15 += ymm10;
		ymm15 += ymm9;
		ymm15 += ymm8;

		int64_t n_remain=n%8;
		if(n_remain & 4){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			x+=4;
		}

		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_15_r8_C(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm15=0e0;
		int64_t n_unroll=(n/15);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm4  = *(x+4);
			ymm5  = *(x+5);
			ymm6  = *(x+6);
			ymm7  = *(x+7);
			ymm8  = *(x+8);
			ymm9  = *(x+9);
			ymm10 = *(x+10);
			ymm11 = *(x+11);
			ymm12 = *(x+12);
			ymm13 = *(x+13);
			ymm14 = *(x+14);
			ymm15 = ymm15 + ymm0;
			ymm15 = ymm15 + ymm1;
			ymm15 = ymm15 + ymm2;
			ymm15 = ymm15 + ymm3;
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;
			ymm15 = ymm15 + ymm8;
			ymm15 = ymm15 + ymm9;
			ymm15 = ymm15 + ymm10;
			ymm15 = ymm15 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;
			ymm15 = ymm15 + ymm14;
			x+=15;
		}

		int64_t n_remain=n%15;
		if(n_remain & 8){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			ymm15 += *(x+4);
			ymm15 += *(x+5);
			ymm15 += *(x+6);
			ymm15 += *(x+7);
			x+=8;
		}

		if(n_remain & 4){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			x+=4;
		}

		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

void sum_up_matrix_unroll_15_i8_C(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		ymm15=0e0;
		int64_t n_unroll=(n/15);
		while( n_unroll-- ){ 
			ymm0  = *(x  );
			ymm1  = *(x+1);
			ymm2  = *(x+2);
			ymm3  = *(x+3);
			ymm4  = *(x+4);
			ymm5  = *(x+5);
			ymm6  = *(x+6);
			ymm7  = *(x+7);
			ymm8  = *(x+8);
			ymm9  = *(x+9);
			ymm10 = *(x+10);
			ymm11 = *(x+11);
			ymm12 = *(x+12);
			ymm13 = *(x+13);
			ymm14 = *(x+14);
			ymm15 = ymm15 + ymm0;
			ymm15 = ymm15 + ymm1;
			ymm15 = ymm15 + ymm2;
			ymm15 = ymm15 + ymm3;
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;
			ymm15 = ymm15 + ymm8;
			ymm15 = ymm15 + ymm9;
			ymm15 = ymm15 + ymm10;
			ymm15 = ymm15 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;
			ymm15 = ymm15 + ymm14;
			x+=15;
		}

		int64_t n_remain=n%15;
		if(n_remain & 8){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			ymm15 += *(x+4);
			ymm15 += *(x+5);
			ymm15 += *(x+6);
			ymm15 += *(x+7);
			x+=8;
		}

		if(n_remain & 4){
			ymm15 += *(x);
			ymm15 += *(x+1);
			ymm15 += *(x+2);
			ymm15 += *(x+3);
			x+=4;
		}

		if(n_remain & 2){
			ymm15 += *(x);
			ymm15 += *(x+1);
			x+=2;
		}

		if(n_remain & 1){
			ymm15 += *(x);
			x+=1;
		}

		x_sum[jj]=ymm15;
	}
}

// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
void sum_up_matrix_unroll_04_r8_ASM(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<4){
		sum_up_matrix_naive_r8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x]), %%ymm0           \n\t"
			"subq $-4*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm15, %%ymm15 \n\t"
				"vmovupd 0*8(%[x]), %%ymm0      \n\t"
				"subq $-4*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm15, %%ymm15        \n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			"vpxor          %%ymm3, %%ymm3, %%ymm3  \n\t"
			::
		);

		int64_t n_rem=n%4;
		__asm__ __volatile__ (
			"pxor  %%xmm2, %%xmm2 \n\t"
			"pxor  %%xmm3, %%xmm3 \n\t"
			::
		);

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm2 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x]), %%xmm3 \n\t"
				"addpd  %%xmm3,   %%xmm2 \n\t"
				"subq   $-1*8, %[x]      \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		double result;
		__asm__ __volatile__ (
			"addpd  %%xmm0, %%xmm2 \n\t"
			"movupd %%xmm2, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm2     \n\t"
			"addpd %%xmm2, %%xmm3  \n\t"
			"movsd %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_04_i8_ASM(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<4){
		sum_up_matrix_naive_i8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>2);

		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
			"subq $-4*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"VPADDQ %%ymm0, %%ymm15, %%ymm15 \n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0      \n\t"
				"subq $-4*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ %%ymm0, %%ymm15, %%ymm15        \n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm2 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ 0*8(%[x]), %%xmm3  \n\t"
				"PADDQ  %%xmm3,   %%xmm2  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"PADDQ  %%xmm0, %%xmm2 \n\t"
			"MOVDQU %%xmm2, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm2     \n\t"
			"PADDQ %%xmm2, %%xmm3  \n\t"
			"MOVQ %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_08_r8_ASM(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<8){
		sum_up_matrix_naive_r8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>3);

		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x]), %%ymm0           \n\t"
			"vmovupd 4*8(%[x]), %%ymm1           \n\t"
			"subq $-8*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vmovupd 0*8(%[x]), %%ymm0      \n\t"
				"vmovupd 4*8(%[x]), %%ymm1      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"subq $-8*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm1, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm5 \n\t"
				"movupd 2*8(%[x]), %%xmm6 \n\t"
				"addpd  %%xmm5, %%xmm6    \n\t"
				"addpd  %%xmm6, %%xmm2    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm4 \n\t"
				"addpd  %%xmm4,    %%xmm2 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x]), %%xmm3  \n\t"
				"addpd  %%xmm3,   %%xmm2  \n\t"
				"subq   $-1*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		double result;
		__asm__ __volatile__ (
			"addpd  %%xmm0, %%xmm2 \n\t"
			"movupd %%xmm2, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm2     \n\t"
			"addpd %%xmm2, %%xmm3  \n\t"
			"movsd %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_08_i8_ASM(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<8){
		sum_up_matrix_naive_i8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>3);
		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU 4*8(%[x]), %%ymm1           \n\t"
			"subq $-8*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"VPADDQ %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0      \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1      \n\t"
				"VPADDQ %%ymm14, %%ymm15, %%ymm15 \n\t"
				"subq $-8*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ %%ymm0,  %%ymm1,  %%ymm14        \n\t"
			"VPADDQ %%ymm14, %%ymm15, %%ymm15        \n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm5 \n\t"
				"MOVDQU 2*8(%[x]), %%xmm6 \n\t"
				"PADDQ  %%xmm5, %%xmm0    \n\t"
				"PADDQ  %%xmm6, %%xmm0    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"PADDQ  %%xmm4, %%xmm0    \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ 0*8(%[x]), %%xmm3  \n\t"
				"PADDQ  %%xmm3,  %%xmm0 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"PADDQ  %%xmm0, %%xmm2 \n\t"
			"MOVDQU %%xmm2, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm2     \n\t"
			"PADDQ %%xmm2, %%xmm3  \n\t"
			"MOVQ %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_16_r8_ASM(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<16){
		sum_up_matrix_naive_r8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>4);

		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd  0*8(%[x]), %%ymm0           \n\t"
			"vmovupd  4*8(%[x]), %%ymm1           \n\t"
			"vmovupd  8*8(%[x]), %%ymm2           \n\t"
			"vmovupd 12*8(%[x]), %%ymm3           \n\t"
			"subq $-16*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vmovupd  0*8(%[x]), %%ymm0      \n\t"
				"vmovupd  4*8(%[x]), %%ymm1      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm3, %%ymm14 \n\t"
				"vmovupd  8*8(%[x]), %%ymm2      \n\t"
				"vmovupd  12*8(%[x]), %%ymm3      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"subq $-16*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm1, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"vaddpd %%ymm2, %%ymm3, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%16;
		if (n_rem & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm5 \n\t"
				"vmovupd 4*8(%[x]), %%ymm6 \n\t"
				"vaddpd  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTF64X2   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTF64X2   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"addpd %%xmm2, %%xmm0 \n\t"
				"addpd %%xmm3, %%xmm0 \n\t"
				"subq   $-8*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm5 \n\t"
				"movupd 2*8(%[x]), %%xmm6 \n\t"
				"addpd  %%xmm5, %%xmm6    \n\t"
				"addpd  %%xmm6, %%xmm0    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm4 \n\t"
				"addpd  %%xmm4,    %%xmm0 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x]), %%xmm3  \n\t"
				"addpd  %%xmm3,   %%xmm0  \n\t"
				"subq   $-1*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		double result;
		__asm__ __volatile__ (
			"movupd %%xmm0, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm0     \n\t"
			"addpd %%xmm0, %%xmm3  \n\t"
			"movsd %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_16_i8_ASM(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<16){
		sum_up_matrix_naive_i8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>4);
		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"subq $-16*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"VPADDQ %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0      \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1      \n\t"
				"VPADDQ %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"VPADDQ %%ymm2, %%ymm3, %%ymm13 \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2      \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3      \n\t"
				"VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
				"subq $-16*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ %%ymm0,  %%ymm1,  %%ymm14        \n\t"
			"VPADDQ %%ymm14, %%ymm15, %%ymm15        \n\t"
			"VPADDQ %%ymm2,  %%ymm3,  %%ymm13        \n\t"
			"VPADDQ %%ymm13, %%ymm15, %%ymm15        \n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%16;
		if (n_rem & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm5 \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm6 \n\t"
				"vPADDQ  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTI128   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"paddq %%xmm2, %%xmm0 \n\t"
				"paddq %%xmm3, %%xmm0 \n\t"
				"subq   $-8*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm5 \n\t"
				"MOVDQU 2*8(%[x]), %%xmm6 \n\t"
				"PADDQ  %%xmm5, %%xmm0    \n\t"
				"PADDQ  %%xmm6, %%xmm0    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"PADDQ  %%xmm4, %%xmm0    \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ 0*8(%[x]), %%xmm3  \n\t"
				"PADDQ  %%xmm3,  %%xmm0 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"MOVDQU %%xmm0, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm0     \n\t"
			"PADDQ %%xmm0, %%xmm3  \n\t"
			"MOVQ %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_32_r8_ASM(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<32){
		sum_up_matrix_naive_r8_C(x_sum, x, n, c);
		return;
	}
	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>5);

		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd  0*8(%[x]), %%ymm0           \n\t"
			"vmovupd  4*8(%[x]), %%ymm1           \n\t"
			"vmovupd  8*8(%[x]), %%ymm2           \n\t"
			"vmovupd 12*8(%[x]), %%ymm3           \n\t"
			"vmovupd 16*8(%[x]), %%ymm4           \n\t"
			"vmovupd 20*8(%[x]), %%ymm5           \n\t"
			"vmovupd 24*8(%[x]), %%ymm6           \n\t"
			"vmovupd 28*8(%[x]), %%ymm7           \n\t"
			"subq $-32*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vmovupd  0*8(%[x]), %%ymm0      \n\t"
				"vmovupd  4*8(%[x]), %%ymm1      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm3, %%ymm14 \n\t"
				"vmovupd  8*8(%[x]), %%ymm2      \n\t"
				"vmovupd  12*8(%[x]), %%ymm3      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vaddpd %%ymm4, %%ymm5, %%ymm14 \n\t"
				"vmovupd 16*8(%[x]), %%ymm4      \n\t"
				"vmovupd 20*8(%[x]), %%ymm5      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vaddpd %%ymm6, %%ymm7, %%ymm14 \n\t"
				"vmovupd  24*8(%[x]), %%ymm6      \n\t"
				"vmovupd  28*8(%[x]), %%ymm7      \n\t"
				"vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
				"subq $-32*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm1, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"vaddpd %%ymm2, %%ymm3, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"vaddpd %%ymm4, %%ymm5, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"vaddpd %%ymm6, %%ymm7, %%ymm14         \n\t"
			"vaddpd %%ymm14, %%ymm15, %%ymm15       \n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%32;
		if (n_rem & 16){
			__asm__ __volatile__ (
				"vmovupd  0*8(%[x]), %%ymm5 \n\t"
				"vmovupd  4*8(%[x]), %%ymm6 \n\t"
				"vmovupd  8*8(%[x]), %%ymm7 \n\t"
				"vmovupd 12*8(%[x]), %%ymm8 \n\t"
				"vaddpd  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"vaddpd  %%ymm7, %%ymm6, %%ymm6 \n\t"
				"vaddpd  %%ymm8, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTF64X2   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTF64X2   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"addpd %%xmm2, %%xmm0 \n\t"
				"addpd %%xmm3, %%xmm0 \n\t"
				"subq   $-16*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm5 \n\t"
				"vmovupd 4*8(%[x]), %%ymm6 \n\t"
				"vaddpd  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTF64X2   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTF64X2   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"addpd %%xmm2, %%xmm0 \n\t"
				"addpd %%xmm3, %%xmm0 \n\t"
				"subq   $-8*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm5 \n\t"
				"movupd 2*8(%[x]), %%xmm6 \n\t"
				"addpd  %%xmm5, %%xmm6    \n\t"
				"addpd  %%xmm6, %%xmm0    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm4 \n\t"
				"addpd  %%xmm4,    %%xmm0 \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x]), %%xmm3  \n\t"
				"addpd  %%xmm3,   %%xmm0  \n\t"
				"subq   $-1*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		double result;
		__asm__ __volatile__ (
			"movupd %%xmm0, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm0     \n\t"
			"addpd %%xmm0, %%xmm3  \n\t"
			"movsd %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

void sum_up_matrix_unroll_32_i8_ASM(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<32){
		sum_up_matrix_naive_i8_C(x_sum, x, n, c);
		return;
	}

	int64_t ii, jj;

	for(jj=0; jj<c; jj++){
		int64_t n_unroll=(n>>5);
		__asm__ __volatile__ (
			"vpxor     %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0           \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1           \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2           \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3           \n\t"
			"vMOVDQU 16*8(%[x]), %%ymm4           \n\t"
			"vMOVDQU 20*8(%[x]), %%ymm5           \n\t"
			"vMOVDQU 24*8(%[x]), %%ymm6           \n\t"
			"vMOVDQU 28*8(%[x]), %%ymm7           \n\t"
			"subq $-32*8, %[x]                    \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
		n_unroll--;

		while( n_unroll-- ){
			__asm__ __volatile__ (
				"VPADDQ %%ymm0, %%ymm1, %%ymm14 \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0      \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1      \n\t"
				"VPADDQ %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"VPADDQ %%ymm2, %%ymm3, %%ymm13 \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2      \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3      \n\t"
				"VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"VPADDQ %%ymm4, %%ymm5, %%ymm14 \n\t"
				"vMOVDQU 16*8(%[x]), %%ymm4      \n\t"
				"vMOVDQU 20*8(%[x]), %%ymm5      \n\t"
				"VPADDQ %%ymm14, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"VPADDQ %%ymm6, %%ymm7, %%ymm13 \n\t"
				"vMOVDQU 24*8(%[x]), %%ymm6      \n\t"
				"vMOVDQU 28*8(%[x]), %%ymm7      \n\t"
				"VPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-32*8, %[x]               \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__(
			"VPADDQ %%ymm0,  %%ymm1,  %%ymm14        \n\t"
			"VPADDQ %%ymm14, %%ymm15, %%ymm15        \n\t"
			"VPADDQ %%ymm2,  %%ymm3,  %%ymm13        \n\t"
			"VPADDQ %%ymm13, %%ymm15, %%ymm15        \n\t"
			"VPADDQ %%ymm4,  %%ymm5,  %%ymm14        \n\t"
			"VPADDQ %%ymm14, %%ymm15, %%ymm15        \n\t"
			"VPADDQ %%ymm6,  %%ymm7,  %%ymm13        \n\t"
			"VPADDQ %%ymm13, %%ymm15, %%ymm15        \n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0    \n\t" // extract lower  128 bits
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1    \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm0    \n\t" // sum
			"vpxor          %%ymm2, %%ymm2, %%ymm2  \n\t"
			::
		);

		int64_t n_rem=n%32;
		if (n_rem & 16){
			__asm__ __volatile__ (
				"vMOVDQU  0*8(%[x]), %%ymm5 \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm6 \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm7 \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm8 \n\t"
				"VPADDQ  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"VPADDQ  %%ymm7, %%ymm6, %%ymm6 \n\t"
				"VPADDQ  %%ymm8, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTI128   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"PADDQ %%xmm2, %%xmm0 \n\t"
				"PADDQ %%xmm3, %%xmm0 \n\t"
				"subq   $-16*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm5 \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm6 \n\t"
				"vPADDQ  %%ymm5, %%ymm6, %%ymm6 \n\t"
				"VEXTRACTI128   $0,  %%ymm6, %%xmm2    \n\t" // extract lower  128 bits
				"VEXTRACTI128   $1,  %%ymm6, %%xmm3    \n\t" // extract higher 128 bits
				"paddq %%xmm2, %%xmm0 \n\t"
				"paddq %%xmm3, %%xmm0 \n\t"
				"subq   $-8*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 4){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm5 \n\t"
				"MOVDQU 2*8(%[x]), %%xmm6 \n\t"
				"PADDQ  %%xmm5, %%xmm0    \n\t"
				"PADDQ  %%xmm6, %%xmm0    \n\t"
				"subq   $-4*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm4 \n\t"
				"PADDQ  %%xmm4, %%xmm0    \n\t"
				"subq   $-2*8, %[x]       \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ 0*8(%[x]), %%xmm3  \n\t"
				"PADDQ  %%xmm3,  %%xmm0 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		int64_t result;
		__asm__ __volatile__ (
			"MOVDQU %%xmm0, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm0     \n\t"
			"PADDQ %%xmm0, %%xmm3  \n\t"
			"MOVQ %%xmm3, %[r]   \n\t"
			:[r]"=m"(result)
			:
		);
		x_sum[jj] = result;
	}
}

// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
void sum_up_matrix_unroll_04_04_r8_C(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<4){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm8  = ymm8  + ymm0;
			ymm8  = ymm8  + ymm1;
			ymm8  = ymm8  + ymm2;
			ymm8  = ymm8  + ymm3;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm9 = ymm9  + ymm4;
			ymm9 = ymm9  + ymm5;
			ymm9 = ymm9  + ymm6;
			ymm9 = ymm9  + ymm7;

			ymm0  = *(x2);
			ymm1  = *(x2+1);
			ymm2  = *(x2+2);
			ymm3  = *(x2+3);
			ymm10 = ymm10 + ymm0;
			ymm10 = ymm10 + ymm1;
			ymm10 = ymm10 + ymm2;
			ymm10 = ymm10 + ymm3;

			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm6 = *(x3+2);
			ymm7 = *(x3+3);
			ymm11 = ymm11 + ymm4;
			ymm11 = ymm11 + ymm5;
			ymm11 = ymm11 + ymm6;
			ymm11 = ymm11 + ymm7;

			x0+=4;
			x1+=4;
			x2+=4;
			x3+=4;
		}

		int64_t n_rem=n%4;
		while(n_rem--){
			ymm8  += *(x0);
			ymm9  += *(x1);
			ymm10 += *(x2);
			ymm11 += *(x3);
			x0 += 1;
			x1 += 1;
			x2 += 1;
			x3 += 1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}

	int64_t c_rem=c%4;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while(n_unroll--){
			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm6 = *(x3+2);
			ymm7 = *(x3+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x3+=4;
		}
		x_sum[j3] = ymm15;
		j3+=1;
	}
}

void sum_up_matrix_unroll_04_04_i8_C(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<4){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm8  = ymm8  + ymm0;
			ymm8  = ymm8  + ymm1;
			ymm8  = ymm8  + ymm2;
			ymm8  = ymm8  + ymm3;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm9 = ymm9  + ymm4;
			ymm9 = ymm9  + ymm5;
			ymm9 = ymm9  + ymm6;
			ymm9 = ymm9  + ymm7;

			ymm0  = *(x2);
			ymm1  = *(x2+1);
			ymm2  = *(x2+2);
			ymm3  = *(x2+3);
			ymm10 = ymm10 + ymm0;
			ymm10 = ymm10 + ymm1;
			ymm10 = ymm10 + ymm2;
			ymm10 = ymm10 + ymm3;

			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm6 = *(x3+2);
			ymm7 = *(x3+3);
			ymm11 = ymm11 + ymm4;
			ymm11 = ymm11 + ymm5;
			ymm11 = ymm11 + ymm6;
			ymm11 = ymm11 + ymm7;

			x0+=4;
			x1+=4;
			x2+=4;
			x3+=4;
		}

		int64_t n_rem=n%4;
		while(n_rem--){
			ymm8  += *(x0);
			ymm9  += *(x1);
			ymm10 += *(x2);
			ymm11 += *(x3);
			x0 += 1;
			x1 += 1;
			x2 += 1;
			x3 += 1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}

	int64_t c_rem=c%4;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while(n_unroll--){
			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm6 = *(x3+2);
			ymm7 = *(x3+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x3+=4;
		}
		x_sum[j3] = ymm15;
		j3+=1;
	}
}

void sum_up_matrix_unroll_08_02_r8_C(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1;
	x1 = x0;

    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		ymm14=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm4  = *(x0+4);
			ymm5  = *(x0+5);
			ymm6  = *(x0+6);
			ymm7  = *(x0+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm14 = ymm14 + ymm12;
			ymm14 = ymm14 + ymm13;

			ymm0  = *(x1);
			ymm1  = *(x1+1);
			ymm2  = *(x1+2);
			ymm3  = *(x1+3);
			ymm4  = *(x1+4);
			ymm5  = *(x1+5);
			ymm6  = *(x1+6);
			ymm7  = *(x1+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;

			x0+=8;
			x1+=8;
		}

		int64_t n_rem=n%8;
		while(n_rem--){
			ymm14 += *(x0);
			ymm15 += *(x1);
			x0 += 1;
			x1 += 1;
		}

		x_sum[j0] = ymm14;
		x_sum[j1] = ymm15;

		j0+=1;
		j1+=1;
	}

	int64_t c_rem=c%2;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>3);
		while(n_unroll--){
			ymm0  = *(x1);
			ymm1  = *(x1+1);
			ymm2  = *(x1+2);
			ymm3  = *(x1+3);
			ymm4  = *(x1+4);
			ymm5  = *(x1+5);
			ymm6  = *(x1+6);
			ymm7  = *(x1+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;

			x1+=8;
		}
		x_sum[j1] = ymm15;
		j1+=1;
	}
}

void sum_up_matrix_unroll_08_02_i8_C(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1;
	x1 = x0;

    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		ymm14=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm4  = *(x0+4);
			ymm5  = *(x0+5);
			ymm6  = *(x0+6);
			ymm7  = *(x0+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm14 = ymm14 + ymm12;
			ymm14 = ymm14 + ymm13;

			ymm0  = *(x1);
			ymm1  = *(x1+1);
			ymm2  = *(x1+2);
			ymm3  = *(x1+3);
			ymm4  = *(x1+4);
			ymm5  = *(x1+5);
			ymm6  = *(x1+6);
			ymm7  = *(x1+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;

			x0+=8;
			x1+=8;
		}

		int64_t n_rem=n%8;
		while(n_rem--){
			ymm14 += *(x0);
			ymm15 += *(x1);
			x0 += 1;
			x1 += 1;
		}

		x_sum[j0] = ymm14;
		x_sum[j1] = ymm15;

		j0+=1;
		j1+=1;
	}

	int64_t c_rem=c%2;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>3);
		while(n_unroll--){
			ymm0  = *(x1);
			ymm1  = *(x1+1);
			ymm2  = *(x1+2);
			ymm3  = *(x1+3);
			ymm4  = *(x1+4);
			ymm5  = *(x1+5);
			ymm6  = *(x1+6);
			ymm7  = *(x1+7);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm4  + ymm5;
			ymm11 = ymm6  + ymm7;
			ymm12 = ymm8  + ymm9;
			ymm13 = ymm10 + ymm11;
			ymm15 = ymm15 + ymm12;
			ymm15 = ymm15 + ymm13;

			x1+=8;
		}
		x_sum[j1] = ymm15;
		j1+=1;
	}
}

void sum_up_matrix_unroll_02_08_r8_C(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<2 || c<8){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3, *x4, *x5, *x6, *x7;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x4 = x0;
	x5 = x0;
	x6 = x0;
	x7 = x0;

    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/8, c_unroll=(c/8);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	x4+=skip*n*4;
	x5+=skip*n*5;
	x6+=skip*n*6;
	x7+=skip*n*7;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3, j4=skip*4, j5=skip*5, j6=skip*6, j7=skip*7;
	while(c_unroll--){
		int64_t n_unroll=(n>>1);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm8  = ymm8 + ymm0;
			ymm8  = ymm8 + ymm1;

			ymm2 = *(x1);
			ymm3 = *(x1+1);
			ymm9  = ymm9 + ymm2;
			ymm9  = ymm9 + ymm3;

			ymm4  = *(x2);
			ymm5  = *(x2+1);
			ymm10 = ymm10 + ymm4;
			ymm10 = ymm10 + ymm5;

			ymm6 = *(x3);
			ymm7 = *(x3+1);
			ymm11 = ymm11 + ymm6;
			ymm11 = ymm11 + ymm7;

			ymm0  = *(x4);
			ymm1  = *(x4+1);
			ymm12 = ymm12+ ymm0;
			ymm12 = ymm12+ ymm1;

			ymm2 = *(x5);
			ymm3 = *(x5+1);
			ymm13  = ymm13 + ymm2;
			ymm13  = ymm13 + ymm3;

			ymm4  = *(x6);
			ymm5  = *(x6+1);
			ymm14 = ymm14 + ymm4;
			ymm14 = ymm14 + ymm5;

			ymm6 = *(x7);
			ymm7 = *(x7+1);
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;


			x0+=2;
			x1+=2;
			x2+=2;
			x3+=2;
			x4+=2;
			x5+=2;
			x6+=2;
			x7+=2;
		}

		int64_t n_rem=n%2;
		while(n_rem--){
			ymm0  = *(x0);
			ymm8  = ymm8 + ymm0;

			ymm2 = *(x1);
			ymm9  = ymm9 + ymm2;

			ymm4  = *(x2);
			ymm10 = ymm10 + ymm4;

			ymm6 = *(x3);
			ymm11 = ymm11 + ymm6;

			ymm0  = *(x4);
			ymm12 = ymm12+ ymm0;

			ymm2 = *(x5);
			ymm13  = ymm13 + ymm2;

			ymm4  = *(x6);
			ymm14 = ymm14 + ymm4;

			ymm6 = *(x7);
			ymm15 = ymm15 + ymm6;

			x0+=1;
			x1+=1;
			x2+=1;
			x3+=1;
			x4+=1;
			x5+=1;
			x6+=1;
			x7+=1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;
		x_sum[j4] = ymm12;
		x_sum[j5] = ymm13;
		x_sum[j6] = ymm14;
		x_sum[j7] = ymm15;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
		j4+=1;
		j5+=1;
		j6+=1;
		j7+=1;
	}

	int64_t c_rem=c%8;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while(n_unroll--){
			ymm4 = *(x7);
			ymm5 = *(x7+1);
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;

			x7+=2;
		}
		x_sum[j7] = ymm15;
		j7+=1;
	}
}

void sum_up_matrix_unroll_02_08_i8_C(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<2 || c<8){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3, *x4, *x5, *x6, *x7;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x4 = x0;
	x5 = x0;
	x6 = x0;
	x7 = x0;

    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/8, c_unroll=(c/8);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	x4+=skip*n*4;
	x5+=skip*n*5;
	x6+=skip*n*6;
	x7+=skip*n*7;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3, j4=skip*4, j5=skip*5, j6=skip*6, j7=skip*7;

	while(c_unroll--){
		int64_t n_unroll=(n>>1);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		ymm12=0e0;
		ymm13=0e0;
		ymm14=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm8  = ymm8 + ymm0;
			ymm8  = ymm8 + ymm1;

			ymm2 = *(x1);
			ymm3 = *(x1+1);
			ymm9  = ymm9 + ymm2;
			ymm9  = ymm9 + ymm3;

			ymm4  = *(x2);
			ymm5  = *(x2+1);
			ymm10 = ymm10 + ymm4;
			ymm10 = ymm10 + ymm5;

			ymm6 = *(x3);
			ymm7 = *(x3+1);
			ymm11 = ymm11 + ymm6;
			ymm11 = ymm11 + ymm7;

			ymm0  = *(x4);
			ymm1  = *(x4+1);
			ymm12 = ymm12+ ymm0;
			ymm12 = ymm12+ ymm1;

			ymm2 = *(x5);
			ymm3 = *(x5+1);
			ymm13  = ymm13 + ymm2;
			ymm13  = ymm13 + ymm3;

			ymm4  = *(x6);
			ymm5  = *(x6+1);
			ymm14 = ymm14 + ymm4;
			ymm14 = ymm14 + ymm5;

			ymm6 = *(x7);
			ymm7 = *(x7+1);
			ymm15 = ymm15 + ymm6;
			ymm15 = ymm15 + ymm7;


			x0+=2;
			x1+=2;
			x2+=2;
			x3+=2;
			x4+=2;
			x5+=2;
			x6+=2;
			x7+=2;
		}

		int64_t n_rem=n%2;
		while(n_rem--){
			ymm0  = *(x0);
			ymm8  = ymm8 + ymm0;

			ymm2 = *(x1);
			ymm9  = ymm9 + ymm2;

			ymm4  = *(x2);
			ymm10 = ymm10 + ymm4;

			ymm6 = *(x3);
			ymm11 = ymm11 + ymm6;

			ymm0  = *(x4);
			ymm12 = ymm12+ ymm0;

			ymm2 = *(x5);
			ymm13  = ymm13 + ymm2;

			ymm4  = *(x6);
			ymm14 = ymm14 + ymm4;

			ymm6 = *(x7);
			ymm15 = ymm15 + ymm6;

			x0+=1;
			x1+=1;
			x2+=1;
			x3+=1;
			x4+=1;
			x5+=1;
			x6+=1;
			x7+=1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;
		x_sum[j4] = ymm12;
		x_sum[j5] = ymm13;
		x_sum[j6] = ymm14;
		x_sum[j7] = ymm15;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
		j4+=1;
		j5+=1;
		j6+=1;
		j7+=1;
	}

	int64_t c_rem=c%8;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while(n_unroll--){
			ymm4 = *(x7);
			ymm5 = *(x7+1);
			ymm15 = ymm15 + ymm4;
			ymm15 = ymm15 + ymm5;

			x7+=2;
		}
		x_sum[j7] = ymm15;
		j7+=1;
	}
}

void sum_up_matrix_unroll_04_02_r8_C(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1;
	x1 = x0;

    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		ymm11=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm8  + ymm9;
			ymm11 = ymm11 + ymm10;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x0+=4;
			x1+=4;
		}

		int64_t n_rem=n%4;
		while(n_rem--){
			ymm11 += *(x0);
			ymm15 += *(x1);
			x0 += 1;
			x1 += 1;
		}

		x_sum[j0] = ymm11;
		x_sum[j1] = ymm15;

		j0+=1;
		j1+=1;
	}

	int64_t c_rem=c%2;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while(n_unroll--){
			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x1+=4;
		}
		x_sum[j1] = ymm15;
		j1+=1;
	}

}

void sum_up_matrix_unroll_04_02_i8_C(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1;
	x1 = x0;

    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		ymm11=0e0;
		ymm15=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm2  = *(x0+2);
			ymm3  = *(x0+3);
			ymm8  = ymm0  + ymm1;
			ymm9  = ymm2  + ymm3;
			ymm10 = ymm8  + ymm9;
			ymm11 = ymm11 + ymm10;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x0+=4;
			x1+=4;
		}

		int64_t n_rem=n%4;
		while(n_rem--){
			ymm11 += *(x0);
			ymm15 += *(x1);
			x0 += 1;
			x1 += 1;
		}

		x_sum[j0] = ymm11;
		x_sum[j1] = ymm15;

		j0+=1;
		j1+=1;
	}

	int64_t c_rem=c%2;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>2);
		while(n_unroll--){
			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm6 = *(x1+2);
			ymm7 = *(x1+3);
			ymm12 = ymm4  + ymm5;
			ymm13 = ymm6  + ymm7;
			ymm14 = ymm12 + ymm13;
			ymm15 = ymm15 + ymm14;

			x1+=4;
		}
		x_sum[j1] = ymm15;
		j1+=1;
	}
}

void sum_up_matrix_unroll_02_04_r8_C(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<2 || c<4){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

    double ymm0,ymm1,ymm2,ymm3;
    double ymm4,ymm5,ymm6,ymm7;
    double ymm8,ymm9,ymm10,ymm11;
    double ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>1);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm8  = ymm8  + ymm0;
			ymm8  = ymm8  + ymm1;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm9 = ymm9  + ymm4;
			ymm9 = ymm9  + ymm5;

			ymm0  = *(x2);
			ymm1  = *(x2+1);
			ymm10 = ymm10 + ymm0;
			ymm10 = ymm10 + ymm1;

			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm11 = ymm11 + ymm4;
			ymm11 = ymm11 + ymm5;

			x0+=2;
			x1+=2;
			x2+=2;
			x3+=2;
		}

		int64_t n_rem=n%2;
		while(n_rem--){
			ymm8  += *(x0);
			ymm9  += *(x1);
			ymm10 += *(x2);
			ymm11 += *(x3);
			x0 += 1;
			x1 += 1;
			x2 += 1;
			x3 += 1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}

	int64_t c_rem=c%4;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while(n_unroll--){
			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm12 = ymm4  + ymm5;
			ymm15 = ymm15 + ymm12;

			x3+=2;
		}
		x_sum[j3] = ymm15;
		j3+=1;
	}
}

void sum_up_matrix_unroll_02_04_i8_C(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<2 || c<4){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

    int64_t ymm0,ymm1,ymm2,ymm3;
    int64_t ymm4,ymm5,ymm6,ymm7;
    int64_t ymm8,ymm9,ymm10,ymm11;
    int64_t ymm12,ymm13,ymm14,ymm15;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>1);
		ymm8=0e0;
		ymm9=0e0;
		ymm10=0e0;
		ymm11=0e0;
		while(n_unroll--){
			ymm0  = *(x0);
			ymm1  = *(x0+1);
			ymm8  = ymm8  + ymm0;
			ymm8  = ymm8  + ymm1;

			ymm4 = *(x1);
			ymm5 = *(x1+1);
			ymm9 = ymm9  + ymm4;
			ymm9 = ymm9  + ymm5;

			ymm0  = *(x2);
			ymm1  = *(x2+1);
			ymm10 = ymm10 + ymm0;
			ymm10 = ymm10 + ymm1;

			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm11 = ymm11 + ymm4;
			ymm11 = ymm11 + ymm5;

			x0+=2;
			x1+=2;
			x2+=2;
			x3+=2;
		}

		int64_t n_rem=n%2;
		while(n_rem--){
			ymm8  += *(x0);
			ymm9  += *(x1);
			ymm10 += *(x2);
			ymm11 += *(x3);
			x0 += 1;
			x1 += 1;
			x2 += 1;
			x3 += 1;
		}

		x_sum[j0] = ymm8;
		x_sum[j1] = ymm9;
		x_sum[j2] = ymm10;
		x_sum[j3] = ymm11;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}

	int64_t c_rem=c%4;
	while(c_rem--){
		ymm15=0e0;
		int64_t n_unroll=(n>>1);
		while(n_unroll--){
			ymm4 = *(x3);
			ymm5 = *(x3+1);
			ymm12 = ymm4  + ymm5;
			ymm15 = ymm15 + ymm12;

			x3+=2;
		}
		x_sum[j3] = ymm15;
		j3+=1;
	}
}

// ----------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------
void sum_up_matrix_unroll_04_04_r8_ASM(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<4){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1 \n\t"
			"vmovupd 0*8(%[x2]), %%ymm2 \n\t"
			"vmovupd 0*8(%[x3]), %%ymm3 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"subq $-4*8, %[x2]          \n\t"
			"subq $-4*8, %[x3]          \n\t"
			"vpxor %%ymm4, %%ymm4, %%ymm4 \n\t"
			"vpxor %%ymm5, %%ymm5, %%ymm5 \n\t"
			"vpxor %%ymm6, %%ymm6, %%ymm6 \n\t"
			"vpxor %%ymm7, %%ymm7, %%ymm7 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm4, %%ymm4 \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vaddpd %%ymm1, %%ymm5, %%ymm5 \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm6, %%ymm6 \n\t"
				"vmovupd 0*8(%[x2]), %%ymm2    \n\t"
				"\n\t"
				"vaddpd %%ymm3, %%ymm7, %%ymm7 \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				"subq $-4*8, %[x2]             \n\t"
				"subq $-4*8, %[x3]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm4, %%ymm4 \n\t"
			"vaddpd %%ymm1, %%ymm5, %%ymm5 \n\t"
			"vaddpd %%ymm2, %%ymm6, %%ymm6 \n\t"
			"vaddpd %%ymm3, %%ymm7, %%ymm7 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $1,  %%ymm4, %%xmm0  \n\t" // extract higher 128 bits
			"addpd               %%xmm0, %%xmm4  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm5, %%xmm1  \n\t" // extract higher 128 bits
			"addpd               %%xmm1, %%xmm5  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm6, %%xmm2  \n\t" // extract higher 128 bits
			"addpd               %%xmm2, %%xmm6  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm7, %%xmm3  \n\t" // extract higher 128 bits
			"addpd               %%xmm3, %%xmm7  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"movupd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm0, %%xmm4      \n\t"
				"addpd %%xmm1, %%xmm5      \n\t"
				"addpd %%xmm2, %%xmm6      \n\t"
				"addpd %%xmm3, %%xmm7      \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd  %%xmm0,   %%xmm4  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"movsd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd  %%xmm1,   %%xmm5  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"movsd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd  %%xmm2,   %%xmm6  \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"movsd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd  %%xmm3,   %%xmm7  \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		double sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"movupd %%xmm4, %%xmm8 \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"addpd %%xmm4, %%xmm8  \n\t"
			"movsd %%xmm8, %[s0]   \n\t"
			"\n\t"
			"movupd %%xmm5, %%xmm9 \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"addpd %%xmm5, %%xmm9  \n\t"
			"movsd %%xmm9, %[s1]   \n\t"
			"\n\t"
			"movupd %%xmm6, %%xmm10 \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"addpd %%xmm6, %%xmm10  \n\t"
			"movsd %%xmm10, %[s2]   \n\t"
			"\n\t"
			"movupd %%xmm7, %%xmm11 \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"addpd %%xmm7, %%xmm11  \n\t"
			"movsd %%xmm11, %[s3]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}
}

void sum_up_matrix_unroll_04_04_i8_ASM(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<4){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1 \n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm2 \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm3 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"subq $-4*8, %[x2]          \n\t"
			"subq $-4*8, %[x3]          \n\t"
			"vpxor %%ymm4, %%ymm4, %%ymm4 \n\t"
			"vpxor %%ymm5, %%ymm5, %%ymm5 \n\t"
			"vpxor %%ymm6, %%ymm6, %%ymm6 \n\t"
			"vpxor %%ymm7, %%ymm7, %%ymm7 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vPADDQ %%ymm0, %%ymm4, %%ymm4 \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vPADDQ %%ymm1, %%ymm5, %%ymm5 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"vPADDQ %%ymm2, %%ymm6, %%ymm6 \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2    \n\t"
				"\n\t"
				"vPADDQ %%ymm3, %%ymm7, %%ymm7 \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				"subq $-4*8, %[x2]             \n\t"
				"subq $-4*8, %[x3]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__(
			"vPADDQ %%ymm0, %%ymm4, %%ymm4 \n\t"
			"vPADDQ %%ymm1, %%ymm5, %%ymm5 \n\t"
			"vPADDQ %%ymm2, %%ymm6, %%ymm6 \n\t"
			"vPADDQ %%ymm3, %%ymm7, %%ymm7 \n\t"
			"\n\t"
			"VEXTRACTI128   $1,  %%ymm4, %%xmm0  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm0, %%xmm4  \n\t" // sum
			"VEXTRACTI128   $1,  %%ymm5, %%xmm1  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1, %%xmm5  \n\t" // sum
			"VEXTRACTI128   $1,  %%ymm6, %%xmm2  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm2, %%xmm6  \n\t" // sum
			"VEXTRACTI128   $1,  %%ymm7, %%xmm3  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm3, %%xmm7  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm0, %%xmm4      \n\t"
				"PADDQ %%xmm1, %%xmm5      \n\t"
				"PADDQ %%xmm2, %%xmm6      \n\t"
				"PADDQ %%xmm3, %%xmm7      \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ  0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ  %%xmm0,   %%xmm4  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ  %%xmm1,   %%xmm5  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ  %%xmm2,   %%xmm6  \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ  %%xmm3,   %%xmm7  \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		int64_t sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"MOVDQU %%xmm4, %%xmm8 \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"PADDQ %%xmm4, %%xmm8  \n\t"
			"MOVQ  %%xmm8, %[s0]   \n\t"
			"\n\t"
			"MOVDQU %%xmm5, %%xmm9 \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"PADDQ %%xmm5, %%xmm9  \n\t"
			"MOVQ  %%xmm9, %[s1]   \n\t"
			"\n\t"
			"MOVDQU %%xmm6, %%xmm10 \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"PADDQ %%xmm6, %%xmm10  \n\t"
			"MOVQ  %%xmm10, %[s2]   \n\t"
			"\n\t"
			"MOVDQU %%xmm7, %%xmm11 \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"PADDQ %%xmm7, %%xmm11  \n\t"
			"MOVQ  %%xmm11, %[s3]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}
}

void sum_up_matrix_unroll_08_02_r8_ASM(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1;
	x1 = x0;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		__asm__ __volatile__ (
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
			"vmovupd 4*8(%[x0]), %%ymm1 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm2 \n\t"
			"vmovupd 4*8(%[x1]), %%ymm3 \n\t"
			"subq $-8*8, %[x0]          \n\t"
			"subq $-8*8, %[x1]          \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 4*8(%[x0]), %%ymm1       \n\t"
				"vaddpd %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd 0*8(%[x1]), %%ymm2       \n\t"
				"vmovupd 4*8(%[x1]), %%ymm3       \n\t"
				"vaddpd %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]             \n\t"
				"subq $-8*8, %[x1]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm1,  %%ymm8   \n\t"
			"vaddpd %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vaddpd %%ymm2, %%ymm3,  %%ymm9   \n\t"
			"vaddpd %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm0   \n\t" // extract higher 128 bits
			"addpd               %%xmm0,  %%xmm12  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm1   \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm13  \n\t" // sum
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm0,  %%xmm8  \n\t" // extract higher 08 bits
				"addpd               %%xmm8,  %%xmm0  \n\t" // sum
				"addpd               %%xmm0,  %%xmm12 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm1,  %%xmm9  \n\t" // extract higher 28 bits
				"addpd               %%xmm9,  %%xmm1  \n\t" // sum
				"addpd               %%xmm1,  %%xmm13 \n\t"
				"subq   $-4*8, %[x0]       \n\t"
				"subq   $-4*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"pxor %%xmm0, %%xmm0      \n\t"
				"pxor %%xmm1, %%xmm1      \n\t"
				"pxor %%xmm2, %%xmm2      \n\t"
				"pxor %%xmm3, %%xmm3      \n\t"
				"\n\t"
				"movsd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd  %%xmm0,   %%xmm12 \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"movsd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd  %%xmm1,   %%xmm13 \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		double sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"movupd %%xmm12, %%xmm0 \n\t"
			"PSRLDQ $8, %%xmm12     \n\t"
			"addpd %%xmm12, %%xmm0  \n\t"
			"movsd %%xmm0, %[s0]   \n\t"
			"\n\t"
			"movupd %%xmm13, %%xmm1 \n\t"
			"PSRLDQ $8, %%xmm13     \n\t"
			"addpd %%xmm13, %%xmm1  \n\t"
			"movsd %%xmm1, %[s1]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;

		j0+=1;
		j1+=1;
	}
}

void sum_up_matrix_unroll_08_02_i8_ASM(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1;
	x1 = x0;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		__asm__ __volatile__ (
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
			"vMOVDQU 4*8(%[x0]), %%ymm1 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm2 \n\t"
			"vMOVDQU 4*8(%[x1]), %%ymm3 \n\t"
			"subq $-8*8, %[x0]          \n\t"
			"subq $-8*8, %[x1]          \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vPADDQ %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm1       \n\t"
				"vPADDQ %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vPADDQ %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm2       \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm3       \n\t"
				"vPADDQ %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]             \n\t"
				"subq $-8*8, %[x1]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__(
			"vPADDQ %%ymm0, %%ymm1,  %%ymm8   \n\t"
			"vPADDQ %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vPADDQ %%ymm2, %%ymm3,  %%ymm9   \n\t"
			"vPADDQ %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"VEXTRACTI128  $1,  %%ymm12, %%xmm0   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm0,  %%xmm12  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm13, %%xmm1   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm13  \n\t" // sum
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1 \n\t"
				"VEXTRACTI128  $1,  %%ymm0,  %%xmm8  \n\t" // extract higher 08 bits
				"PADDQ               %%xmm8,  %%xmm0  \n\t" // sum
				"PADDQ               %%xmm0,  %%xmm12 \n\t"
				"VEXTRACTI128  $1,  %%ymm1,  %%xmm9  \n\t" // extract higher 28 bits
				"PADDQ               %%xmm9,  %%xmm1  \n\t" // sum
				"PADDQ               %%xmm1,  %%xmm13 \n\t"
				"subq   $-4*8, %[x0]       \n\t"
				"subq   $-4*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"pxor %%xmm0, %%xmm0      \n\t"
				"pxor %%xmm1, %%xmm1      \n\t"
				"pxor %%xmm2, %%xmm2      \n\t"
				"pxor %%xmm3, %%xmm3      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ  %%xmm0,   %%xmm12 \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ  %%xmm1,   %%xmm13 \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		int64_t sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"MOVDQU %%xmm12, %%xmm0 \n\t"
			"PSRLDQ $8, %%xmm12     \n\t"
			"PADDQ %%xmm12, %%xmm0  \n\t"
			"MOVQ  %%xmm0, %[s0]   \n\t"
			"\n\t"
			"MOVDQU %%xmm13, %%xmm1 \n\t"
			"PSRLDQ $8, %%xmm13     \n\t"
			"PADDQ %%xmm13, %%xmm1  \n\t"
			"MOVQ  %%xmm1, %[s1]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;

		j0+=1;
		j1+=1;
	}
}

void sum_up_matrix_unroll_08_04_r8_ASM(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<8 || c<4){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		__asm__ __volatile__ (
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
			"vmovupd 4*8(%[x0]), %%ymm1 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm2 \n\t"
			"vmovupd 4*8(%[x1]), %%ymm3 \n\t"
			"vmovupd 0*8(%[x2]), %%ymm4 \n\t"
			"vmovupd 4*8(%[x2]), %%ymm5 \n\t"
			"vmovupd 0*8(%[x3]), %%ymm6 \n\t"
			"vmovupd 4*8(%[x3]), %%ymm7 \n\t"
			"subq $-8*8, %[x0]          \n\t"
			"subq $-8*8, %[x1]          \n\t"
			"subq $-8*8, %[x2]          \n\t"
			"subq $-8*8, %[x3]          \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 4*8(%[x0]), %%ymm1       \n\t"
				"vaddpd %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd 0*8(%[x1]), %%ymm2       \n\t"
				"vmovupd 4*8(%[x1]), %%ymm3       \n\t"
				"vaddpd %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vaddpd %%ymm4, %%ymm5, %%ymm10   \n\t"
				"vmovupd 0*8(%[x2]), %%ymm4       \n\t"
				"vmovupd 4*8(%[x2]), %%ymm5       \n\t"
				"vaddpd %%ymm10, %%ymm14, %%ymm14 \n\t"
				"\n\t"
				"vaddpd %%ymm6, %%ymm7, %%ymm11   \n\t"
				"vmovupd 0*8(%[x3]), %%ymm6       \n\t"
				"vmovupd 4*8(%[x3]), %%ymm7       \n\t"
				"vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-8*8, %[x0]             \n\t"
				"subq $-8*8, %[x1]             \n\t"
				"subq $-8*8, %[x2]             \n\t"
				"subq $-8*8, %[x3]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm1,  %%ymm8   \n\t"
			"vaddpd %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vaddpd %%ymm2, %%ymm3,  %%ymm9   \n\t"
			"vaddpd %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"vaddpd %%ymm4,  %%ymm5,  %%ymm10 \n\t"
			"vaddpd %%ymm10, %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"vaddpd %%ymm6,  %%ymm7,  %%ymm11 \n\t"
			"vaddpd %%ymm11, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm0   \n\t" // extract higher 128 bits
			"addpd               %%xmm0,  %%xmm12  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm1   \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm13  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm2   \n\t" // extract higher 128 bits
			"addpd               %%xmm2,  %%xmm14  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm3   \n\t" // extract higher 128 bits
			"addpd               %%xmm3,  %%xmm15  \n\t" // sum
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1 \n\t"
				"vmovupd 0*8(%[x2]), %%ymm2 \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm0,  %%xmm8  \n\t" // extract higher 08 bits
				"addpd               %%xmm8,  %%xmm0  \n\t" // sum
				"addpd               %%xmm0,  %%xmm12 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm1,  %%xmm9  \n\t" // extract higher 28 bits
				"addpd               %%xmm9,  %%xmm1  \n\t" // sum
				"addpd               %%xmm1,  %%xmm13 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm2,  %%xmm10 \n\t" // extract highe2128 bits
				"addpd               %%xmm10, %%xmm2  \n\t" // sum
				"addpd               %%xmm2,  %%xmm14 \n\t"
				"VEXTRACTF64X2  $1,  %%ymm3,  %%xmm11 \n\t" // extract high3 128 bits
				"addpd               %%xmm11, %%xmm3  \n\t" // sum
				"addpd               %%xmm3,  %%xmm15 \n\t"
				"subq   $-4*8, %[x0]       \n\t"
				"subq   $-4*8, %[x1]       \n\t"
				"subq   $-4*8, %[x2]       \n\t"
				"subq   $-4*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"movupd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"pxor %%xmm0, %%xmm0      \n\t"
				"pxor %%xmm1, %%xmm1      \n\t"
				"pxor %%xmm2, %%xmm2      \n\t"
				"pxor %%xmm3, %%xmm3      \n\t"
				"\n\t"
				"movsd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd  %%xmm0,   %%xmm12 \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"movsd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd  %%xmm1,   %%xmm13 \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"movsd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd  %%xmm2,   %%xmm14 \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"movsd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd  %%xmm3,   %%xmm15 \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		double sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"movupd %%xmm12, %%xmm0 \n\t"
			"PSRLDQ $8, %%xmm12     \n\t"
			"addpd %%xmm12, %%xmm0  \n\t"
			"movsd %%xmm0, %[s0]   \n\t"
			"\n\t"
			"movupd %%xmm13, %%xmm1 \n\t"
			"PSRLDQ $8, %%xmm13     \n\t"
			"addpd %%xmm13, %%xmm1  \n\t"
			"movsd %%xmm1, %[s1]   \n\t"
			"\n\t"
			"movupd %%xmm14, %%xmm2 \n\t"
			"PSRLDQ $8, %%xmm14     \n\t"
			"addpd %%xmm14, %%xmm2  \n\t"
			"movsd %%xmm2, %[s2]   \n\t"
			"\n\t"
			"movupd %%xmm15, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm15     \n\t"
			"addpd %%xmm15, %%xmm3  \n\t"
			"movsd %%xmm3, %[s3]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}
}

void sum_up_matrix_unroll_08_04_i8_ASM(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<8 || c<4){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3;
	x1 = x0;
	x2 = x0;
	x3 = x0;

	int64_t skip=c/4, c_unroll=(c/4);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3;
	while(c_unroll--){
		int64_t n_unroll=(n>>3);
		__asm__ __volatile__ (
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
			"vMOVDQU 4*8(%[x0]), %%ymm1 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm2 \n\t"
			"vMOVDQU 4*8(%[x1]), %%ymm3 \n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm4 \n\t"
			"vMOVDQU 4*8(%[x2]), %%ymm5 \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm6 \n\t"
			"vMOVDQU 4*8(%[x3]), %%ymm7 \n\t"
			"subq $-8*8, %[x0]          \n\t"
			"subq $-8*8, %[x1]          \n\t"
			"subq $-8*8, %[x2]          \n\t"
			"subq $-8*8, %[x3]          \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vPADDQ %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm1       \n\t"
				"vPADDQ %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vPADDQ %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm2       \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm3       \n\t"
				"vPADDQ %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vPADDQ %%ymm4, %%ymm5, %%ymm10   \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm4       \n\t"
				"vMOVDQU 4*8(%[x2]), %%ymm5       \n\t"
				"vPADDQ %%ymm10, %%ymm14, %%ymm14 \n\t"
				"\n\t"
				"vPADDQ %%ymm6, %%ymm7, %%ymm11   \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm6       \n\t"
				"vMOVDQU 4*8(%[x3]), %%ymm7       \n\t"
				"vPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"subq $-8*8, %[x0]             \n\t"
				"subq $-8*8, %[x1]             \n\t"
				"subq $-8*8, %[x2]             \n\t"
				"subq $-8*8, %[x3]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__(
			"vPADDQ %%ymm0, %%ymm1,  %%ymm8   \n\t"
			"vPADDQ %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vPADDQ %%ymm2, %%ymm3,  %%ymm9   \n\t"
			"vPADDQ %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"vPADDQ %%ymm4,  %%ymm5,  %%ymm10 \n\t"
			"vPADDQ %%ymm10, %%ymm14, %%ymm14 \n\t"
			"\n\t"
			"vPADDQ %%ymm6,  %%ymm7,  %%ymm11 \n\t"
			"vPADDQ %%ymm11, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTI128  $1,  %%ymm12, %%xmm0   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm0,  %%xmm12  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm13, %%xmm1   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm13  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm14, %%xmm2   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm2,  %%xmm14  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm15, %%xmm3   \n\t" // extract higher 128 bits
			"PADDQ               %%xmm3,  %%xmm15  \n\t" // sum
			::
		);

		int64_t n_rem=n%8;
		if (n_rem & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1 \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2 \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3 \n\t"
				"VEXTRACTI128  $1,  %%ymm0,  %%xmm8  \n\t" // extract higher 08 bits
				"PADDQ               %%xmm8,  %%xmm0  \n\t" // sum
				"PADDQ               %%xmm0,  %%xmm12 \n\t"
				"VEXTRACTI128  $1,  %%ymm1,  %%xmm9  \n\t" // extract higher 28 bits
				"PADDQ               %%xmm9,  %%xmm1  \n\t" // sum
				"PADDQ               %%xmm1,  %%xmm13 \n\t"
				"VEXTRACTI128  $1,  %%ymm2,  %%xmm10 \n\t" // extract highe2128 bits
				"PADDQ               %%xmm10, %%xmm2  \n\t" // sum
				"PADDQ               %%xmm2,  %%xmm14 \n\t"
				"VEXTRACTI128  $1,  %%ymm3,  %%xmm11 \n\t" // extract high3 128 bits
				"PADDQ               %%xmm11, %%xmm3  \n\t" // sum
				"PADDQ               %%xmm3,  %%xmm15 \n\t"
				"subq   $-4*8, %[x0]       \n\t"
				"subq   $-4*8, %[x1]       \n\t"
				"subq   $-4*8, %[x2]       \n\t"
				"subq   $-4*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"pxor %%xmm0, %%xmm0      \n\t"
				"pxor %%xmm1, %%xmm1      \n\t"
				"pxor %%xmm2, %%xmm2      \n\t"
				"pxor %%xmm3, %%xmm3      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ  %%xmm0,   %%xmm12 \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ  %%xmm1,   %%xmm13 \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ  %%xmm2,   %%xmm14 \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ  %%xmm3,   %%xmm15 \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		int64_t sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"MOVDQU %%xmm12, %%xmm0 \n\t"
			"PSRLDQ $8, %%xmm12     \n\t"
			"PADDQ %%xmm12, %%xmm0  \n\t"
			"MOVQ  %%xmm0, %[s0]   \n\t"
			"\n\t"
			"MOVDQU %%xmm13, %%xmm1 \n\t"
			"PSRLDQ $8, %%xmm13     \n\t"
			"PADDQ %%xmm13, %%xmm1  \n\t"
			"MOVQ  %%xmm1, %[s1]   \n\t"
			"\n\t"
			"MOVDQU %%xmm14, %%xmm2 \n\t"
			"PSRLDQ $8, %%xmm14     \n\t"
			"PADDQ %%xmm14, %%xmm2  \n\t"
			"MOVQ  %%xmm2, %[s2]   \n\t"
			"\n\t"
			"MOVDQU %%xmm15, %%xmm3 \n\t"
			"PSRLDQ $8, %%xmm15     \n\t"
			"PADDQ %%xmm15, %%xmm3  \n\t"
			"MOVQ  %%xmm3, %[s3]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
	}
}

void sum_up_matrix_unroll_04_02_r8_ASM(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1;
	x1 = x0;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"vpxor %%ymm4, %%ymm4, %%ymm4 \n\t"
			"vpxor %%ymm5, %%ymm5, %%ymm5 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm4, %%ymm4 \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vaddpd %%ymm1, %%ymm5, %%ymm5 \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm4, %%ymm4 \n\t"
			"vaddpd %%ymm1, %%ymm5, %%ymm5 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $1,  %%ymm4, %%xmm0  \n\t" // extract higher 128 bits
			"addpd               %%xmm0, %%xmm4  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm5, %%xmm1  \n\t" // extract higher 128 bits
			"addpd               %%xmm1, %%xmm5  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm0, %%xmm4      \n\t"
				"addpd %%xmm1, %%xmm5      \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd  %%xmm0,   %%xmm4  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"movsd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd  %%xmm1,   %%xmm5  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		double sum0, sum1;
		__asm__ __volatile__ (
			"movupd %%xmm4, %%xmm8 \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"addpd %%xmm4, %%xmm8  \n\t"
			"movsd %%xmm8, %[s0]   \n\t"
			"\n\t"
			"movupd %%xmm5, %%xmm9 \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"addpd %%xmm5, %%xmm9  \n\t"
			"movsd %%xmm9, %[s1]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		j0+=1;
		j1+=1;
	}
}

void sum_up_matrix_unroll_04_02_i8_ASM(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1;
	x1 = x0;

	int64_t skip=c/2, c_unroll=(c/2);
	x1+=skip*n;
	int64_t j0=0, j1=skip;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"vpxor %%ymm4, %%ymm4, %%ymm4 \n\t"
			"vpxor %%ymm5, %%ymm5, %%ymm5 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vPADDQ %%ymm0, %%ymm4, %%ymm4 \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vPADDQ %%ymm1, %%ymm5, %%ymm5 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__(
			"vPADDQ %%ymm0, %%ymm4, %%ymm4 \n\t"
			"vPADDQ %%ymm1, %%ymm5, %%ymm5 \n\t"
			"\n\t"
			"VEXTRACTI128   $1,  %%ymm4, %%xmm0  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm0, %%xmm4  \n\t" // sum
			"VEXTRACTI128   $1,  %%ymm5, %%xmm1  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1, %%xmm5  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm0, %%xmm4      \n\t"
				"PADDQ %%xmm1, %%xmm5      \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ  0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ  %%xmm0,   %%xmm4  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"MOVQ  0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ  %%xmm1,   %%xmm5  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		int64_t sum0, sum1, sum2, sum3;
		__asm__ __volatile__ (
			"MOVDQU %%xmm4, %%xmm8 \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"PADDQ %%xmm4, %%xmm8  \n\t"
			"MOVQ  %%xmm8, %[s0]   \n\t"
			"\n\t"
			"MOVDQU %%xmm5, %%xmm9 \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"PADDQ %%xmm5, %%xmm9  \n\t"
			"MOVQ  %%xmm9, %[s1]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;

		j0+=1;
		j1+=1;
	}
}

void sum_up_matrix_unroll_04_08_r8_ASM(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<8){
		sum_up_matrix_naive_r8_C(x_sum, x0, n, c);
		return;
	}
	double *x1, *x2, *x3, *x4, *x5, *x6, *x7;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x4 = x0;
	x5 = x0;
	x6 = x0;
	x7 = x0;

	int64_t skip=c/8, c_unroll=(c/8);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	x4+=skip*n*4;
	x5+=skip*n*5;
	x6+=skip*n*6;
	x7+=skip*n*7;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3, j4=skip*4, j5=skip*5, j6=skip*6, j7=skip*7;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vmovupd 0*8(%[x0]), %%ymm0 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1 \n\t"
			"vmovupd 0*8(%[x2]), %%ymm2 \n\t"
			"vmovupd 0*8(%[x3]), %%ymm3 \n\t"
			"vmovupd 0*8(%[x4]), %%ymm4 \n\t"
			"vmovupd 0*8(%[x5]), %%ymm5 \n\t"
			"vmovupd 0*8(%[x6]), %%ymm6 \n\t"
			"vmovupd 0*8(%[x7]), %%ymm7 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"subq $-4*8, %[x2]          \n\t"
			"subq $-4*8, %[x3]          \n\t"
			"subq $-4*8, %[x4]          \n\t"
			"subq $-4*8, %[x5]          \n\t"
			"subq $-4*8, %[x6]          \n\t"
			"subq $-4*8, %[x7]          \n\t"
			"vpxor %%ymm8, %%ymm8, %%ymm8 \n\t"
			"vpxor %%ymm9, %%ymm9, %%ymm9 \n\t"
			"vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
			"vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vaddpd %%ymm0, %%ymm8, %%ymm8 \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vaddpd %%ymm1, %%ymm9, %%ymm9 \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"vaddpd %%ymm2, %%ymm10, %%ymm10 \n\t"
				"vmovupd 0*8(%[x2]), %%ymm2    \n\t"
				"\n\t"
				"vaddpd %%ymm3, %%ymm11, %%ymm11 \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3    \n\t"
				"\n\t"
				"vaddpd %%ymm4, %%ymm12, %%ymm12 \n\t"
				"vmovupd 0*8(%[x4]), %%ymm4    \n\t"
				"\n\t"
				"vaddpd %%ymm5, %%ymm13, %%ymm13 \n\t"
				"vmovupd 0*8(%[x5]), %%ymm5    \n\t"
				"\n\t"
				"vaddpd %%ymm6, %%ymm14, %%ymm14 \n\t"
				"vmovupd 0*8(%[x6]), %%ymm6    \n\t"
				"\n\t"
				"vaddpd %%ymm7, %%ymm15, %%ymm15 \n\t"
				"vmovupd 0*8(%[x7]), %%ymm7    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				"subq $-4*8, %[x2]             \n\t"
				"subq $-4*8, %[x3]             \n\t"
				"subq $-4*8, %[x4]             \n\t"
				"subq $-4*8, %[x5]             \n\t"
				"subq $-4*8, %[x6]             \n\t"
				"subq $-4*8, %[x7]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		__asm__ __volatile__(
			"vaddpd %%ymm0, %%ymm8, %%ymm8 \n\t"
			"vaddpd %%ymm1, %%ymm9, %%ymm9 \n\t"
			"vaddpd %%ymm2, %%ymm10, %%ymm10 \n\t"
			"vaddpd %%ymm3, %%ymm11, %%ymm11 \n\t"
			"vaddpd %%ymm4, %%ymm12, %%ymm12 \n\t"
			"vaddpd %%ymm5, %%ymm13, %%ymm13 \n\t"
			"vaddpd %%ymm6, %%ymm14, %%ymm14 \n\t"
			"vaddpd %%ymm7, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $1,  %%ymm8,  %%xmm0  \n\t" // extract higher 128 bits
			"addpd               %%xmm0,  %%xmm8  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm9,  %%xmm1  \n\t" // extract higher 128 bits
			"addpd               %%xmm1,  %%xmm9  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm10, %%xmm2  \n\t" // extract higher 128 bits
			"addpd               %%xmm2,  %%xmm10  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm11, %%xmm3  \n\t" // extract higher 128 bits
			"addpd               %%xmm3,  %%xmm11  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm4  \n\t" // extract higher 128 bits
			"addpd               %%xmm4,  %%xmm12  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm5  \n\t" // extract higher 128 bits
			"addpd               %%xmm5,  %%xmm13  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm6  \n\t" // extract higher 128 bits
			"addpd               %%xmm6,  %%xmm14  \n\t" // sum
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm7  \n\t" // extract higher 128 bits
			"addpd               %%xmm7,  %%xmm15  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"movupd 0*8(%[x3]), %%xmm3 \n\t"
				"movupd 0*8(%[x4]), %%xmm4 \n\t"
				"movupd 0*8(%[x5]), %%xmm5 \n\t"
				"movupd 0*8(%[x6]), %%xmm6 \n\t"
				"movupd 0*8(%[x7]), %%xmm7 \n\t"
				"addpd %%xmm0, %%xmm8      \n\t"
				"addpd %%xmm1, %%xmm9      \n\t"
				"addpd %%xmm2, %%xmm10     \n\t"
				"addpd %%xmm3, %%xmm11     \n\t"
				"addpd %%xmm4, %%xmm12     \n\t"
				"addpd %%xmm5, %%xmm13     \n\t"
				"addpd %%xmm6, %%xmm14     \n\t"
				"addpd %%xmm7, %%xmm15     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				"subq   $-2*8, %[x4]       \n\t"
				"subq   $-2*8, %[x5]       \n\t"
				"subq   $-2*8, %[x6]       \n\t"
				"subq   $-2*8, %[x7]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"movsd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd  %%xmm0,   %%xmm8  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"movsd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd  %%xmm1,   %%xmm9  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"movsd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd  %%xmm2,   %%xmm10  \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"movsd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd  %%xmm3,   %%xmm11  \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				"movsd 0*8(%[x4]), %%xmm4 \n\t"
				"addpd  %%xmm4,   %%xmm12  \n\t"
				"subq   $-1*8, %[x4]      \n\t"
				"\n\t"
				"movsd 0*8(%[x5]), %%xmm5 \n\t"
				"addpd  %%xmm5,   %%xmm13  \n\t"
				"subq   $-1*8, %[x5]      \n\t"
				"\n\t"
				"movsd 0*8(%[x6]), %%xmm6 \n\t"
				"addpd  %%xmm6,   %%xmm14  \n\t"
				"subq   $-1*8, %[x6]      \n\t"
				"\n\t"
				"movsd 0*8(%[x7]), %%xmm7 \n\t"
				"addpd  %%xmm7,   %%xmm15  \n\t"
				"subq   $-1*8, %[x7]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		double sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7;
		__asm__ __volatile__ (
			"movupd %%xmm8, %%xmm4  \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"addpd %%xmm4, %%xmm8  \n\t"
			"movsd %%xmm8, %[s0]   \n\t"
			"\n\t"
			"movupd %%xmm9, %%xmm5  \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"addpd %%xmm5, %%xmm9  \n\t"
			"movsd %%xmm9, %[s1]   \n\t"
			"\n\t"
			"movupd %%xmm10, %%xmm6  \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"addpd %%xmm6, %%xmm10  \n\t"
			"movsd %%xmm10, %[s2]   \n\t"
			"\n\t"
			"movupd %%xmm11, %%xmm7  \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"addpd %%xmm7, %%xmm11  \n\t"
			"movsd %%xmm11, %[s3]   \n\t"
			"\n\t"
			"movupd %%xmm12, %%xmm4  \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"addpd %%xmm4, %%xmm12  \n\t"
			"movsd %%xmm12, %[s4]   \n\t"
			"\n\t"
			"movupd %%xmm13, %%xmm5  \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"addpd %%xmm5, %%xmm13  \n\t"
			"movsd %%xmm13, %[s5]   \n\t"
			"\n\t"
			"movupd %%xmm14, %%xmm6  \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"addpd %%xmm6, %%xmm14  \n\t"
			"movsd %%xmm14, %[s6]   \n\t"
			"\n\t"
			"movupd %%xmm15, %%xmm7  \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"addpd %%xmm7, %%xmm15  \n\t"
			"movsd %%xmm15, %[s7]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3), [s4]"=m"(sum4), [s5]"=m"(sum5), [s6]"=m"(sum6), [s7]"=m"(sum7)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;
		x_sum[j4] = sum4;
		x_sum[j5] = sum5;
		x_sum[j6] = sum6;
		x_sum[j7] = sum7;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
		j4+=1;
		j5+=1;
		j6+=1;
		j7+=1;
	}
}

void sum_up_matrix_unroll_04_08_i8_ASM(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<8){
		sum_up_matrix_naive_i8_C(x_sum, x0, n, c);
		return;
	}
	int64_t *x1, *x2, *x3, *x4, *x5, *x6, *x7;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x4 = x0;
	x5 = x0;
	x6 = x0;
	x7 = x0;

	int64_t skip=c/8, c_unroll=(c/8);
	x1+=skip*n;
	x2+=skip*n*2;
	x3+=skip*n*3;
	x4+=skip*n*4;
	x5+=skip*n*5;
	x6+=skip*n*6;
	x7+=skip*n*7;
	int64_t j0=0, j1=skip, j2=skip*2, j3=skip*3, j4=skip*4, j5=skip*5, j6=skip*6, j7=skip*7;
	while(c_unroll--){
		int64_t n_unroll=(n>>2);
		__asm__ __volatile__ (
			"vMOVDQU 0*8(%[x0]), %%ymm0 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1 \n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm2 \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm3 \n\t"
			"vMOVDQU 0*8(%[x4]), %%ymm4 \n\t"
			"vMOVDQU 0*8(%[x5]), %%ymm5 \n\t"
			"vMOVDQU 0*8(%[x6]), %%ymm6 \n\t"
			"vMOVDQU 0*8(%[x7]), %%ymm7 \n\t"
			"subq $-4*8, %[x0]          \n\t"
			"subq $-4*8, %[x1]          \n\t"
			"subq $-4*8, %[x2]          \n\t"
			"subq $-4*8, %[x3]          \n\t"
			"subq $-4*8, %[x4]          \n\t"
			"subq $-4*8, %[x5]          \n\t"
			"subq $-4*8, %[x6]          \n\t"
			"subq $-4*8, %[x7]          \n\t"
			"vpxor %%ymm8, %%ymm8, %%ymm8 \n\t"
			"vpxor %%ymm9, %%ymm9, %%ymm9 \n\t"
			"vpxor %%ymm10, %%ymm10, %%ymm10 \n\t"
			"vpxor %%ymm11, %%ymm11, %%ymm11 \n\t"
			"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
		);
		n_unroll--;
		while(n_unroll--){
			__asm__ __volatile__ (
				"vPADDQ %%ymm0, %%ymm8, %%ymm8 \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0    \n\t"
				"\n\t"
				"vPADDQ %%ymm1, %%ymm9, %%ymm9 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1    \n\t"
				"\n\t"
				"vPADDQ %%ymm2, %%ymm10, %%ymm10 \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2    \n\t"
				"\n\t"
				"vPADDQ %%ymm3, %%ymm11, %%ymm11 \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3    \n\t"
				"\n\t"
				"vPADDQ %%ymm4, %%ymm12, %%ymm12 \n\t"
				"vMOVDQU 0*8(%[x4]), %%ymm4    \n\t"
				"\n\t"
				"vPADDQ %%ymm5, %%ymm13, %%ymm13 \n\t"
				"vMOVDQU 0*8(%[x5]), %%ymm5    \n\t"
				"\n\t"
				"vPADDQ %%ymm6, %%ymm14, %%ymm14 \n\t"
				"vMOVDQU 0*8(%[x6]), %%ymm6    \n\t"
				"\n\t"
				"vPADDQ %%ymm7, %%ymm15, %%ymm15 \n\t"
				"vMOVDQU 0*8(%[x7]), %%ymm7    \n\t"
				"\n\t"
				"subq $-4*8, %[x0]             \n\t"
				"subq $-4*8, %[x1]             \n\t"
				"subq $-4*8, %[x2]             \n\t"
				"subq $-4*8, %[x3]             \n\t"
				"subq $-4*8, %[x4]             \n\t"
				"subq $-4*8, %[x5]             \n\t"
				"subq $-4*8, %[x6]             \n\t"
				"subq $-4*8, %[x7]             \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		__asm__ __volatile__(
			"vPADDQ %%ymm0, %%ymm8, %%ymm8 \n\t"
			"vPADDQ %%ymm1, %%ymm9, %%ymm9 \n\t"
			"vPADDQ %%ymm2, %%ymm10, %%ymm10 \n\t"
			"vPADDQ %%ymm3, %%ymm11, %%ymm11 \n\t"
			"vPADDQ %%ymm4, %%ymm12, %%ymm12 \n\t"
			"vPADDQ %%ymm5, %%ymm13, %%ymm13 \n\t"
			"vPADDQ %%ymm6, %%ymm14, %%ymm14 \n\t"
			"vPADDQ %%ymm7, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTI128  $1,  %%ymm8,  %%xmm0  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm0,  %%xmm8  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm9,  %%xmm1  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm1,  %%xmm9  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm10, %%xmm2  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm2,  %%xmm10  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm11, %%xmm3  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm3,  %%xmm11  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm12, %%xmm4  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm4,  %%xmm12  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm13, %%xmm5  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm5,  %%xmm13  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm14, %%xmm6  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm6,  %%xmm14  \n\t" // sum
			"VEXTRACTI128  $1,  %%ymm15, %%xmm7  \n\t" // extract higher 128 bits
			"PADDQ               %%xmm7,  %%xmm15  \n\t" // sum
			::
		);

		int64_t n_rem=n%4;
		if (n_rem & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
				"MOVDQU 0*8(%[x4]), %%xmm4 \n\t"
				"MOVDQU 0*8(%[x5]), %%xmm5 \n\t"
				"MOVDQU 0*8(%[x6]), %%xmm6 \n\t"
				"MOVDQU 0*8(%[x7]), %%xmm7 \n\t"
				"PADDQ %%xmm0, %%xmm8      \n\t"
				"PADDQ %%xmm1, %%xmm9      \n\t"
				"PADDQ %%xmm2, %%xmm10     \n\t"
				"PADDQ %%xmm3, %%xmm11     \n\t"
				"PADDQ %%xmm4, %%xmm12     \n\t"
				"PADDQ %%xmm5, %%xmm13     \n\t"
				"PADDQ %%xmm6, %%xmm14     \n\t"
				"PADDQ %%xmm7, %%xmm15     \n\t"
				"subq   $-2*8, %[x0]       \n\t"
				"subq   $-2*8, %[x1]       \n\t"
				"subq   $-2*8, %[x2]       \n\t"
				"subq   $-2*8, %[x3]       \n\t"
				"subq   $-2*8, %[x4]       \n\t"
				"subq   $-2*8, %[x5]       \n\t"
				"subq   $-2*8, %[x6]       \n\t"
				"subq   $-2*8, %[x7]       \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		if (n_rem & 1){
			__asm__ __volatile__ (
				"MOVQ 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ  %%xmm0,   %%xmm8  \n\t"
				"subq   $-1*8, %[x0]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ  %%xmm1,   %%xmm9  \n\t"
				"subq   $-1*8, %[x1]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ  %%xmm2,   %%xmm10  \n\t"
				"subq   $-1*8, %[x2]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ  %%xmm3,   %%xmm11  \n\t"
				"subq   $-1*8, %[x3]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x4]), %%xmm4 \n\t"
				"PADDQ  %%xmm4,   %%xmm12  \n\t"
				"subq   $-1*8, %[x4]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x5]), %%xmm5 \n\t"
				"PADDQ  %%xmm5,   %%xmm13  \n\t"
				"subq   $-1*8, %[x5]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x6]), %%xmm6 \n\t"
				"PADDQ  %%xmm6,   %%xmm14  \n\t"
				"subq   $-1*8, %[x6]      \n\t"
				"\n\t"
				"MOVQ 0*8(%[x7]), %%xmm7 \n\t"
				"PADDQ  %%xmm7,   %%xmm15  \n\t"
				"subq   $-1*8, %[x7]      \n\t"
				"\n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3), [x4]"=r"(x4), [x5]"=r"(x5), [x6]"=r"(x6), [x7]"=r"(x7)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3), "4"(x4), "5"(x5), "6"(x6), "7"(x7)
			);
		}

		int64_t sum0, sum1, sum2, sum3, sum4, sum5, sum6, sum7;
		__asm__ __volatile__ (
			"MOVDQU %%xmm8, %%xmm4  \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"PADDQ %%xmm4, %%xmm8  \n\t"
			"MOVQ %%xmm8, %[s0]   \n\t"
			"\n\t"
			"MOVDQU %%xmm9, %%xmm5  \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"PADDQ %%xmm5, %%xmm9  \n\t"
			"MOVQ %%xmm9, %[s1]   \n\t"
			"\n\t"
			"MOVDQU %%xmm10, %%xmm6  \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"PADDQ %%xmm6, %%xmm10  \n\t"
			"MOVQ %%xmm10, %[s2]   \n\t"
			"\n\t"
			"MOVDQU %%xmm11, %%xmm7  \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"PADDQ %%xmm7, %%xmm11  \n\t"
			"MOVQ %%xmm11, %[s3]   \n\t"
			"\n\t"
			"MOVDQU %%xmm12, %%xmm4  \n\t"
			"PSRLDQ $8, %%xmm4     \n\t"
			"PADDQ %%xmm4, %%xmm12  \n\t"
			"MOVQ %%xmm12, %[s4]   \n\t"
			"\n\t"
			"MOVDQU %%xmm13, %%xmm5  \n\t"
			"PSRLDQ $8, %%xmm5     \n\t"
			"PADDQ %%xmm5, %%xmm13  \n\t"
			"MOVQ %%xmm13, %[s5]   \n\t"
			"\n\t"
			"MOVDQU %%xmm14, %%xmm6  \n\t"
			"PSRLDQ $8, %%xmm6     \n\t"
			"PADDQ %%xmm6, %%xmm14  \n\t"
			"MOVQ %%xmm14, %[s6]   \n\t"
			"\n\t"
			"MOVDQU %%xmm15, %%xmm7  \n\t"
			"PSRLDQ $8, %%xmm7     \n\t"
			"PADDQ %%xmm7, %%xmm15  \n\t"
			"MOVQ %%xmm15, %[s7]   \n\t"
			:[s0]"=m"(sum0), [s1]"=m"(sum1), [s2]"=m"(sum2), [s3]"=m"(sum3), [s4]"=m"(sum4), [s5]"=m"(sum5), [s6]"=m"(sum6), [s7]"=m"(sum7)
			:
		);

		x_sum[j0] = sum0;
		x_sum[j1] = sum1;
		x_sum[j2] = sum2;
		x_sum[j3] = sum3;
		x_sum[j4] = sum4;
		x_sum[j5] = sum5;
		x_sum[j6] = sum6;
		x_sum[j7] = sum7;

		j0+=1;
		j1+=1;
		j2+=1;
		j3+=1;
		j4+=1;
		j5+=1;
		j6+=1;
		j7+=1;
	}
}
