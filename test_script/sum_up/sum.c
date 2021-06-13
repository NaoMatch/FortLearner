#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
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

