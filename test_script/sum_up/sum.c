#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double sum_xy(double x, double y){
    return(x+y);
}

int64_t sum_naive_i8_c(int64_t x[], int64_t n){
    int64_t res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

double sum_naive_r8_c(double x[], int64_t n){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

int64_t sum_unroll_i8_c(int64_t x[], int64_t n){
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

double sum_unroll_r8_c(double x[], int64_t n){
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

double sqsum_naive_r8_c(double x[], int64_t n){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += pow(x[i],2);
    }
    return(res);
}

int64_t sum_assembl_i8_04_C(int64_t x[], int64_t n){
	if (n<4){
		return(sum_naive_i8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 2;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vaddpd      %%ymm0 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"subq $-4*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
		// "vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm12, %%ymm12, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm12, %%ymm12\n\t"
		"vhaddpd           %%ymm12, %%ymm12, %%ymm12\n\t"
		"movsd             %%xmm12, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		int64_t res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm0, %%xmm0       \n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_i8_04:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"loop remsum_i8_04 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

double sum_assembl_r8_04_C(double x[], int64_t n){
	if (n<4){
		return(sum_naive_r8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 2;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	while( n_unroll-- ){ 
		__asm__ __volatile__(
			"\n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vaddpd      %%ymm0 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"subq $-4*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	// value = ymm12 + ymm13 + ymm14 + ymm15;
	double value=0e0;
	__asm__ __volatile__(
		"\n\t"
		// "vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm12, %%ymm12, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm12, %%ymm12\n\t"
		"vhaddpd           %%ymm12, %%ymm12, %%ymm12\n\t"
		"movsd             %%xmm12, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		double res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm0, %%xmm0       \n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_04:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"loop remsum_04 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

int64_t sum_assembl_i8_08_C(int64_t x[], int64_t n){
	if (n<8){
		return(sum_naive_i8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 3;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm2 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm2 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm2 \n\t"
		"vaddpd      %%ymm2 , %%ymm12, %%ymm12\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
		// "vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm12, %%ymm12, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm12, %%ymm12\n\t"
		"vhaddpd           %%ymm12, %%ymm12, %%ymm12\n\t"
		"movsd             %%xmm12, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		int64_t res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_i8_08:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"sub $1, %[n]              \n\t"
			"loop remsum_i8_08 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

double sum_assembl_r8_08_C(double x[], int64_t n){
	if (n<8){
		return(sum_naive_r8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 3;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm2 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm2 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm2 \n\t"
		"vaddpd      %%ymm2 , %%ymm12, %%ymm12\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	double value=0e0;
	__asm__ __volatile__(
		"\n\t"
		// "vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm12, %%ymm12, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm12, %%ymm12\n\t"
		"vhaddpd           %%ymm12, %%ymm12, %%ymm12\n\t"
		"movsd             %%xmm12, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		double res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_08:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"sub $1, %[n]              \n\t"
			"loop remsum_08 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

int64_t sum_assembl_i8_16_C(int64_t x[], int64_t n){
	if (n<16){
		return(sum_naive_i8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 4;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
		"vmovupd   8*8(%[x]), %%ymm2 \n\t"
		"vmovupd  12*8(%[x]), %%ymm3 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm4 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm4 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"\n\t"
			"vaddpd      %%ymm2, %%ymm3, %%ymm5 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vaddpd      %%ymm5 , %%ymm13, %%ymm13\n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm4 \n\t"
		"vaddpd      %%ymm4 , %%ymm12, %%ymm12\n\t"
		"vaddpd      %%ymm2, %%ymm3, %%ymm5 \n\t"
		"vaddpd      %%ymm5 , %%ymm13, %%ymm13\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
		"vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm13, %%ymm13, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm13, %%ymm13\n\t"
		"vhaddpd           %%ymm13, %%ymm13, %%ymm13\n\t"
		"movsd             %%xmm13, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		int64_t res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_i8_16:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"sub $1, %[n]              \n\t"
			"loop remsum_i8_16 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}

	return value;
}

double sum_assembl_r8_16_C(double x[], int64_t n){
	if (n<16){
		return(sum_naive_r8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 4;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
		"vmovupd   8*8(%[x]), %%ymm2 \n\t"
		"vmovupd  12*8(%[x]), %%ymm3 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm4 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm4 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"\n\t"
			"vaddpd      %%ymm2, %%ymm3, %%ymm5 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vaddpd      %%ymm5 , %%ymm13, %%ymm13\n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm4 \n\t"
		"vaddpd      %%ymm4 , %%ymm12, %%ymm12\n\t"
		"vaddpd      %%ymm2, %%ymm3, %%ymm5 \n\t"
		"vaddpd      %%ymm5 , %%ymm13, %%ymm13\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	double value=0e0;
	__asm__ __volatile__(
		"\n\t"
		"vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		// "vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		// "vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm13, %%ymm13, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm13, %%ymm13\n\t"
		"vhaddpd           %%ymm13, %%ymm13, %%ymm13\n\t"
		"movsd             %%xmm13, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		double res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_16:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"sub $1, %[n]              \n\t"
			"loop remsum_16 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}

	return value;
}

int64_t sum_assembl_i8_32_C(int64_t x[], int64_t n){
	if (n<32){
		return(sum_naive_i8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 5;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
		"vmovupd   8*8(%[x]), %%ymm2 \n\t"
		"vmovupd  12*8(%[x]), %%ymm3 \n\t"
		"vmovupd  16*8(%[x]), %%ymm4 \n\t"
		"vmovupd  20*8(%[x]), %%ymm5 \n\t"
		"vmovupd  24*8(%[x]), %%ymm6 \n\t"
		"vmovupd  28*8(%[x]), %%ymm7 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm8 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"vaddpd      %%ymm2, %%ymm3, %%ymm9 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
			"\n\t"
			"vaddpd      %%ymm4, %%ymm5, %%ymm10 \n\t"
			"vmovupd  16*8(%[x]), %%ymm4 \n\t"
			"vmovupd  20*8(%[x]), %%ymm5 \n\t"
			"vaddpd      %%ymm10 , %%ymm14, %%ymm14\n\t"
			"\n\t"
			"vaddpd      %%ymm6, %%ymm7, %%ymm11 \n\t"
			"vmovupd  24*8(%[x]), %%ymm6 \n\t"
			"vmovupd  28*8(%[x]), %%ymm7 \n\t"
			"vaddpd      %%ymm11 , %%ymm15, %%ymm15\n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm8 \n\t"
		"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
		"\n\t"
		"vaddpd      %%ymm2, %%ymm3, %%ymm9 \n\t"
		"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
		"\n\t"
		"vaddpd      %%ymm4, %%ymm5, %%ymm10 \n\t"
		"vaddpd      %%ymm10 , %%ymm14, %%ymm14\n\t"
		"\n\t"
		"vaddpd      %%ymm6, %%ymm7, %%ymm11 \n\t"
		"vaddpd      %%ymm11 , %%ymm15, %%ymm15\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
		"vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		"vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		"vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		int64_t res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_i8_32:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"loop remsum_i8_32 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

double sum_assembl_r8_32_C(double x[], int64_t n){
	if (n<32){
		return(sum_naive_r8_c(x,n));
	}

	// Zero Clear
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);

	int64_t n_unroll, pow2, unroll_size;
	pow2 = 5;
	unroll_size = pow(2, pow2);
	n_unroll = (n>>pow2);  // Unroll

	// Add Main elements
	__asm__ __volatile__(
		"\n\t"
		"vmovupd   0*8(%[x]), %%ymm0 \n\t"
		"vmovupd   4*8(%[x]), %%ymm1 \n\t"
		"vmovupd   8*8(%[x]), %%ymm2 \n\t"
		"vmovupd  12*8(%[x]), %%ymm3 \n\t"
		"vmovupd  16*8(%[x]), %%ymm4 \n\t"
		"vmovupd  20*8(%[x]), %%ymm5 \n\t"
		"vmovupd  24*8(%[x]), %%ymm6 \n\t"
		"vmovupd  28*8(%[x]), %%ymm7 \n\t"
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
			"vaddpd      %%ymm0, %%ymm1, %%ymm8 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"vaddpd      %%ymm2, %%ymm3, %%ymm9 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
			"\n\t"
			"vaddpd      %%ymm4, %%ymm5, %%ymm10 \n\t"
			"vmovupd  16*8(%[x]), %%ymm4 \n\t"
			"vmovupd  20*8(%[x]), %%ymm5 \n\t"
			"vaddpd      %%ymm10 , %%ymm14, %%ymm14\n\t"
			"\n\t"
			"vaddpd      %%ymm6, %%ymm7, %%ymm11 \n\t"
			"vmovupd  24*8(%[x]), %%ymm6 \n\t"
			"vmovupd  28*8(%[x]), %%ymm7 \n\t"
			"vaddpd      %%ymm11 , %%ymm15, %%ymm15\n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vaddpd      %%ymm0, %%ymm1, %%ymm8 \n\t"
		"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
		"\n\t"
		"vaddpd      %%ymm2, %%ymm3, %%ymm9 \n\t"
		"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
		"\n\t"
		"vaddpd      %%ymm4, %%ymm5, %%ymm10 \n\t"
		"vaddpd      %%ymm10 , %%ymm14, %%ymm14\n\t"
		"\n\t"
		"vaddpd      %%ymm6, %%ymm7, %%ymm11 \n\t"
		"vaddpd      %%ymm11 , %%ymm15, %%ymm15\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	double value=0e0;
	__asm__ __volatile__(
		"\n\t"
		"vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		"vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		"vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	int64_t n_rem=n%unroll_size;
	if (n_rem>0){
		double res;
		__asm__ __volatile__ (
			"\n\t"
			"pxor %%xmm1, %%xmm1       \n\t"
			"mov %[n], %%rcx \n\t"
			"\n\t"
			"remsum_32:"
			"movsd 0*8(%[x]), %%xmm0   \n\t"
			"addsd %%xmm0, %%xmm1      \n\t"
			"subq $-1*8, %[x]          \n\t"
			"loop remsum_32 \n\t"
			"\n\t"
			"movsd %%xmm1, %[v] \n\t"
			:[x]"=r"(x), [v]"=m"(res), [n]"=m"(n_rem)
			:"0"(x)
		);
		value+=res;
	}
	return value;
}

double sqsum_assembl(double x[], int64_t n){
	if (n<32){
		return(sqsum_naive_r8_c(x,n));
	}
	/*
		xalign : boundary pattern
			1)     aligned -> Lower 4 bits ......0000
			2) NOT aligned -> Lower 4 bits ......1011 etc.

		is_align : alignment aase number
			0 = x not aligned
			1 = x     aligned
	*/
	u_int64_t xalign   = ((u_int64_t)x) & ALIGN_CHECK;
	u_int64_t is_align = (xalign>0)?1:0;

	/*
		YMM regiser variables
	*/
	double ymm0,ymm1,ymm2,ymm3;
	double ymm4,ymm5,ymm6,ymm7;
	double ymm8,ymm9,ymm10,ymm11;
	double ymm12,ymm13,ymm14,ymm15;

	/*
		Unrolled Indeces
	*/
	int64_t n_unroll, i, n_rem;
	double value = 0e0, res=0e0;

	n_rem = n%32;
    for (i=n-n_rem; i<n; i++){
        res += pow(x[i],2);
    }

	// Zero Clear
	// ymm12 = 0e0;
	// ymm13 = 0e0;
	// ymm14 = 0e0;
	// ymm15 = 0e0;
	__asm__ __volatile__(
		"vpxor %%ymm12, %%ymm12, %%ymm12 \n\t"
		"vpxor %%ymm13, %%ymm13, %%ymm13 \n\t"
		"vpxor %%ymm14, %%ymm14, %%ymm14 \n\t"
		"vpxor %%ymm15, %%ymm15, %%ymm15 \n\t"
		::
	);
	n_unroll = (n>>5);  // Unroll by 32 elements

	/********************************************************
		ALIGNMENT CASE 0
	********************************************************/
	if( !is_align ){
		// Initial Load to avoid pipe line hazard
		__asm__ __volatile__(
			"\n\t"
			"vmovapd   0*8(%[x]), %%ymm0 \n\t"
			"vmovapd   4*8(%[x]), %%ymm1 \n\t"
			"vmovapd   8*8(%[x]), %%ymm2 \n\t"
			"vmovapd  12*8(%[x]), %%ymm3 \n\t"
			"vmovapd  16*8(%[x]), %%ymm4 \n\t"
			"vmovapd  20*8(%[x]), %%ymm5 \n\t"
			"vmovapd  24*8(%[x]), %%ymm6 \n\t"
			"vmovapd  28*8(%[x]), %%ymm7 \n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		n_unroll--;
		while( n_unroll-- ){ 
			// ymm0  = *(x  ); // Load
			// ymm1  = *(x+1); // Load
			// ymm2  = *(x+2); // Load
			// ymm3  = *(x+3); // Load
			// ymm4  = *(x+4); // Load
			// ymm5  = *(x+5); // Load
			// ymm6  = *(x+6); // Load
			// ymm7  = *(x+7); // Load
			// ymm8  = ymm0  + ymm4; // Add
			// ymm9  = ymm1  + ymm5; // Add
			// ymm10 = ymm2  + ymm6; // Add
			// ymm11 = ymm3  + ymm7; // Add
			// ymm12 = ymm12 + ymm8; // Add
			// ymm13 = ymm13 + ymm9; // Add
			// ymm14 = ymm14 + ymm10;// Add
			// ymm15 = ymm15 + ymm11;// Add
			// x+=16;
			__asm__ __volatile__(
				"\n\t"
				"vmulpd      %%ymm0 , %%ymm0 , %%ymm0 \n\t"
				"vmulpd      %%ymm4 , %%ymm4 , %%ymm4 \n\t"
				"vaddpd      %%ymm0 , %%ymm4 , %%ymm8 \n\t"
				"vmovapd   0*8(%[x]), %%ymm0 \n\t"
				"vmovapd   4*8(%[x]), %%ymm4 \n\t"
				"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
				"\n\t"
				"vmulpd      %%ymm1 , %%ymm1 , %%ymm1 \n\t"
				"vmulpd      %%ymm5 , %%ymm5 , %%ymm5 \n\t"
				"vaddpd      %%ymm1 , %%ymm5 , %%ymm9 \n\t"
				"vmovapd   8*8(%[x]), %%ymm1 \n\t"
				"vmovapd  12*8(%[x]), %%ymm5 \n\t"
				"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
				"\n\t"
				"vmulpd      %%ymm2 , %%ymm2 , %%ymm2 \n\t"
				"vmulpd      %%ymm6 , %%ymm6 , %%ymm6 \n\t"
				"vaddpd      %%ymm2 , %%ymm6 , %%ymm10\n\t"
				"vmovapd  16*8(%[x]), %%ymm2 \n\t"
				"vmovapd  20*8(%[x]), %%ymm6 \n\t"
				"vaddpd      %%ymm10, %%ymm14, %%ymm14\n\t"
				"\n\t"
				"vmulpd      %%ymm3 , %%ymm3 , %%ymm3 \n\t"
				"vmulpd      %%ymm7 , %%ymm7 , %%ymm7 \n\t"
				"vaddpd      %%ymm3 , %%ymm7 , %%ymm11\n\t"
				"vmovapd  24*8(%[x]), %%ymm3 \n\t"
				"vmovapd  28*8(%[x]), %%ymm7 \n\t"
				"vaddpd      %%ymm11, %%ymm15, %%ymm15\n\t"
				"\n\t"
				"subq $-32*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}
	/********************************************************
	   ALIGNMENT CASE 1, 2, 3
	********************************************************/
	}else{
		// Initial Load to avoid pipe line hazard
		__asm__ __volatile__(
			"\n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vmovupd  16*8(%[x]), %%ymm4 \n\t"
			"vmovupd  20*8(%[x]), %%ymm5 \n\t"
			"vmovupd  24*8(%[x]), %%ymm6 \n\t"
			"vmovupd  28*8(%[x]), %%ymm7 \n\t"
			"\n\t"
			"subq $-32*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		n_unroll--;
		while( n_unroll-- ){ 
			// ymm0  = *(x  ); // Load
			// ymm1  = *(x+1); // Load
			// ymm2  = *(x+2); // Load
			// ymm3  = *(x+3); // Load
			// ymm4  = *(x+4); // Load
			// ymm5  = *(x+5); // Load
			// ymm6  = *(x+6); // Load
			// ymm7  = *(x+7); // Load
			// ymm8  = ymm0  + ymm4; // Add
			// ymm9  = ymm1  + ymm5; // Add
			// ymm10 = ymm2  + ymm6; // Add
			// ymm11 = ymm3  + ymm7; // Add
			// ymm12 = ymm12 + ymm8; // Add
			// ymm13 = ymm13 + ymm9; // Add
			// ymm14 = ymm14 + ymm10;// Add
			// ymm15 = ymm15 + ymm11;// Add
			// x+=16;
			__asm__ __volatile__(
				"\n\t"
				"vmulpd      %%ymm0 , %%ymm0 , %%ymm0 \n\t"
				"vmulpd      %%ymm4 , %%ymm4 , %%ymm4 \n\t"
				"vaddpd      %%ymm0 , %%ymm4 , %%ymm8 \n\t"
				"vmovupd   0*8(%[x]), %%ymm0 \n\t"
				"vmovupd   4*8(%[x]), %%ymm4 \n\t"
				"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
				"\n\t"
				"vmulpd      %%ymm1 , %%ymm1 , %%ymm1 \n\t"
				"vmulpd      %%ymm5 , %%ymm5 , %%ymm5 \n\t"
				"vaddpd      %%ymm1 , %%ymm5 , %%ymm9 \n\t"
				"vmovupd   8*8(%[x]), %%ymm1 \n\t"
				"vmovupd  12*8(%[x]), %%ymm5 \n\t"
				"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
				"\n\t"
				"vmulpd      %%ymm2 , %%ymm2 , %%ymm2 \n\t"
				"vmulpd      %%ymm6 , %%ymm6 , %%ymm6 \n\t"
				"vaddpd      %%ymm2 , %%ymm6 , %%ymm10\n\t"
				"vmovupd  16*8(%[x]), %%ymm2 \n\t"
				"vmovupd  20*8(%[x]), %%ymm6 \n\t"
				"vaddpd      %%ymm10, %%ymm14, %%ymm14\n\t"
				"\n\t"
				"vmulpd      %%ymm3 , %%ymm3 , %%ymm3 \n\t"
				"vmulpd      %%ymm7 , %%ymm7 , %%ymm7 \n\t"
				"vaddpd      %%ymm3 , %%ymm7 , %%ymm11\n\t"
				"vmovupd  24*8(%[x]), %%ymm3 \n\t"
				"vmovupd  28*8(%[x]), %%ymm7 \n\t"
				"vaddpd      %%ymm11, %%ymm15, %%ymm15\n\t"
				"\n\t"
				"subq $-32*8, %[x]\n\t"
				"\n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}
	}

	// Last Loaded values
	// ymm8  = ymm0  + ymm4; // Add
	// ymm9  = ymm1  + ymm5; // Add
	// ymm10 = ymm2  + ymm6; // Add
	// ymm11 = ymm3  + ymm7; // Add
	// ymm12 = ymm12 + ymm8; // Add
	// ymm13 = ymm13 + ymm9; // Add
	// ymm14 = ymm14 + ymm10;// Add
	// ymm15 = ymm15 + ymm11;// Add
	__asm__ __volatile__(
		"\n\t"
		"vmulpd      %%ymm0 , %%ymm0 , %%ymm0 \n\t"
		"vmulpd      %%ymm1 , %%ymm1 , %%ymm1 \n\t"
		"vmulpd      %%ymm2 , %%ymm2 , %%ymm2 \n\t"
		"vmulpd      %%ymm3 , %%ymm3 , %%ymm3 \n\t"
		"vmulpd      %%ymm4 , %%ymm4 , %%ymm4 \n\t"
		"vmulpd      %%ymm5 , %%ymm5 , %%ymm5 \n\t"
		"vmulpd      %%ymm6 , %%ymm6 , %%ymm6 \n\t"
		"vmulpd      %%ymm7 , %%ymm7 , %%ymm7 \n\t"
		"vaddpd      %%ymm0 , %%ymm4 , %%ymm8 \n\t"
		"vaddpd      %%ymm1 , %%ymm5 , %%ymm9 \n\t"
		"vaddpd      %%ymm2 , %%ymm6 , %%ymm10\n\t"
		"vaddpd      %%ymm3 , %%ymm7 , %%ymm11\n\t"
		"vaddpd      %%ymm8 , %%ymm12, %%ymm12\n\t"
		"vaddpd      %%ymm9 , %%ymm13, %%ymm13\n\t"
		"vaddpd      %%ymm10, %%ymm14, %%ymm14\n\t"
		"vaddpd      %%ymm11, %%ymm15, %%ymm15\n\t"
		"\n\t"
		::
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	__asm__ __volatile__(
		"\n\t"
		"vaddpd            %%ymm12, %%ymm13, %%ymm13\n\t"
		"vaddpd            %%ymm14, %%ymm15, %%ymm15\n\t"
		"vaddpd            %%ymm13, %%ymm15, %%ymm15\n\t"
		"vperm2f128 $0x01, %%ymm15, %%ymm15, %%ymm14\n\t" // exchange |a|b|c|d| -> |c|d|a|b|
		"vhaddpd           %%ymm14, %%ymm15, %%ymm15\n\t"
		"vhaddpd           %%ymm15, %%ymm15, %%ymm15\n\t"
		"movsd             %%xmm15, %[v] \n\t"
		"\n\t"
		:[v]"=m"(value)
	);

	return value+res;
}
