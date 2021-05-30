#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

int64_t sum_assembl_i8_08_C(int64_t x[], int64_t n){
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
			"vPADDQ      %%ymm0, %%ymm1, %%ymm2 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vPADDQ      %%ymm2 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"subq $-8*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vPADDQ      %%ymm0, %%ymm1, %%ymm2 \n\t"
		"vPADDQ      %%ymm2 , %%ymm12, %%ymm12\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
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
			"PADDQ  %%xmm0, %%xmm1      \n\t"
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
			"vPADDQ      %%ymm0, %%ymm1, %%ymm4 \n\t"
			"vmovupd   0*8(%[x]), %%ymm0 \n\t"
			"vmovupd   4*8(%[x]), %%ymm1 \n\t"
			"vPADDQ      %%ymm4 , %%ymm12, %%ymm12\n\t"
			"\n\t"
			"\n\t"
			"vPADDQ      %%ymm2, %%ymm3, %%ymm5 \n\t"
			"vmovupd   8*8(%[x]), %%ymm2 \n\t"
			"vmovupd  12*8(%[x]), %%ymm3 \n\t"
			"vPADDQ      %%ymm5 , %%ymm13, %%ymm13\n\t"
			"\n\t"
			"subq $-16*8, %[x]\n\t"
			"\n\t"
			:[x]"=r"(x)
			:"0"(x)
		);
	}
	__asm__ __volatile__(
		"\n\t"
		"vPADDQ      %%ymm0, %%ymm1, %%ymm4 \n\t"
		"vPADDQ      %%ymm4 , %%ymm12, %%ymm12\n\t"
		"vPADDQ      %%ymm2, %%ymm3, %%ymm5 \n\t"
		"vPADDQ      %%ymm5 , %%ymm13, %%ymm13\n\t"
		"\n\t"
		:[x]"=r"(x)
		:"0"(x)
	);

	// value = ymm12 + ymm13 + ymm14 + ymm15;
	int64_t value=0e0;
	__asm__ __volatile__(
		"\n\t"
		"vPADDQ            %%ymm12, %%ymm13, %%ymm13\n\t"
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
			"PADDQ %%xmm0, %%xmm1      \n\t"
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

