#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111


// --------------------------------------------------------------------------------
int64_t sum_up_C_i8(int64_t x[], int64_t n){
    int64_t res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

double sum_up_C_r8(double x[], int64_t n){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        res += x[i];
    }
    return(res);
}

// --------------------------------------------------------------------------------
int64_t sum_up_02_C_i8(int64_t x[], int64_t n){
    int64_t r0,r1;
    int64_t tmp=0;

	int64_t n_unroll=(n>>1), n_remain=n%2;
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r0  = r0  + r1;
		tmp += r0;
		x+=2;
	}

	if (n_remain%2){
		r0  = *(x  );
		tmp += r0;
	}
	return(tmp);
}

double sum_up_02_C_r8(double x[], int64_t n){
    double r0,r1;
    double tmp=0;

	int64_t n_unroll=(n>>1), n_remain=n%2;
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r0  = r0  + r1;
		tmp += r0;
		x+=2;
	}

	if (n_remain%2){
		r0  = *(x  );
		tmp += r0;
	}
	return(tmp);
}

// --------------------------------------------------------------------------------
int64_t sum_up_04_C_i8(int64_t x[], int64_t n){
    int64_t r0,r1,r2,r3;
    int64_t r12,r13,r14,r15;
    int64_t tmp=0;

	int64_t n_unroll=(n>>2);
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r2  = *(x+2);
		r3  = *(x+3);
		r0  = r0 + r1;
		r2  = r2 + r3;
		r0  = r0 + r2;
		tmp += r0;
		x+=4;
	}

	int64_t n_unroll_remain=(n%4);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);
}

double sum_up_04_C_r8(double x[], int64_t n){
    double r0,r1,r2,r3;
    double r12,r13,r14,r15;
    double tmp=0;

	int64_t n_unroll=(n>>2);
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r2  = *(x+2);
		r3  = *(x+3);
		r0  = r0 + r1;
		r2  = r2 + r3;
		r0  = r0 + r2;
		tmp += r0;
		x+=4;
	}

	int64_t n_unroll_remain=(n%4);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);
}

// --------------------------------------------------------------------------------
int64_t sum_up_08_C_i8(int64_t x[], int64_t n){
    int64_t r0,r1,r2,r3;
    int64_t r4,r5,r6,r7;
    int64_t r8,r9,r10,r11;
    int64_t r12,r13,r14,r15;

	int64_t res=0e0;
	r12 = 0e0;
	r13 = 0e0;
	r14 = 0e0;
	r15 = 0e0;
	int64_t n_unroll=(n>>3);
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r2  = *(x+2);
		r3  = *(x+3);
		r4  = *(x+4);
		r5  = *(x+5);
		r6  = *(x+6);
		r7  = *(x+7);
		r8  = r0  + r4;
		r9  = r1  + r5;
		r10 = r2  + r6;
		r11 = r3  + r7;
		r12 = r12 + r8;
		r13 = r13 + r9;
		r14 = r14 + r10;
		r15 = r15 + r11;
		x+=8;
	}

	int64_t n_unroll_remain=(n%8);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		r12 = r12 + r0;
		x+=1;
	}
	return(r12 + r13 + r14 + r15);
}

double sum_up_08_C_r8(double x[], int64_t n){
    double r0,r1,r2,r3;
    double r4,r5,r6,r7;
    double r8,r9,r10,r11;
    double r12,r13,r14,r15;

	double res=0e0;
	r12 = 0e0;
	r13 = 0e0;
	r14 = 0e0;
	r15 = 0e0;
	int64_t n_unroll=(n>>3);
	while( n_unroll-- ){ 
		r0  = *(x  );
		r1  = *(x+1);
		r2  = *(x+2);
		r3  = *(x+3);
		r4  = *(x+4);
		r5  = *(x+5);
		r6  = *(x+6);
		r7  = *(x+7);
		r8  = r0  + r4;
		r9  = r1  + r5;
		r10 = r2  + r6;
		r11 = r3  + r7;
		r12 = r12 + r8;
		r13 = r13 + r9;
		r14 = r14 + r10;
		r15 = r15 + r11;
		x+=8;
	}

	int64_t n_unroll_remain=(n%8);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		r12 = r12 + r0;
		x+=1;
	}
	return(r12 + r13 + r14 + r15);
}

// --------------------------------------------------------------------------------
int64_t sum_up_16_C_i8(int64_t x[], int64_t n){
    int64_t r0,r1,r2,r3;
    int64_t r4,r5,r6,r7;
    int64_t r8,r9,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp=0;

	int64_t n_unroll=(n/16);
	while( n_unroll-- ){ 
		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  = r0 + r1;
		r2  = r2 + r3;
		r4  = r4 + r5;
		r6  = r6 + r7;
		r8  = r8 + r9;
		r10 = r10 + r11;
		r12 = r12 + r13;
		r14 = r14 + r15;

		r0  = r0 + r2;
		r4  = r4 + r6;
		r8  = r8 + r10;
		r12 = r12 + r14;

		r0 = r0 + r4;
		r8 = r8 + r12;

		r0 = r0 + r8;

		tmp += r0;
		x+=16;
	}

	int64_t n_unroll_remain=(n%16);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);
}

double sum_up_16_C_r8(double x[], int64_t n){
    double r0,r1,r2,r3;
    double r4,r5,r6,r7;
    double r8,r9,r10,r11;
    double r12,r13,r14,r15;
    double tmp=0;

	double res=0e0;
	r15 = 0e0;
	int64_t n_unroll=(n/16);
	while( n_unroll-- ){ 
		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  += r1;
		r2  += r3;
		r4  += r5;
		r6  += r7;
		r8  += r9;
		r10 += r11;
		r12 += r13;
		r14 += r15;

		r0  += r2;
		r4  += r6;
		r8  += r10;
		r12 += r14;

		r0 += r4;
		r8 += r12;

		r0 += r8;

		tmp += r0;
		x+=16;
	}

	int64_t n_unroll_remain=(n%16);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);
}

// --------------------------------------------------------------------------------
int64_t sum_up_32_C_i8(int64_t x[], int64_t n){
    int64_t r0,r1,r2,r3;
    int64_t r4,r5,r6,r7;
    int64_t r8,r9,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp=0;

	int64_t n_unroll=(n/32);
	while( n_unroll-- ){ 
		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  += r1;
		r2  += r3;
		r4  += r5;
		r6  += r7;
		r8  += r9;
		r10 += r11;
		r12 += r13;
		r14 += r15;

		r0  += r2;
		r4  += r6;
		r8  += r10;
		r12 += r14;

		r0 += r4;
		r8 += r12;

		r0 += r8;

		tmp += r0;
		x+=16;

		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  += r1;
		r2  += r3;
		r4  += r5;
		r6  += r7;
		r8  += r9;
		r10 += r11;
		r12 += r13;
		r14 += r15;

		r0  += r2;
		r4  += r6;
		r8  += r10;
		r12 += r14;

		r0 += r4;
		r8 += r12;

		r0 += r8;

		tmp += r0;
		x+=16;
	}

	int64_t n_unroll_remain=(n%32);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);

}

double sum_up_32_C_r8(double x[], int64_t n){
    double r0,r1,r2,r3;
    double r4,r5,r6,r7;
    double r8,r9,r10,r11;
    double r12,r13,r14,r15;
    double tmp=0;

	int64_t n_unroll=(n/32);
	while( n_unroll-- ){ 
		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  += r1;
		r2  += r3;
		r4  += r5;
		r6  += r7;
		r8  += r9;
		r10 += r11;
		r12 += r13;
		r14 += r15;

		r0  += r2;
		r4  += r6;
		r8  += r10;
		r12 += r14;

		r0 += r4;
		r8 += r12;

		r0 += r8;

		tmp += r0;
		x+=16;

		r0   = *(x  );
		r1   = *(x+1);
		r2   = *(x+2);
		r3   = *(x+3);
		r4   = *(x+4);
		r5   = *(x+5);
		r6   = *(x+6);
		r7   = *(x+7);
		r8   = *(x+8);
		r9   = *(x+9);
		r10  = *(x+10);
		r11  = *(x+11);
		r12  = *(x+12);
		r13  = *(x+13);
		r14  = *(x+14);
		r15  = *(x+15);

		r0  += r1;
		r2  += r3;
		r4  += r5;
		r6  += r7;
		r8  += r9;
		r10 += r11;
		r12 += r13;
		r14 += r15;

		r0  += r2;
		r4  += r6;
		r8  += r10;
		r12 += r14;

		r0 += r4;
		r8 += r12;

		r0 += r8;

		tmp += r0;
		x+=16;
	}

	int64_t n_unroll_remain=(n%32);
	while( n_unroll_remain-- ){ 
		r0  = *(x  );
		tmp += r0;
		x+=1;
	}
	return(tmp);

}

// --------------------------------------------------------------------------------
int64_t sum_up_04_ASM_i8(int64_t x[], int64_t n){
	#if _x86_64
		if (n<4){
			return(sum_up_C_i8(x,n));
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

double sum_up_04_ASM_r8(double x[], int64_t n){
	#if _x86_64
		if (n<4){
			return(sum_up_C_r8(x,n));
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
	#endif
}

// --------------------------------------------------------------------------------
int64_t sum_up_08_ASM_i8(int64_t x[], int64_t n){
	#if _x86_64
		if (n<8){
			return(sum_up_C_i8(x,n));
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

double sum_up_08_ASM_r8(double x[], int64_t n){
	#if _x86_64
		if (n<8){
			return(sum_up_C_r8(x,n));
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
	#endif
}

// --------------------------------------------------------------------------------
int64_t sum_up_16_ASM_i8(int64_t x[], int64_t n){
	#if _x86_64
		if (n<16){
			return(sum_up_C_i8(x,n));
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

double sum_up_16_ASM_r8(double x[], int64_t n){
	#if _x86_64
		if (n<16){
			return(sum_up_C_r8(x,n));
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
	#endif
}

// --------------------------------------------------------------------------------
int64_t sum_up_32_ASM_i8(int64_t x[], int64_t n){
	#if _x86_64
		if (n<32){
			return(sum_up_C_i8(x,n));
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

double sum_up_32_ASM_r8(double x[], int64_t n){
	#if _x86_64
		if (n<32){
			return(sum_up_C_r8(x,n));
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
	#endif
}

// --------------------------------------------------------------------------------
int64_t sum_up_C_hybrid_i8(int64_t x[], int64_t n){
	int64_t res;
    if (n < 32){
		res = sum_up_16_C_i8(x,n);
	} else if (n < 10000000){
		res = sum_up_32_ASM_i8(x,n);
	} else {
		res = sum_up_04_ASM_i8(x,n);
	}
    return(res);
}

double sum_up_C_hybrid_r8(double x[], int64_t n){
	double res;
    if (n < 32){
		res = sum_up_16_C_r8(x,n);
	} else if (n < 10000000){
		res = sum_up_32_ASM_r8(x,n);
	} else {
		res = sum_up_04_ASM_r8(x,n);
	}
    return(res);
}

