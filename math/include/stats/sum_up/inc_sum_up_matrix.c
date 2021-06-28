#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double sum_up_04_ASM_r8(double x[], int64_t n);

double sum_up_C_hybrid_i8(int64_t x[], int64_t n);
double sum_up_C_hybrid_r8(double x[], int64_t n);

// --------------------------------------------------------------------------------
void sum_up_matrix_C_hybrid_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
	double tmp;
	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=sum_up_C_hybrid_r8(x, n);
		x += n;
		x_sum[jj]=tmp;
	}
}

void sum_up_matrix_C_hybrid_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj,j0;
	int64_t tmp;
	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=sum_up_C_hybrid_i8(x, n);
		x += n;
		x_sum[jj]=tmp;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
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

void sum_up_matrix_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
	int64_t tmp;
	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=0;
		for(ii=0; ii<n; ii++){
			tmp += x[ii+j0];
		}
		x_sum[jj]=tmp;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_02_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			r15 = r15 + r00;
			x+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

void sum_up_matrix_02_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    int64_t r00,r01,r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			r15 = r15 + r00;
			x+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r02,r03;
    double r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0e0;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

			r15 = r15 + r00;
			x+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            r15 = r15 + r00;
			x+=2;
		}

		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

void sum_up_matrix_04_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    int64_t r00,r01,r02,r03;
    int64_t r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0e0;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

			r15 = r15 + r00;
			x+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            r15 = r15 + r00;
			x+=2;
		}

		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0e0;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

			r15 = r15 + r00;
			x+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            r15 = r15 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            r15 = r15 + r00;
			x+=2;
		}

		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

void sum_up_matrix_08_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r15;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		r15=0e0;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

			r15 = r15 + r00;
			x+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            r15 = r15 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;
            
            r15 = r15 + r00;
			x+=2;
		}

		if(n_remain & 1){
			r15 += *(x);
			x+=1;
		}

		x_sum[jj]=r15;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_16_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=0e0;
		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp = tmp + r00;
			x+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp = tmp + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp = tmp + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp = tmp + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp += *(x);
			x+=1;
		}

		x_sum[jj]=tmp;
	}
}

void sum_up_matrix_16_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp=0;
		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp = tmp + r00;
			x+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp = tmp + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp = tmp + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp = tmp + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp += *(x);
			x+=1;
		}

		x_sum[jj]=tmp;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_32_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double r16,r17,r18,r19;
    double r20,r21,r22,r23;
    double r24,r25,r26,r27;
    double r28,r29,r30,r31;
    double tmp0, tmp1;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp0=0e0, tmp1=0e0;
		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);
            
			r16 = r16 + r17;
			r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;
			x+=32;
		}

        tmp0 = tmp0 + tmp1;

		int64_t n_remain=n%32;
		if(n_remain & 16){
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;
			x+=16;
		}

		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp0 = tmp0 + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp0 = tmp0 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp0 = tmp0 + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			x+=1;
		}

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_32_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double r16,r17,r18,r19;
    double r20,r21,r22,r23;
    double r24,r25,r26,r27;
    double r28,r29,r30,r31;
    double tmp0, tmp1;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp0=0, tmp1=0;
		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);
            
			r16 = r16 + r17;
			r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;
			x+=32;
		}

        tmp0 = tmp0 + tmp1;

		int64_t n_remain=n%32;
		if(n_remain & 16){
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;
			x+=16;
		}

		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp0 = tmp0 + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp0 = tmp0 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp0 = tmp0 + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			x+=1;
		}

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_64_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;
    double r16, r17, r18, r19;
    double r20, r21, r22, r23;
    double r24, r25, r26, r27;
    double r28, r29, r30, r31;
    double r32, r33, r34, r35;
    double r36, r37, r38, r39;
    double r40, r41, r42, r43;
    double r44, r45, r46, r47;
    double r48, r49, r50, r51;
    double r52, r53, r54, r55;
    double r56, r57, r58, r59;
    double r60, r61, r62, r63;
    double tmp0, tmp1, tmp2, tmp3;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp0=0e0, tmp1=0e0, tmp2=0e0, tmp3=0e0;
		int64_t n_unroll=(n>>6);
		while( n_unroll-- ){ 
            r00 = *(x+0);
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
            r14 = *(x+14);
            r15 = *(x+15);
            
            r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;
			tmp0 = tmp0 + r00;


            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);

            r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;
			tmp1 = tmp1 + r16;

            r32 = *(x+32);
            r33 = *(x+33);
            r34 = *(x+34);
            r35 = *(x+35);
            r36 = *(x+36);
            r37 = *(x+37);
            r38 = *(x+38);
            r39 = *(x+39);
            r40 = *(x+40);
            r41 = *(x+41);
            r42 = *(x+42);
            r43 = *(x+43);
            r44 = *(x+44);
            r45 = *(x+45);
            r46 = *(x+46);
            r47 = *(x+47);
 
            r32 = r32 + r33;
            r34 = r34 + r35;
            r36 = r36 + r37;
            r38 = r38 + r39;
            r40 = r40 + r41;
            r42 = r42 + r43;
            r44 = r44 + r45;
            r46 = r46 + r47;

            r32 = r32 + r34;
            r36 = r36 + r38;
            r40 = r40 + r42;
            r44 = r44 + r46;

            r32 = r32 + r36;
            r40 = r40 + r44;

            r32 = r32 + r40;
			tmp2 = tmp2 + r32;

            r48 = *(x+48);
            r49 = *(x+49);
            r50 = *(x+50);
            r51 = *(x+51);
            r52 = *(x+52);
            r53 = *(x+53);
            r54 = *(x+54);
            r55 = *(x+55);
            r56 = *(x+56);
            r57 = *(x+57);
            r58 = *(x+58);
            r59 = *(x+59);
            r60 = *(x+60);
            r61 = *(x+61);
            r62 = *(x+62);
            r63 = *(x+63);
 
            r48 = r48 + r49;
            r50 = r50 + r51;
            r52 = r52 + r53;
            r54 = r54 + r55;
            r56 = r56 + r57;
            r58 = r58 + r59;
            r60 = r60 + r61;
            r62 = r62 + r63;

            r48 = r48 + r50;
            r52 = r52 + r54;
            r56 = r56 + r58;
            r60 = r60 + r62;

            r48 = r48 + r52;
            r56 = r56 + r60;

            r48 = r48 + r56;
			tmp3 = tmp3 + r48;

			x+=64;
		}

        tmp0 = tmp0 + tmp1 + tmp2 + tmp3;

		int64_t n_remain=n%64;
		if(n_remain & 32){
            r00 = *(x+0);
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
            r14 = *(x+14);
            r15 = *(x+15);
            
            r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;
			tmp0 = tmp0 + r00;


            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);

            r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;
			tmp1 = tmp1 + r16;
            x+=32;

            tmp0 += tmp1;
		}

		if(n_remain & 16){
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;
			x+=16;
		}

		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp0 = tmp0 + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp0 = tmp0 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp0 = tmp0 + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			x+=1;
		}

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_64_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	int64_t ii, jj, j0;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06, r07;
    int64_t r08, r09, r10, r11;
    int64_t r12, r13, r14, r15;
    int64_t r16, r17, r18, r19;
    int64_t r20, r21, r22, r23;
    int64_t r24, r25, r26, r27;
    int64_t r28, r29, r30, r31;
    int64_t r32, r33, r34, r35;
    int64_t r36, r37, r38, r39;
    int64_t r40, r41, r42, r43;
    int64_t r44, r45, r46, r47;
    int64_t r48, r49, r50, r51;
    int64_t r52, r53, r54, r55;
    int64_t r56, r57, r58, r59;
    int64_t r60, r61, r62, r63;
    int64_t tmp0, tmp1, tmp2, tmp3;

	for(jj=0; jj<c; jj++){
		j0 = jj*n;
		tmp0=0, tmp1=0, tmp2=0, tmp3=0;
		int64_t n_unroll=(n>>6);
		while( n_unroll-- ){ 
            r00 = *(x+0);
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
            r14 = *(x+14);
            r15 = *(x+15);
            
            r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;
			tmp0 = tmp0 + r00;


            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);

            r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;
			tmp1 = tmp1 + r16;

            r32 = *(x+32);
            r33 = *(x+33);
            r34 = *(x+34);
            r35 = *(x+35);
            r36 = *(x+36);
            r37 = *(x+37);
            r38 = *(x+38);
            r39 = *(x+39);
            r40 = *(x+40);
            r41 = *(x+41);
            r42 = *(x+42);
            r43 = *(x+43);
            r44 = *(x+44);
            r45 = *(x+45);
            r46 = *(x+46);
            r47 = *(x+47);
 
            r32 = r32 + r33;
            r34 = r34 + r35;
            r36 = r36 + r37;
            r38 = r38 + r39;
            r40 = r40 + r41;
            r42 = r42 + r43;
            r44 = r44 + r45;
            r46 = r46 + r47;

            r32 = r32 + r34;
            r36 = r36 + r38;
            r40 = r40 + r42;
            r44 = r44 + r46;

            r32 = r32 + r36;
            r40 = r40 + r44;

            r32 = r32 + r40;
			tmp2 = tmp2 + r32;

            r48 = *(x+48);
            r49 = *(x+49);
            r50 = *(x+50);
            r51 = *(x+51);
            r52 = *(x+52);
            r53 = *(x+53);
            r54 = *(x+54);
            r55 = *(x+55);
            r56 = *(x+56);
            r57 = *(x+57);
            r58 = *(x+58);
            r59 = *(x+59);
            r60 = *(x+60);
            r61 = *(x+61);
            r62 = *(x+62);
            r63 = *(x+63);
 
            r48 = r48 + r49;
            r50 = r50 + r51;
            r52 = r52 + r53;
            r54 = r54 + r55;
            r56 = r56 + r57;
            r58 = r58 + r59;
            r60 = r60 + r61;
            r62 = r62 + r63;

            r48 = r48 + r50;
            r52 = r52 + r54;
            r56 = r56 + r58;
            r60 = r60 + r62;

            r48 = r48 + r52;
            r56 = r56 + r60;

            r48 = r48 + r56;
			tmp3 = tmp3 + r48;

			x+=64;
		}

        tmp0 = tmp0 + tmp1 + tmp2 + tmp3;

		int64_t n_remain=n%64;
		if(n_remain & 32){
            r00 = *(x+0);
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
            r14 = *(x+14);
            r15 = *(x+15);
            
            r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;
			tmp0 = tmp0 + r00;


            r16 = *(x+16);
            r17 = *(x+17);
            r18 = *(x+18);
            r19 = *(x+19);
            r20 = *(x+20);
            r21 = *(x+21);
            r22 = *(x+22);
            r23 = *(x+23);
            r24 = *(x+24);
            r25 = *(x+25);
            r26 = *(x+26);
            r27 = *(x+27);
            r28 = *(x+28);
            r29 = *(x+29);
            r30 = *(x+30);
            r31 = *(x+31);

            r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;
			tmp1 = tmp1 + r16;
            x+=32;

            tmp0 += tmp1;
		}

		if(n_remain & 16){
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;

            r00 = r00 + r08;

			tmp0 = tmp0 + r00;
			x+=16;
		}

		if(n_remain & 8){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
			r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;

            r00 = r00 + r04;

            tmp0 = tmp0 + r00;
			x+=8;
		}

		if(n_remain & 4){
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
			r02 = r02 + r03;

            r00 = r00 + r02;

            tmp0 = tmp0 + r00;
			x+=4;
		}

		if(n_remain & 2){
			r00 = *(x);
			r01 = *(x+1);

            r00 = r00 + r01;

            tmp0 = tmp0 + r00;
			x+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			x+=1;
		}

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_02_02_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<2 || c<2){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    double *x1;
    double r00,r01,r02,r03;
    double tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

			x+=2;
            x1+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			x+=1;
			x1+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;

		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x1  );
			r01 = *(x1+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            x1+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x1);
			x1+=1;
		}
        x_sum[jj+step]=tmp0;
    }
}

void sum_up_matrix_02_02_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<2 || c<2){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    int64_t *x1;
    int64_t r00,r01,r02,r03;
    int64_t tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0;
		tmp1=0;

		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

			x+=2;
            x1+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			x+=1;
			x1+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;

		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x1  );
			r01 = *(x1+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            x1+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x1);
			x1+=1;
		}
        x_sum[jj+step]=tmp0;
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_02_04_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<2 || c<4){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t ii, jj=0, step=c/4;
    double *x1, *x2, *x3;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double tmp0, tmp1, tmp2, tmp3;

    x1=x;
    x2=x;
    x3=x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
            // ----------------------------------
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            // ----------------------------------
			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

            // ----------------------------------
			r04 = *(x2  );
			r05 = *(x2+1);

			r04 = r04 + r05;

			tmp2 = tmp2 + r04;

            // ----------------------------------
			r06 = *(x3  );
			r07 = *(x3+1);

			r06 = r06 + r07;

			tmp3 = tmp3 + r06;

			x+=2;
            x1+=2;
            x2+=2;
            x3+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
        while(c_remain--){
            tmp0=0e0;

            int64_t n_unroll=(n>>1);
            while( n_unroll-- ){ 
                r00 = *(x3  );
                r01 = *(x3+1);

                r00 = r00 + r01;

                tmp0 = tmp0 + r00;

                x3+=2;
            }

            int64_t n_remain=n%2;
            if(n_remain & 1){
                tmp0 += *(x3);
                x3+=1;
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

void sum_up_matrix_02_04_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<2 || c<4){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t ii, jj=0, step=c/4;
    int64_t *x1, *x2, *x3;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t tmp0, tmp1, tmp2, tmp3;

    x1=x;
    x2=x;
    x3=x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
            // ----------------------------------
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            // ----------------------------------
			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

            // ----------------------------------
			r04 = *(x2  );
			r05 = *(x2+1);

			r04 = r04 + r05;

			tmp2 = tmp2 + r04;

            // ----------------------------------
			r06 = *(x3  );
			r07 = *(x3+1);

			r06 = r06 + r07;

			tmp3 = tmp3 + r06;

			x+=2;
            x1+=2;
            x2+=2;
            x3+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
        while(c_remain--){
            tmp0=0e0;

            int64_t n_unroll=(n>>1);
            while( n_unroll-- ){ 
                r00 = *(x3  );
                r01 = *(x3+1);

                r00 = r00 + r01;

                tmp0 = tmp0 + r00;

                x3+=2;
            }

            int64_t n_remain=n%2;
            if(n_remain & 1){
                tmp0 += *(x3);
                x3+=1;
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_02_08_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<2 || c<8){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t ii, jj=0, step=c/8;
    double *x1, *x2, *x3;
    double *x4, *x5, *x6, *x7;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp0, tmp1, tmp2, tmp3;
    double tmp5, tmp6, tmp7, tmp8;

    x1=x;
    x2=x;
    x3=x;
    x4=x;
    x5=x;
    x6=x;
    x7=x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    x4+=step*n*4;
    x5+=step*n*5;
    x6+=step*n*6;
    x7+=step*n*7;
    int64_t c_unroll=(c>>3);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		tmp5=0e0;
		tmp6=0e0;
		tmp7=0e0;
		tmp8=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
            // ----------------------------------
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            // ----------------------------------
			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

            // ----------------------------------
			r04 = *(x2  );
			r05 = *(x2+1);

			r04 = r04 + r05;

			tmp2 = tmp2 + r04;

            // ----------------------------------
			r06 = *(x3  );
			r07 = *(x3+1);

			r06 = r06 + r07;

			tmp3 = tmp3 + r06;

            // ----------------------------------
			r00 = *(x4  );
			r01 = *(x4+1);

			r00 = r00 + r01;

			tmp5 = tmp5 + r00;

            // ----------------------------------
			r02 = *(x5  );
			r03 = *(x5+1);

			r02 = r02 + r03;

			tmp6 = tmp6 + r02;

            // ----------------------------------
			r04 = *(x6  );
			r05 = *(x6+1);

			r04 = r04 + r05;

			tmp7 = tmp7 + r04;

            // ----------------------------------
			r06 = *(x7  );
			r07 = *(x7+1);

			r06 = r06 + r07;

			tmp8 = tmp8 + r06;

			x+=2;
            x1+=2;
            x2+=2;
            x3+=2;
            x4+=2;
            x5+=2;
            x6+=2;
            x7+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			tmp5 += *(x4);
			tmp6 += *(x5);
			tmp7 += *(x6);
			tmp8 += *(x7);
			x+=1;
            x1+=1;
            x2+=1;
            x3+=1;
            x4+=1;
            x5+=1;
            x6+=1;
            x7+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
		x_sum[jj+step*4]=tmp5;
		x_sum[jj+step*5]=tmp6;
		x_sum[jj+step*6]=tmp7;
		x_sum[jj+step*7]=tmp8;
        jj++;
	}

    int64_t c_remain=(c%8);
    if (c_remain>0){
        jj=jj+step*7;
        while(c_remain--){
            tmp0=0e0;

            int64_t n_unroll=(n>>1);
            while( n_unroll-- ){ 
                r00 = *(x7  );
                r01 = *(x7+1);

                r00 = r00 + r01;

                tmp0 = tmp0 + r00;

                x7+=2;
            }

            int64_t n_remain=n%2;
            if(n_remain & 1){
                tmp0 += *(x7);
                x7+=1;
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

void sum_up_matrix_02_08_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<2 || c<8){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t ii, jj=0, step=c/8;
    int64_t *x1, *x2, *x3;
    int64_t *x4, *x5, *x6, *x7;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp0, tmp1, tmp2, tmp3;
    int64_t tmp5, tmp6, tmp7, tmp8;

    x1=x;
    x2=x;
    x3=x;
    x4=x;
    x5=x;
    x6=x;
    x7=x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    x4+=step*n*4;
    x5+=step*n*5;
    x6+=step*n*6;
    x7+=step*n*7;
    int64_t c_unroll=(c>>3);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		tmp5=0e0;
		tmp6=0e0;
		tmp7=0e0;
		tmp8=0e0;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
            // ----------------------------------
			r00 = *(x  );
			r01 = *(x+1);

			r00 = r00 + r01;

			tmp0 = tmp0 + r00;

            // ----------------------------------
			r02 = *(x1  );
			r03 = *(x1+1);

			r02 = r02 + r03;

			tmp1 = tmp1 + r02;

            // ----------------------------------
			r04 = *(x2  );
			r05 = *(x2+1);

			r04 = r04 + r05;

			tmp2 = tmp2 + r04;

            // ----------------------------------
			r06 = *(x3  );
			r07 = *(x3+1);

			r06 = r06 + r07;

			tmp3 = tmp3 + r06;

            // ----------------------------------
			r00 = *(x4  );
			r01 = *(x4+1);

			r00 = r00 + r01;

			tmp5 = tmp5 + r00;

            // ----------------------------------
			r02 = *(x5  );
			r03 = *(x5+1);

			r02 = r02 + r03;

			tmp6 = tmp6 + r02;

            // ----------------------------------
			r04 = *(x6  );
			r05 = *(x6+1);

			r04 = r04 + r05;

			tmp7 = tmp7 + r04;

            // ----------------------------------
			r06 = *(x7  );
			r07 = *(x7+1);

			r06 = r06 + r07;

			tmp8 = tmp8 + r06;

			x+=2;
            x1+=2;
            x2+=2;
            x3+=2;
            x4+=2;
            x5+=2;
            x6+=2;
            x7+=2;
		}

		int64_t n_remain=n%2;
		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			tmp5 += *(x4);
			tmp6 += *(x5);
			tmp7 += *(x6);
			tmp8 += *(x7);
			x+=1;
            x1+=1;
            x2+=1;
            x3+=1;
            x4+=1;
            x5+=1;
            x6+=1;
            x7+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
		x_sum[jj+step*4]=tmp5;
		x_sum[jj+step*5]=tmp6;
		x_sum[jj+step*6]=tmp7;
		x_sum[jj+step*7]=tmp8;
        jj++;
	}

    int64_t c_remain=(c%8);
    if (c_remain>0){
        jj=jj+step*7;
        while(c_remain--){
            tmp0=0e0;

            int64_t n_unroll=(n>>1);
            while( n_unroll-- ){ 
                r00 = *(x7  );
                r01 = *(x7+1);

                r00 = r00 + r01;

                tmp0 = tmp0 + r00;

                x7+=2;
            }

            int64_t n_remain=n%2;
            if(n_remain & 1){
                tmp0 += *(x7);
                x7+=1;
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_02_C_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_C_r8(x_sum, x0, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    double *x1;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double tmp0, tmp1;

    x1 = x0;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			r00 = *(x0  );
			r01 = *(x0+1);
			r02 = *(x0+2);
			r03 = *(x0+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

			x0+=4;
            x1+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			tmp0 += *(x0);
			tmp1 += *(x1);
			tmp0 += *(x0+1);
			tmp1 += *(x1+1);
			x0+=2;
			x1+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x0);
			tmp1 += *(x1);
			x0+=1;
			x1+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
		jj=jj+step;
		int64_t n_unroll=(n>>1);
		while( n_unroll-- ){ 
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            x1+=4;
		}

		int64_t n_remain=n%2;
		if(n_remain & 2){
			tmp0 += *(x1);
			tmp0 += *(x1+1);
			x1+=2;
		}
		if(n_remain & 1){
			tmp0 += *(x1);
			x1+=1;
		}
        x_sum[jj]=tmp0;
    }
}

void sum_up_matrix_04_02_C_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_C_i8(x_sum, x0, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    int64_t *x1;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t tmp0, tmp1;

    x1 = x0;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0;
		tmp1=0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			r00 = *(x0  );
			r01 = *(x0+1);
			r02 = *(x0+2);
			r03 = *(x0+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

			x0+=4;
            x1+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			tmp0 += *(x0);
			tmp1 += *(x1);
			tmp0 += *(x0+1);
			tmp1 += *(x1+1);
			x0+=2;
			x1+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x0);
			tmp1 += *(x1);
			x0+=1;
			x1+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0;
		jj=jj+step;
		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            x1+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			tmp0 += *(x1);
			tmp0 += *(x1+1);
			x1+=2;
		}
		if(n_remain & 1){
			tmp0 += *(x1);
			x1+=1;
		}
        x_sum[jj]=tmp0;
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_04_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    double *x1, *x2, *x3;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
            // -----------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            // -----------------------------------
			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

            // -----------------------------------
			r08 = *(x2  );
			r09 = *(x2+1);
			r10 = *(x2+2);
			r11 = *(x2+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp2 = tmp2 + r08;

            // -----------------------------------
			r12 = *(x3  );
			r13 = *(x3+1);
			r14 = *(x3+2);
			r15 = *(x3+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp3 = tmp3 + r12;

			x+=4;
            x1+=4;
            x2+=4;
            x3+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			tmp0 += *(x+1);
			tmp1 += *(x1+1);
			tmp2 += *(x2+1);
			tmp3 += *(x3+1);
			x+=2;
			x1+=2;
			x2+=2;
			x3+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
		jj=jj+step*3;
		while (c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>2);
			while( n_unroll-- ){ 
				r00 = *(x3  );
				r01 = *(x3+1);
				r02 = *(x3+2);
				r03 = *(x3+3);

				r00 = r00 + r01;
				r02 = r02 + r03;

				r00 = r00 + r02;

				tmp0 = tmp0 + r00;

				x3+=4;
			}

			int64_t n_remain=n%4;
			if(n_remain & 2){
				tmp0 += *(x3);
				tmp0 += *(x3+1);
				x3+=2;
			}
			if(n_remain & 1){
				tmp0 += *(x3);
				x3+=1;
			}
			x_sum[jj]=tmp0;
			jj++;
		}
    }
}

void sum_up_matrix_04_04_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<4 || c<2){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    int64_t *x1, *x2, *x3;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
            // -----------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            // -----------------------------------
			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

            // -----------------------------------
			r08 = *(x2  );
			r09 = *(x2+1);
			r10 = *(x2+2);
			r11 = *(x2+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp2 = tmp2 + r08;

            // -----------------------------------
			r12 = *(x3  );
			r13 = *(x3+1);
			r14 = *(x3+2);
			r15 = *(x3+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp3 = tmp3 + r12;

			x+=4;
            x1+=4;
            x2+=4;
            x3+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp0 += *(x+1);
			tmp1 += *(x1+1);
			tmp0 += *(x+2);
			tmp1 += *(x1+2);
			tmp0 += *(x+3);
			tmp1 += *(x1+3);
			x+=2;
			x1+=2;
			x2+=2;
			x3+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
		jj=jj+step*3;
		while (c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>2);
			while( n_unroll-- ){ 
				r00 = *(x3  );
				r01 = *(x3+1);
				r02 = *(x3+2);
				r03 = *(x3+3);

				r00 = r00 + r01;
				r02 = r02 + r03;

				r00 = r00 + r02;

				tmp0 = tmp0 + r00;

				x3+=4;
			}

			int64_t n_remain=n%4;
			if(n_remain & 2){
				tmp0 += *(x3);
				tmp0 += *(x3+1);
				x3+=2;
			}
			if(n_remain & 1){
				tmp0 += *(x3);
				x3+=1;
			}
			x_sum[jj]=tmp0;
			jj++;
		}
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_08_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<4 || c<8){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/8;
    double *x1, *x2, *x3;
    double *x4, *x5, *x6, *x7;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp0, tmp1, tmp2, tmp3;
    double tmp4, tmp5, tmp6, tmp7;

    x1 = x;
    x2 = x;
    x3 = x;
    x4 = x;
    x5 = x;
    x6 = x;
    x7 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    x4+=step*n*4;
    x5+=step*n*5;
    x6+=step*n*6;
    x7+=step*n*7;

    int64_t c_unroll=(c>>3);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		tmp4=0e0;
		tmp5=0e0;
		tmp6=0e0;
		tmp7=0e0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
            // -----------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            // -----------------------------------
			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

            // -----------------------------------
			r08 = *(x2  );
			r09 = *(x2+1);
			r10 = *(x2+2);
			r11 = *(x2+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp2 = tmp2 + r08;

            // -----------------------------------
			r12 = *(x3  );
			r13 = *(x3+1);
			r14 = *(x3+2);
			r15 = *(x3+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp3 = tmp3 + r12;

            // -----------------------------------
			r00 = *(x4  );
			r01 = *(x4+1);
			r02 = *(x4+2);
			r03 = *(x4+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp4 = tmp4 + r00;

            // -----------------------------------
			r04 = *(x5  );
			r05 = *(x5+1);
			r06 = *(x5+2);
			r07 = *(x5+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp5 = tmp5 + r04;

            // -----------------------------------
			r08 = *(x6  );
			r09 = *(x6+1);
			r10 = *(x6+2);
			r11 = *(x6+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp6 = tmp6 + r08;

            // -----------------------------------
			r12 = *(x7  );
			r13 = *(x7+1);
			r14 = *(x7+2);
			r15 = *(x7+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp7 = tmp7 + r12;

			x+=4;
            x1+=4;
            x2+=4;
            x3+=4;
            x4+=4;
            x5+=4;
            x6+=4;
            x7+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
            // -----------------------------
			tmp0 += *(x);
            tmp0 += *(x+1);
            // -----------------------------
			tmp1 += *(x1);
            tmp1 += *(x1+1);
            // -----------------------------
			tmp2 += *(x2);
            tmp2 += *(x2+1);
            // -----------------------------
			tmp3 += *(x3);
            tmp3 += *(x3+1);
            // -----------------------------
			tmp4 += *(x4);
            tmp4 += *(x4+1);
            // -----------------------------
			tmp5 += *(x5);
            tmp5 += *(x5+1);
            // -----------------------------
			tmp6 += *(x6);
            tmp6 += *(x6+1);
            // -----------------------------
			tmp7 += *(x7);
            tmp7 += *(x7+1);
			x+=2;
			x1+=2;
			x2+=2;
			x3+=2;
			x4+=2;
			x5+=2;
			x6+=2;
			x7+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			tmp4 += *(x4);
			tmp5 += *(x5);
			tmp6 += *(x6);
			tmp7 += *(x7);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
			x4+=1;
			x5+=1;
			x6+=1;
			x7+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
		x_sum[jj+step*4]=tmp4;
		x_sum[jj+step*5]=tmp5;
		x_sum[jj+step*6]=tmp6;
		x_sum[jj+step*7]=tmp7;
        jj++;
	}

    int64_t c_remain=(c%8);
    if (c_remain>0){
		jj=jj+step*7;
		while(c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>1);
			while( n_unroll-- ){ 
				r00 = *(x7  );
				r01 = *(x7+1);
				r02 = *(x7+2);
				r03 = *(x7+3);

				r00 = r00 + r01;
				r02 = r02 + r03;

				r00 = r00 + r02;

				tmp0 = tmp0 + r00;

				x7+=4;
			}

			int64_t n_remain=n%2;
			if(n_remain & 2){
				tmp0 += *(x7);
				tmp0 += *(x7+1);
				x7+=2;
			}
			if(n_remain & 1){
				tmp0 += *(x7);
				x7+=1;
			}
			x_sum[jj]=tmp0;
			jj++;
		}
    }
}

void sum_up_matrix_04_08_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<4 || c<8){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/8;
    int64_t *x1, *x2, *x3;
    int64_t *x4, *x5, *x6, *x7;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp0, tmp1, tmp2, tmp3;
    int64_t tmp4, tmp5, tmp6, tmp7;

    x1 = x;
    x2 = x;
    x3 = x;
    x4 = x;
    x5 = x;
    x6 = x;
    x7 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;
    x4+=step*n*4;
    x5+=step*n*5;
    x6+=step*n*6;
    x7+=step*n*7;

    int64_t c_unroll=(c>>3);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;
		tmp4=0e0;
		tmp5=0e0;
		tmp6=0e0;
		tmp7=0e0;

		int64_t n_unroll=(n>>2);
		while( n_unroll-- ){ 
            // -----------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp0 = tmp0 + r00;

            // -----------------------------------
			r04 = *(x1  );
			r05 = *(x1+1);
			r06 = *(x1+2);
			r07 = *(x1+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp1 = tmp1 + r04;

            // -----------------------------------
			r08 = *(x2  );
			r09 = *(x2+1);
			r10 = *(x2+2);
			r11 = *(x2+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp2 = tmp2 + r08;

            // -----------------------------------
			r12 = *(x3  );
			r13 = *(x3+1);
			r14 = *(x3+2);
			r15 = *(x3+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp3 = tmp3 + r12;

            // -----------------------------------
			r00 = *(x4  );
			r01 = *(x4+1);
			r02 = *(x4+2);
			r03 = *(x4+3);

			r00 = r00 + r01;
            r02 = r02 + r03;

            r00 = r00 + r02;

			tmp4 = tmp4 + r00;

            // -----------------------------------
			r04 = *(x5  );
			r05 = *(x5+1);
			r06 = *(x5+2);
			r07 = *(x5+3);

			r04 = r04 + r05;
            r06 = r06 + r07;

            r04 = r04 + r06;

			tmp5 = tmp5 + r04;

            // -----------------------------------
			r08 = *(x6  );
			r09 = *(x6+1);
			r10 = *(x6+2);
			r11 = *(x6+3);

			r08 = r08 + r09;
            r10 = r10 + r11;

            r08 = r08 + r10;

			tmp6 = tmp6 + r08;

            // -----------------------------------
			r12 = *(x7  );
			r13 = *(x7+1);
			r14 = *(x7+2);
			r15 = *(x7+3);

			r12 = r12 + r13;
            r14 = r14 + r15;

            r12 = r12 + r14;

			tmp7 = tmp7 + r12;

			x+=4;
            x1+=4;
            x2+=4;
            x3+=4;
            x4+=4;
            x5+=4;
            x6+=4;
            x7+=4;
		}

		int64_t n_remain=n%4;
		if(n_remain & 2){
            // -----------------------------
			tmp0 += *(x);
            tmp0 += *(x+1);
            // -----------------------------
			tmp1 += *(x1);
            tmp1 += *(x1+1);
            // -----------------------------
			tmp2 += *(x2);
            tmp2 += *(x2+1);
            // -----------------------------
			tmp3 += *(x3);
            tmp3 += *(x3+1);
            // -----------------------------
			tmp4 += *(x4);
            tmp4 += *(x4+1);
            // -----------------------------
			tmp5 += *(x5);
            tmp5 += *(x5+1);
            // -----------------------------
			tmp6 += *(x6);
            tmp6 += *(x6+1);
            // -----------------------------
			tmp7 += *(x7);
            tmp7 += *(x7+1);
			x+=2;
			x1+=2;
			x2+=2;
			x3+=2;
			x4+=2;
			x5+=2;
			x6+=2;
			x7+=2;
		}

		if(n_remain & 1){
			tmp0 += *(x);
			tmp1 += *(x1);
			tmp2 += *(x2);
			tmp3 += *(x3);
			tmp4 += *(x4);
			tmp5 += *(x5);
			tmp6 += *(x6);
			tmp7 += *(x7);
			x+=1;
			x1+=1;
			x2+=1;
			x3+=1;
			x4+=1;
			x5+=1;
			x6+=1;
			x7+=1;
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
		x_sum[jj+step*4]=tmp4;
		x_sum[jj+step*5]=tmp5;
		x_sum[jj+step*6]=tmp6;
		x_sum[jj+step*7]=tmp7;
        jj++;
	}

    int64_t c_remain=(c%8);
    if (c_remain>0){
		jj=jj+step*7;
		while(c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>2);
			while( n_unroll-- ){ 
				r00 = *(x7  );
				r01 = *(x7+1);
				r02 = *(x7+2);
				r03 = *(x7+3);

				r00 = r00 + r01;
				r02 = r02 + r03;

				r00 = r00 + r02;

				tmp0 = tmp0 + r00;

				x7+=4;
			}

			int64_t n_remain=n%4;
			if(n_remain & 2){
				tmp0 += *(x7);
				tmp0 += *(x7+1);
				x7+=2;
			}
			if(n_remain & 1){
				tmp0 += *(x7);
				x7+=1;
			}
			x_sum[jj]=tmp0;
			jj++;
		}
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_02_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    double *x1;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r08 = *(x1  );
			r09 = *(x1+1);
			r10 = *(x1+2);
			r11 = *(x1+3);
			r12 = *(x1+4);
			r13 = *(x1+5);
			r14 = *(x1+6);
			r15 = *(x1+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp1 = tmp1 + r08;

			x+=8;
            x1+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            x1+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

void sum_up_matrix_08_02_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    int64_t *x1;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r08 = *(x1  );
			r09 = *(x1+1);
			r10 = *(x1+2);
			r11 = *(x1+3);
			r12 = *(x1+4);
			r13 = *(x1+5);
			r14 = *(x1+6);
			r15 = *(x1+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp1 = tmp1 + r08;

			x+=8;
            x1+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            x1+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_04_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<8 || c<4){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    double *x1, *x2, *x3;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r08 = *(x1  );
			r09 = *(x1+1);
			r10 = *(x1+2);
			r11 = *(x1+3);
			r12 = *(x1+4);
			r13 = *(x1+5);
			r14 = *(x1+6);
			r15 = *(x1+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp1 = tmp1 + r08;

            // --------------------------------------
			r00 = *(x2  );
			r01 = *(x2+1);
			r02 = *(x2+2);
			r03 = *(x2+3);
			r04 = *(x2+4);
			r05 = *(x2+5);
			r06 = *(x2+6);
			r07 = *(x2+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp2 = tmp2 + r00;

            // --------------------------------------
			r08 = *(x3  );
			r09 = *(x3+1);
			r10 = *(x3+2);
			r11 = *(x3+3);
			r12 = *(x3+4);
			r13 = *(x3+5);
			r14 = *(x3+6);
			r15 = *(x3+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp3 = tmp3 + r08;

			x+=8;
            x1+=8;
            x2+=8;
            x3+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                tmp2 += *(x2);
                tmp3 += *(x3);
                x+=1;
                x1+=1;
                x2+=1;
                x3+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
        while(c_remain--){
            tmp0=0e0;
            int64_t n_unroll=(n>>3);
            while( n_unroll-- ){ 
                // --------------------------------------
                r00 = *(x3  );
                r01 = *(x3+1);
                r02 = *(x3+2);
                r03 = *(x3+3);
                r04 = *(x3+4);
                r05 = *(x3+5);
                r06 = *(x3+6);
                r07 = *(x3+7);

                r00 = r00 + r01;
                r02 = r02 + r03;
                r04 = r04 + r05;
                r06 = r06 + r07;

                r00 = r00 + r02;
                r04 = r04 + r06;
                
                r00 = r00 + r04;

                tmp0 = tmp0 + r00;

                x3+=8;
            }

            int64_t n_remain=n%8;
            if(n_remain > 0){
                while(n_remain--){
                    tmp0 += *(x3);
                    x3+=1;
                }
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

void sum_up_matrix_08_04_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<8 || c<4){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    int64_t *x1, *x2, *x3;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>3);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
			r01 = *(x+1);
			r02 = *(x+2);
			r03 = *(x+3);
			r04 = *(x+4);
			r05 = *(x+5);
			r06 = *(x+6);
			r07 = *(x+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r08 = *(x1  );
			r09 = *(x1+1);
			r10 = *(x1+2);
			r11 = *(x1+3);
			r12 = *(x1+4);
			r13 = *(x1+5);
			r14 = *(x1+6);
			r15 = *(x1+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp1 = tmp1 + r08;

            // --------------------------------------
			r00 = *(x2  );
			r01 = *(x2+1);
			r02 = *(x2+2);
			r03 = *(x2+3);
			r04 = *(x2+4);
			r05 = *(x2+5);
			r06 = *(x2+6);
			r07 = *(x2+7);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;

            r00 = r00 + r02;
            r04 = r04 + r06;
            
            r00 = r00 + r04;

			tmp2 = tmp2 + r00;

            // --------------------------------------
			r08 = *(x3  );
			r09 = *(x3+1);
			r10 = *(x3+2);
			r11 = *(x3+3);
			r12 = *(x3+4);
			r13 = *(x3+5);
			r14 = *(x3+6);
			r15 = *(x3+7);

			r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r08 = r08 + r10;
            r12 = r12 + r14;

            r08 = r08 + r12;

			tmp3 = tmp3 + r08;

			x+=8;
            x1+=8;
            x2+=8;
            x3+=8;
		}

		int64_t n_remain=n%8;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                tmp2 += *(x2);
                tmp3 += *(x3);
                x+=1;
                x1+=1;
                x2+=1;
                x3+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
        while(c_remain--){
            tmp0=0e0;
            int64_t n_unroll=(n>>3);
            while( n_unroll-- ){ 
                // --------------------------------------
                r00 = *(x3  );
                r01 = *(x3+1);
                r02 = *(x3+2);
                r03 = *(x3+3);
                r04 = *(x3+4);
                r05 = *(x3+5);
                r06 = *(x3+6);
                r07 = *(x3+7);

                r00 = r00 + r01;
                r02 = r02 + r03;
                r04 = r04 + r05;
                r06 = r06 + r07;

                r00 = r00 + r02;
                r04 = r04 + r06;
                
                r00 = r00 + r04;

                tmp0 = tmp0 + r00;

                x3+=8;
            }

            int64_t n_remain=n%8;
            if(n_remain > 0){
                while(n_remain--){
                    tmp0 += *(x3);
                    x3+=1;
                }
            }
            x_sum[jj]=tmp0;
            jj++;
        }
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_16_02_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<16 || c<2){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    double *x1;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double r16,r17,r18,r19;
    double r20,r21,r22,r23;
    double r24,r25,r26,r27;
    double r28,r29,r30,r31;
    double tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r16 = *(x1  );
			r17 = *(x1+1);
			r18 = *(x1+2);
			r19 = *(x1+3);
			r20 = *(x1+4);
			r21 = *(x1+5);
			r22 = *(x1+6);
			r23 = *(x1+7);
			r24 = *(x1+8);
			r25 = *(x1+9);
			r26 = *(x1+10);
			r27 = *(x1+11);
			r28 = *(x1+12);
			r29 = *(x1+13);
			r30 = *(x1+14);
			r31 = *(x1+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;

			x+=16;
            x1+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            x1+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

void sum_up_matrix_16_02_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<8 || c<2){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    int64_t *x1;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t r16,r17,r18,r19;
    int64_t r20,r21,r22,r23;
    int64_t r24,r25,r26,r27;
    int64_t r28,r29,r30,r31;
    int64_t tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r16 = *(x1  );
			r17 = *(x1+1);
			r18 = *(x1+2);
			r19 = *(x1+3);
			r20 = *(x1+4);
			r21 = *(x1+5);
			r22 = *(x1+6);
			r23 = *(x1+7);
			r24 = *(x1+8);
			r25 = *(x1+9);
			r26 = *(x1+10);
			r27 = *(x1+11);
			r28 = *(x1+12);
			r29 = *(x1+13);
			r30 = *(x1+14);
			r31 = *(x1+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;

			x+=16;
            x1+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            x1+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_16_04_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<16 || c<4){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    double *x1, *x2, *x3;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double r16,r17,r18,r19;
    double r20,r21,r22,r23;
    double r24,r25,r26,r27;
    double r28,r29,r30,r31;
    double tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r16 = *(x1  );
			r17 = *(x1+1);
			r18 = *(x1+2);
			r19 = *(x1+3);
			r20 = *(x1+4);
			r21 = *(x1+5);
			r22 = *(x1+6);
			r23 = *(x1+7);
			r24 = *(x1+8);
			r25 = *(x1+9);
			r26 = *(x1+10);
			r27 = *(x1+11);
			r28 = *(x1+12);
			r29 = *(x1+13);
			r30 = *(x1+14);
			r31 = *(x1+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;
            // --------------------------------------
			r00 = *(x2  );
			r01 = *(x2+1);
			r02 = *(x2+2);
			r03 = *(x2+3);
			r04 = *(x2+4);
			r05 = *(x2+5);
			r06 = *(x2+6);
			r07 = *(x2+7);
			r08 = *(x2+8);
			r09 = *(x2+9);
			r10 = *(x2+10);
			r11 = *(x2+11);
			r12 = *(x2+12);
			r13 = *(x2+13);
			r14 = *(x2+14);
			r15 = *(x2+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp2 = tmp2 + r00;

            // --------------------------------------
			r16 = *(x3  );
			r17 = *(x3+1);
			r18 = *(x3+2);
			r19 = *(x3+3);
			r20 = *(x3+4);
			r21 = *(x3+5);
			r22 = *(x3+6);
			r23 = *(x3+7);
			r24 = *(x3+8);
			r25 = *(x3+9);
			r26 = *(x3+10);
			r27 = *(x3+11);
			r28 = *(x3+12);
			r29 = *(x3+13);
			r30 = *(x3+14);
			r31 = *(x3+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp3 = tmp3 + r16;

			x+=16;
            x1+=16;
            x2+=16;
            x3+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                tmp2 += *(x2);
                tmp3 += *(x3);
                x+=1;
                x1+=1;
                x2+=1;
                x3+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
		while(c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>4);
			while( n_unroll-- ){ 
				// --------------------------------------
				r00 = *(x3  );
				r01 = *(x3+1);
				r02 = *(x3+2);
				r03 = *(x3+3);
				r04 = *(x3+4);
				r05 = *(x3+5);
				r06 = *(x3+6);
				r07 = *(x3+7);
				r08 = *(x3+8);
				r09 = *(x3+9);
				r10 = *(x3+10);
				r11 = *(x3+11);
				r12 = *(x3+12);
				r13 = *(x3+13);
				r14 = *(x3+14);
				r15 = *(x3+15);

				r00 = r00 + r01;
				r02 = r02 + r03;
				r04 = r04 + r05;
				r06 = r06 + r07;
				r08 = r08 + r09;
				r10 = r10 + r11;
				r12 = r12 + r13;
				r14 = r14 + r15;

				r00 = r00 + r02;
				r04 = r04 + r06;
				r08 = r08 + r10;
				r12 = r12 + r14;

				r00 = r00 + r04;
				r08 = r08 + r12;
				
				r00 = r00 + r08;

				tmp0 = tmp0 + r00;

				x3+=16;
			}

			int64_t n_remain=n%16;
			if(n_remain > 0){
				while(n_remain--){
					tmp0 += *(x3);
					x+=1;
					x3+=1;
				}
			}

			x_sum[jj]=tmp0;
			jj++;
		}
    }
}

void sum_up_matrix_16_04_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<16 || c<4){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/4;
    int64_t *x1, *x2, *x3;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t r16,r17,r18,r19;
    int64_t r20,r21,r22,r23;
    int64_t r24,r25,r26,r27;
    int64_t r28,r29,r30,r31;
    int64_t tmp0, tmp1, tmp2, tmp3;

    x1 = x;
    x2 = x;
    x3 = x;
    x1+=step*n;
    x2+=step*n*2;
    x3+=step*n*3;

    int64_t c_unroll=(c>>2);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;
		tmp2=0e0;
		tmp3=0e0;

		int64_t n_unroll=(n>>4);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r16 = *(x1  );
			r17 = *(x1+1);
			r18 = *(x1+2);
			r19 = *(x1+3);
			r20 = *(x1+4);
			r21 = *(x1+5);
			r22 = *(x1+6);
			r23 = *(x1+7);
			r24 = *(x1+8);
			r25 = *(x1+9);
			r26 = *(x1+10);
			r27 = *(x1+11);
			r28 = *(x1+12);
			r29 = *(x1+13);
			r30 = *(x1+14);
			r31 = *(x1+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp1 = tmp1 + r16;
            // --------------------------------------
			r00 = *(x2  );
			r01 = *(x2+1);
			r02 = *(x2+2);
			r03 = *(x2+3);
			r04 = *(x2+4);
			r05 = *(x2+5);
			r06 = *(x2+6);
			r07 = *(x2+7);
			r08 = *(x2+8);
			r09 = *(x2+9);
			r10 = *(x2+10);
			r11 = *(x2+11);
			r12 = *(x2+12);
			r13 = *(x2+13);
			r14 = *(x2+14);
			r15 = *(x2+15);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;

            r00 = r00 + r04;
            r08 = r08 + r12;
            
            r00 = r00 + r08;

			tmp2 = tmp2 + r00;

            // --------------------------------------
			r16 = *(x3  );
			r17 = *(x3+1);
			r18 = *(x3+2);
			r19 = *(x3+3);
			r20 = *(x3+4);
			r21 = *(x3+5);
			r22 = *(x3+6);
			r23 = *(x3+7);
			r24 = *(x3+8);
			r25 = *(x3+9);
			r26 = *(x3+10);
			r27 = *(x3+11);
			r28 = *(x3+12);
			r29 = *(x3+13);
			r30 = *(x3+14);
			r31 = *(x3+15);

			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r16 = r16 + r20;
            r24 = r24 + r28;

            r16 = r16 + r24;

			tmp3 = tmp3 + r16;

			x+=16;
            x1+=16;
            x2+=16;
            x3+=16;
		}

		int64_t n_remain=n%16;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                tmp2 += *(x2);
                tmp3 += *(x3);
                x+=1;
                x1+=1;
                x2+=1;
                x3+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
		x_sum[jj+step*2]=tmp2;
		x_sum[jj+step*3]=tmp3;
        jj++;
	}

    int64_t c_remain=(c%4);
    if (c_remain>0){
        jj=jj+step*3;
		while(c_remain--){
			tmp0=0e0;
			int64_t n_unroll=(n>>4);
			while( n_unroll-- ){ 
				// --------------------------------------
				r00 = *(x3  );
				r01 = *(x3+1);
				r02 = *(x3+2);
				r03 = *(x3+3);
				r04 = *(x3+4);
				r05 = *(x3+5);
				r06 = *(x3+6);
				r07 = *(x3+7);
				r08 = *(x3+8);
				r09 = *(x3+9);
				r10 = *(x3+10);
				r11 = *(x3+11);
				r12 = *(x3+12);
				r13 = *(x3+13);
				r14 = *(x3+14);
				r15 = *(x3+15);

				r00 = r00 + r01;
				r02 = r02 + r03;
				r04 = r04 + r05;
				r06 = r06 + r07;
				r08 = r08 + r09;
				r10 = r10 + r11;
				r12 = r12 + r13;
				r14 = r14 + r15;

				r00 = r00 + r02;
				r04 = r04 + r06;
				r08 = r08 + r10;
				r12 = r12 + r14;

				r00 = r00 + r04;
				r08 = r08 + r12;
				
				r00 = r00 + r08;

				tmp0 = tmp0 + r00;

				x3+=16;
			}

			int64_t n_remain=n%16;
			if(n_remain > 0){
				while(n_remain--){
					tmp0 += *(x3);
					x+=1;
					x3+=1;
				}
			}

			x_sum[jj]=tmp0;
			jj++;
		}
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_32_02_C_r8(double *x_sum, double *x, int64_t n, int64_t c){
	if (n<32 || c<2){
		sum_up_matrix_C_r8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    double *x1;
    double r00,r01,r02,r03;
    double r04,r05,r06,r07;
    double r08,r09,r10,r11;
    double r12,r13,r14,r15;
    double r16,r17,r18,r19;
    double r20,r21,r22,r23;
    double r24,r25,r26,r27;
    double r28,r29,r30,r31;
    double tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);
			r16 = *(x+16);
			r17 = *(x+17);
			r18 = *(x+18);
			r19 = *(x+19);
			r20 = *(x+20);
			r21 = *(x+21);
			r22 = *(x+22);
			r23 = *(x+23);
			r24 = *(x+24);
			r25 = *(x+25);
			r26 = *(x+26);
			r27 = *(x+27);
			r28 = *(x+28);
			r29 = *(x+29);
			r30 = *(x+30);
			r31 = *(x+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);
			r16 = *(x1+16);
			r17 = *(x1+17);
			r18 = *(x1+18);
			r19 = *(x1+19);
			r20 = *(x1+20);
			r21 = *(x1+21);
			r22 = *(x1+22);
			r23 = *(x1+23);
			r24 = *(x1+24);
			r25 = *(x1+25);
			r26 = *(x1+26);
			r27 = *(x1+27);
			r28 = *(x1+28);
			r29 = *(x1+29);
			r30 = *(x1+30);
			r31 = *(x1+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp1 = tmp1 + r00;

			x+=32;
            x1+=32;
		}

		int64_t n_remain=n%32;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);
			r16 = *(x1+16);
			r17 = *(x1+17);
			r18 = *(x1+18);
			r19 = *(x1+19);
			r20 = *(x1+20);
			r21 = *(x1+21);
			r22 = *(x1+22);
			r23 = *(x1+23);
			r24 = *(x1+24);
			r25 = *(x1+25);
			r26 = *(x1+26);
			r27 = *(x1+27);
			r28 = *(x1+28);
			r29 = *(x1+29);
			r30 = *(x1+30);
			r31 = *(x1+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp0 = tmp0 + r00;


            x1+=32;
		}

		int64_t n_remain=n%32;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

void sum_up_matrix_32_02_C_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	if (n<32 || c<2){
		sum_up_matrix_C_i8(x_sum, x, n, c);
		return;
	}

	int64_t jj=0, step=c/2;
    int64_t *x1;
    int64_t r00,r01,r02,r03;
    int64_t r04,r05,r06,r07;
    int64_t r08,r09,r10,r11;
    int64_t r12,r13,r14,r15;
    int64_t r16,r17,r18,r19;
    int64_t r20,r21,r22,r23;
    int64_t r24,r25,r26,r27;
    int64_t r28,r29,r30,r31;
    int64_t tmp0, tmp1;

    x1 = x;
    x1+=step*n;

    int64_t c_unroll=(c>>1);
	while(c_unroll--){
		tmp0=0e0;
		tmp1=0e0;

		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x  );
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
			r14 = *(x+14);
			r15 = *(x+15);
			r16 = *(x+16);
			r17 = *(x+17);
			r18 = *(x+18);
			r19 = *(x+19);
			r20 = *(x+20);
			r21 = *(x+21);
			r22 = *(x+22);
			r23 = *(x+23);
			r24 = *(x+24);
			r25 = *(x+25);
			r26 = *(x+26);
			r27 = *(x+27);
			r28 = *(x+28);
			r29 = *(x+29);
			r30 = *(x+30);
			r31 = *(x+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp0 = tmp0 + r00;

            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);
			r16 = *(x1+16);
			r17 = *(x1+17);
			r18 = *(x1+18);
			r19 = *(x1+19);
			r20 = *(x1+20);
			r21 = *(x1+21);
			r22 = *(x1+22);
			r23 = *(x1+23);
			r24 = *(x1+24);
			r25 = *(x1+25);
			r26 = *(x1+26);
			r27 = *(x1+27);
			r28 = *(x1+28);
			r29 = *(x1+29);
			r30 = *(x1+30);
			r31 = *(x1+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp1 = tmp1 + r00;

			x+=32;
            x1+=32;
		}

		int64_t n_remain=n%32;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x);
                tmp1 += *(x1);
                x+=1;
                x1+=1;
            }
		}

		x_sum[jj]=tmp0;
		x_sum[jj+step]=tmp1;
        jj++;
	}

    int64_t c_remain=(c%2);
    if (c_remain&1){
		tmp0=0e0;
        jj=jj+step;
		int64_t n_unroll=(n>>5);
		while( n_unroll-- ){ 
            // --------------------------------------
			r00 = *(x1  );
			r01 = *(x1+1);
			r02 = *(x1+2);
			r03 = *(x1+3);
			r04 = *(x1+4);
			r05 = *(x1+5);
			r06 = *(x1+6);
			r07 = *(x1+7);
			r08 = *(x1+8);
			r09 = *(x1+9);
			r10 = *(x1+10);
			r11 = *(x1+11);
			r12 = *(x1+12);
			r13 = *(x1+13);
			r14 = *(x1+14);
			r15 = *(x1+15);
			r16 = *(x1+16);
			r17 = *(x1+17);
			r18 = *(x1+18);
			r19 = *(x1+19);
			r20 = *(x1+20);
			r21 = *(x1+21);
			r22 = *(x1+22);
			r23 = *(x1+23);
			r24 = *(x1+24);
			r25 = *(x1+25);
			r26 = *(x1+26);
			r27 = *(x1+27);
			r28 = *(x1+28);
			r29 = *(x1+29);
			r30 = *(x1+30);
			r31 = *(x1+31);

			r00 = r00 + r01;
            r02 = r02 + r03;
            r04 = r04 + r05;
            r06 = r06 + r07;
            r08 = r08 + r09;
            r10 = r10 + r11;
            r12 = r12 + r13;
            r14 = r14 + r15;
			r16 = r16 + r17;
            r18 = r18 + r19;
            r20 = r20 + r21;
            r22 = r22 + r23;
            r24 = r24 + r25;
            r26 = r26 + r27;
            r28 = r28 + r29;
            r30 = r30 + r31;

            r00 = r00 + r02;
            r04 = r04 + r06;
            r08 = r08 + r10;
            r12 = r12 + r14;
            r16 = r16 + r18;
            r20 = r20 + r22;
            r24 = r24 + r26;
            r28 = r28 + r30;

            r00 = r00 + r04;
            r08 = r08 + r12;
            r16 = r16 + r20;
            r24 = r24 + r28;
            
            r00 = r00 + r08;
            r16 = r16 + r24;

            r00 = r00 + r16;

			tmp0 = tmp0 + r00;


            x1+=32;
		}

		int64_t n_remain=n%32;
		if(n_remain > 0){
            while(n_remain--){
                tmp0 += *(x1);
                x+=1;
                x1+=1;
            }
		}

        x_sum[jj]=tmp0;
        jj++;
    }
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_ASM_r8(double *x_sum, double *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x]), %%ymm0        \n\t"
			"subq $-4*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movsd  0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1 \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movupd              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movsd               %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_04_ASM_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
			"subq $-4*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm15, %%ymm15 \n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"MOVQ   0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1 \n\t"
			"PADDQ               %%xmm1,  %%xmm0 \n\t"
			"MOVDQU              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"PADDQ               %%xmm1,  %%xmm0 \n\t"
			"MOVQ                %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_ASM_r8(double *x_sum, double *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x]), %%ymm0        \n\t"
			"vmovupd 4*8(%[x]), %%ymm1        \n\t"
			"subq $-8*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1,  %%ymm8   \n\t"
				"vmovupd 0*8(%[x]), %%ymm0         \n\t"
				"vmovupd 4*8(%[x]), %%ymm1         \n\t"
				"vaddpd  %%ymm8, %%ymm15,  %%ymm15 \n\t"
				"subq $-8*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
			"vaddpd  %%ymm1, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movsd  0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1 \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movupd              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movsd               %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_08_ASM_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
			"vMOVDQU 4*8(%[x]), %%ymm1        \n\t"
			"subq $-8*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm1,  %%ymm8   \n\t"
				"vMOVDQU 0*8(%[x]), %%ymm0         \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1         \n\t"
				"vPADDQ  %%ymm8, %%ymm15,  %%ymm15 \n\t"
				"subq $-8*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm15, %%ymm15 \n\t"
			"vPADDQ  %%ymm1, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vPADDQ %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movq   0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1 \n\t"
			"PADDQ               %%xmm1,  %%xmm0 \n\t"
			"MOVDQU              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"PADDQ               %%xmm1,  %%xmm0 \n\t"
			"movq                %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_16_ASM_r8(double *x_sum, double *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14  \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15  \n\t"
			"vmovupd  0*8(%[x]), %%ymm0        \n\t"
			"vmovupd  4*8(%[x]), %%ymm1        \n\t"
			"vmovupd  8*8(%[x]), %%ymm2        \n\t"
			"vmovupd 12*8(%[x]), %%ymm3        \n\t"
			"subq $-16*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1,  %%ymm8    \n\t"
				"vmovupd  0*8(%[x]), %%ymm0         \n\t"
				"vmovupd  4*8(%[x]), %%ymm1         \n\t"
				"vaddpd  %%ymm8, %%ymm14,  %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3,  %%ymm9    \n\t"
				"vmovupd  8*8(%[x]), %%ymm2         \n\t"
				"vmovupd 12*8(%[x]), %%ymm3         \n\t"
				"vaddpd  %%ymm9, %%ymm15,  %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0,  %%ymm1, %%ymm8    \n\t"
			"vaddpd  %%ymm2,  %%ymm3, %%ymm9    \n\t"
			"vaddpd  %%ymm8,  %%ymm14, %%ymm14  \n\t"
			"vaddpd  %%ymm9,  %%ymm15, %%ymm15  \n\t"
			"vaddpd  %%ymm14, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"vmovupd 4*8(%[x]), %%ymm1        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movsd  0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1 \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movupd              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movsd               %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_16_ASM_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14  \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15  \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0        \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1        \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2        \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3        \n\t"
			"subq $-16*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1,  %%ymm8    \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0         \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1         \n\t"
				"vaddpd  %%ymm8, %%ymm14,  %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3,  %%ymm9    \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2         \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3         \n\t"
				"vaddpd  %%ymm9, %%ymm15,  %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x]                  \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0,  %%ymm1, %%ymm8    \n\t"
			"vaddpd  %%ymm2,  %%ymm3, %%ymm9    \n\t"
			"vaddpd  %%ymm8,  %%ymm14, %%ymm14  \n\t"
			"vaddpd  %%ymm9,  %%ymm15, %%ymm15  \n\t"
			"vaddpd  %%ymm14, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movsd  0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1 \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"MOVDQU              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movsd               %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_32_ASM_r8(double *x_sum, double *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12  \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13  \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14  \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15  \n\t"
			"vmovupd  0*8(%[x]), %%ymm0        \n\t"
			"vmovupd  4*8(%[x]), %%ymm1        \n\t"
			"vmovupd  8*8(%[x]), %%ymm2        \n\t"
			"vmovupd 12*8(%[x]), %%ymm3        \n\t"
			"vmovupd 16*8(%[x]), %%ymm4        \n\t"
			"vmovupd 20*8(%[x]), %%ymm5        \n\t"
			"vmovupd 24*8(%[x]), %%ymm6        \n\t"
			"vmovupd 28*8(%[x]), %%ymm7        \n\t"
			"subq $-32*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1,  %%ymm8     \n\t"
				"vmovupd  0*8(%[x]), %%ymm0          \n\t"
				"vmovupd  4*8(%[x]), %%ymm1          \n\t"
				"vaddpd  %%ymm8, %%ymm12,  %%ymm12   \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3,  %%ymm9     \n\t"
				"vmovupd  8*8(%[x]), %%ymm2          \n\t"
				"vmovupd 12*8(%[x]), %%ymm3          \n\t"
				"vaddpd  %%ymm9, %%ymm13,  %%ymm13   \n\t"
				"\n\t"
				"vaddpd  %%ymm4, %%ymm5,  %%ymm10    \n\t"
				"vmovupd 16*8(%[x]), %%ymm4          \n\t"
				"vmovupd 20*8(%[x]), %%ymm5          \n\t"
				"vaddpd  %%ymm10, %%ymm14,  %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm6, %%ymm7,  %%ymm11    \n\t"
				"vmovupd 24*8(%[x]), %%ymm6          \n\t"
				"vmovupd 28*8(%[x]), %%ymm7          \n\t"
				"vaddpd  %%ymm11, %%ymm15,  %%ymm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x]                   \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0,  %%ymm1, %%ymm8     \n\t"
			"vaddpd  %%ymm2,  %%ymm3, %%ymm9     \n\t"
			"vaddpd  %%ymm4,  %%ymm5, %%ymm10    \n\t"
			"vaddpd  %%ymm6,  %%ymm7, %%ymm11    \n\t"
			"\n\t"
			"vaddpd  %%ymm8,   %%ymm12, %%ymm12  \n\t"
			"vaddpd  %%ymm9,   %%ymm13, %%ymm13  \n\t"
			"vaddpd  %%ymm10,  %%ymm14, %%ymm14  \n\t"
			"vaddpd  %%ymm11,  %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vaddpd  %%ymm12,  %%ymm13, %%ymm13  \n\t"
			"vaddpd  %%ymm14,  %%ymm15, %%ymm15  \n\t"
			"vaddpd  %%ymm13,  %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"vmovupd  0*8(%[x]), %%ymm0        \n\t"
				"vmovupd  4*8(%[x]), %%ymm1        \n\t"
				"vmovupd  8*8(%[x]), %%ymm2        \n\t"
				"vmovupd 12*8(%[x]), %%ymm3        \n\t"
				"vaddpd %%ymm0, %%ymm1, %%ymm1     \n\t"
				"vaddpd %%ymm2, %%ymm3, %%ymm3     \n\t"
				"vaddpd %%ymm2, %%ymm2, %%ymm0     \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-16*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"vmovupd 4*8(%[x]), %%ymm1        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x]), %%ymm0        \n\t"
				"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movsd  0*8(%[x]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1 \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movupd              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"addpd               %%xmm1,  %%xmm0 \n\t"
			"movsd               %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_32_ASM_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12  \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13  \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14  \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15  \n\t"
			"vMOVDQU  0*8(%[x]), %%ymm0        \n\t"
			"vMOVDQU  4*8(%[x]), %%ymm1        \n\t"
			"vMOVDQU  8*8(%[x]), %%ymm2        \n\t"
			"vMOVDQU 12*8(%[x]), %%ymm3        \n\t"
			"vMOVDQU 16*8(%[x]), %%ymm4        \n\t"
			"vMOVDQU 20*8(%[x]), %%ymm5        \n\t"
			"vMOVDQU 24*8(%[x]), %%ymm6        \n\t"
			"vMOVDQU 28*8(%[x]), %%ymm7        \n\t"
			"subq $-32*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vpaddq  %%ymm0, %%ymm1,  %%ymm8     \n\t"
				"vMOVDQU  0*8(%[x]), %%ymm0          \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1          \n\t"
				"vpaddq  %%ymm8, %%ymm12,  %%ymm12   \n\t"
				"\n\t"
				"vpaddq  %%ymm2, %%ymm3,  %%ymm9     \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2          \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3          \n\t"
				"vpaddq  %%ymm9, %%ymm13,  %%ymm13   \n\t"
				"\n\t"
				"vpaddq  %%ymm4, %%ymm5,  %%ymm10    \n\t"
				"vMOVDQU 16*8(%[x]), %%ymm4          \n\t"
				"vMOVDQU 20*8(%[x]), %%ymm5          \n\t"
				"vpaddq  %%ymm10, %%ymm14,  %%ymm14  \n\t"
				"\n\t"
				"vpaddq  %%ymm6, %%ymm7,  %%ymm11    \n\t"
				"vMOVDQU 24*8(%[x]), %%ymm6          \n\t"
				"vMOVDQU 28*8(%[x]), %%ymm7          \n\t"
				"vpaddq  %%ymm11, %%ymm15,  %%ymm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x]                   \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vpaddq  %%ymm0,  %%ymm1, %%ymm8     \n\t"
			"vpaddq  %%ymm2,  %%ymm3, %%ymm9     \n\t"
			"vpaddq  %%ymm4,  %%ymm5, %%ymm10    \n\t"
			"vpaddq  %%ymm6,  %%ymm7, %%ymm11    \n\t"
			"\n\t"
			"vpaddq  %%ymm8,   %%ymm12, %%ymm12  \n\t"
			"vpaddq  %%ymm9,   %%ymm13, %%ymm13  \n\t"
			"vpaddq  %%ymm10,  %%ymm14, %%ymm14  \n\t"
			"vpaddq  %%ymm11,  %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vpaddq  %%ymm12,  %%ymm13, %%ymm13  \n\t"
			"vpaddq  %%ymm14,  %%ymm15, %%ymm15  \n\t"
			"vpaddq  %%ymm13,  %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"vMOVDQU  0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1        \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2        \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3        \n\t"
				"vpaddq %%ymm0, %%ymm1, %%ymm1     \n\t"
				"vpaddq %%ymm2, %%ymm3, %%ymm3     \n\t"
				"vpaddq %%ymm2, %%ymm2, %%ymm0     \n\t"
				"vpaddq %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-16*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1        \n\t"
				"vpaddq %%ymm0, %%ymm15, %%ymm15  \n\t"
				"vpaddq %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vpaddq %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm0 \n\t"
				"paddq %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"movq   0*8(%[x]), %%xmm0 \n\t"
				"paddq %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0 \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1 \n\t"
			"paddq               %%xmm1,  %%xmm0 \n\t"
			"MOVDQU              %%xmm0,  %%xmm1 \n\t"
			"PSRLDQ         $8,  %%xmm1          \n\t"
			"paddq               %%xmm1,  %%xmm0 \n\t"
			"movq                %%xmm0, %[t]    \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_64_ASM_r8(double *x_sum, double *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"VXORPD   %%zmm12, %%zmm12, %%zmm12  \n\t"
			"VXORPD   %%zmm13, %%zmm13, %%zmm13  \n\t"
			"VXORPD   %%zmm14, %%zmm14, %%zmm14  \n\t"
			"VXORPD   %%zmm15, %%zmm15, %%zmm15  \n\t"
			"vmovupd  0*8(%[x]), %%zmm0        \n\t"
			"vmovupd  8*8(%[x]), %%zmm1        \n\t"
			"vmovupd 16*8(%[x]), %%zmm2        \n\t"
			"vmovupd 24*8(%[x]), %%zmm3        \n\t"
			"vmovupd 32*8(%[x]), %%zmm4        \n\t"
			"vmovupd 40*8(%[x]), %%zmm5        \n\t"
			"vmovupd 48*8(%[x]), %%zmm6        \n\t"
			"vmovupd 56*8(%[x]), %%zmm7        \n\t"
			"subq $-64*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>6), n_remain=n%64;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%zmm0, %%zmm1,  %%zmm8     \n\t"
				"vmovupd  0*8(%[x]), %%zmm0          \n\t"
				"vmovupd  8*8(%[x]), %%zmm1          \n\t"
				"vaddpd  %%zmm8, %%zmm12,  %%zmm12   \n\t"
				"\n\t"
				"vaddpd  %%zmm2, %%zmm3,  %%zmm9     \n\t"
				"vmovupd 16*8(%[x]), %%zmm2          \n\t"
				"vmovupd 24*8(%[x]), %%zmm3          \n\t"
				"vaddpd  %%zmm9, %%zmm13,  %%zmm13   \n\t"
				"\n\t"
				"vaddpd  %%zmm4, %%zmm5,  %%zmm10    \n\t"
				"vmovupd 32*8(%[x]), %%zmm4          \n\t"
				"vmovupd 40*8(%[x]), %%zmm5          \n\t"
				"vaddpd  %%zmm10, %%zmm14,  %%zmm14  \n\t"
				"\n\t"
				"vaddpd  %%zmm6, %%zmm7,  %%zmm11    \n\t"
				"vmovupd 48*8(%[x]), %%zmm6          \n\t"
				"vmovupd 56*8(%[x]), %%zmm7          \n\t"
				"vaddpd  %%zmm11, %%zmm15,  %%zmm15  \n\t"
				"\n\t"
				"subq $-64*8, %[x]                   \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%zmm0,  %%zmm1, %%zmm8     \n\t"
			"vaddpd  %%zmm2,  %%zmm3, %%zmm9     \n\t"
			"vaddpd  %%zmm4,  %%zmm5, %%zmm10    \n\t"
			"vaddpd  %%zmm6,  %%zmm7, %%zmm11    \n\t"
			"\n\t"
			"vaddpd  %%zmm8,   %%zmm12, %%zmm12  \n\t"
			"vaddpd  %%zmm9,   %%zmm13, %%zmm13  \n\t"
			"vaddpd  %%zmm10,  %%zmm14, %%zmm14  \n\t"
			"vaddpd  %%zmm11,  %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vaddpd  %%zmm12,  %%zmm13, %%zmm13  \n\t"
			"vaddpd  %%zmm14,  %%zmm15, %%zmm15  \n\t"
			"vaddpd  %%zmm13,  %%zmm15, %%zmm15  \n\t"
			::
		);

		// if(n_remain & 32){
		// 	__asm__ __volatile__ (
		// 		"vmovupd  0*8(%[x]), %%ymm0        \n\t"
		// 		"vmovupd  4*8(%[x]), %%ymm1        \n\t"
		// 		"vmovupd  8*8(%[x]), %%ymm2        \n\t"
		// 		"vmovupd 12*8(%[x]), %%ymm3        \n\t"
		// 		"vmovupd 16*8(%[x]), %%ymm4        \n\t"
		// 		"vmovupd 20*8(%[x]), %%ymm5        \n\t"
		// 		"vmovupd 24*8(%[x]), %%ymm6        \n\t"
		// 		"vmovupd 28*8(%[x]), %%ymm7        \n\t"
		// 		"\n\t"
		// 		"vaddpd %%ymm0, %%ymm1, %%ymm1     \n\t"
		// 		"vaddpd %%ymm2, %%ymm3, %%ymm3     \n\t"
		// 		"vaddpd %%ymm4, %%ymm5, %%ymm5     \n\t"
		// 		"vaddpd %%ymm6, %%ymm7, %%ymm7     \n\t"
		// 		"\n\t"
		// 		"vaddpd %%ymm6, %%ymm6, %%ymm4     \n\t"
		// 		"vaddpd %%ymm2, %%ymm2, %%ymm0     \n\t"
		// 		"\n\t"
		// 		"vaddpd %%ymm4, %%ymm4, %%ymm0     \n\t"
		// 		"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
		// 		"subq $-16*8, %[x]                 \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// if(n_remain & 16){
		// 	__asm__ __volatile__ (
		// 		"vmovupd  0*8(%[x]), %%ymm0        \n\t"
		// 		"vmovupd  4*8(%[x]), %%ymm1        \n\t"
		// 		"vmovupd  8*8(%[x]), %%ymm2        \n\t"
		// 		"vmovupd 12*8(%[x]), %%ymm3        \n\t"
		// 		"vaddpd %%ymm0, %%ymm1, %%ymm1     \n\t"
		// 		"vaddpd %%ymm2, %%ymm3, %%ymm3     \n\t"
		// 		"vaddpd %%ymm2, %%ymm2, %%ymm0     \n\t"
		// 		"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
		// 		"subq $-16*8, %[x]                 \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// if(n_remain & 8){
		// 	__asm__ __volatile__ (
		// 		"vmovupd 0*8(%[x]), %%ymm0        \n\t"
		// 		"vmovupd 4*8(%[x]), %%ymm1        \n\t"
		// 		"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
		// 		"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
		// 		"subq $-8*8, %[x]                 \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// if(n_remain & 4){
		// 	__asm__ __volatile__ (
		// 		"vmovupd 0*8(%[x]), %%ymm0        \n\t"
		// 		"vaddpd %%ymm0, %%ymm15, %%ymm15  \n\t"
		// 		"subq $-4*8, %[x]                 \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// if(n_remain & 2){
		// 	__asm__ __volatile__ (
		// 		"movupd 0*8(%[x]), %%xmm0 \n\t"
		// 		"addpd %%xmm0, %%xmm15    \n\t"
		// 		"subq $-2*8, %[x]         \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// if(n_remain & 1){
		// 	__asm__ __volatile__ (
		// 		"pxor      %%xmm0, %%xmm0 \n\t"
		// 		"movsd  0*8(%[x]), %%xmm0 \n\t"
		// 		"addpd %%xmm0, %%xmm15    \n\t"
		// 		"subq $-1*8, %[x]         \n\t"
		// 		:[x]"=r"(x)
		// 		:"0"(x)
		// 	);
		// }

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vaddpd      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t]     \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

void sum_up_matrix_64_ASM_i8(int64_t *x_sum, int64_t *x, int64_t n, int64_t c){
	for(int64_t jj=0; jj<c; jj++){
		__asm__ __volatile__ (
			"VXORPD   %%zmm12, %%zmm12, %%zmm12  \n\t"
			"VXORPD   %%zmm13, %%zmm13, %%zmm13  \n\t"
			"VXORPD   %%zmm14, %%zmm14, %%zmm14  \n\t"
			"VXORPD   %%zmm15, %%zmm15, %%zmm15  \n\t"
			"VMOVDQU64 0*8(%[x]), %%zmm0        \n\t"
			"VMOVDQU64 8*8(%[x]), %%zmm1        \n\t"
			"VMOVDQU64 6*8(%[x]), %%zmm2        \n\t"
			"VMOVDQU64 4*8(%[x]), %%zmm3        \n\t"
			"VMOVDQU64 2*8(%[x]), %%zmm4        \n\t"
			"VMOVDQU64 0*8(%[x]), %%zmm5        \n\t"
			"VMOVDQU64 8*8(%[x]), %%zmm6        \n\t"
			"VMOVDQU64 6*8(%[x]), %%zmm7        \n\t"
			"subq $-64*8, %[x]                 \n\t"
			:[x]"=r"(x)
			:"0"(x)
		);

		int64_t n_unroll=(n>>6), n_remain=n%64;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%zmm0, %%zmm1,  %%zmm8     \n\t"
				"VMOVDQU64 0*8(%[x]), %%zmm0          \n\t"
				"VMOVDQU64 8*8(%[x]), %%zmm1          \n\t"
				"vPADDQ  %%zmm8, %%zmm12,  %%zmm12   \n\t"
				"\n\t"
				"vPADDQ  %%zmm2, %%zmm3,  %%zmm9     \n\t"
				"VMOVDQU64 6*8(%[x]), %%zmm2          \n\t"
				"VMOVDQU64 4*8(%[x]), %%zmm3          \n\t"
				"vPADDQ  %%zmm9, %%zmm13,  %%zmm13   \n\t"
				"\n\t"
				"vPADDQ  %%zmm4, %%zmm5,  %%zmm10    \n\t"
				"VMOVDQU64 2*8(%[x]), %%zmm4          \n\t"
				"VMOVDQU64 0*8(%[x]), %%zmm5          \n\t"
				"vPADDQ  %%zmm10, %%zmm14,  %%zmm14  \n\t"
				"\n\t"
				"vPADDQ  %%zmm6, %%zmm7,  %%zmm11    \n\t"
				"VMOVDQU64 8*8(%[x]), %%zmm6          \n\t"
				"VMOVDQU64 6*8(%[x]), %%zmm7          \n\t"
				"vPADDQ  %%zmm11, %%zmm15,  %%zmm15  \n\t"
				"\n\t"
				"subq $-64*8, %[x]                   \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%zmm0,  %%zmm1, %%zmm8     \n\t"
			"vPADDQ  %%zmm2,  %%zmm3, %%zmm9     \n\t"
			"vPADDQ  %%zmm4,  %%zmm5, %%zmm10    \n\t"
			"vPADDQ  %%zmm6,  %%zmm7, %%zmm11    \n\t"
			"\n\t"
			"vPADDQ  %%zmm8,   %%zmm12, %%zmm12  \n\t"
			"vPADDQ  %%zmm9,   %%zmm13, %%zmm13  \n\t"
			"vPADDQ  %%zmm10,  %%zmm14, %%zmm14  \n\t"
			"vPADDQ  %%zmm11,  %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vPADDQ  %%zmm12,  %%zmm13, %%zmm13  \n\t"
			"vPADDQ  %%zmm14,  %%zmm15, %%zmm15  \n\t"
			"vPADDQ  %%zmm13,  %%zmm15, %%zmm15  \n\t"
			::
		);

		if(n_remain & 32){
			__asm__ __volatile__ (
				"vMOVDQU  0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1        \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2        \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3        \n\t"
				"vMOVDQU 16*8(%[x]), %%ymm4        \n\t"
				"vMOVDQU 20*8(%[x]), %%ymm5        \n\t"
				"vMOVDQU 24*8(%[x]), %%ymm6        \n\t"
				"vMOVDQU 28*8(%[x]), %%ymm7        \n\t"
				"\n\t"
				"vPADDQ %%ymm0, %%ymm1, %%ymm1     \n\t"
				"vPADDQ %%ymm2, %%ymm3, %%ymm3     \n\t"
				"vPADDQ %%ymm4, %%ymm5, %%ymm5     \n\t"
				"vPADDQ %%ymm6, %%ymm7, %%ymm7     \n\t"
				"\n\t"
				"vPADDQ %%ymm6, %%ymm6, %%ymm4     \n\t"
				"vPADDQ %%ymm2, %%ymm2, %%ymm0     \n\t"
				"\n\t"
				"vPADDQ %%ymm4, %%ymm4, %%ymm0     \n\t"
				"vPADDQ %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-16*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 16){
			__asm__ __volatile__ (
				"vMOVDQU  0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU  4*8(%[x]), %%ymm1        \n\t"
				"vMOVDQU  8*8(%[x]), %%ymm2        \n\t"
				"vMOVDQU 12*8(%[x]), %%ymm3        \n\t"
				"vPADDQ %%ymm0, %%ymm1, %%ymm1     \n\t"
				"vPADDQ %%ymm2, %%ymm3, %%ymm3     \n\t"
				"vPADDQ %%ymm2, %%ymm2, %%ymm0     \n\t"
				"vPADDQ %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-16*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vMOVDQU 4*8(%[x]), %%ymm1        \n\t"
				"vPADDQ %%ymm0, %%ymm15, %%ymm15  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x]), %%ymm0        \n\t"
				"vPADDQ %%ymm0, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x]                 \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-2*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0 \n\t"
				"MOVQ   0*8(%[x]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm15    \n\t"
				"subq $-1*8, %[x]         \n\t"
				:[x]"=r"(x)
				:"0"(x)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vPADDQ      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t]     \n\t"
			:[t]"=m"(tmp0)
			:
		);

		x_sum[jj]=tmp0;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_02_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0        \n\t"
				"vaddpd  %%ymm1, %%ymm15, %%ymm15  \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1        \n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm14, %%ymm14 \n\t"
			"vaddpd  %%ymm1, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
			"subq $-4*8, %[x1]                \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm1, %%ymm15, %%ymm15  \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1        \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm1, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp1;
		jj++;

	}
}

void sum_up_matrix_04_02_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0        \n\t"
				"\n\t"
				"vPADDQ  %%ymm1, %%ymm15, %%ymm15  \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1        \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm14, %%ymm14 \n\t"
			"vPADDQ  %%ymm1, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);

			int64_t n_unroll=(n>>2), n_remain=n%4;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vPADDQ  %%ymm1, %%ymm15, %%ymm15  \n\t"
					"vMOVDQU 0*8(%[x1]), %%ymm1        \n\t"
					"\n\t"
					"subq $-4*8, %[x1]                 \n\t"
					:[x1]"=r"(x1)
					:"0"(x1)
				);
			}

			__asm__ __volatile__ (
				"vPADDQ  %%ymm1, %%ymm15, %%ymm15 \n\t"
				::
			);

			if(n_remain & 2){
				__asm__ __volatile__ (
					"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
					"PADDQ %%xmm1, %%xmm15     \n\t"
					"subq $-2*8, %[x1]         \n\t"
					:[x1]"=r"(x1)
					:"0"(x1)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm1, %%xmm1  \n\t"
					"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
					"PADDQ %%xmm1, %%xmm15     \n\t"
					"subq $-1*8, %[x1]         \n\t"
					:[x1]"=r"(x1)
					:"0"(x1)
				);
			}

			// Extract Result
			int64_t tmp1;
			__asm__ __volatile__ (
				"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVDQU              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVQ                %%xmm0, %[t1]     \n\t"
				:[t1]"=m"(tmp1)
				:
			);

			x_sum[jj]=tmp1;
			jj++;

		}
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_04_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
			"vmovupd 0*8(%[x2]), %%ymm2       \n\t"
			"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			"subq $-4*8, %[x2]                \n\t"
			"subq $-4*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0        \n\t"
				"\n\t"
				"vaddpd  %%ymm1, %%ymm13, %%ymm13  \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1        \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vmovupd 0*8(%[x2]), %%ymm2        \n\t"
				"\n\t"
				"vaddpd  %%ymm3, %%ymm15, %%ymm15  \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3        \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				"subq $-4*8, %[x2]                 \n\t"
				"subq $-4*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
			"vaddpd  %%ymm1, %%ymm13, %%ymm13 \n\t"
			"vaddpd  %%ymm2, %%ymm14, %%ymm14 \n\t"
			"vaddpd  %%ymm3, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"movupd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movsd  0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"movsd  0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		double tmp2;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		double tmp3;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step*3;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
				"subq $-4*8, %[x3]                \n\t"
				:[x3]"=r"(x3)
				:"0"(x3)
			);

			int64_t n_unroll=(n>>2), n_remain=n%4;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vaddpd  %%ymm3, %%ymm15, %%ymm15  \n\t"
					"vmovupd 0*8(%[x3]), %%ymm3        \n\t"
					"\n\t"
					"subq $-4*8, %[x3]                 \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			__asm__ __volatile__ (
				"vaddpd  %%ymm3, %%ymm15, %%ymm15 \n\t"
				::
			);

			if(n_remain & 2){
				__asm__ __volatile__ (
					"movupd 0*8(%[x3]), %%xmm3 \n\t"
					"addpd %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-2*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm3, %%xmm3  \n\t"
					"movsd  0*8(%[x3]), %%xmm3 \n\t"
					"addpd %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-1*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			// Extract Result
			double tmp3;
			__asm__ __volatile__ (
				"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movupd              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movsd               %%xmm0, %[t3]     \n\t"
				:[t3]"=m"(tmp3)
				:
			);

			x_sum[jj]=tmp3;
			jj++;
		}
	}
}

void sum_up_matrix_04_04_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm2       \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm3       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			"subq $-4*8, %[x2]                \n\t"
			"subq $-4*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0        \n\t"
				"\n\t"
				"vPADDQ  %%ymm1, %%ymm13, %%ymm13  \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1        \n\t"
				"\n\t"
				"vPADDQ  %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2        \n\t"
				"\n\t"
				"vPADDQ  %%ymm3, %%ymm15, %%ymm15  \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3        \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				"subq $-4*8, %[x2]                 \n\t"
				"subq $-4*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm12, %%ymm12 \n\t"
			"vPADDQ  %%ymm1, %%ymm13, %%ymm13 \n\t"
			"vPADDQ  %%ymm2, %%ymm14, %%ymm14 \n\t"
			"vPADDQ  %%ymm3, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVQ   0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"MOVQ   0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm12, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm13, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		int64_t tmp2;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		int64_t tmp3;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step*3;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3       \n\t"
				"subq $-4*8, %[x3]                \n\t"
				:[x3]"=r"(x3)
				:"0"(x3)
			);

			int64_t n_unroll=(n>>2), n_remain=n%4;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vPADDQ  %%ymm3, %%ymm15, %%ymm15  \n\t"
					"vMOVDQU 0*8(%[x3]), %%ymm3        \n\t"
					"\n\t"
					"subq $-4*8, %[x3]                 \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			__asm__ __volatile__ (
				"vPADDQ  %%ymm3, %%ymm15, %%ymm15 \n\t"
				::
			);

			if(n_remain & 2){
				__asm__ __volatile__ (
					"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
					"PADDQ %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-2*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm3, %%xmm3  \n\t"
					"MOVQ   0*8(%[x3]), %%xmm3 \n\t"
					"PADDQ %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-1*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			// Extract Result
			int64_t tmp3;
			__asm__ __volatile__ (
				"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVDQU              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVQ                %%xmm0, %[t3]     \n\t"
				:[t3]"=m"(tmp3)
				:
			);

			x_sum[jj]=tmp3;
			jj++;
		}

	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_04_04_ASM_Pre_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
			"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
			"vmovupd 0*8(%[x2]), %%ymm2       \n\t"
			"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			"subq $-4*8, %[x2]                \n\t"
			"subq $-4*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"prefetcht1 4*8(%[x1])             \n\n"
				"vaddpd  %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x3])             \n\n"
				"vaddpd  %%ymm1, %%ymm13, %%ymm13  \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x2])             \n\n"
				"vaddpd  %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vmovupd 0*8(%[x2]), %%ymm2        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x0])             \n\n"
				"vaddpd  %%ymm3, %%ymm15, %%ymm15  \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3        \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				"subq $-4*8, %[x2]                 \n\t"
				"subq $-4*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
			"vaddpd  %%ymm1, %%ymm13, %%ymm13 \n\t"
			"vaddpd  %%ymm2, %%ymm14, %%ymm14 \n\t"
			"vaddpd  %%ymm3, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"movupd 0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movsd  0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"movsd  0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		double tmp2;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		double tmp3;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step*3;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
				"subq $-4*8, %[x3]                \n\t"
				:[x3]"=r"(x3)
				:"0"(x3)
			);

			int64_t n_unroll=(n>>2), n_remain=n%4;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vaddpd  %%ymm3, %%ymm15, %%ymm15  \n\t"
					"vmovupd 0*8(%[x3]), %%ymm3        \n\t"
					"\n\t"
					"subq $-4*8, %[x3]                 \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			__asm__ __volatile__ (
				"vaddpd  %%ymm3, %%ymm15, %%ymm15 \n\t"
				::
			);

			if(n_remain & 2){
				__asm__ __volatile__ (
					"movupd 0*8(%[x3]), %%xmm3 \n\t"
					"addpd %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-2*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm3, %%xmm3  \n\t"
					"movsd  0*8(%[x3]), %%xmm3 \n\t"
					"addpd %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-1*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			// Extract Result
			double tmp3;
			__asm__ __volatile__ (
				"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movupd              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movsd               %%xmm0, %[t3]     \n\t"
				:[t3]"=m"(tmp3)
				:
			);

			x_sum[jj]=tmp3;
			jj++;
		}
	}
}

void sum_up_matrix_04_04_ASM_Pre_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm2       \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm3       \n\t"
			"subq $-4*8, %[x0]                \n\t"
			"subq $-4*8, %[x1]                \n\t"
			"subq $-4*8, %[x2]                \n\t"
			"subq $-4*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>2), n_remain=n%4;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"prefetcht1 4*8(%[x1])             \n\n"
				"vPADDQ  %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x3])             \n\n"
				"vPADDQ  %%ymm1, %%ymm13, %%ymm13  \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x2])             \n\n"
				"vPADDQ  %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2        \n\t"
				"\n\t"
				"prefetcht1 4*8(%[x0])             \n\n"
				"vPADDQ  %%ymm3, %%ymm15, %%ymm15  \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3        \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                 \n\t"
				"subq $-4*8, %[x1]                 \n\t"
				"subq $-4*8, %[x2]                 \n\t"
				"subq $-4*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm12, %%ymm12 \n\t"
			"vPADDQ  %%ymm1, %%ymm13, %%ymm13 \n\t"
			"vPADDQ  %%ymm2, %%ymm14, %%ymm14 \n\t"
			"vPADDQ  %%ymm3, %%ymm15, %%ymm15 \n\t"
			::
		);

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"MOVQ  0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"\n\t"
				"MOVQ  0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVQ  0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"\n\t"
				"MOVQ  0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm12, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm13, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		int64_t tmp2;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ               %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		int64_t tmp3;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ               %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_02_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
			"vmovupd 4*8(%[x0]), %%ymm1       \n\t"
			"vmovupd 0*8(%[x1]), %%ymm2       \n\t"
			"vmovupd 4*8(%[x1]), %%ymm3       \n\t"
			"subq $-8*8, %[x0]                \n\t"
			"subq $-8*8, %[x1]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0        \n\t"
				"vmovupd 4*8(%[x0]), %%ymm1        \n\t"
				"vaddpd  %%ymm8, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd 0*8(%[x1]), %%ymm2        \n\t"
				"vmovupd 4*8(%[x1]), %%ymm3        \n\t"
				"vaddpd  %%ymm9, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]                 \n\t"
				"subq $-8*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm1, %%ymm8    \n\t"
			"vaddpd  %%ymm8, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vaddpd  %%ymm9, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd 0*8(%[x1]), %%ymm2       \n\t"
			"vmovupd 4*8(%[x1]), %%ymm3       \n\t"
			"subq $-8*8, %[x1]                \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd 0*8(%[x1]), %%ymm2        \n\t"
				"vmovupd 4*8(%[x1]), %%ymm3        \n\t"
				"vaddpd  %%ymm9, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vaddpd  %%ymm9, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp1;
	}
}

void sum_up_matrix_08_02_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
			"vMOVDQU 4*8(%[x0]), %%ymm1       \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm2       \n\t"
			"vMOVDQU 4*8(%[x1]), %%ymm3       \n\t"
			"subq $-8*8, %[x0]                \n\t"
			"subq $-8*8, %[x1]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0        \n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm1        \n\t"
				"vPADDQ  %%ymm8, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm2        \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm3        \n\t"
				"vPADDQ  %%ymm9, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]                 \n\t"
				"subq $-8*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm1, %%ymm8    \n\t"
			"vPADDQ  %%ymm8, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vPADDQ  %%ymm9, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		__asm__ __volatile__ (
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm2       \n\t"
			"vMOVDQU 4*8(%[x1]), %%ymm3       \n\t"
			"subq $-8*8, %[x1]                \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm2        \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm3        \n\t"
				"vPADDQ  %%ymm9, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vPADDQ  %%ymm9, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp1;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_08_04_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
			"vmovupd 4*8(%[x0]), %%ymm1       \n\t"
			"vmovupd 0*8(%[x1]), %%ymm2       \n\t"
			"vmovupd 4*8(%[x1]), %%ymm3       \n\t"
			"\n\t"
			"vmovupd 0*8(%[x2]), %%ymm4       \n\t"
			"vmovupd 4*8(%[x2]), %%ymm5       \n\t"
			"vmovupd 0*8(%[x3]), %%ymm6       \n\t"
			"vmovupd 4*8(%[x3]), %%ymm7       \n\t"
			"\n\t"
			"subq $-8*8, %[x0]                \n\t"
			"subq $-8*8, %[x1]                \n\t"
			"subq $-8*8, %[x2]                \n\t"
			"subq $-8*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vmovupd 0*8(%[x0]), %%ymm0        \n\t"
				"vmovupd 4*8(%[x0]), %%ymm1        \n\t"
				"vaddpd  %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd 0*8(%[x1]), %%ymm2        \n\t"
				"vmovupd 4*8(%[x1]), %%ymm3        \n\t"
				"vaddpd  %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vmovupd 0*8(%[x2]), %%ymm4        \n\t"
				"vmovupd 4*8(%[x2]), %%ymm5        \n\t"
				"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vmovupd 0*8(%[x3]), %%ymm6        \n\t"
				"vmovupd 4*8(%[x3]), %%ymm7        \n\t"
				"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]                 \n\t"
				"subq $-8*8, %[x1]                 \n\t"
				"subq $-8*8, %[x2]                 \n\t"
				"subq $-8*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm1, %%ymm8     \n\t"
			"vaddpd  %%ymm8, %%ymm12, %%ymm12   \n\t"
			"\n\t"
			"vaddpd  %%ymm2, %%ymm3, %%ymm9     \n\t"
			"vaddpd  %%ymm9, %%ymm13, %%ymm13   \n\t"
			"\n\t"
			"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vaddpd %%ymm1, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vmovupd 0*8(%[x2]), %%ymm2       \n\t"
				"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
				"vaddpd %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm3, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				"subq $-4*8, %[x2]                \n\t"
				"subq $-4*8, %[x3]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movupd 0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"movupd 0*8(%[x3]), %%xmm4 \n\t"
				"addpd %%xmm4, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm12     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"movsd  0*8(%[x2]), %%xmm2 \n\t"
				"addpd %%xmm2, %%xmm14     \n\t"
				"movsd  0*8(%[x3]), %%xmm3 \n\t"
				"addpd %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm12, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm13, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		double tmp2;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		double tmp3;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step*3;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vmovupd 0*8(%[x3]), %%ymm6       \n\t"
				"vmovupd 4*8(%[x3]), %%ymm7       \n\t"
				"\n\t"
				"subq $-8*8, %[x3]                \n\t"
				:[x3]"=r"(x3)
				:"0"(x3)
			);

			int64_t n_unroll=(n>>3), n_remain=n%8;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
					"vmovupd 0*8(%[x3]), %%ymm6        \n\t"
					"vmovupd 4*8(%[x3]), %%ymm7        \n\t"
					"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
					"\n\t"
					"subq $-8*8, %[x3]                 \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			__asm__ __volatile__ (
				"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
				::
			);

			if(n_remain & 4){
				__asm__ __volatile__ (
					"vmovupd 0*8(%[x3]), %%ymm3       \n\t"
					"vaddpd %%ymm3, %%ymm15, %%ymm15  \n\t"
					"\n\t"
					"subq $-4*8, %[x3]                \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 2){
				__asm__ __volatile__ (
					"movupd 0*8(%[x3]), %%xmm4 \n\t"
					"addpd %%xmm4, %%xmm15     \n\t"
					"subq $-2*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm3, %%xmm3  \n\t"
					"movsd  0*8(%[x3]), %%xmm3 \n\t"
					"addpd %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-1*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			// Extract Result
			double tmp3;
			__asm__ __volatile__ (
				"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movupd              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"addpd               %%xmm1,  %%xmm0  \n\t"
				"movsd               %%xmm0, %[t3]     \n\t"
				:[t3]"=m"(tmp3)
				:
			);

			x_sum[jj]=tmp3;
			jj++;
		}
	}
}

void sum_up_matrix_08_04_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1, *x2, *x3;
	int64_t c_unroll=(c>>2), c_remain=c%4, c_step=(c>>2), jj=0;
	x1 = x0;
	x2 = x0;
	x3 = x0;
	x1 += c_step * n;
	x2 += c_step * n * 2;
	x3 += c_step * n * 3;
	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"\n\t"
			"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
			"vMOVDQU 4*8(%[x0]), %%ymm1       \n\t"
			"vMOVDQU 0*8(%[x1]), %%ymm2       \n\t"
			"vMOVDQU 4*8(%[x1]), %%ymm3       \n\t"
			"\n\t"
			"vMOVDQU 0*8(%[x2]), %%ymm4       \n\t"
			"vMOVDQU 4*8(%[x2]), %%ymm5       \n\t"
			"vMOVDQU 0*8(%[x3]), %%ymm6       \n\t"
			"vMOVDQU 4*8(%[x3]), %%ymm7       \n\t"
			"\n\t"
			"subq $-8*8, %[x0]                \n\t"
			"subq $-8*8, %[x1]                \n\t"
			"subq $-8*8, %[x2]                \n\t"
			"subq $-8*8, %[x3]                \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
			:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
		);

		int64_t n_unroll=(n>>3), n_remain=n%8;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vMOVDQU 0*8(%[x0]), %%ymm0        \n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm1        \n\t"
				"vPADDQ  %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm2        \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm3        \n\t"
				"vPADDQ  %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm4        \n\t"
				"vMOVDQU 4*8(%[x2]), %%ymm5        \n\t"
				"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm6        \n\t"
				"vMOVDQU 4*8(%[x3]), %%ymm7        \n\t"
				"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-8*8, %[x0]                 \n\t"
				"subq $-8*8, %[x1]                 \n\t"
				"subq $-8*8, %[x2]                 \n\t"
				"subq $-8*8, %[x3]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm1, %%ymm8     \n\t"
			"vPADDQ  %%ymm8, %%ymm12, %%ymm12   \n\t"
			"\n\t"
			"vPADDQ  %%ymm2, %%ymm3, %%ymm9     \n\t"
			"vPADDQ  %%ymm9, %%ymm13, %%ymm13   \n\t"
			"\n\t"
			"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm12, %%ymm12  \n\t"
				"vPADDQ %%ymm1, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vMOVDQU 0*8(%[x2]), %%ymm2       \n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm3       \n\t"
				"vPADDQ %%ymm2, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm3, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				"subq $-4*8, %[x2]                \n\t"
				"subq $-4*8, %[x3]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVDQU 0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"MOVDQU 0*8(%[x3]), %%xmm4 \n\t"
				"PADDQ %%xmm4, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				"subq $-2*8, %[x2]         \n\t"
				"subq $-2*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"pxor      %%xmm2, %%xmm2  \n\t"
				"pxor      %%xmm3, %%xmm3  \n\t"
				"\n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm12     \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm13     \n\t"
				"\n\t"
				"MOVQ   0*8(%[x2]), %%xmm2 \n\t"
				"PADDQ %%xmm2, %%xmm14     \n\t"
				"MOVQ   0*8(%[x3]), %%xmm3 \n\t"
				"PADDQ %%xmm3, %%xmm15     \n\t"
				"\n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				"subq $-1*8, %[x2]         \n\t"
				"subq $-1*8, %[x3]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1), [x2]"=r"(x2), [x3]"=r"(x3)
				:"0"(x0), "1"(x1), "2"(x2), "3"(x3)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm12, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm12, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm13, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm13, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		int64_t tmp2;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t2]     \n\t"
			:[t2]"=m"(tmp2)
			:
		);

		int64_t tmp3;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t3]     \n\t"
			:[t3]"=m"(tmp3)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		x_sum[jj+c_step*2]=tmp2;
		x_sum[jj+c_step*3]=tmp3;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step*3;
		while(c_remain--){
			__asm__ __volatile__ (
				"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
				"\n\t"
				"vMOVDQU 0*8(%[x3]), %%ymm6       \n\t"
				"vMOVDQU 4*8(%[x3]), %%ymm7       \n\t"
				"\n\t"
				"subq $-8*8, %[x3]                \n\t"
				:[x3]"=r"(x3)
				:"0"(x3)
			);

			int64_t n_unroll=(n>>3), n_remain=n%8;
			n_unroll--;
			while( n_unroll-- ){ 
				__asm__ __volatile__ (
					"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
					"vMOVDQU 0*8(%[x3]), %%ymm6        \n\t"
					"vMOVDQU 4*8(%[x3]), %%ymm7        \n\t"
					"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
					"\n\t"
					"subq $-8*8, %[x3]                 \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			__asm__ __volatile__ (
				"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
				::
			);

			if(n_remain & 4){
				__asm__ __volatile__ (
					"vMOVDQU 0*8(%[x3]), %%ymm3       \n\t"
					"vPADDQ %%ymm3, %%ymm15, %%ymm15  \n\t"
					"\n\t"
					"subq $-4*8, %[x3]                \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 2){
				__asm__ __volatile__ (
					"MOVDQU 0*8(%[x3]), %%xmm4 \n\t"
					"PADDQ %%xmm4, %%xmm15     \n\t"
					"subq $-2*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			if(n_remain & 1){
				__asm__ __volatile__ (
					"pxor      %%xmm3, %%xmm3  \n\t"
					"\n\t"
					"MOVQ   0*8(%[x3]), %%xmm3 \n\t"
					"PADDQ %%xmm3, %%xmm15     \n\t"
					"\n\t"
					"subq $-1*8, %[x3]         \n\t"
					:[x3]"=r"(x3)
					:"0"(x3)
				);
			}

			// Extract Result
			int64_t tmp3;
			__asm__ __volatile__ (
				"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
				"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVDQU              %%xmm0,  %%xmm1  \n\t"
				"PSRLDQ         $8,  %%xmm1           \n\t"
				"PADDQ               %%xmm1,  %%xmm0  \n\t"
				"MOVQ                %%xmm0, %[t3]     \n\t"
				:[t3]"=m"(tmp3)
				:
			);

			x_sum[jj]=tmp3;
			jj++;
		}
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_16_02_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;

	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd  0*8(%[x0]), %%ymm0      \n\t"
			"vmovupd  4*8(%[x0]), %%ymm1      \n\t"
			"vmovupd  8*8(%[x0]), %%ymm2      \n\t"
			"vmovupd 12*8(%[x0]), %%ymm3      \n\t"
			"vmovupd  0*8(%[x1]), %%ymm4      \n\t"
			"vmovupd  4*8(%[x1]), %%ymm5      \n\t"
			"vmovupd  8*8(%[x1]), %%ymm6      \n\t"
			"vmovupd 12*8(%[x1]), %%ymm7      \n\t"
			"subq $-16*8, %[x0]               \n\t"
			"subq $-16*8, %[x1]               \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"prefetcht1 0*8(%[x0]) \n\t" 
				"vaddpd  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vmovupd  0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd  4*8(%[x0]), %%ymm1       \n\t"
				"vaddpd  %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vmovupd  8*8(%[x0]), %%ymm2       \n\t"
				"vmovupd 12*8(%[x0]), %%ymm3       \n\t"
				"vaddpd  %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vmovupd  0*8(%[x1]), %%ymm4        \n\t"
				"vmovupd  4*8(%[x1]), %%ymm5        \n\t"
				"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vmovupd  8*8(%[x1]), %%ymm6        \n\t"
				"vmovupd 12*8(%[x1]), %%ymm7        \n\t"
				"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x0]                 \n\t"
				"subq $-16*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm0, %%ymm1, %%ymm8    \n\t"
			"vaddpd  %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vaddpd  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vaddpd  %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vaddpd  %%ymm14, %%ymm15, %%ymm15  \n\t"
			"vaddpd  %%ymm12, %%ymm13, %%ymm14  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vmovupd 4*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 4*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x0]                \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vmovupd  0*8(%[x1]), %%ymm4      \n\t"
			"vmovupd  4*8(%[x1]), %%ymm5      \n\t"
			"vmovupd  8*8(%[x1]), %%ymm6      \n\t"
			"vmovupd 12*8(%[x1]), %%ymm7      \n\t"
			"subq $-16*8, %[x1]               \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vmovupd  0*8(%[x1]), %%ymm4        \n\t"
				"vmovupd  4*8(%[x1]), %%ymm5        \n\t"
				"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vmovupd  8*8(%[x1]), %%ymm6        \n\t"
				"vmovupd 12*8(%[x1]), %%ymm7        \n\t"
				"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vaddpd  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vaddpd  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vaddpd  %%ymm11, %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vaddpd  %%ymm14, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"vmovupd 4*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp1;
		jj++;
	}
}

void sum_up_matrix_16_02_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;

	while(c_unroll--){
		__asm__ __volatile__ (
			"vpxor  %%ymm12, %%ymm12, %%ymm12 \n\t"
			"vpxor  %%ymm13, %%ymm13, %%ymm13 \n\t"
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU  0*8(%[x0]), %%ymm0      \n\t"
			"vMOVDQU  4*8(%[x0]), %%ymm1      \n\t"
			"vMOVDQU  8*8(%[x0]), %%ymm2      \n\t"
			"vMOVDQU 12*8(%[x0]), %%ymm3      \n\t"
			"vMOVDQU  0*8(%[x1]), %%ymm4      \n\t"
			"vMOVDQU  4*8(%[x1]), %%ymm5      \n\t"
			"vMOVDQU  8*8(%[x1]), %%ymm6      \n\t"
			"vMOVDQU 12*8(%[x1]), %%ymm7      \n\t"
			"subq $-16*8, %[x0]               \n\t"
			"subq $-16*8, %[x1]               \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"prefetcht1 0*8(%[x0]) \n\t" 
				"vPADDQ  %%ymm0, %%ymm1, %%ymm8    \n\t"
				"vMOVDQU  0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU  4*8(%[x0]), %%ymm1       \n\t"
				"vPADDQ  %%ymm8, %%ymm12, %%ymm12  \n\t"
				"\n\t"
				"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
				"vMOVDQU  8*8(%[x0]), %%ymm2       \n\t"
				"vMOVDQU 12*8(%[x0]), %%ymm3       \n\t"
				"vPADDQ  %%ymm9, %%ymm13, %%ymm13  \n\t"
				"\n\t"
				"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vMOVDQU  0*8(%[x1]), %%ymm4        \n\t"
				"vMOVDQU  4*8(%[x1]), %%ymm5        \n\t"
				"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vMOVDQU  8*8(%[x1]), %%ymm6        \n\t"
				"vMOVDQU 12*8(%[x1]), %%ymm7        \n\t"
				"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x0]                 \n\t"
				"subq $-16*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm0, %%ymm1, %%ymm8    \n\t"
			"vPADDQ  %%ymm8, %%ymm12, %%ymm12  \n\t"
			"\n\t"
			"vPADDQ  %%ymm2, %%ymm3, %%ymm9    \n\t"
			"vPADDQ  %%ymm9, %%ymm13, %%ymm13  \n\t"
			"\n\t"
			"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vPADDQ  %%ymm14, %%ymm15, %%ymm15  \n\t"
			"vPADDQ  %%ymm12, %%ymm13, %%ymm14  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x0]                \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]     \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		jj=jj+c_step;
		__asm__ __volatile__ (
			"vpxor  %%ymm14, %%ymm14, %%ymm14 \n\t"
			"vpxor  %%ymm15, %%ymm15, %%ymm15 \n\t"
			"vMOVDQU  0*8(%[x1]), %%ymm4      \n\t"
			"vMOVDQU  4*8(%[x1]), %%ymm5      \n\t"
			"vMOVDQU  8*8(%[x1]), %%ymm6      \n\t"
			"vMOVDQU 12*8(%[x1]), %%ymm7      \n\t"
			"subq $-16*8, %[x1]               \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>4), n_remain=n%16;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
				"vMOVDQU  0*8(%[x1]), %%ymm4        \n\t"
				"vMOVDQU  4*8(%[x1]), %%ymm5        \n\t"
				"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
				"\n\t"
				"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
				"vMOVDQU  8*8(%[x1]), %%ymm6        \n\t"
				"vMOVDQU 12*8(%[x1]), %%ymm7        \n\t"
				"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"subq $-16*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%ymm4, %%ymm5, %%ymm10    \n\t"
			"vPADDQ  %%ymm10, %%ymm14, %%ymm14  \n\t"
			"\n\t"
			"vPADDQ  %%ymm6, %%ymm7, %%ymm11    \n\t"
			"vPADDQ  %%ymm11, %%ymm15, %%ymm15  \n\t"
			"\n\t"
			"vPADDQ  %%ymm14, %%ymm15, %%ymm15  \n\t"
			::
		);

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]     \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp1;
	}
}

// --------------------------------------------------------------------------------
void sum_up_matrix_32_02_ASM_r8(double *x_sum, double *x0, int64_t n, int64_t c){
	double *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"VXORPD %%zmm12, %%zmm12, %%zmm12 \n\t"
			"VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
			"VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
			"VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
			"vmovupd  0*8(%[x0]), %%zmm0      \n\t"
			"vmovupd  8*8(%[x0]), %%zmm1      \n\t"
			"vmovupd 16*8(%[x0]), %%zmm2      \n\t"
			"vmovupd 24*8(%[x0]), %%zmm3      \n\t"
			"vmovupd  0*8(%[x1]), %%zmm4      \n\t"
			"vmovupd  8*8(%[x1]), %%zmm5      \n\t"
			"vmovupd 16*8(%[x1]), %%zmm6      \n\t"
			"vmovupd 24*8(%[x1]), %%zmm7      \n\t"
			"subq $-32*8, %[x0]               \n\t"
			"subq $-32*8, %[x1]               \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%zmm0, %%zmm1, %%zmm8    \n\t"
				"vmovupd  0*8(%[x0]), %%zmm0       \n\t"
				"vmovupd  8*8(%[x0]), %%zmm1       \n\t"
				"vaddpd  %%zmm8, %%zmm12, %%zmm12  \n\t"
				"\n\t"
				"vaddpd  %%zmm2, %%zmm3, %%zmm9    \n\t"
				"vmovupd 16*8(%[x0]), %%zmm2       \n\t"
				"vmovupd 24*8(%[x0]), %%zmm3       \n\t"
				"vaddpd  %%zmm9, %%zmm13, %%zmm13  \n\t"
				"\n\t"
				"vaddpd  %%zmm4, %%zmm5, %%zmm10    \n\t"
				"vmovupd  0*8(%[x1]), %%zmm4        \n\t"
				"vmovupd  8*8(%[x1]), %%zmm5        \n\t"
				"vaddpd  %%zmm10, %%zmm14, %%zmm14  \n\t"
				"\n\t"
				"vaddpd  %%zmm6, %%zmm7, %%zmm11    \n\t"
				"vmovupd 16*8(%[x1]), %%zmm6        \n\t"
				"vmovupd 24*8(%[x1]), %%zmm7        \n\t"
				"vaddpd  %%zmm11, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x0]                 \n\t"
				"subq $-32*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%zmm0, %%zmm1, %%zmm8    \n\t"
			"vaddpd  %%zmm8, %%zmm12, %%zmm12  \n\t"
			"\n\t"
			"vaddpd  %%zmm2, %%zmm3, %%zmm9    \n\t"
			"vaddpd  %%zmm9, %%zmm13, %%zmm13  \n\t"
			"\n\t"
			"vaddpd  %%zmm4, %%zmm5, %%zmm10    \n\t"
			"vaddpd  %%zmm10, %%zmm14, %%zmm14  \n\t"
			"\n\t"
			"vaddpd  %%zmm6, %%zmm7, %%zmm11    \n\t"
			"vaddpd  %%zmm11, %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vaddpd  %%zmm14, %%zmm15, %%zmm15  \n\t"
			"vaddpd  %%zmm12, %%zmm13, %%zmm14  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%zmm0       \n\t"
				"vmovupd 0*8(%[x1]), %%zmm1       \n\t"
				"vaddpd %%zmm0, %%zmm14, %%zmm14  \n\t"
				"vaddpd %%zmm1, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"vmovupd 8*8(%[x0]), %%zmm0       \n\t"
				"vmovupd 8*8(%[x1]), %%zmm1       \n\t"
				"vaddpd %%zmm0, %%zmm14, %%zmm14  \n\t"
				"vaddpd %%zmm1, %%zmm15, %%zmm15  \n\t"
				"subq $-16*8, %[x0]                \n\t"
				"subq $-16*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vmovupd 4*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 4*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x0]                \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x0]), %%ymm0       \n\t"
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x0]), %%xmm0 \n\t"
				"addpd %%xmm0, %%xmm14     \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		double tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm14, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm14, %%ymm1  \n\t"
			"vaddpd      %%ymm0, %%ymm1,  %%ymm14 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm14, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t0]    \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vaddpd      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]    \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		__asm__ __volatile__ (
			"VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
			"VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
			"vmovupd  0*8(%[x1]), %%zmm4      \n\t"
			"vmovupd  8*8(%[x1]), %%zmm5      \n\t"
			"vmovupd 16*8(%[x1]), %%zmm6      \n\t"
			"vmovupd 24*8(%[x1]), %%zmm7      \n\t"
			"subq $-32*8, %[x1]               \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vaddpd  %%zmm4, %%zmm5, %%zmm10    \n\t"
				"vmovupd  0*8(%[x1]), %%zmm4        \n\t"
				"vmovupd  8*8(%[x1]), %%zmm5        \n\t"
				"vaddpd  %%zmm10, %%zmm14, %%zmm14  \n\t"
				"\n\t"
				"vaddpd  %%zmm6, %%zmm7, %%zmm11    \n\t"
				"vmovupd 16*8(%[x1]), %%zmm6        \n\t"
				"vmovupd 24*8(%[x1]), %%zmm7        \n\t"
				"vaddpd  %%zmm11, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vaddpd  %%zmm4, %%zmm5, %%zmm10    \n\t"
			"vaddpd  %%zmm10, %%zmm14, %%zmm14  \n\t"
			"\n\t"
			"vaddpd  %%zmm6, %%zmm7, %%zmm11    \n\t"
			"vaddpd  %%zmm11, %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vaddpd  %%zmm14, %%zmm15, %%zmm15  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%zmm1       \n\t"
				"vaddpd %%zmm1, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"vmovupd 8*8(%[x1]), %%zmm1       \n\t"
				"vaddpd %%zmm1, %%zmm15, %%zmm15  \n\t"
				"subq $-16*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vmovupd 4*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vmovupd 0*8(%[x1]), %%ymm1       \n\t"
				"vaddpd %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"movupd 0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"movsd  0*8(%[x1]), %%xmm1 \n\t"
				"addpd %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		double tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vaddpd      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTF64X2  $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTF64X2  $1,  %%ymm15, %%xmm1  \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movupd              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"addpd               %%xmm1,  %%xmm0  \n\t"
			"movsd               %%xmm0, %[t1]    \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		x_sum[jj+c_step]=tmp1;
	}
}

void sum_up_matrix_32_02_ASM_i8(int64_t *x_sum, int64_t *x0, int64_t n, int64_t c){
	int64_t *x1;
	int64_t c_unroll=(c>>1), c_remain=c%2, c_step=(c>>1), jj=0;
	x1 = x0;
	x1 += c_step * n;
	while(c_unroll--){
		__asm__ __volatile__ (
			"VXORPD %%zmm12, %%zmm12, %%zmm12 \n\t"
			"VXORPD %%zmm13, %%zmm13, %%zmm13 \n\t"
			"VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
			"VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
			"VMOVDQU64  0*8(%[x0]), %%zmm0      \n\t"
			"VMOVDQU64  8*8(%[x0]), %%zmm1      \n\t"
			"VMOVDQU64 16*8(%[x0]), %%zmm2      \n\t"
			"VMOVDQU64 24*8(%[x0]), %%zmm3      \n\t"
			"VMOVDQU64  0*8(%[x1]), %%zmm4      \n\t"
			"VMOVDQU64  8*8(%[x1]), %%zmm5      \n\t"
			"VMOVDQU64 16*8(%[x1]), %%zmm6      \n\t"
			"VMOVDQU64 24*8(%[x1]), %%zmm7      \n\t"
			"subq $-32*8, %[x0]               \n\t"
			"subq $-32*8, %[x1]               \n\t"
			:[x0]"=r"(x0), [x1]"=r"(x1)
			:"0"(x0), "1"(x1)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%zmm0, %%zmm1, %%zmm8    \n\t"
				"VMOVDQU64  0*8(%[x0]), %%zmm0       \n\t"
				"VMOVDQU64  8*8(%[x0]), %%zmm1       \n\t"
				"vPADDQ  %%zmm8, %%zmm12, %%zmm12  \n\t"
				"\n\t"
				"vPADDQ  %%zmm2, %%zmm3, %%zmm9    \n\t"
				"VMOVDQU64 16*8(%[x0]), %%zmm2       \n\t"
				"VMOVDQU64 24*8(%[x0]), %%zmm3       \n\t"
				"vPADDQ  %%zmm9, %%zmm13, %%zmm13  \n\t"
				"\n\t"
				"vPADDQ  %%zmm4, %%zmm5, %%zmm10    \n\t"
				"VMOVDQU64  0*8(%[x1]), %%zmm4        \n\t"
				"VMOVDQU64  8*8(%[x1]), %%zmm5        \n\t"
				"vPADDQ  %%zmm10, %%zmm14, %%zmm14  \n\t"
				"\n\t"
				"vPADDQ  %%zmm6, %%zmm7, %%zmm11    \n\t"
				"VMOVDQU64 16*8(%[x1]), %%zmm6        \n\t"
				"VMOVDQU64 24*8(%[x1]), %%zmm7        \n\t"
				"vPADDQ  %%zmm11, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x0]                 \n\t"
				"subq $-32*8, %[x1]                 \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%zmm0, %%zmm1, %%zmm8    \n\t"
			"vPADDQ  %%zmm8, %%zmm12, %%zmm12  \n\t"
			"\n\t"
			"vPADDQ  %%zmm2, %%zmm3, %%zmm9    \n\t"
			"vPADDQ  %%zmm9, %%zmm13, %%zmm13  \n\t"
			"\n\t"
			"vPADDQ  %%zmm4, %%zmm5, %%zmm10    \n\t"
			"vPADDQ  %%zmm10, %%zmm14, %%zmm14  \n\t"
			"\n\t"
			"vPADDQ  %%zmm6, %%zmm7, %%zmm11    \n\t"
			"vPADDQ  %%zmm11, %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vPADDQ  %%zmm14, %%zmm15, %%zmm15  \n\t"
			"vPADDQ  %%zmm12, %%zmm13, %%zmm14  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"VMOVDQU64 0*8(%[x0]), %%zmm0       \n\t"
				"VMOVDQU64 0*8(%[x1]), %%zmm1       \n\t"
				"vPADDQ %%zmm0, %%zmm14, %%zmm14  \n\t"
				"vPADDQ %%zmm1, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"VMOVDQU64 8*8(%[x0]), %%zmm0       \n\t"
				"VMOVDQU64 8*8(%[x1]), %%zmm1       \n\t"
				"vPADDQ %%zmm0, %%zmm14, %%zmm14  \n\t"
				"vPADDQ %%zmm1, %%zmm15, %%zmm15  \n\t"
				"subq $-16*8, %[x0]                \n\t"
				"subq $-16*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vMOVDQU 4*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x0]                \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x0]), %%ymm0       \n\t"
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm0, %%ymm14, %%ymm14  \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x0]                \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x0]         \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm0, %%xmm0  \n\t"
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x0]), %%xmm0 \n\t"
				"PADDQ %%xmm0, %%xmm14     \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x0]         \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x0]"=r"(x0), [x1]"=r"(x1)
				:"0"(x0), "1"(x1)
			);
		}

		// Extract Result
		int64_t tmp0;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm14, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm14, %%ymm1  \n\t"
			"vPADDQ      %%ymm0, %%ymm1,  %%ymm14 \n\t"
			"\n\t"
			"VEXTRACTI128   $0,  %%ymm14, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm14, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t0]    \n\t"
			:[t0]"=m"(tmp0)
			:
		);

		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vPADDQ      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]    \n\t"
			:[t1]"=m"(tmp1)
			:
		);

		x_sum[jj]=tmp0;
		x_sum[jj+c_step]=tmp1;
		jj++;
	}

	if (c_remain>0){
		__asm__ __volatile__ (
			"VXORPD %%zmm14, %%zmm14, %%zmm14 \n\t"
			"VXORPD %%zmm15, %%zmm15, %%zmm15 \n\t"
			"VMOVDQU64  0*8(%[x1]), %%zmm4      \n\t"
			"VMOVDQU64  8*8(%[x1]), %%zmm5      \n\t"
			"VMOVDQU64 16*8(%[x1]), %%zmm6      \n\t"
			"VMOVDQU64 24*8(%[x1]), %%zmm7      \n\t"
			"subq $-32*8, %[x1]               \n\t"
			:[x1]"=r"(x1)
			:"0"(x1)
		);

		int64_t n_unroll=(n>>5), n_remain=n%32;
		n_unroll--;
		while( n_unroll-- ){ 
			__asm__ __volatile__ (
				"vPADDQ  %%zmm4, %%zmm5, %%zmm10    \n\t"
				"VMOVDQU64  0*8(%[x1]), %%zmm4        \n\t"
				"VMOVDQU64  8*8(%[x1]), %%zmm5        \n\t"
				"vPADDQ  %%zmm10, %%zmm14, %%zmm14  \n\t"
				"\n\t"
				"vPADDQ  %%zmm6, %%zmm7, %%zmm11    \n\t"
				"VMOVDQU64 16*8(%[x1]), %%zmm6        \n\t"
				"VMOVDQU64 24*8(%[x1]), %%zmm7        \n\t"
				"vPADDQ  %%zmm11, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"subq $-32*8, %[x1]                 \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		__asm__ __volatile__ (
			"vPADDQ  %%zmm4, %%zmm5, %%zmm10    \n\t"
			"vPADDQ  %%zmm10, %%zmm14, %%zmm14  \n\t"
			"\n\t"
			"vPADDQ  %%zmm6, %%zmm7, %%zmm11    \n\t"
			"vPADDQ  %%zmm11, %%zmm15, %%zmm15  \n\t"
			"\n\t"
			"vPADDQ  %%zmm14, %%zmm15, %%zmm15  \n\t"
			::
		);

		if(n_remain & 16){
			__asm__ __volatile__ (
				"VMOVDQU64 0*8(%[x1]), %%zmm1       \n\t"
				"vPADDQ %%zmm1, %%zmm15, %%zmm15  \n\t"
				"\n\t"
				"VMOVDQU64 8*8(%[x1]), %%zmm1       \n\t"
				"vPADDQ %%zmm1, %%zmm15, %%zmm15  \n\t"
				"subq $-16*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 8){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"\n\t"
				"vMOVDQU 4*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-8*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 4){
			__asm__ __volatile__ (
				"vMOVDQU 0*8(%[x1]), %%ymm1       \n\t"
				"vPADDQ %%ymm1, %%ymm15, %%ymm15  \n\t"
				"subq $-4*8, %[x1]                \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 2){
			__asm__ __volatile__ (
				"MOVDQU 0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-2*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		if(n_remain & 1){
			__asm__ __volatile__ (
				"pxor      %%xmm1, %%xmm1  \n\t"
				"MOVQ   0*8(%[x1]), %%xmm1 \n\t"
				"PADDQ %%xmm1, %%xmm15     \n\t"
				"subq $-1*8, %[x1]         \n\t"
				:[x1]"=r"(x1)
				:"0"(x1)
			);
		}

		// Extract Result
		int64_t tmp1;
		__asm__ __volatile__ (
			"VEXTRACTF64X4  $0,  %%zmm15, %%ymm0  \n\t"
			"VEXTRACTF64X4  $1,  %%zmm15, %%ymm1  \n\t"
			"vPADDQ      %%ymm0, %%ymm1,  %%ymm15 \n\t"
			"\n\t"
			"VEXTRACTI128   $0,  %%ymm15, %%xmm0  \n\t"
			"VEXTRACTI128   $1,  %%ymm15, %%xmm1  \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVDQU              %%xmm0,  %%xmm1  \n\t"
			"PSRLDQ         $8,  %%xmm1           \n\t"
			"PADDQ               %%xmm1,  %%xmm0  \n\t"
			"MOVQ                %%xmm0, %[t1]    \n\t"
			:[t1]"=m"(tmp1)
			:
		);
		x_sum[jj+c_step]=tmp1;		
	}
}
