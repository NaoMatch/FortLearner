#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <omp.h>
#include <immintrin.h>
#include <xmmintrin.h>
#define N_UNROLL (16)
#define NUM_ELES_IN_ZMM (8)


void prefix_sum_naive(double a[], double b[], double tmp, int64_t n){
    for(int i=0; i<n; i++){
        tmp += a[i];
        b[i] = tmp;
    }
}

void prefix_sum_4elem(double a[], double b[], __m256d ymm_base){
    __m256d ymm_f, ymm_tmp;
    __m256i idx;
    idx = _mm256_set1_epi64x(1);

    // Prefix sum per 4 elements
    ymm_f = _mm256_loadu_pd(&a[0]);

    ymm_tmp = _mm256_maskz_permute_pd(0xA, ymm_f, 0x0);
    ymm_f = _mm256_add_pd(ymm_f, ymm_tmp);

    ymm_tmp = _mm256_maskz_permutexvar_pd(0xC, idx, ymm_f);
    ymm_f = _mm256_add_pd(ymm_f, ymm_tmp);

    ymm_f = _mm256_add_pd(ymm_f, ymm_base);
    _mm256_storeu_pd(&b[0], ymm_f);
}

void prefix_sum(double a[], double b[], int64_t n){
    double s=0;
    __m256d ymm_base = _mm256_set1_pd(0.0);
    prefix_sum_4elem(&a[0], &b[0], ymm_base);
    ymm_base = _mm256_set1_pd(b[3]);
    a += 4;
    b += 4;

    int64_t n_block=(n/4)*4;
    for (int i=4; i<n_block; i+=4){
        prefix_sum_4elem(&a[0], &b[0], ymm_base);
        ymm_base = _mm256_set1_pd(b[3]);
        a += 4;
        b += 4;
    }

    int64_t n_remain=n-n_block;
    s=b[-1];
    prefix_sum_naive(a, b, s, n_remain);
}


void prefix_sum_omp_simd(double a[], double b[], int64_t n){
    double tmp = 0.0;
#pragma omp simd reduction(+:tmp)
    for(int64_t i=0; i<n; i++){
        tmp += a[i];
#pragma omp scan inclusive(tmp)
        b[i] = tmp;
    }
}


void prefix_sum_simd(double a[], double b[], int64_t n){
    
    __m512d zmm0, zmm1, zmm2, zmm3, zmm4;
    __m512d zmm5, zmm6, zmm7, zmm8, zmm9;
    __m512d zmm10, zmm11;
    __m512d zmm_acc, zmm_tmp1, zmm_tmp2;
    __m512i idx, acc_idx;

    int64_t n_block = (n/N_UNROLL) * N_UNROLL;

    zmm_acc = _mm512_set1_pd(0.0);
    acc_idx = _mm512_set1_epi64(7);
    idx     = _mm512_set1_epi64(3);

    for(int i=0; i<n_block; i+=N_UNROLL){
        zmm0  = _mm512_loadu_pd(&a[0]);
        zmm5  = _mm512_loadu_pd(&a[0+NUM_ELES_IN_ZMM]);

        zmm2  = _mm512_maskz_permute_pd(0xAA, zmm0, 0x00);
        zmm7  = _mm512_maskz_permute_pd(0xAA, zmm5, 0x00);
        zmm10 = _mm512_add_pd(zmm0, zmm2);
        zmm11 = _mm512_add_pd(zmm5, zmm7);

        zmm1  = _mm512_maskz_permutex_pd(0xCC, zmm0, 0x40);
        zmm6  = _mm512_maskz_permutex_pd(0xCC, zmm5, 0x40);
        zmm10 = _mm512_add_pd(zmm10, zmm1);
        zmm11 = _mm512_add_pd(zmm11, zmm6);

        zmm1  = _mm512_maskz_permute_pd(0xCC, zmm1, 0x44);
        zmm6  = _mm512_maskz_permute_pd(0xCC, zmm6, 0x44);
        zmm10 = _mm512_add_pd(zmm10, zmm1);
        zmm11 = _mm512_add_pd(zmm11, zmm6);

        zmm_tmp1 = _mm512_maskz_permutexvar_pd(0xF0, idx, zmm10);
        zmm_tmp2 = _mm512_maskz_permutexvar_pd(0xF0, idx, zmm11);
        zmm10 = _mm512_add_pd(zmm10, zmm_tmp1);
        zmm11 = _mm512_add_pd(zmm11, zmm_tmp2);

        zmm_tmp1 = _mm512_add_pd(zmm10, zmm_acc);
        zmm_acc = _mm512_add_pd(zmm11, zmm_tmp1);
        zmm_acc = _mm512_permutexvar_pd(acc_idx, zmm_acc);

        zmm_tmp2 = _mm512_permutexvar_pd(acc_idx, zmm_tmp1);
        _mm512_storeu_pd(&b[0], zmm_tmp1);

        zmm11 = _mm512_add_pd(zmm11, zmm_tmp2);
        _mm512_storeu_pd(&b[0+NUM_ELES_IN_ZMM], zmm11);

        a += N_UNROLL;
        b += N_UNROLL;
    }

    int64_t n_remain=n-n_block;
    if (n_remain>0){
        double s=b[-1];
        prefix_sum_naive(a, b, s, n_remain);
    }
}

void prefix_sum_simd_2(double a[], double b[], int64_t n){
    
    __m512d zmm0, zmm1, zmm2, zmm3, zmm4;
    __m512d zmm5, zmm6, zmm7, zmm8, zmm9;
    __m512d zmm10, zmm11;
    __m512d zmm_acc, zmm_tmp1, zmm_tmp2;
    __m512i idx, acc_idx;

    int64_t n_block = (n/N_UNROLL) * N_UNROLL;

    zmm_acc = _mm512_set1_pd(0.0);
    acc_idx = _mm512_set1_epi64(7);
    idx     = _mm512_set1_epi64(3);

    zmm0  = _mm512_loadu_pd(&a[0]);
    zmm5  = _mm512_loadu_pd(&a[0+NUM_ELES_IN_ZMM]);

    for(int i=0; i<n_block; i+=N_UNROLL){

        zmm2  = _mm512_maskz_permute_pd(0xAA, zmm0, 0x00);
        zmm7  = _mm512_maskz_permute_pd(0xAA, zmm5, 0x00);
        zmm10 = _mm512_add_pd(zmm0, zmm2);
        zmm11 = _mm512_add_pd(zmm5, zmm7);

        zmm1  = _mm512_maskz_permutex_pd(0xCC, zmm0, 0x40);
        zmm6  = _mm512_maskz_permutex_pd(0xCC, zmm5, 0x40);
        zmm10 = _mm512_add_pd(zmm10, zmm1);
        zmm11 = _mm512_add_pd(zmm11, zmm6);

        zmm1  = _mm512_maskz_permute_pd(0xCC, zmm1, 0x44);
        zmm6  = _mm512_maskz_permute_pd(0xCC, zmm6, 0x44);
        zmm10 = _mm512_add_pd(zmm10, zmm1);
        zmm11 = _mm512_add_pd(zmm11, zmm6);

        zmm_tmp1 = _mm512_maskz_permutexvar_pd(0xF0, idx, zmm10);
        zmm_tmp2 = _mm512_maskz_permutexvar_pd(0xF0, idx, zmm11);
        zmm10 = _mm512_add_pd(zmm10, zmm_tmp1);
        zmm11 = _mm512_add_pd(zmm11, zmm_tmp2);

        zmm_tmp1 = _mm512_add_pd(zmm10, zmm_acc);
        zmm_tmp2 = _mm512_permutexvar_pd(acc_idx, zmm_tmp1);

        zmm_acc = _mm512_add_pd(zmm11, zmm_tmp1);
        zmm_acc = _mm512_permutexvar_pd(acc_idx, zmm_acc);

        zmm11 = _mm512_add_pd(zmm11, zmm_tmp2);

        _mm512_storeu_pd(&b[0], zmm_tmp1);
        _mm512_storeu_pd(&b[0+NUM_ELES_IN_ZMM], zmm11);

        a += N_UNROLL;
        b += N_UNROLL;

        zmm0  = _mm512_loadu_pd(&a[0]);
        zmm5  = _mm512_loadu_pd(&a[0+NUM_ELES_IN_ZMM]);
    }
    
    int64_t n_remain=n-n_block;
    if (n_remain>0){
        double s=b[-1];
        prefix_sum_naive(a, b, s, n_remain);
    }
}
