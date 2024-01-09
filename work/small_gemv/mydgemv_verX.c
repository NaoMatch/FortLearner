#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <omp.h>
#include <immintrin.h>
#include <xmmintrin.h>

double sum_zmm_elements_verX(__m512d zmmX){
    __m256d ymm_upper = _mm512_extractf64x4_pd(zmmX, 1);
    __m256d ymm_lower = _mm512_extractf64x4_pd(zmmX, 0);

    __m256d ymm_sum = _mm256_add_pd(ymm_upper, ymm_lower);

    __m128d xmm_sum = _mm_add_pd(_mm256_extractf128_pd(ymm_sum, 1), _mm256_extractf128_pd(ymm_sum, 0));

    __m128d xmm_high_low = _mm_add_pd(xmm_sum, _mm_unpackhi_pd(xmm_sum, xmm_sum));

    double result;
    _mm_store_sd(&result, xmm_high_low);

    return result;
} 

void sum_up_vectors(double vec1[], double vec2[], int64_t lda){
    __m512d zmm1, zmm2;
    int64_t i_remain = lda % 8;

    for (int64_t i=0; i<lda-i_remain; i+=8){
        zmm1 = _mm512_loadu_pd(&vec1[i]);
        zmm2 = _mm512_loadu_pd(&vec2[i]);
        zmm1 = _mm512_add_pd(zmm1, zmm2);
        _mm512_storeu_pd(&vec1[i], zmm1);
    }

    for (int64_t i=lda-i_remain; i<lda; i++){
        vec1[i] += vec2[i];
    }
}

double sum_up_vector(double tmp[], int64_t ldx){
    __m512d zmm0, zmm1;

    zmm0 = _mm512_setzero_pd();
    for (int64_t k=0; k<ldx; k+=8){
        zmm1 = _mm512_loadu_pd(&tmp[k]);
        zmm0 = _mm512_add_pd(zmm1, zmm0);
    }

    return (sum_zmm_elements_verX(zmm0));
}

void mydgemv_n_verX(
    double a[], double tmp[], double x[], double y[], 
    int64_t lda, int64_t ldx, int64_t ldy){
    for (int64_t i=0; i<lda; i++){
        y[i] = 0.0;
    }

    int64_t i_remain = lda % 8;
    __m512d zmm0, zmm1, zmm2; 
    for (int64_t k=0; k<ldx; k++){
        double vec[8], tmp_x = x[k];
        for (int64_t i = 0; i < 8; i++) {
            vec[i] = tmp_x;
        }

        zmm0 = _mm512_loadu_pd(vec);
        for (int64_t i=0; i<lda-i_remain; i+=8){
            zmm1 = _mm512_loadu_pd(&a[i]);
            zmm1 = _mm512_mul_pd(zmm0, zmm1);
            _mm512_storeu_pd(&tmp[i], zmm1);
        }

        for (int64_t i=lda-i_remain; i<lda; i++){
            tmp[i] = a[i] * x[k];
        }

        sum_up_vectors(y, tmp, lda);
        a += lda;
    }
}

void mydgemv_t_verX(
    double a_t[], double tmp_t[], double x[], double y[], 
    int64_t lda, int64_t ldx, int64_t ldy){
    
    __m512d zmm0, zmm1; 
    for (int64_t i=0; i<lda; i++){
        for (int64_t k=0; k<ldx; k+=8){
            zmm0 = _mm512_loadu_pd(&x[k]);

            zmm1 = _mm512_loadu_pd(&a_t[k]);

            zmm1 = _mm512_mul_pd(zmm1, zmm0);

            _mm512_storeu_pd(&tmp_t[k], zmm1);            
        }

        y[i] = sum_up_vector(tmp_t, ldx);
        a_t += ldx;
    }
}
