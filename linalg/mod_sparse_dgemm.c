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


void sparse_dgemm_ver0(
        double c[], 
        int64_t rows[], int64_t cols[], double vals[], 
        double b[],
        int64_t n, int64_t m, int64_t k, int64_t l
    ){

    // int64_t ll_block = (l>>4)<<4;
    // int64_t kk_block = (k>>3)<<3;

    // for(int64_t ii=0; ii<m; ii++){
    //     int64_t ini = rows[ii];
    //     int64_t fin = rows[ii+1];
    //     for(int64_t ll=0; ll<ll_block; ll+=16){
    //         __m512d zmm_accum0 = _mm512_setzero_pd();
    //         __m512d zmm_accum1 = _mm512_setzero_pd();
    //         for(int64_t kk=ini; kk<fin; kk++){
    //             int64_t idx = cols[kk];
    //             double  val = vals[kk];

    //             __m512d zmm_val = _mm512_set1_pd(val);

    //             __m512d zmm_b0 = _mm512_loadu_pd(&b[idx*l+ll+0]);
    //             __m512d zmm_b1 = _mm512_loadu_pd(&b[idx*l+ll+8]);
    //             zmm_accum0 = _mm512_fmadd_pd(zmm_b0, zmm_val, zmm_accum0);
    //             zmm_accum1 = _mm512_fmadd_pd(zmm_b1, zmm_val, zmm_accum1);
    //         }
    //         _mm512_storeu_pd(&c[ii*l+ll+0], zmm_accum0);
    //         _mm512_storeu_pd(&c[ii*l+ll+8], zmm_accum1);
    //     }
    // }

    
    int64_t ll_block = (l>>3)<<3;
    int64_t kk_block = (k>>3)<<3;

    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        for(int64_t ll=0; ll<ll_block; ll+=8){
            __m512d zmm_accum = _mm512_setzero_pd();
            for(int64_t kk=ini; kk<fin; kk++){
                int64_t idx = cols[kk];
                double  val = vals[kk];

                __m512d zmm_val = _mm512_set1_pd(val);

                __m512d zmm_b = _mm512_loadu_pd(&b[idx*l+ll]);
                zmm_accum = _mm512_fmadd_pd(zmm_b, zmm_val, zmm_accum);
            }
            _mm512_storeu_pd(&c[ii*l+ll], zmm_accum);
        }
        // return;
    }
}

void sparse_dgemm(
        double c[], 
        int64_t rows[], int64_t cols[], double vals[], 
        double b[],
        int64_t n, int64_t m, int64_t k, int64_t l, int64_t n_jobs
    ){

    int64_t ll_block = (l>>4)<<4;
    int64_t kk_block = (k>>3)<<3;

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        for(int64_t ll=0; ll<ll_block; ll+=16){
            __m512d zmm_accum0 = _mm512_setzero_pd();
            __m512d zmm_accum1 = _mm512_setzero_pd();
            for(int64_t kk=ini; kk<fin; kk++){
                int64_t idx = cols[kk];
                double  val = vals[kk];

                __m512d zmm_val = _mm512_set1_pd(val);

                __m512d zmm_b0 = _mm512_loadu_pd(&b[idx*l+ll+0]);
                __m512d zmm_b1 = _mm512_loadu_pd(&b[idx*l+ll+8]);
                zmm_accum0 = _mm512_fmadd_pd(zmm_b0, zmm_val, zmm_accum0);
                zmm_accum1 = _mm512_fmadd_pd(zmm_b1, zmm_val, zmm_accum1);
            }
            _mm512_storeu_pd(&c[ii*l+ll+0], zmm_accum0);
            _mm512_storeu_pd(&c[ii*l+ll+8], zmm_accum1);
        }
    }


    // int64_t ll_block = (l>>3)<<3;
    // int64_t kk_block = (k>>3)<<3;

    // omp_set_num_threads(n_jobs);
    // #pragma omp parallel for
    // for(int64_t ii=0; ii<m; ii++){
    //     int64_t ini = rows[ii];
    //     int64_t fin = rows[ii+1];
    //     for(int64_t ll=0; ll<ll_block; ll+=8){
    //         __m512d zmm_accum = _mm512_setzero_pd();
    //         for(int64_t kk=ini; kk<fin; kk++){
    //             int64_t idx = cols[kk];
    //             double  val = vals[kk];

    //             __m512d zmm_val = _mm512_set1_pd(val);

    //             __m512d zmm_b = _mm512_loadu_pd(&b[idx*l+ll]);
    //             zmm_accum = _mm512_fmadd_pd(zmm_b, zmm_val, zmm_accum);
    //         }
    //         _mm512_storeu_pd(&c[ii*l+ll], zmm_accum);
    //     }
    // }
}


void sparse_dgemm_ver2(
        double c[], 
        int64_t rows[], int64_t cols[], double vals[], 
        double b[],
        int64_t n, int64_t m, int64_t k, int64_t l, int64_t n_jobs
    ){

    int64_t ll_block = (l>>4)<<4;
    int64_t kk_block = (k>>3)<<3;

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        for(int64_t ll=0; ll<ll_block; ll+=16){
            __m512d zmm_accum0 = _mm512_setzero_pd();
            __m512d zmm_accum1 = _mm512_setzero_pd();
            for(int64_t kk=ini; kk<fin; kk++){
                int64_t idx = cols[kk];
                double  val = vals[kk];

                __m512d zmm_val = _mm512_set1_pd(val);

                __m512d zmm_b0 = _mm512_loadu_pd(&b[idx*l+ll+0]);
                __m512d zmm_b1 = _mm512_loadu_pd(&b[idx*l+ll+8]);
                zmm_accum0 = _mm512_fmadd_pd(zmm_b0, zmm_val, zmm_accum0);
                zmm_accum1 = _mm512_fmadd_pd(zmm_b1, zmm_val, zmm_accum1);
            }
            _mm512_storeu_pd(&c[ii*l+ll+0], zmm_accum0);
            _mm512_storeu_pd(&c[ii*l+ll+8], zmm_accum1);
        }
    }


    // int64_t ll_block = (l>>3)<<3;
    // int64_t kk_block = (k>>3)<<3;

    // omp_set_num_threads(n_jobs);
    // #pragma omp parallel for
    // for(int64_t ii=0; ii<m; ii++){
    //     int64_t ini = rows[ii];
    //     int64_t fin = rows[ii+1];
    //     for(int64_t ll=0; ll<ll_block; ll+=8){
    //         __m512d zmm_accum = _mm512_setzero_pd();
    //         for(int64_t kk=ini; kk<fin; kk++){
    //             int64_t idx = cols[kk];
    //             double  val = vals[kk];

    //             __m512d zmm_val = _mm512_set1_pd(val);

    //             __m512d zmm_b = _mm512_loadu_pd(&b[idx*l+ll]);
    //             zmm_accum = _mm512_fmadd_pd(zmm_b, zmm_val, zmm_accum);
    //         }
    //         _mm512_storeu_pd(&c[ii*l+ll], zmm_accum);
    //     }
    // }
}


