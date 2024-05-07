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

void sparse_mv_ver0(
        double vec[], int64_t cols[], double vals[], 
        double b_t[], 
        int64_t n, int64_t m, int64_t k, int64_t l
    ){

    __m512d zmm_vec, zmm_val, zmm_b_t;
    __m512d zmm_vec0, zmm_b_t0;
    __m512d zmm_vec1, zmm_b_t1;
    int64_t l_block = (l>>4)<<4;
    int64_t idx;
    double val;
    double *b_t_ptr;

    for(int64_t i=0; i<n; i++){
        idx = cols[i]-1;
        val = vals[i];
        zmm_val = _mm512_set1_pd(val);
        b_t_ptr = &b_t[idx*l];

        // for(int64_t j=0; j<l_block; j+=16){
        //     zmm_vec0 = _mm512_loadu_pd(&vec[0]);
        //     zmm_b_t0 = _mm512_loadu_pd(&b_t_ptr[0]);

        //     zmm_vec1 = _mm512_loadu_pd(&vec[8]);
        //     zmm_b_t1 = _mm512_loadu_pd(&b_t_ptr[8]);

        //     zmm_vec0 = _mm512_fmadd_pd(zmm_b_t0, zmm_val, zmm_vec0);
        //     zmm_vec1 = _mm512_fmadd_pd(zmm_b_t1, zmm_val, zmm_vec1);

        //     _mm512_storeu_pd(&vec[0], zmm_vec0);
        //     _mm512_storeu_pd(&vec[8], zmm_vec1);

        //     vec += 16;
        //     b_t_ptr += 16;
        // }
        for(int64_t j=0; j<l_block; j+=8){
            zmm_vec = _mm512_loadu_pd(&vec[0]);
            zmm_b_t = _mm512_loadu_pd(&b_t_ptr[0]);

            zmm_vec = _mm512_fmadd_pd(zmm_b_t, zmm_val, zmm_vec);

            _mm512_storeu_pd(&vec[0], zmm_vec);

            vec += 8;
            b_t_ptr += 8;
        }
        vec -= l_block;
    }
}

void sparse_mv_ver1(
        double vec[], int64_t cols[], double vals[], 
        double b_t[], 
        int64_t n, int64_t m, int64_t k, int64_t l
    ){

    __m512d zmm_vec, zmm_val, zmm_b_t;
    __m512d zmm_vec0, zmm_b_t0;
    __m512d zmm_vec1, zmm_b_t1;
    int64_t l_block = (l>>4)<<4;
    int64_t idx;
    double val;
    double *b_t_ptr;

    for(int64_t i=0; i<n; i++){
        idx = cols[i]-1;
        val = vals[i];
        zmm_val = _mm512_set1_pd(val);
        b_t_ptr = &b_t[idx*l];

        // for(int64_t j=0; j<l_block; j+=16){
        //     zmm_vec0 = _mm512_loadu_pd(&vec[0]);
        //     zmm_b_t0 = _mm512_loadu_pd(&b_t_ptr[0]);

        //     zmm_vec1 = _mm512_loadu_pd(&vec[8]);
        //     zmm_b_t1 = _mm512_loadu_pd(&b_t_ptr[8]);

        //     zmm_vec0 = _mm512_fmadd_pd(zmm_b_t0, zmm_val, zmm_vec0);
        //     zmm_vec1 = _mm512_fmadd_pd(zmm_b_t1, zmm_val, zmm_vec1);

        //     _mm512_storeu_pd(&vec[0], zmm_vec0);
        //     _mm512_storeu_pd(&vec[8], zmm_vec1);

        //     vec += 16;
        //     b_t_ptr += 16;
        // }
        for(int64_t j=0; j<l_block; j+=8){
            zmm_vec = _mm512_loadu_pd(&vec[0]);
            zmm_b_t = _mm512_loadu_pd(&b_t_ptr[0]);

            zmm_vec = _mm512_fmadd_pd(zmm_b_t, zmm_val, zmm_vec);

            _mm512_storeu_pd(&vec[0], zmm_vec);

            vec += 8;
            b_t_ptr += 8;
        }
        vec -= l_block;
    }
}


void sparse_dgemv_ver0( // Dense_C = CSR_A x Dense_B
    double c[],
    int64_t rows[], int64_t cols[], double vals[], 
    double b[],
    int64_t n, int64_t m, int64_t k, int64_t n_jobs
    ){

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        double accm = 0.0;
        for(int64_t kk=ini; kk<fin; kk++){
            int64_t idx = cols[kk];
            double  val = vals[kk];
            double b0 = b[idx];
            accm += b0 * val;
        }
        c[ii] = accm;
    }
}

void sparse_dgemv_ver1( // Dense_C = CSR_A x Dense_B
    double c[],
    int64_t rows[], int64_t cols[], double vals[], 
    double b[],
    int64_t n, int64_t m, int64_t k, int64_t n_jobs
    ){

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        double accm = 0.0;
        for(int64_t kk=ini; kk<fin; kk+=8){
            double bs[8] = {b[cols[kk+0]], b[cols[kk+1]], b[cols[kk+2]], b[cols[kk+3]], b[cols[kk+4]], b[cols[kk+5]], b[cols[kk+6]], b[cols[kk+7]]};

            __m512d zmm_b = _mm512_loadu_pd(&bs[0]);
            __m512d zmm_v = _mm512_loadu_pd(&vals[kk]);
            __m512d zmm_bv = _mm512_mul_pd(zmm_b, zmm_v);
            accm += _mm512_reduce_add_pd(zmm_bv);
        }
        c[ii] = accm;
    }
}

void sparse_dgemv_ver2( // Dense_C = CSR_A x Dense_B
    double c[],
    int64_t rows[], int64_t cols[], double vals[], 
    double b[],
    int64_t n, int64_t m, int64_t k, int64_t n_jobs
    ){

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        double accm = 0.0;
        for(int64_t kk=ini; kk<fin; kk+=8){
            for(int64_t jj=0; jj<8; jj++){
                accm += b[cols[kk+jj]] * vals[kk+jj];
            }
        }
        c[ii] = accm;
    }
}


void sparse_dgemv_ver3( // Dense_C = CSR_A x Dense_B
    double c[],
    int64_t rows[], int64_t cols[], double vals[], 
    double b[],
    int64_t n, int64_t m, int64_t k, int64_t n_jobs
    ){

    int64_t ii_block=(m>>n_jobs)<<n_jobs;

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    for(int64_t ii=0; ii<m; ii+=ii_block){
        for(int64_t iii=0; iii<ii_block; iii++){
            int64_t ini = rows[ii+iii];
            int64_t fin = rows[ii+iii+1];
            double accm = 0.0;
            for(int64_t kk=ini; kk<fin; kk++){
                int64_t idx = cols[kk];
                double  val = vals[kk];
                double b0 = b[idx];
                accm += b0 * val;
            }
            c[ii+iii] = accm;
        }
    }
}

void sparse_dgemv_ver4( // Dense_C = CSR_A x Dense_B
    double c[],
    int64_t rows[], int64_t cols[], double vals[], 
    double b[],
    int64_t n, int64_t m, int64_t k, int64_t n_jobs
    ){

    omp_set_num_threads(n_jobs);
    #pragma omp parallel for
    #pragma nomemorydepend
    #pragma prefetch cols, vals, b
    for(int64_t ii=0; ii<m; ii++){
        int64_t ini = rows[ii];
        int64_t fin = rows[ii+1];
        double accm = 0.0;
        for(int64_t kk=ini; kk<fin; kk++){
            int64_t idx = cols[kk];
            double  val = vals[kk];
            double b0 = b[idx];
            accm += b0 * val;
        }
        c[ii] = accm;
    }
}