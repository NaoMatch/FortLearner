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

void sparse_dgemv( // Dense_C = CSR_A x Dense_B
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
        _mm_prefetch(&b[k], _MM_HINT_T0);
        for(int64_t kk=ini; kk<fin; kk++){
            int64_t idx = cols[kk];
            double  val = vals[kk];
            double b0 = b[idx];
            accm += b0 * val;
        }
        c[ii] = accm;
    }
}

void sparse_dgemv_ver5( // Dense_C = CSR_A x Dense_B
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

        double accm00 = b[cols[ini+ 0]] * vals[ini+ 0];
        double accm01 = b[cols[ini+ 1]] * vals[ini+ 1];
        accm00 = accm00 + accm01;

        double accm02 = b[cols[ini+ 2]] * vals[ini+ 2];
        double accm03 = b[cols[ini+ 3]] * vals[ini+ 3];
        accm02 = accm02 + accm03;

        double accm04 = b[cols[ini+ 4]] * vals[ini+ 4];
        double accm05 = b[cols[ini+ 5]] * vals[ini+ 5];
        accm04 = accm04 + accm05;
        
        double accm06 = b[cols[ini+ 6]] * vals[ini+ 6];
        double accm07 = b[cols[ini+ 7]] * vals[ini+ 7];
        accm06 = accm06 + accm07;
        
        double accm08 = b[cols[ini+ 8]] * vals[ini+ 8];
        double accm09 = b[cols[ini+ 9]] * vals[ini+ 9];
        accm08 = accm08 + accm09;
        
        double accm10 = b[cols[ini+10]] * vals[ini+10];
        double accm11 = b[cols[ini+11]] * vals[ini+11];
        accm10 = accm10 + accm11;

        double accm12 = b[cols[ini+12]] * vals[ini+12];
        double accm13 = b[cols[ini+13]] * vals[ini+13];
        accm12 = accm12 + accm13;

        double accm14 = b[cols[ini+14]] * vals[ini+14];
        double accm15 = b[cols[ini+15]] * vals[ini+15];
        accm14 = accm14 + accm15;

        double accm16 = b[cols[ini+16]] * vals[ini+16];
        double accm17 = b[cols[ini+17]] * vals[ini+17];
        accm16 = accm16 + accm17;

        double accm18 = b[cols[ini+18]] * vals[ini+18];
        double accm19 = b[cols[ini+19]] * vals[ini+19];
        accm18 = accm18 + accm19;

        double accm20 = b[cols[ini+20]] * vals[ini+20];
        double accm21 = b[cols[ini+21]] * vals[ini+21];
        accm20 = accm20 + accm21;

        double accm22 = b[cols[ini+22]] * vals[ini+22];
        double accm23 = b[cols[ini+23]] * vals[ini+23];
        accm22 = accm22 + accm23;

        double accm24 = b[cols[ini+24]] * vals[ini+24];
        double accm25 = b[cols[ini+25]] * vals[ini+25];
        accm24 = accm24 + accm25;

        double accm26 = b[cols[ini+26]] * vals[ini+26];
        double accm27 = b[cols[ini+27]] * vals[ini+27];
        accm26 = accm26 + accm27;

        double accm28 = b[cols[ini+28]] * vals[ini+28];
        double accm29 = b[cols[ini+29]] * vals[ini+29];
        accm28 = accm28 + accm29;

        double accm30 = b[cols[ini+30]] * vals[ini+30];
        double accm31 = b[cols[ini+31]] * vals[ini+31];            
        accm30 = accm30 + accm31;

        accm00 = accm00 + accm02;
        accm04 = accm04 + accm06;
        accm08 = accm08 + accm10;
        accm12 = accm12 + accm14;
        accm16 = accm16 + accm18;
        accm20 = accm20 + accm22;
        accm24 = accm24 + accm26;
        accm28 = accm28 + accm30;

        accm00 = accm00 + accm04;
        accm08 = accm08 + accm12;
        accm16 = accm16 + accm20;
        accm24 = accm24 + accm28;

        accm00 = accm00 + accm08;
        accm16 = accm16 + accm24;

        accm00 = accm00 + accm16;
        c[ii] = accm00;
    }
}

void sparse_dgemv_ver6( // Dense_C = CSR_A x Dense_B
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

        double accm00, accm01, accm02, accm03, accm04, accm05, accm06, accm07; 
        double accm = 0.0;
        int64_t k_size=fin-ini;
        
        int64_t k_step_8=(k_size>>3)<<3, k_remain_8=k_size%8;
        _mm_prefetch(&b[k_step_8], _MM_HINT_T0);
        for(int64_t kk=ini; kk<ini+k_step_8; kk+=8){
            accm00 = b[cols[kk + 0]] * vals[kk + 0];
            accm01 = b[cols[kk + 1]] * vals[kk + 1];
            accm00 = accm00 + accm01;

            accm02 = b[cols[kk + 2]] * vals[kk + 2];
            accm03 = b[cols[kk + 3]] * vals[kk + 3];
            accm02 = accm02 + accm03;

            accm04 = b[cols[kk + 4]] * vals[kk + 4];
            accm05 = b[cols[kk + 5]] * vals[kk + 5];
            accm04 = accm04 + accm05;
            
            accm06 = b[cols[kk + 6]] * vals[kk + 6];
            accm07 = b[cols[kk + 7]] * vals[kk + 7];
            accm06 = accm06 + accm07;

            accm00 = accm00 + accm02;
            accm04 = accm04 + accm06;

            accm00 = accm00 + accm04;
            accm += accm00;
        }

        
        int64_t k_step_4=(k_remain_8>>2)<<2, k_remain_4=k_remain_8%4;
        for(int64_t kk=ini+k_step_8; kk<ini+k_step_8+k_step_4; kk+=4){
            accm00 = b[cols[kk + 0]] * vals[kk + 0];
            accm01 = b[cols[kk + 1]] * vals[kk + 1];
            accm00 = accm00 + accm01;

            accm02 = b[cols[kk + 2]] * vals[kk + 2];
            accm03 = b[cols[kk + 3]] * vals[kk + 3];
            accm02 = accm02 + accm03;

            accm00 = accm00 + accm02;
            accm += accm00;
        }

        for(int64_t kk=ini+k_step_8+k_step_4; kk<fin; kk++){
            accm00 = b[cols[kk + 0]] * vals[kk + 0];
            accm += accm00;
        }
        c[ii] = accm;
    }
}