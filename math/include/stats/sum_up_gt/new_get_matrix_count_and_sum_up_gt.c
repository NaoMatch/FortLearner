#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <omp.h>
#include <immintrin.h>
#include <xmmintrin.h>

void new_get_matrix_count_and_sum_up_gt(
    double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
    int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

    for(int64_t k=0; k<n_cols; k++){
        sum_vals_r[k] = 0.0;
        cnt_vals_r[k] = 0;
    }
    
    int64_t i_unroll = (n_idxs>>3)<<3;
    omp_set_num_threads(n_jobs);
    #pragma omp parallel for reduction(+: sum_vals_r[0:n_cols]) reduction(+: cnt_vals_r[0:n_cols])
    for(int64_t i=0; i<i_unroll; i+=8){
        // Load indices and prepare row pointers and y values
        int64_t idx1 = indices[i+0]-1;
        int64_t idx2 = indices[i+1]-1;
        int64_t idx3 = indices[i+2]-1;
        int64_t idx4 = indices[i+3]-1;
        int64_t idx5 = indices[i+4]-1;
        int64_t idx6 = indices[i+5]-1;
        int64_t idx7 = indices[i+6]-1;
        int64_t idx8 = indices[i+7]-1;
        double *row_ptr1 = &mat_t[idx1*n_cols];
        double *row_ptr2 = &mat_t[idx2*n_cols];
        double *row_ptr3 = &mat_t[idx3*n_cols];
        double *row_ptr4 = &mat_t[idx4*n_cols];
        double *row_ptr5 = &mat_t[idx5*n_cols];
        double *row_ptr6 = &mat_t[idx6*n_cols];
        double *row_ptr7 = &mat_t[idx7*n_cols];
        double *row_ptr8 = &mat_t[idx8*n_cols];
        __m512d y1_ = _mm512_set1_pd(y[idx1]);
        __m512d y2_ = _mm512_set1_pd(y[idx2]);
        __m512d y3_ = _mm512_set1_pd(y[idx3]);
        __m512d y4_ = _mm512_set1_pd(y[idx4]);
        __m512d y5_ = _mm512_set1_pd(y[idx5]);
        __m512d y6_ = _mm512_set1_pd(y[idx6]);
        __m512d y7_ = _mm512_set1_pd(y[idx7]);
        __m512d y8_ = _mm512_set1_pd(y[idx8]);
        __m512i o_ = _mm512_set1_epi64(1);

        // Process each set of 8 columns with 4 indices unrolled
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k=0; k<k_unroll; k+=8){
            __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
            __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

            __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
            __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
            __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
            __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
            __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);
            __m512d row5 = _mm512_loadu_pd(&row_ptr5[k]);
            __m512d row6 = _mm512_loadu_pd(&row_ptr6[k]);
            __m512d row7 = _mm512_loadu_pd(&row_ptr7[k]);
            __m512d row8 = _mm512_loadu_pd(&row_ptr8[k]);

            // Compare and accumulate for the first pair of indices
            __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
            __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
            __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
            __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
            __mmask8 mask_d5 = _mm512_cmp_pd_mask(row5, thr, _CMP_GT_OS);
            __mmask8 mask_d6 = _mm512_cmp_pd_mask(row6, thr, _CMP_GT_OS);
            __mmask8 mask_d7 = _mm512_cmp_pd_mask(row7, thr, _CMP_GT_OS);
            __mmask8 mask_d8 = _mm512_cmp_pd_mask(row8, thr, _CMP_GT_OS);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d2, sum_v1, y2_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d4, sum_v1, y4_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d5, sum_v1, y5_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d6, sum_v1, y6_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d7, sum_v1, y7_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d8, sum_v1, y8_);

            // Convert masks to 16-bit for counting
            __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
            __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
            __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
            __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);
            __mmask16 mask_i5 = _cvtmask8_u32(mask_d5);
            __mmask16 mask_i6 = _cvtmask8_u32(mask_d6);
            __mmask16 mask_i7 = _cvtmask8_u32(mask_d7);
            __mmask16 mask_i8 = _cvtmask8_u32(mask_d8);

            // Accumulate counts
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i5, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i6, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i7, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i8, cnt_v1, o_);

            // Store results
            _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
            _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double sum_v1 = sum_vals_r[k];
            double cnt_v1 = cnt_vals_r[k];

            double thr = thr_vals[k];
            double row1 = row_ptr1[k];
            double row2 = row_ptr2[k];
            double row3 = row_ptr3[k];
            double row4 = row_ptr4[k];
            double row5 = row_ptr5[k];
            double row6 = row_ptr6[k];
            double row7 = row_ptr7[k];
            double row8 = row_ptr8[k];

            // Compare and accumulate for the first pair of indices
            int64_t mask_d1 = row1>thr;
            int64_t mask_d2 = row2>thr;
            int64_t mask_d3 = row3>thr;
            int64_t mask_d4 = row4>thr;
            int64_t mask_d5 = row5>thr;
            int64_t mask_d6 = row6>thr;
            int64_t mask_d7 = row7>thr;
            int64_t mask_d8 = row8>thr;
            sum_v1 = sum_v1 + mask_d1*y[idx1];
            sum_v1 = sum_v1 + mask_d2*y[idx2];
            sum_v1 = sum_v1 + mask_d3*y[idx3];
            sum_v1 = sum_v1 + mask_d4*y[idx4];
            sum_v1 = sum_v1 + mask_d5*y[idx5];
            sum_v1 = sum_v1 + mask_d6*y[idx6];
            sum_v1 = sum_v1 + mask_d7*y[idx7];
            sum_v1 = sum_v1 + mask_d8*y[idx8];

            // Accumulate counts
            cnt_v1 = cnt_v1 + mask_d1;
            cnt_v1 = cnt_v1 + mask_d2;
            cnt_v1 = cnt_v1 + mask_d3;
            cnt_v1 = cnt_v1 + mask_d4;
            cnt_v1 = cnt_v1 + mask_d5;
            cnt_v1 = cnt_v1 + mask_d6;
            cnt_v1 = cnt_v1 + mask_d7;
            cnt_v1 = cnt_v1 + mask_d8;

            // Store results
            sum_vals_r[k] = sum_v1;
            cnt_vals_r[k] = cnt_v1;
        }
    }

    for(int64_t i=i_unroll; i<n_idxs; i++){
        // Load indices and prepare row pointers and y values
        int64_t idx1 = indices[i+0]-1;
        double *row_ptr1 = &mat_t[idx1*n_cols];
        __m512d y1_ = _mm512_set1_pd(y[idx1]);
        __m512i o_ = _mm512_set1_epi64(1);

        // Process each set of 8 columns with 4 indices unrolled
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k=0; k<k_unroll; k+=8){
            __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
            __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

            __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
            __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);

            // Compare and accumulate for the first pair of indices
            __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);

            // Convert masks to 16-bit for counting
            __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);

            // Accumulate counts
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);

            // Store results
            _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
            _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double sum_v1 = sum_vals_r[k];
            double cnt_v1 = cnt_vals_r[k];

            double thr = thr_vals[k];
            double row1 = row_ptr1[k];

            // Compare and accumulate for the first pair of indices
            int64_t mask_d1 = row1>thr;
            sum_v1 = sum_v1 + mask_d1*y[idx1];

            // Accumulate counts
            cnt_v1 = cnt_v1 + mask_d1;

            // Store results
            sum_vals_r[k] = sum_v1;
            cnt_vals_r[k] = cnt_v1;
        }
    }    
}


void new_get_matrix_count_and_sum_up_gt_single(
    double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
    int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    for(int64_t k=0; k<n_cols; k++){
        sum_vals_r[k] = 0.0;
        cnt_vals_r[k] = 0;
    }
    
    int64_t i_unroll = (n_idxs>>3)<<3;
    for(int64_t i=0; i<i_unroll; i+=8){
        // Load indices and prepare row pointers and y values
        int64_t idx1 = indices[i+0]-1;
        int64_t idx2 = indices[i+1]-1;
        int64_t idx3 = indices[i+2]-1;
        int64_t idx4 = indices[i+3]-1;
        int64_t idx5 = indices[i+4]-1;
        int64_t idx6 = indices[i+5]-1;
        int64_t idx7 = indices[i+6]-1;
        int64_t idx8 = indices[i+7]-1;
        double *row_ptr1 = &mat_t[idx1*n_cols];
        double *row_ptr2 = &mat_t[idx2*n_cols];
        double *row_ptr3 = &mat_t[idx3*n_cols];
        double *row_ptr4 = &mat_t[idx4*n_cols];
        double *row_ptr5 = &mat_t[idx5*n_cols];
        double *row_ptr6 = &mat_t[idx6*n_cols];
        double *row_ptr7 = &mat_t[idx7*n_cols];
        double *row_ptr8 = &mat_t[idx8*n_cols];
        __m512d y1_ = _mm512_set1_pd(y[idx1]);
        __m512d y2_ = _mm512_set1_pd(y[idx2]);
        __m512d y3_ = _mm512_set1_pd(y[idx3]);
        __m512d y4_ = _mm512_set1_pd(y[idx4]);
        __m512d y5_ = _mm512_set1_pd(y[idx5]);
        __m512d y6_ = _mm512_set1_pd(y[idx6]);
        __m512d y7_ = _mm512_set1_pd(y[idx7]);
        __m512d y8_ = _mm512_set1_pd(y[idx8]);
        __m512i o_ = _mm512_set1_epi64(1);

        // Process each set of 8 columns with 4 indices unrolled
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k=0; k<k_unroll; k+=8){
            __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
            __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

            __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
            __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
            __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
            __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
            __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);
            __m512d row5 = _mm512_loadu_pd(&row_ptr5[k]);
            __m512d row6 = _mm512_loadu_pd(&row_ptr6[k]);
            __m512d row7 = _mm512_loadu_pd(&row_ptr7[k]);
            __m512d row8 = _mm512_loadu_pd(&row_ptr8[k]);

            // Compare and accumulate for the first pair of indices
            __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
            __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
            __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
            __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
            __mmask8 mask_d5 = _mm512_cmp_pd_mask(row5, thr, _CMP_GT_OS);
            __mmask8 mask_d6 = _mm512_cmp_pd_mask(row6, thr, _CMP_GT_OS);
            __mmask8 mask_d7 = _mm512_cmp_pd_mask(row7, thr, _CMP_GT_OS);
            __mmask8 mask_d8 = _mm512_cmp_pd_mask(row8, thr, _CMP_GT_OS);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d2, sum_v1, y2_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d4, sum_v1, y4_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d5, sum_v1, y5_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d6, sum_v1, y6_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d7, sum_v1, y7_);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d8, sum_v1, y8_);

            // Convert masks to 16-bit for counting
            __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
            __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
            __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
            __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);
            __mmask16 mask_i5 = _cvtmask8_u32(mask_d5);
            __mmask16 mask_i6 = _cvtmask8_u32(mask_d6);
            __mmask16 mask_i7 = _cvtmask8_u32(mask_d7);
            __mmask16 mask_i8 = _cvtmask8_u32(mask_d8);

            // Accumulate counts
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i5, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i6, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i7, cnt_v1, o_);
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i8, cnt_v1, o_);

            // Store results
            _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
            _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double sum_v1 = sum_vals_r[k];
            double cnt_v1 = cnt_vals_r[k];

            double thr = thr_vals[k];
            double row1 = row_ptr1[k];
            double row2 = row_ptr2[k];
            double row3 = row_ptr3[k];
            double row4 = row_ptr4[k];
            double row5 = row_ptr5[k];
            double row6 = row_ptr6[k];
            double row7 = row_ptr7[k];
            double row8 = row_ptr8[k];

            // Compare and accumulate for the first pair of indices
            int64_t mask_d1 = row1>thr;
            int64_t mask_d2 = row2>thr;
            int64_t mask_d3 = row3>thr;
            int64_t mask_d4 = row4>thr;
            int64_t mask_d5 = row5>thr;
            int64_t mask_d6 = row6>thr;
            int64_t mask_d7 = row7>thr;
            int64_t mask_d8 = row8>thr;
            sum_v1 = sum_v1 + mask_d1*y[idx1];
            sum_v1 = sum_v1 + mask_d2*y[idx2];
            sum_v1 = sum_v1 + mask_d3*y[idx3];
            sum_v1 = sum_v1 + mask_d4*y[idx4];
            sum_v1 = sum_v1 + mask_d5*y[idx5];
            sum_v1 = sum_v1 + mask_d6*y[idx6];
            sum_v1 = sum_v1 + mask_d7*y[idx7];
            sum_v1 = sum_v1 + mask_d8*y[idx8];

            // Accumulate counts
            cnt_v1 = cnt_v1 + mask_d1;
            cnt_v1 = cnt_v1 + mask_d2;
            cnt_v1 = cnt_v1 + mask_d3;
            cnt_v1 = cnt_v1 + mask_d4;
            cnt_v1 = cnt_v1 + mask_d5;
            cnt_v1 = cnt_v1 + mask_d6;
            cnt_v1 = cnt_v1 + mask_d7;
            cnt_v1 = cnt_v1 + mask_d8;

            // Store results
            sum_vals_r[k] = sum_v1;
            cnt_vals_r[k] = cnt_v1;
        }
    }

    for(int64_t i=i_unroll; i<n_idxs; i++){
        // Load indices and prepare row pointers and y values
        int64_t idx1 = indices[i+0]-1;
        double *row_ptr1 = &mat_t[idx1*n_cols];
        __m512d y1_ = _mm512_set1_pd(y[idx1]);
        __m512i o_ = _mm512_set1_epi64(1);

        // Process each set of 8 columns with 4 indices unrolled
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k=0; k<k_unroll; k+=8){
            __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
            __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

            __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
            __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);

            // Compare and accumulate for the first pair of indices
            __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
            sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);

            // Convert masks to 16-bit for counting
            __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);

            // Accumulate counts
            cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);

            // Store results
            _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
            _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double sum_v1 = sum_vals_r[k];
            double cnt_v1 = cnt_vals_r[k];

            double thr = thr_vals[k];
            double row1 = row_ptr1[k];

            // Compare and accumulate for the first pair of indices
            int64_t mask_d1 = row1>thr;
            sum_v1 = sum_v1 + mask_d1*y[idx1];

            // Accumulate counts
            cnt_v1 = cnt_v1 + mask_d1;

            // Store results
            sum_vals_r[k] = sum_v1;
            cnt_vals_r[k] = cnt_v1;
        }
    }    
}


// void new_get_matrix_count_and_sum_up_gt(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){


//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     for(int64_t i=0; i<n_idxs; i++){
//         int64_t idx = indices[i]-1;
//         double *row_ptr = &mat_t[idx*n_cols];
//         __m512d y_ = _mm512_set1_pd(y[idx]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         for(int64_t k=0; k<n_cols; k+=8){
//             __m512d sum_v = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row = _mm512_loadu_pd(&row_ptr[k]);

//             __mmask8 mask_d = _mm512_cmp_pd_mask(row, thr, _CMP_GT_OS);
//             sum_v = _mm512_mask_add_pd(sum_v, mask_d, sum_v, y_);

//             __mmask16 mask_i = _cvtmask8_u32(mask_d);
//             cnt_v = _mm512_mask_add_epi64(cnt_v, mask_i, cnt_v, o_);

//             _mm512_storeu_pd(&sum_vals_r[k], sum_v);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v);
//         }
//     }
// }

// void new_get_matrix_count_and_sum_up_gt_ver01(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){


//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     for(int64_t i=0; i<n_idxs; i+=2){
//         int64_t idx1 = indices[i+0]-1;
//         int64_t idx2 = indices[i+1]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         double *row_ptr2 = &mat_t[idx2*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512d y2_ = _mm512_set1_pd(y[idx2]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         for(int64_t k=0; k<n_cols; k+=8){
//             __m512d sum_v = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
//             __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);

//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             sum_v = _mm512_mask_add_pd(sum_v, mask_d1, sum_v, y1_);
//             __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
//             sum_v = _mm512_mask_add_pd(sum_v, mask_d2, sum_v, y2_);

//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
//             __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
//             cnt_v = _mm512_mask_add_epi64(cnt_v, mask_i1, cnt_v, o_);
//             cnt_v = _mm512_mask_add_epi64(cnt_v, mask_i2, cnt_v, o_);

//             _mm512_storeu_pd(&sum_vals_r[k], sum_v);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v);
//         }
//     }
// }

// void new_get_matrix_count_and_sum_up_gt_ver02(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     for(int64_t i=0; i<n_idxs; i+=4){
//         // Load indices and prepare row pointers and y values
//         int64_t idx1 = indices[i+0]-1;
//         int64_t idx2 = indices[i+1]-1;
//         int64_t idx3 = indices[i+2]-1;
//         int64_t idx4 = indices[i+3]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         double *row_ptr2 = &mat_t[idx2*n_cols];
//         double *row_ptr3 = &mat_t[idx3*n_cols];
//         double *row_ptr4 = &mat_t[idx4*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512d y2_ = _mm512_set1_pd(y[idx2]);
//         __m512d y3_ = _mm512_set1_pd(y[idx3]);
//         __m512d y4_ = _mm512_set1_pd(y[idx4]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         // Process each set of 8 columns with 4 indices unrolled
//         for(int64_t k=0; k<n_cols; k+=8){
//             __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
//             __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
//             __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
//             __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);

//             // Compare and accumulate for the first pair of indices
//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
//             __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d2, sum_v1, y2_);
//             __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
//             __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d4, sum_v1, y4_);

//             // Convert masks to 16-bit for counting
//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
//             __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
//             __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
//             __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);

//             // Accumulate counts
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);

//             // Store results
//             _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
//         }
//     }
// }

// void new_get_matrix_count_and_sum_up_gt_ver03(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     int64_t i_unroll = (n_idxs>>3)<<3;
//     omp_set_num_threads(n_jobs);
//     #pragma omp parallel for reduction(+: sum_vals_r[0:n_cols]) reduction(+: cnt_vals_r[0:n_cols])
//     for(int64_t i=0; i<n_idxs; i+=8){
//         // Load indices and prepare row pointers and y values
//         int64_t idx1 = indices[i+0]-1;
//         int64_t idx2 = indices[i+1]-1;
//         int64_t idx3 = indices[i+2]-1;
//         int64_t idx4 = indices[i+3]-1;
//         int64_t idx5 = indices[i+4]-1;
//         int64_t idx6 = indices[i+5]-1;
//         int64_t idx7 = indices[i+6]-1;
//         int64_t idx8 = indices[i+7]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         double *row_ptr2 = &mat_t[idx2*n_cols];
//         double *row_ptr3 = &mat_t[idx3*n_cols];
//         double *row_ptr4 = &mat_t[idx4*n_cols];
//         double *row_ptr5 = &mat_t[idx5*n_cols];
//         double *row_ptr6 = &mat_t[idx6*n_cols];
//         double *row_ptr7 = &mat_t[idx7*n_cols];
//         double *row_ptr8 = &mat_t[idx8*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512d y2_ = _mm512_set1_pd(y[idx2]);
//         __m512d y3_ = _mm512_set1_pd(y[idx3]);
//         __m512d y4_ = _mm512_set1_pd(y[idx4]);
//         __m512d y5_ = _mm512_set1_pd(y[idx5]);
//         __m512d y6_ = _mm512_set1_pd(y[idx6]);
//         __m512d y7_ = _mm512_set1_pd(y[idx7]);
//         __m512d y8_ = _mm512_set1_pd(y[idx8]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         // Process each set of 8 columns with 4 indices unrolled
//         int64_t k_unroll = (n_cols>>3)<<3;
//         for(int64_t k=0; k<k_unroll; k+=8){
//             __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
//             __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
//             __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
//             __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);
//             __m512d row5 = _mm512_loadu_pd(&row_ptr5[k]);
//             __m512d row6 = _mm512_loadu_pd(&row_ptr6[k]);
//             __m512d row7 = _mm512_loadu_pd(&row_ptr7[k]);
//             __m512d row8 = _mm512_loadu_pd(&row_ptr8[k]);

//             // Compare and accumulate for the first pair of indices
//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
//             __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
//             __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
//             __mmask8 mask_d5 = _mm512_cmp_pd_mask(row5, thr, _CMP_GT_OS);
//             __mmask8 mask_d6 = _mm512_cmp_pd_mask(row6, thr, _CMP_GT_OS);
//             __mmask8 mask_d7 = _mm512_cmp_pd_mask(row7, thr, _CMP_GT_OS);
//             __mmask8 mask_d8 = _mm512_cmp_pd_mask(row8, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d2, sum_v1, y2_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d4, sum_v1, y4_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d5, sum_v1, y5_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d6, sum_v1, y6_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d7, sum_v1, y7_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d8, sum_v1, y8_);

//             // Convert masks to 16-bit for counting
//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
//             __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
//             __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
//             __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);
//             __mmask16 mask_i5 = _cvtmask8_u32(mask_d5);
//             __mmask16 mask_i6 = _cvtmask8_u32(mask_d6);
//             __mmask16 mask_i7 = _cvtmask8_u32(mask_d7);
//             __mmask16 mask_i8 = _cvtmask8_u32(mask_d8);

//             // Accumulate counts
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i5, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i6, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i7, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i8, cnt_v1, o_);

//             // Store results
//             _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
//         }

//         for(int64_t k=k_unroll; k<n_cols; k++){
//             double sum_v1 = sum_vals_r[k];
//             double cnt_v1 = cnt_vals_r[k];

//             double thr = thr_vals[k];
//             double row1 = row_ptr1[k];
//             double row2 = row_ptr2[k];
//             double row3 = row_ptr3[k];
//             double row4 = row_ptr4[k];
//             double row5 = row_ptr5[k];
//             double row6 = row_ptr6[k];
//             double row7 = row_ptr7[k];
//             double row8 = row_ptr8[k];

//             // Compare and accumulate for the first pair of indices
//             int64_t mask_d1 = row1>thr;
//             int64_t mask_d2 = row2>thr;
//             int64_t mask_d3 = row3>thr;
//             int64_t mask_d4 = row4>thr;
//             int64_t mask_d5 = row5>thr;
//             int64_t mask_d6 = row6>thr;
//             int64_t mask_d7 = row7>thr;
//             int64_t mask_d8 = row8>thr;
//             sum_v1 = sum_v1 + mask_d1*y[idx1];
//             sum_v1 = sum_v1 + mask_d2*y[idx2];
//             sum_v1 = sum_v1 + mask_d3*y[idx3];
//             sum_v1 = sum_v1 + mask_d4*y[idx4];
//             sum_v1 = sum_v1 + mask_d5*y[idx5];
//             sum_v1 = sum_v1 + mask_d6*y[idx6];
//             sum_v1 = sum_v1 + mask_d7*y[idx7];
//             sum_v1 = sum_v1 + mask_d8*y[idx8];

//             // Accumulate counts
//             cnt_v1 = cnt_v1 + mask_d1;
//             cnt_v1 = cnt_v1 + mask_d2;
//             cnt_v1 = cnt_v1 + mask_d3;
//             cnt_v1 = cnt_v1 + mask_d4;
//             cnt_v1 = cnt_v1 + mask_d5;
//             cnt_v1 = cnt_v1 + mask_d6;
//             cnt_v1 = cnt_v1 + mask_d7;
//             cnt_v1 = cnt_v1 + mask_d8;

//             // Store results
//             sum_vals_r[k] = sum_v1;
//             cnt_vals_r[k] = cnt_v1;
//         }
//     }

//     for(int64_t i=i_unroll; i<n_idxs; i++){
//         // Load indices and prepare row pointers and y values
//         int64_t idx1 = indices[i+0]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         // Process each set of 8 columns with 4 indices unrolled
//         int64_t k_unroll = (n_cols>>3)<<3;
//         for(int64_t k=0; k<k_unroll; k+=8){
//             __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);

//             // Compare and accumulate for the first pair of indices
//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);

//             // Convert masks to 16-bit for counting
//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);

//             // Accumulate counts
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);

//             // Store results
//             _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
//         }

//         for(int64_t k=k_unroll; k<n_cols; k++){
//             double sum_v1 = sum_vals_r[k];
//             double cnt_v1 = cnt_vals_r[k];

//             double thr = thr_vals[k];
//             double row1 = row_ptr1[k];

//             // Compare and accumulate for the first pair of indices
//             int64_t mask_d1 = row1>thr;
//             sum_v1 = sum_v1 + mask_d1*y[idx1];

//             // Accumulate counts
//             cnt_v1 = cnt_v1 + mask_d1;

//             // Store results
//             sum_vals_r[k] = sum_v1;
//             cnt_vals_r[k] = cnt_v1;
//         }
//     }    
// }

// void new_get_matrix_count_and_sum_up_gt_ver04(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     for(int64_t i=0; i<n_idxs; i+=16){
//         // Load indices and prepare row pointers and y values
//         int64_t idx1 = indices[i+0]-1;
//         int64_t idx2 = indices[i+1]-1;
//         int64_t idx3 = indices[i+2]-1;
//         int64_t idx4 = indices[i+3]-1;
//         int64_t idx5 = indices[i+4]-1;
//         int64_t idx6 = indices[i+5]-1;
//         int64_t idx7 = indices[i+6]-1;
//         int64_t idx8 = indices[i+7]-1;
//         int64_t idx9 = indices[i+8]-1;
//         int64_t idx10 = indices[i+9]-1;
//         int64_t idx11 = indices[i+10]-1;
//         int64_t idx12 = indices[i+11]-1;
//         int64_t idx13 = indices[i+12]-1;
//         int64_t idx14 = indices[i+13]-1;
//         int64_t idx15 = indices[i+14]-1;
//         int64_t idx16 = indices[i+15]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         double *row_ptr2 = &mat_t[idx2*n_cols];
//         double *row_ptr3 = &mat_t[idx3*n_cols];
//         double *row_ptr4 = &mat_t[idx4*n_cols];
//         double *row_ptr5 = &mat_t[idx5*n_cols];
//         double *row_ptr6 = &mat_t[idx6*n_cols];
//         double *row_ptr7 = &mat_t[idx7*n_cols];
//         double *row_ptr8 = &mat_t[idx8*n_cols];
//         double *row_ptr9 = &mat_t[idx9*n_cols];
//         double *row_ptr10 = &mat_t[idx10*n_cols];
//         double *row_ptr11 = &mat_t[idx11*n_cols];
//         double *row_ptr12 = &mat_t[idx12*n_cols];
//         double *row_ptr13 = &mat_t[idx13*n_cols];
//         double *row_ptr14 = &mat_t[idx14*n_cols];
//         double *row_ptr15 = &mat_t[idx15*n_cols];
//         double *row_ptr16 = &mat_t[idx16*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512d y2_ = _mm512_set1_pd(y[idx2]);
//         __m512d y3_ = _mm512_set1_pd(y[idx3]);
//         __m512d y4_ = _mm512_set1_pd(y[idx4]);
//         __m512d y5_ = _mm512_set1_pd(y[idx5]);
//         __m512d y6_ = _mm512_set1_pd(y[idx6]);
//         __m512d y7_ = _mm512_set1_pd(y[idx7]);
//         __m512d y8_ = _mm512_set1_pd(y[idx8]);
//         __m512d y9_ = _mm512_set1_pd(y[idx9]);
//         __m512d y10_ = _mm512_set1_pd(y[idx10]);
//         __m512d y11_ = _mm512_set1_pd(y[idx11]);
//         __m512d y12_ = _mm512_set1_pd(y[idx12]);
//         __m512d y13_ = _mm512_set1_pd(y[idx13]);
//         __m512d y14_ = _mm512_set1_pd(y[idx14]);
//         __m512d y15_ = _mm512_set1_pd(y[idx15]);
//         __m512d y16_ = _mm512_set1_pd(y[idx16]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         // Process each set of 8 columns with 4 indices unrolled
//         for(int64_t k=0; k<n_cols; k+=8){
//             __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
//             __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
//             __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
//             __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);
//             __m512d row5 = _mm512_loadu_pd(&row_ptr5[k]);
//             __m512d row6 = _mm512_loadu_pd(&row_ptr6[k]);
//             __m512d row7 = _mm512_loadu_pd(&row_ptr7[k]);
//             __m512d row8 = _mm512_loadu_pd(&row_ptr8[k]);
//             __m512d row9 = _mm512_loadu_pd(&row_ptr9[k]);
//             __m512d row10 = _mm512_loadu_pd(&row_ptr10[k]);
//             __m512d row11 = _mm512_loadu_pd(&row_ptr11[k]);
//             __m512d row12 = _mm512_loadu_pd(&row_ptr12[k]);
//             __m512d row13 = _mm512_loadu_pd(&row_ptr13[k]);
//             __m512d row14 = _mm512_loadu_pd(&row_ptr14[k]);
//             __m512d row15 = _mm512_loadu_pd(&row_ptr15[k]);
//             __m512d row16 = _mm512_loadu_pd(&row_ptr16[k]);

//             // Compare and accumulate for the first pair of indices
//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
//             __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d2, sum_v1, y2_);
//             __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
//             __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d4, sum_v1, y4_);
//             __mmask8 mask_d5 = _mm512_cmp_pd_mask(row5, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d5, sum_v1, y5_);
//             __mmask8 mask_d6 = _mm512_cmp_pd_mask(row6, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d6, sum_v1, y6_);
//             __mmask8 mask_d7 = _mm512_cmp_pd_mask(row7, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d7, sum_v1, y7_);
//             __mmask8 mask_d8 = _mm512_cmp_pd_mask(row8, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d8, sum_v1, y8_);
//             __mmask8 mask_d9 = _mm512_cmp_pd_mask(row9, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d9, sum_v1, y9_);
//             __mmask8 mask_d10 = _mm512_cmp_pd_mask(row10, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d10, sum_v1, y10_);
//             __mmask8 mask_d11 = _mm512_cmp_pd_mask(row11, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d11, sum_v1, y11_);
//             __mmask8 mask_d12 = _mm512_cmp_pd_mask(row12, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d12, sum_v1, y12_);
//             __mmask8 mask_d13 = _mm512_cmp_pd_mask(row13, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d13, sum_v1, y13_);
//             __mmask8 mask_d14 = _mm512_cmp_pd_mask(row14, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d14, sum_v1, y14_);
//             __mmask8 mask_d15 = _mm512_cmp_pd_mask(row15, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d15, sum_v1, y15_);
//             __mmask8 mask_d16 = _mm512_cmp_pd_mask(row16, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d16, sum_v1, y16_);

//             // Convert masks to 16-bit for counting
//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
//             __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
//             __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
//             __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);
//             __mmask16 mask_i5 = _cvtmask8_u32(mask_d5);
//             __mmask16 mask_i6 = _cvtmask8_u32(mask_d6);
//             __mmask16 mask_i7 = _cvtmask8_u32(mask_d7);
//             __mmask16 mask_i8 = _cvtmask8_u32(mask_d8);
//             __mmask16 mask_i9 = _cvtmask8_u32(mask_d9);
//             __mmask16 mask_i10 = _cvtmask8_u32(mask_d10);
//             __mmask16 mask_i11 = _cvtmask8_u32(mask_d11);
//             __mmask16 mask_i12 = _cvtmask8_u32(mask_d12);
//             __mmask16 mask_i13 = _cvtmask8_u32(mask_d13);
//             __mmask16 mask_i14 = _cvtmask8_u32(mask_d14);
//             __mmask16 mask_i15 = _cvtmask8_u32(mask_d15);
//             __mmask16 mask_i16 = _cvtmask8_u32(mask_d16);

//             // Accumulate counts
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i5, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i6, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i7, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i8, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i9, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i10, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i11, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i12, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i13, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i14, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i15, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i16, cnt_v1, o_);

//             // Store results
//             _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
//         }
//     }
// }

// void new_get_matrix_count_and_sum_up_gt_ver05(
//     double *sum_vals_r, int64_t *cnt_vals_r, double *thr_vals, double *mat_t, double *y,
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

//     for(int64_t k=0; k<n_cols; k++){
//         sum_vals_r[k] = 0.0;
//         cnt_vals_r[k] = 0;
//     }
    
//     __m512d tmp_sum1, tmp_sum2, tmp_sum3, tmp_sum4, tmp_sum5, tmp_sum6, tmp_sum7, tmp_sum8; 

//     omp_set_num_threads(n_jobs);
//     #pragma omp parallel for reduction(+: sum_vals_r[0:n_cols]) reduction(+: cnt_vals_r[0:n_cols])
//     for(int64_t i=0; i<n_idxs; i+=8){
//         // Load indices and prepare row pointers and y values
//         int64_t idx1 = indices[i+0]-1;
//         int64_t idx2 = indices[i+1]-1;
//         int64_t idx3 = indices[i+2]-1;
//         int64_t idx4 = indices[i+3]-1;
//         int64_t idx5 = indices[i+4]-1;
//         int64_t idx6 = indices[i+5]-1;
//         int64_t idx7 = indices[i+6]-1;
//         int64_t idx8 = indices[i+7]-1;
//         double *row_ptr1 = &mat_t[idx1*n_cols];
//         double *row_ptr2 = &mat_t[idx2*n_cols];
//         double *row_ptr3 = &mat_t[idx3*n_cols];
//         double *row_ptr4 = &mat_t[idx4*n_cols];
//         double *row_ptr5 = &mat_t[idx5*n_cols];
//         double *row_ptr6 = &mat_t[idx6*n_cols];
//         double *row_ptr7 = &mat_t[idx7*n_cols];
//         double *row_ptr8 = &mat_t[idx8*n_cols];
//         __m512d y1_ = _mm512_set1_pd(y[idx1]);
//         __m512d y2_ = _mm512_set1_pd(y[idx2]);
//         __m512d y3_ = _mm512_set1_pd(y[idx3]);
//         __m512d y4_ = _mm512_set1_pd(y[idx4]);
//         __m512d y5_ = _mm512_set1_pd(y[idx5]);
//         __m512d y6_ = _mm512_set1_pd(y[idx6]);
//         __m512d y7_ = _mm512_set1_pd(y[idx7]);
//         __m512d y8_ = _mm512_set1_pd(y[idx8]);
//         __m512i o_ = _mm512_set1_epi64(1);

//         // Process each set of 8 columns with 4 indices unrolled
//         for(int64_t k=0; k<n_cols; k+=8){
//             __m512d sum_v1 = _mm512_loadu_pd(&sum_vals_r[k]);
//             __m512i cnt_v1 = _mm512_loadu_si512(&cnt_vals_r[k]);
//             tmp_sum1 = _mm512_setzero_pd();
//             // tmp_sum2 = _mm512_setzero_pd();
//             // tmp_sum3 = _mm512_setzero_pd();
//             // tmp_sum4 = _mm512_setzero_pd();
//             // tmp_sum5 = _mm512_setzero_pd();
//             // tmp_sum6 = _mm512_setzero_pd();
//             // tmp_sum7 = _mm512_setzero_pd();
//             // tmp_sum8 = _mm512_setzero_pd();

//             __m512d thr = _mm512_loadu_pd(&thr_vals[k]);
//             __m512d row1 = _mm512_loadu_pd(&row_ptr1[k]);
//             __m512d row2 = _mm512_loadu_pd(&row_ptr2[k]);
//             __m512d row3 = _mm512_loadu_pd(&row_ptr3[k]);
//             __m512d row4 = _mm512_loadu_pd(&row_ptr4[k]);
//             __m512d row5 = _mm512_loadu_pd(&row_ptr5[k]);
//             __m512d row6 = _mm512_loadu_pd(&row_ptr6[k]);
//             __m512d row7 = _mm512_loadu_pd(&row_ptr7[k]);
//             __m512d row8 = _mm512_loadu_pd(&row_ptr8[k]);

//             // Compare and accumulate for the first pair of indices
//             __mmask8 mask_d1 = _mm512_cmp_pd_mask(row1, thr, _CMP_GT_OS);
//             __mmask8 mask_d2 = _mm512_cmp_pd_mask(row2, thr, _CMP_GT_OS);
//             __mmask8 mask_d3 = _mm512_cmp_pd_mask(row3, thr, _CMP_GT_OS);
//             __mmask8 mask_d4 = _mm512_cmp_pd_mask(row4, thr, _CMP_GT_OS);
//             __mmask8 mask_d5 = _mm512_cmp_pd_mask(row5, thr, _CMP_GT_OS);
//             __mmask8 mask_d6 = _mm512_cmp_pd_mask(row6, thr, _CMP_GT_OS);
//             __mmask8 mask_d7 = _mm512_cmp_pd_mask(row7, thr, _CMP_GT_OS);
//             __mmask8 mask_d8 = _mm512_cmp_pd_mask(row8, thr, _CMP_GT_OS);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d1, sum_v1, y1_);
//             tmp_sum1 = _mm512_mask_add_pd(tmp_sum1, mask_d2, tmp_sum1, y2_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d3, sum_v1, y3_);
//             tmp_sum1 = _mm512_mask_add_pd(tmp_sum1, mask_d4, tmp_sum1, y4_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d5, sum_v1, y5_);
//             tmp_sum1 = _mm512_mask_add_pd(tmp_sum1, mask_d6, tmp_sum1, y6_);
//             sum_v1 = _mm512_mask_add_pd(sum_v1, mask_d7, sum_v1, y7_);
//             tmp_sum1 = _mm512_mask_add_pd(tmp_sum1, mask_d8, tmp_sum1, y8_);

//             sum_v1 = _mm512_add_pd(sum_v1, tmp_sum1);


//             // Convert masks to 16-bit for counting
//             __mmask16 mask_i1 = _cvtmask8_u32(mask_d1);
//             __mmask16 mask_i2 = _cvtmask8_u32(mask_d2);
//             __mmask16 mask_i3 = _cvtmask8_u32(mask_d3);
//             __mmask16 mask_i4 = _cvtmask8_u32(mask_d4);
//             __mmask16 mask_i5 = _cvtmask8_u32(mask_d5);
//             __mmask16 mask_i6 = _cvtmask8_u32(mask_d6);
//             __mmask16 mask_i7 = _cvtmask8_u32(mask_d7);
//             __mmask16 mask_i8 = _cvtmask8_u32(mask_d8);

//             // Accumulate counts
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i1, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i2, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i3, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i4, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i5, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i6, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i7, cnt_v1, o_);
//             cnt_v1 = _mm512_mask_add_epi64(cnt_v1, mask_i8, cnt_v1, o_);

//             // Store results
//             _mm512_storeu_pd(&sum_vals_r[k], sum_v1);
//             _mm512_storeu_si512(&cnt_vals_r[k], cnt_v1);
//         }
//     }
// }

