#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <omp.h>
#include <immintrin.h>
#include <xmmintrin.h>
#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) > (b) ? (b) : (a))

void new_get_matrix_minmax(
    double *min_vals, double *max_vals, double *mat_t, 
    int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

    double *row_ptr0, *row_ptr1, *row_ptr2, *row_ptr3, *row_ptr4, *row_ptr5, *row_ptr6, *row_ptr7;
    int64_t idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7;

    __m512d minval, maxval, rowval0, rowval1, rowval2, rowval3, rowval4, rowval5, rowval6, rowval7;
    omp_set_num_threads(n_jobs);
    int64_t n_idxs_unroll = (n_idxs>>3)<<3;
    #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
    for(int64_t i = 0; i < n_idxs_unroll; i += 8){
        idx0 = indices[i + 0]-1;
        idx1 = indices[i + 1]-1;
        idx2 = indices[i + 2]-1;
        idx3 = indices[i + 3]-1;
        idx4 = indices[i + 4]-1;
        idx5 = indices[i + 5]-1;
        idx6 = indices[i + 6]-1;
        idx7 = indices[i + 7]-1;

        row_ptr0 = &mat_t[idx0 * n_cols];
        row_ptr1 = &mat_t[idx1 * n_cols];
        row_ptr2 = &mat_t[idx2 * n_cols];
        row_ptr3 = &mat_t[idx3 * n_cols];
        row_ptr4 = &mat_t[idx4 * n_cols];
        row_ptr5 = &mat_t[idx5 * n_cols];
        row_ptr6 = &mat_t[idx6 * n_cols];
        row_ptr7 = &mat_t[idx7 * n_cols];

        #pragma prefetch min_vals
        #pragma prefetch max_vals
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k = 0; k < k_unroll; k += 8){
            minval = _mm512_loadu_pd(&min_vals[k]);
            maxval = _mm512_loadu_pd(&max_vals[k]);

            rowval0 = _mm512_loadu_pd(&row_ptr0[k]);
            rowval1 = _mm512_loadu_pd(&row_ptr1[k]);
            rowval2 = _mm512_loadu_pd(&row_ptr2[k]);
            rowval3 = _mm512_loadu_pd(&row_ptr3[k]);
            rowval4 = _mm512_loadu_pd(&row_ptr4[k]);
            rowval5 = _mm512_loadu_pd(&row_ptr5[k]);
            rowval6 = _mm512_loadu_pd(&row_ptr6[k]);
            rowval7 = _mm512_loadu_pd(&row_ptr7[k]);

            minval = _mm512_min_pd(minval, rowval0);
            maxval = _mm512_max_pd(maxval, rowval0);

            minval = _mm512_min_pd(minval, rowval1);
            maxval = _mm512_max_pd(maxval, rowval1);

            minval = _mm512_min_pd(minval, rowval2);
            maxval = _mm512_max_pd(maxval, rowval2);

            minval = _mm512_min_pd(minval, rowval3);
            maxval = _mm512_max_pd(maxval, rowval3);

            minval = _mm512_min_pd(minval, rowval4);
            maxval = _mm512_max_pd(maxval, rowval4);

            minval = _mm512_min_pd(minval, rowval5);
            maxval = _mm512_max_pd(maxval, rowval5);

            minval = _mm512_min_pd(minval, rowval6);
            maxval = _mm512_max_pd(maxval, rowval6);

            minval = _mm512_min_pd(minval, rowval7);
            maxval = _mm512_max_pd(maxval, rowval7);

            _mm512_storeu_pd(&min_vals[k], minval);
            _mm512_storeu_pd(&max_vals[k], maxval);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double min_v = min_vals[k];
            double max_v = max_vals[k];
            double row_v0 = row_ptr0[k];
            double row_v1 = row_ptr1[k];
            double row_v2 = row_ptr2[k];
            double row_v3 = row_ptr3[k];
            double row_v4 = row_ptr4[k];
            double row_v5 = row_ptr5[k];
            double row_v6 = row_ptr6[k];
            double row_v7 = row_ptr7[k];

            min_v = min(min_v, row_v0);
            max_v = max(max_v, row_v0);
            min_v = min(min_v, row_v1);
            max_v = max(max_v, row_v1);
            min_v = min(min_v, row_v2);
            max_v = max(max_v, row_v2);
            min_v = min(min_v, row_v3);
            max_v = max(max_v, row_v3);
            min_v = min(min_v, row_v4);
            max_v = max(max_v, row_v4);
            min_v = min(min_v, row_v5);
            max_v = max(max_v, row_v5);
            min_v = min(min_v, row_v6);
            max_v = max(max_v, row_v6);
            min_v = min(min_v, row_v7);
            max_v = max(max_v, row_v7);

            min_vals[k] = min_v;
            max_vals[k] = max_v;
        }
    }


    __m512d rowval;
    for(int64_t i=n_idxs_unroll; i<n_idxs; i++){
        int64_t idx = indices[i]-1;
        double *row_ptr = &mat_t[idx*n_cols];

        #pragma prefetch min_vals
        #pragma prefetch max_vals
        int64_t k_unroll = (n_cols>>3)<<3;
        for(int64_t k=0; k<k_unroll; k+=8){
            minval = _mm512_loadu_pd(&min_vals[k]);
            maxval = _mm512_loadu_pd(&max_vals[k]);

            rowval = _mm512_loadu_pd(&row_ptr[k]);

            minval = _mm512_min_pd(minval, rowval);
            maxval = _mm512_max_pd(maxval, rowval);

            _mm512_storeu_pd(&min_vals[k], minval);
            _mm512_storeu_pd(&max_vals[k], maxval);
        }

        for(int64_t k=k_unroll; k<n_cols; k++){
            double min_v = min_vals[k];
            double max_v = max_vals[k];
            double row_v0 = row_ptr[k];

            min_v = min(min_v, row_v0);
            max_v = max(max_v, row_v0);

            min_vals[k] = min_v;
            max_vals[k] = max_v;
        }

    }    
}










// void new_get_matrix_minmax(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr;
//     int64_t idx;

//     __m512d minval, maxval, rowval;
//     #pragma prefetch mat_t
//     for(int64_t i=0; i<n_idxs; i++){
//         idx = indices[i];
//         row_ptr = &mat_t[idx*n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k=0; k<n_cols; k+=8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval = _mm512_loadu_pd(&row_ptr[k]);

//             minval = _mm512_min_pd(minval, rowval);
//             maxval = _mm512_max_pd(maxval, rowval);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }

// void new_get_matrix_minmax_ver00(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr;
//     int64_t idx;

//     __m512d minval, maxval, rowval;
//     #pragma prefetch mat_t
//     omp_set_num_threads(8);
//     #pragma omp parallel for 
//     for(int64_t k=0; k<n_cols; k+=8){
//         minval = _mm512_loadu_pd(&min_vals[k]);
//         maxval = _mm512_loadu_pd(&max_vals[k]);
//         for(int64_t i=0; i<n_idxs; i++){
//             idx = indices[i];
//             rowval = _mm512_loadu_pd(&mat_t[idx*n_cols + k]);

//             minval = _mm512_min_pd(minval, rowval);
//             maxval = _mm512_max_pd(maxval, rowval);

//         }
//         _mm512_storeu_pd(&min_vals[k], minval);
//         _mm512_storeu_pd(&max_vals[k], maxval);
//     }
// }

// void new_get_matrix_minmax_ver01(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr;
//     int64_t idx;

//     __m512d minval0, maxval0, rowval0;
//     __m512d minval1, maxval1, rowval1;
//     #pragma prefetch mat_t
//     omp_set_num_threads(8);
//     #pragma omp parallel for 
//     for(int64_t k=0; k<n_cols; k+=16){
//         minval0 = _mm512_loadu_pd(&min_vals[k]);
//         maxval0 = _mm512_loadu_pd(&max_vals[k]);
//         minval1 = _mm512_loadu_pd(&min_vals[k+8]);
//         maxval1 = _mm512_loadu_pd(&max_vals[k+8]);
//         for(int64_t i=0; i<n_idxs; i++){
//             idx = indices[i];
//             rowval0 = _mm512_loadu_pd(&mat_t[idx*n_cols + k]);
//             rowval1 = _mm512_loadu_pd(&mat_t[idx*n_cols + k + 8]);

//             minval0 = _mm512_min_pd(minval0, rowval0);
//             maxval0 = _mm512_max_pd(maxval0, rowval0);
//             minval1 = _mm512_min_pd(minval1, rowval1);
//             maxval1 = _mm512_max_pd(maxval1, rowval1);
//         }
//         _mm512_storeu_pd(&min_vals[k], minval0);
//         _mm512_storeu_pd(&max_vals[k], maxval0);
//         _mm512_storeu_pd(&min_vals[k+8], minval1);
//         _mm512_storeu_pd(&max_vals[k+8], maxval1);
//     }
// }

// void new_get_matrix_minmax_ver02(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr;
//     int64_t idx;

//     __m512d minval, maxval, rowval;
//     omp_set_num_threads(4);
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i=0; i<n_idxs; i++){
//         idx = indices[i];
//         row_ptr = &mat_t[idx*n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k=0; k<n_cols; k+=8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval = _mm512_loadu_pd(&row_ptr[k]);

//             minval = _mm512_min_pd(minval, rowval);
//             maxval = _mm512_max_pd(maxval, rowval);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }

// void new_get_matrix_minmax_ver03(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr1, *row_ptr2;
//     int64_t idx1, idx2;

//     __m512d minval, maxval, rowval1, rowval2;
//     omp_set_num_threads(4);
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i=0; i<n_idxs; i+=2){
//         idx1 = indices[i+0];
//         idx2 = indices[i+1];
//         row_ptr1 = &mat_t[idx1*n_cols];
//         row_ptr2 = &mat_t[idx2*n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k=0; k<n_cols; k+=8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);            

//             rowval1 = _mm512_loadu_pd(&row_ptr1[k]);
//             rowval2 = _mm512_loadu_pd(&row_ptr2[k]);

//             minval = _mm512_min_pd(minval, rowval1);
//             maxval = _mm512_max_pd(maxval, rowval1);

//             minval = _mm512_min_pd(minval, rowval2);
//             maxval = _mm512_max_pd(maxval, rowval2);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }

// void new_get_matrix_minmax_ver04(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr1, *row_ptr2, *row_ptr3, *row_ptr4;
//     int64_t idx1, idx2, idx3, idx4;

//     __m512d minval, maxval, rowval1, rowval2, rowval3, rowval4;
//     omp_set_num_threads(4);
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i = 0; i < n_idxs; i += 4){
//         idx1 = indices[i + 0];
//         idx2 = indices[i + 1];
//         idx3 = indices[i + 2];
//         idx4 = indices[i + 3];
//         row_ptr1 = &mat_t[idx1 * n_cols];
//         row_ptr2 = &mat_t[idx2 * n_cols];
//         row_ptr3 = &mat_t[idx3 * n_cols];
//         row_ptr4 = &mat_t[idx4 * n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k = 0; k < n_cols; k += 8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval1 = _mm512_loadu_pd(&row_ptr1[k]);
//             rowval2 = _mm512_loadu_pd(&row_ptr2[k]);
//             rowval3 = _mm512_loadu_pd(&row_ptr3[k]);
//             rowval4 = _mm512_loadu_pd(&row_ptr4[k]);

//             minval = _mm512_min_pd(minval, rowval1);
//             maxval = _mm512_max_pd(maxval, rowval1);

//             minval = _mm512_min_pd(minval, rowval2);
//             maxval = _mm512_max_pd(maxval, rowval2);

//             minval = _mm512_min_pd(minval, rowval3);
//             maxval = _mm512_max_pd(maxval, rowval3);

//             minval = _mm512_min_pd(minval, rowval4);
//             maxval = _mm512_max_pd(maxval, rowval4);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }

// void new_get_matrix_minmax_ver05(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_jobs){

//     double *row_ptr0, *row_ptr1, *row_ptr2, *row_ptr3, *row_ptr4, *row_ptr5, *row_ptr6, *row_ptr7;
//     int64_t idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7;

//     __m512d minval, maxval, rowval0, rowval1, rowval2, rowval3, rowval4, rowval5, rowval6, rowval7;
//     omp_set_num_threads(n_jobs);
//     int64_t n_idxs_unroll = (n_idxs>>3)<<3;
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i = 0; i < n_idxs_unroll; i += 8){
//         idx0 = indices[i + 0]-1;
//         idx1 = indices[i + 1]-1;
//         idx2 = indices[i + 2]-1;
//         idx3 = indices[i + 3]-1;
//         idx4 = indices[i + 4]-1;
//         idx5 = indices[i + 5]-1;
//         idx6 = indices[i + 6]-1;
//         idx7 = indices[i + 7]-1;

//         row_ptr0 = &mat_t[idx0 * n_cols];
//         row_ptr1 = &mat_t[idx1 * n_cols];
//         row_ptr2 = &mat_t[idx2 * n_cols];
//         row_ptr3 = &mat_t[idx3 * n_cols];
//         row_ptr4 = &mat_t[idx4 * n_cols];
//         row_ptr5 = &mat_t[idx5 * n_cols];
//         row_ptr6 = &mat_t[idx6 * n_cols];
//         row_ptr7 = &mat_t[idx7 * n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         int64_t k_unroll = (n_cols>>3)<<3;
//         for(int64_t k = 0; k < k_unroll; k += 8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval0 = _mm512_loadu_pd(&row_ptr0[k]);
//             rowval1 = _mm512_loadu_pd(&row_ptr1[k]);
//             rowval2 = _mm512_loadu_pd(&row_ptr2[k]);
//             rowval3 = _mm512_loadu_pd(&row_ptr3[k]);
//             rowval4 = _mm512_loadu_pd(&row_ptr4[k]);
//             rowval5 = _mm512_loadu_pd(&row_ptr5[k]);
//             rowval6 = _mm512_loadu_pd(&row_ptr6[k]);
//             rowval7 = _mm512_loadu_pd(&row_ptr7[k]);

//             minval = _mm512_min_pd(minval, rowval0);
//             maxval = _mm512_max_pd(maxval, rowval0);

//             minval = _mm512_min_pd(minval, rowval1);
//             maxval = _mm512_max_pd(maxval, rowval1);

//             minval = _mm512_min_pd(minval, rowval2);
//             maxval = _mm512_max_pd(maxval, rowval2);

//             minval = _mm512_min_pd(minval, rowval3);
//             maxval = _mm512_max_pd(maxval, rowval3);

//             minval = _mm512_min_pd(minval, rowval4);
//             maxval = _mm512_max_pd(maxval, rowval4);

//             minval = _mm512_min_pd(minval, rowval5);
//             maxval = _mm512_max_pd(maxval, rowval5);

//             minval = _mm512_min_pd(minval, rowval6);
//             maxval = _mm512_max_pd(maxval, rowval6);

//             minval = _mm512_min_pd(minval, rowval7);
//             maxval = _mm512_max_pd(maxval, rowval7);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }

//         for(int64_t k=k_unroll; k<n_cols; k++){
//             double min_v = min_vals[k];
//             double max_v = max_vals[k];
//             double row_v0 = row_ptr0[k];
//             double row_v1 = row_ptr1[k];
//             double row_v2 = row_ptr2[k];
//             double row_v3 = row_ptr3[k];
//             double row_v4 = row_ptr4[k];
//             double row_v5 = row_ptr5[k];
//             double row_v6 = row_ptr6[k];
//             double row_v7 = row_ptr7[k];

//             min_v = min(min_v, row_v0);
//             max_v = max(max_v, row_v0);
//             min_v = min(min_v, row_v1);
//             max_v = max(max_v, row_v1);
//             min_v = min(min_v, row_v2);
//             max_v = max(max_v, row_v2);
//             min_v = min(min_v, row_v3);
//             max_v = max(max_v, row_v3);
//             min_v = min(min_v, row_v4);
//             max_v = max(max_v, row_v4);
//             min_v = min(min_v, row_v5);
//             max_v = max(max_v, row_v5);
//             min_v = min(min_v, row_v6);
//             max_v = max(max_v, row_v6);
//             min_v = min(min_v, row_v7);
//             max_v = max(max_v, row_v7);

//             min_vals[k] = min_v;
//             max_vals[k] = max_v;
//         }
//     }


//     __m512d rowval;
//     for(int64_t i=n_idxs_unroll; i<n_idxs; i++){
//         int64_t idx = indices[i]-1;
//         double *row_ptr = &mat_t[idx*n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         int64_t k_unroll = (n_cols>>3)<<3;
//         for(int64_t k=0; k<k_unroll; k+=8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval = _mm512_loadu_pd(&row_ptr[k]);

//             minval = _mm512_min_pd(minval, rowval);
//             maxval = _mm512_max_pd(maxval, rowval);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }

//         for(int64_t k=k_unroll; k<n_cols; k++){
//             double min_v = min_vals[k];
//             double max_v = max_vals[k];
//             double row_v0 = row_ptr[k];

//             min_v = min(min_v, row_v0);
//             max_v = max(max_v, row_v0);

//             min_vals[k] = min_v;
//             max_vals[k] = max_v;
//         }

//     }    
// }

// void new_get_matrix_minmax_ver06(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr0, *row_ptr1, *row_ptr2, *row_ptr3, *row_ptr4, *row_ptr5, *row_ptr6, *row_ptr7;
//     int64_t idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7;

//     __m512d minval, maxval, rowval0, rowval1, rowval2, rowval3, rowval4, rowval5, rowval6, rowval7;
//     omp_set_num_threads(4);
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i = 0; i < n_idxs; i += 8){
//         idx0 = indices[i + 0];
//         idx1 = indices[i + 1];
//         idx2 = indices[i + 2];
//         idx3 = indices[i + 3];
//         idx4 = indices[i + 4];
//         idx5 = indices[i + 5];
//         idx6 = indices[i + 6];
//         idx7 = indices[i + 7];

//         row_ptr0 = &mat_t[idx0 * n_cols];
//         row_ptr1 = &mat_t[idx1 * n_cols];
//         row_ptr2 = &mat_t[idx2 * n_cols];
//         row_ptr3 = &mat_t[idx3 * n_cols];
//         row_ptr4 = &mat_t[idx4 * n_cols];
//         row_ptr5 = &mat_t[idx5 * n_cols];
//         row_ptr6 = &mat_t[idx6 * n_cols];
//         row_ptr7 = &mat_t[idx7 * n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k = 0; k < n_cols; k += 8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval0 = _mm512_loadu_pd(&row_ptr0[k]);

//             minval = _mm512_min_pd(minval, rowval0);
//             maxval = _mm512_max_pd(maxval, rowval0);
//             rowval1 = _mm512_loadu_pd(&row_ptr1[k]);

//             minval = _mm512_min_pd(minval, rowval1);
//             maxval = _mm512_max_pd(maxval, rowval1);
//             rowval2 = _mm512_loadu_pd(&row_ptr2[k]);

//             minval = _mm512_min_pd(minval, rowval2);
//             maxval = _mm512_max_pd(maxval, rowval2);
//             rowval3 = _mm512_loadu_pd(&row_ptr3[k]);

//             minval = _mm512_min_pd(minval, rowval3);
//             maxval = _mm512_max_pd(maxval, rowval3);
//             rowval4 = _mm512_loadu_pd(&row_ptr4[k]);

//             minval = _mm512_min_pd(minval, rowval4);
//             maxval = _mm512_max_pd(maxval, rowval4);
//             rowval5 = _mm512_loadu_pd(&row_ptr5[k]);

//             minval = _mm512_min_pd(minval, rowval5);
//             maxval = _mm512_max_pd(maxval, rowval5);
//             rowval6 = _mm512_loadu_pd(&row_ptr6[k]);


//             minval = _mm512_min_pd(minval, rowval6);
//             maxval = _mm512_max_pd(maxval, rowval6);
//             rowval7 = _mm512_loadu_pd(&row_ptr7[k]);

//             minval = _mm512_min_pd(minval, rowval7);
//             maxval = _mm512_max_pd(maxval, rowval7);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }

// void new_get_matrix_minmax_ver07(
//     double *min_vals, double *max_vals, double *mat_t, 
//     int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

//     double *row_ptr0, *row_ptr1, *row_ptr2, *row_ptr3, *row_ptr4, *row_ptr5, *row_ptr6, *row_ptr7;
//     double *row_ptr8, *row_ptr9, *row_ptr10, *row_ptr11, *row_ptr12, *row_ptr13, *row_ptr14, *row_ptr15;
//     int64_t idx0, idx1, idx2, idx3, idx4, idx5, idx6, idx7;
//     int64_t idx8, idx9, idx10, idx11, idx12, idx13, idx14, idx15;

//     __m512d minval, maxval, rowval0, rowval1, rowval2, rowval3, rowval4, rowval5, rowval6, rowval7;
//     __m512d rowval8, rowval9, rowval10, rowval11, rowval12, rowval13, rowval14, rowval15;

//     omp_set_num_threads(4);
//     #pragma omp parallel for reduction(min: min_vals[0:n_cols]) reduction(max: max_vals[0:n_cols])
//     for(int64_t i = 0; i < n_idxs; i += 16){
//         idx0 = indices[i + 0];
//         idx1 = indices[i + 1];
//         idx2 = indices[i + 2];
//         idx3 = indices[i + 3];
//         idx4 = indices[i + 4];
//         idx5 = indices[i + 5];
//         idx6 = indices[i + 6];
//         idx7 = indices[i + 7];
//         idx8 = indices[i + 8];
//         idx9 = indices[i + 9];
//         idx10 = indices[i + 10];
//         idx11 = indices[i + 11];
//         idx12 = indices[i + 12];
//         idx13 = indices[i + 13];
//         idx14 = indices[i + 14];
//         idx15 = indices[i + 15];

//         row_ptr0 = &mat_t[idx0 * n_cols];
//         row_ptr1 = &mat_t[idx1 * n_cols];
//         row_ptr2 = &mat_t[idx2 * n_cols];
//         row_ptr3 = &mat_t[idx3 * n_cols];
//         row_ptr4 = &mat_t[idx4 * n_cols];
//         row_ptr5 = &mat_t[idx5 * n_cols];
//         row_ptr6 = &mat_t[idx6 * n_cols];
//         row_ptr7 = &mat_t[idx7 * n_cols];
//         row_ptr8 = &mat_t[idx8 * n_cols];
//         row_ptr9 = &mat_t[idx9 * n_cols];
//         row_ptr10 = &mat_t[idx10 * n_cols];
//         row_ptr11 = &mat_t[idx11 * n_cols];
//         row_ptr12 = &mat_t[idx12 * n_cols];
//         row_ptr13 = &mat_t[idx13 * n_cols];
//         row_ptr14 = &mat_t[idx14 * n_cols];
//         row_ptr15 = &mat_t[idx15 * n_cols];

//         #pragma prefetch min_vals
//         #pragma prefetch max_vals
//         for(int64_t k = 0; k < n_cols; k += 8){
//             minval = _mm512_loadu_pd(&min_vals[k]);
//             maxval = _mm512_loadu_pd(&max_vals[k]);

//             rowval0 = _mm512_loadu_pd(&row_ptr0[k]);
//             rowval1 = _mm512_loadu_pd(&row_ptr1[k]);
//             rowval2 = _mm512_loadu_pd(&row_ptr2[k]);
//             rowval3 = _mm512_loadu_pd(&row_ptr3[k]);
//             rowval4 = _mm512_loadu_pd(&row_ptr4[k]);
//             rowval5 = _mm512_loadu_pd(&row_ptr5[k]);
//             rowval6 = _mm512_loadu_pd(&row_ptr6[k]);
//             rowval7 = _mm512_loadu_pd(&row_ptr7[k]);
//             rowval8 = _mm512_loadu_pd(&row_ptr8[k]);
//             rowval9 = _mm512_loadu_pd(&row_ptr9[k]);
//             rowval10 = _mm512_loadu_pd(&row_ptr10[k]);
//             rowval11 = _mm512_loadu_pd(&row_ptr11[k]);
//             rowval12 = _mm512_loadu_pd(&row_ptr12[k]);
//             rowval13 = _mm512_loadu_pd(&row_ptr13[k]);
//             rowval14 = _mm512_loadu_pd(&row_ptr14[k]);
//             rowval15 = _mm512_loadu_pd(&row_ptr15[k]);

//             minval = _mm512_min_pd(minval, rowval0);
//             maxval = _mm512_max_pd(maxval, rowval0);

//             minval = _mm512_min_pd(minval, rowval1);
//             maxval = _mm512_max_pd(maxval, rowval1);

//             minval = _mm512_min_pd(minval, rowval2);
//             maxval = _mm512_max_pd(maxval, rowval2);

//             minval = _mm512_min_pd(minval, rowval3);
//             maxval = _mm512_max_pd(maxval, rowval3);

//             minval = _mm512_min_pd(minval, rowval4);
//             maxval = _mm512_max_pd(maxval, rowval4);

//             minval = _mm512_min_pd(minval, rowval5);
//             maxval = _mm512_max_pd(maxval, rowval5);

//             minval = _mm512_min_pd(minval, rowval6);
//             maxval = _mm512_max_pd(maxval, rowval6);

//             minval = _mm512_min_pd(minval, rowval7);
//             maxval = _mm512_max_pd(maxval, rowval7);

//             minval = _mm512_min_pd(minval, rowval8);
//             maxval = _mm512_max_pd(maxval, rowval8);

//             minval = _mm512_min_pd(minval, rowval9);
//             maxval = _mm512_max_pd(maxval, rowval9);

//             minval = _mm512_min_pd(minval, rowval10);
//             maxval = _mm512_max_pd(maxval, rowval10);

//             minval = _mm512_min_pd(minval, rowval11);
//             maxval = _mm512_max_pd(maxval, rowval11);

//             minval = _mm512_min_pd(minval, rowval12);
//             maxval = _mm512_max_pd(maxval, rowval12);

//             minval = _mm512_min_pd(minval, rowval13);
//             maxval = _mm512_max_pd(maxval, rowval13);

//             minval = _mm512_min_pd(minval, rowval14);
//             maxval = _mm512_max_pd(maxval, rowval14);

//             minval = _mm512_min_pd(minval, rowval15);
//             maxval = _mm512_max_pd(maxval, rowval15);

//             _mm512_storeu_pd(&min_vals[k], minval);
//             _mm512_storeu_pd(&max_vals[k], maxval);
//         }
//     }
// }
