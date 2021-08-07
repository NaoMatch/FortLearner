#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111


void sum_up_gt_matrix_01_C(double *sum_vals, double *thresholds, double *matrix, double *response, int64_t indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_rows%i_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;

    int64_t idx, i, j, j_base;
    double r00, r01, r02, r03, r05;
    int64_t r04;

    i=0;
    while(i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;
        j_base = idx*n_cols;
        r00 = response[idx];
        while(c_unroll--){
            r01 = sum_vals[j];
            r02 = thresholds[j];
            r03 = matrix[j_base+j];
            r04 = r03>r02;
            r05 += r00 * r04;

            sum_vals[j] += r05;
            j++;
        }
    }
}


void sum_up_gt_matrix_02_C(double *sum_vals, double *thresholds, double *matrix, double *response, int64_t indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_rows%i_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;

    int64_t idx, i, j, j_base;
    double  r00;
    double  r01, r02, r03;
    int64_t r04;
    double  r05, r06, r07;
    int64_t r08;

    i=0;
    while(i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;
        j_base = idx*n_cols;
        r00 = response[idx];
        while(c_unroll--){
            r01 = sum_vals[j];
            r02 = thresholds[j];
            r03 = matrix[j_base+j];
            r04 = r03>r02;
            r01 += r00 * r04;

            r05 = sum_vals[j];
            r06 = thresholds[j];
            r07 = matrix[j_base+j];
            r08 = r07>r06;
            r05 += r00 * r08;

            sum_vals[j]   += r01;
            sum_vals[j+1] += r05;
            j+=c_unroll_size;
        }
    }
}




