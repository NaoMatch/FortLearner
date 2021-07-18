#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

double maxval_r8_(double x, double y){
	if (x>y){
        return x;
    }else{
        return y;
    }
}

double minval_r8_(double x, double y){
	if (x<y){
        return x;
    }else{
        return y;
    }
}

int64_t maxval_i8_(int64_t x, int64_t y){
	if (x>y){
        return x;
    }else{
        return y;
    }
}

int64_t minval_i8_(int64_t x, int64_t y){
	if (x<y){
        return x;
    }else{
        return y;
    }
}


void get_matrix_minmax_loop_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_indices/i_unroll_size, i_remain=n_rows%i_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        j_base = idx*n_cols;
        while (c_unroll--){
            r00 = min_vals[j];
            r01 = max_vals[j];

            r02 = mat_t[j_base+j];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            min_vals[j] = r00;
            max_vals[j] = r01;

            j+=c_unroll_size;
        }
        i++;
    }
}


void get_matrix_minmax_loop_02_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_indices/i_unroll_size, i_remain=n_indices%i_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;
    double r03, r04, r05;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        j_base = idx*n_cols;
        while (c_unroll--){
            // --------------------------------------------
            r00 = min_vals[j];
            r01 = max_vals[j];

            r02 = mat_t[j_base+j];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            r03 = min_vals[j+1];
            r04 = max_vals[j+1];

            r05 = mat_t[j_base+j+1];

            r03 = minval_r8_(r03, r05);
            r04 = maxval_r8_(r04, r05);

            // --------------------------------------------
            min_vals[j] = r00;
            max_vals[j] = r01;
            min_vals[j+1] = r03;
            max_vals[j+1] = r04;

            j+=c_unroll_size;
        }

        while (c_remain--){
            // --------------------------------------------
            r00 = min_vals[j];
            r01 = max_vals[j];

            r02 = mat_t[j_base+j];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            min_vals[j] = r00;
            max_vals[j] = r01;

            j++;
        }

        i++;
    }
}


void get_matrix_minmax_loop_04_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_indices/i_unroll_size, i_remain=n_indices%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;
    double r03, r04, r05;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        j_base = idx*n_cols;
        while (c_unroll--){
            // --------------------------------------------
            r00 = min_vals[j];
            r01 = max_vals[j];

            r02 = mat_t[j_base+j];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            r03 = min_vals[j+1];
            r04 = max_vals[j+1];

            r05 = mat_t[j_base+j+1];

            r03 = minval_r8_(r03, r05);
            r04 = maxval_r8_(r04, r05);

            // --------------------------------------------
            min_vals[j] = r00;
            max_vals[j] = r01;
            min_vals[j+1] = r03;
            max_vals[j+1] = r04;

            // --------------------------------------------
            r00 = min_vals[j+2];
            r01 = max_vals[j+2];

            r02 = mat_t[j_base+j+2];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            r03 = min_vals[j+3];
            r04 = max_vals[j+3];

            r05 = mat_t[j_base+j+3];

            r03 = minval_r8_(r03, r05);
            r04 = maxval_r8_(r04, r05);

            // --------------------------------------------
            min_vals[j+2] = r00;
            max_vals[j+2] = r01;
            min_vals[j+3] = r03;
            max_vals[j+3] = r04;

            j+=c_unroll_size;
        }

        while (c_remain--){
            // --------------------------------------------
            r00 = min_vals[j];
            r01 = max_vals[j];

            r02 = mat_t[j_base+j];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            min_vals[j] = r00;
            max_vals[j] = r01;

            j++;
        }

        i++;
    }
}


void get_matrix_minmax_loop_08_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
}


void get_matrix_minmax_loop_02_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
}


void get_matrix_minmax_loop_04_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
}


void get_matrix_minmax_loop_08_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
}


void get_matrix_minmax_loop_16_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_indices, int64_t n_rows, int64_t n_cols){
}
