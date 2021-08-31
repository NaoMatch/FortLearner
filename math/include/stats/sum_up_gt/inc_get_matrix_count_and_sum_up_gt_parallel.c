#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <omp.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

extern void count_and_sum_up_gt_vector2vector_01_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_02_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_04_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_08_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_16_C_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_02x_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_04x_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_04y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_08y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_08z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_16y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_16z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_32y_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);
extern void count_and_sum_up_gt_vector2vector_32z_A_r8(double x_vals[], double thr_vals[], double sum_vals[], int64_t cnt_vals[], double y_val, int64_t n);

void get_matrix_count_and_sum_up_gt_with_index_parallel_01_C(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_01_C_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_02_C(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_02_C_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_04_C(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_04_C_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_08_C(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_08_C_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }

}

void get_matrix_count_and_sum_up_gt_with_index_parallel_16_C(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_16_C_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_02x_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_02x_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_04x_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_04x_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_04y_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_04y_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_08y_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_08y_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_08z_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_08z_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_16y_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_16y_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_16z_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_16z_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_32y_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_32y_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}

void get_matrix_count_and_sum_up_gt_with_index_parallel_32z_A(double *sum_vals_ptr, int64_t *cnt_vals_ptr, double *thr_vals_ptr, double *mat_t_ptr, double *y_ptr, int64_t *indices_ptr, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx, idx0;
    double *matrix_t_start_idx, y_val, *mat_t_ptr_i;

    for(i=0; i<n_cols; i++){
        sum_vals_ptr[i]=0;
        cnt_vals_ptr[i]=0;
    }

    matrix_t_start_idx = mat_t_ptr;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(+:sum_vals_ptr[0:n_cols]) reduction(+:cnt_vals_ptr[0:n_cols]) private(mat_t_ptr_i, i, idx, idx0, y_val) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx0 = indices_ptr[i]-1;
        idx = idx0*n_cols;
        y_val = y_ptr[idx0];
        mat_t_ptr_i = matrix_t_start_idx + idx;
        count_and_sum_up_gt_vector2vector_32z_A_r8(mat_t_ptr_i, thr_vals_ptr, sum_vals_ptr, cnt_vals_ptr, y_val, n_cols);
    }
}
