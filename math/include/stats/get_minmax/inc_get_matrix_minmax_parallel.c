#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <omp.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

extern void get_minmax_vector2vector_01_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_02_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_04_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_08_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_16_C_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_02x_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_04x_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_04y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_08y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_08z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_16y_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_16z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);
extern void get_minmax_vector2vector_32z_A_r8(double vals[], double min_vals[], double max_vals[], int64_t n);

void get_matrix_minmax_with_index_parallel_02x_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_02x_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_04x_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_04x_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_04y_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_04y_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_08y_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_08y_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_08z_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_08z_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_16y_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_16y_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_16z_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_16z_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_32z_A_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_32z_A_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}


void get_matrix_minmax_with_index_parallel_01_C_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_01_C_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_02_C_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_02_C_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_04_C_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_04_C_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_08_C_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_08_C_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}

void get_matrix_minmax_with_index_parallel_16_C_r8(double *min_vals, double *max_vals, double *matrix_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols, int64_t n_thds){
    int64_t i, idx;
    double *matrix_t_start_idx, *matrix_t_ptr;

    matrix_t_start_idx = matrix_t;
    omp_set_num_threads(n_thds);
    #pragma omp parallel for reduction(min:min_vals[0:n_cols]) reduction(max:max_vals[0:n_cols]) num_threads(4) private(matrix_t_ptr, i, idx) shared(matrix_t_start_idx)
    for (i=0; i<n_idxs; i++){
        idx = (indices[i]-1)*n_cols;
        matrix_t_ptr = matrix_t_start_idx + idx;
        get_minmax_vector2vector_16_C_r8(matrix_t_ptr, min_vals, max_vals, n_cols);
    }
}






