#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

// double maxval_r8_(double x, double y){
// 	return x>y?x:y;
// }

// double minval_r8_(double x, double y){
// 	return x>y?y:x;
// }

// int64_t maxval_i8_(int64_t x, int64_t y){
// 	return x>y?x:y;
// }

// int64_t minval_i8_(int64_t x, int64_t y){
// 	return x>y?y:x;
// }

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


void get_matrix_minmax_loop_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_rows%i_unroll_size;
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


void get_matrix_minmax_loop_02_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;
    double r03, r04, r05;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;
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

        if (c_remain>0){
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
        }

        i++;
    }
}


void get_matrix_minmax_loop_04_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;
    double r03, r04, r05;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;
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

        if (c_remain>0){
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
        }

        i++;
    }
}


void get_matrix_minmax_loop_08_C(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=8, c_unroll=n_cols/c_unroll_size,    c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double r00, r01, r02;
    double r03, r04, r05;

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        j=0;
        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;
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

            // --------------------------------------------
            r00 = min_vals[j+4];
            r01 = max_vals[j+4];

            r02 = mat_t[j_base+j+4];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            r03 = min_vals[j+5];
            r04 = max_vals[j+5];

            r05 = mat_t[j_base+j+5];

            r03 = minval_r8_(r03, r05);
            r04 = maxval_r8_(r04, r05);

            // --------------------------------------------
            min_vals[j+4] = r00;
            max_vals[j+4] = r01;
            min_vals[j+5] = r03;
            max_vals[j+5] = r04;

            // --------------------------------------------
            r00 = min_vals[j+6];
            r01 = max_vals[j+6];

            r02 = mat_t[j_base+j+6];

            r00 = minval_r8_(r00, r02);
            r01 = maxval_r8_(r01, r02);

            // --------------------------------------------
            r03 = min_vals[j+7];
            r04 = max_vals[j+7];

            r05 = mat_t[j_base+j+7];

            r03 = minval_r8_(r03, r05);
            r04 = maxval_r8_(r04, r05);

            // --------------------------------------------
            min_vals[j+6] = r00;
            max_vals[j+6] = r01;
            min_vals[j+7] = r03;
            max_vals[j+7] = r04;

            j+=c_unroll_size;
        }

        if (c_remain>0){
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
        }

        i++;
    }
}


void get_matrix_minmax_loop_02_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
}


void get_matrix_minmax_loop_04_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices_diff, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll = n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift;

    shift = indices_diff[0]*n_cols*8;
    __asm__ __volatile__ (
        "addq %[shift], %[mat]             \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift)
        :"0"(mat_t), "1"(shift)
    );

    back = n_cols * 8;
    i=1;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[min]), %%ymm0   \n\t"
                "vmovupd 0*8(%[max]), %%ymm1   \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat]), %%ymm2   \n\t"
                "\n\t"
                "vminpd %%ymm2, %%ymm0, %%ymm0 \n\t"
                "vmaxpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[min])   \n\t"
                "vmovupd %%ymm1, 0*8(%[max])   \n\t"
                "\n\t"
                "subq $-4*8, %[min]            \n\t"
                "subq $-4*8, %[max]            \n\t"
                "subq $-4*8, %[mat]            \n\t"
                "\n\t"
                :[min]"=r"(min_vals), [max]"=r"(max_vals), [mat]"=r"(mat_t)
                :"0"(min_vals), "1"(max_vals), "2"(mat_t)
            );
        }

        shift = (indices_diff[i]-1)*n_cols*8;
        __asm__ __volatile__ (
            "addq %[shift], %[mat]             \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift)
            :"0"(mat_t), "1"(shift)
        );

        __asm__ __volatile__ (
            "subq %[back], %[min] \n\t"
            "subq %[back], %[max] \n\t"
            :[min]"=r"(min_vals), [max]"=r"(max_vals), [back]"=r"(back)
            :"0"(min_vals), "1"(max_vals), "2"(back)
        );
        i++;
    }
}


void get_matrix_minmax_loop_08_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices_diff, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=8, c_unroll = n_cols/c_unroll_size,  c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift;

    shift = indices_diff[0]*n_cols*8;
    __asm__ __volatile__ (
        "addq %[shift], %[mat]             \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift)
        :"0"(mat_t), "1"(shift)
    );

    back = n_cols * 8;
    i=1;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[min]), %%ymm0   \n\t"
                "vmovupd 0*8(%[max]), %%ymm1   \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat]), %%ymm2   \n\t"
                "\n\t"
                "vminpd %%ymm2, %%ymm0, %%ymm0 \n\t"
                "vmaxpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[min])   \n\t"
                "vmovupd %%ymm1, 0*8(%[max])   \n\t"
                "\n\t"
                "vmovupd 4*8(%[min]), %%ymm3   \n\t"
                "vmovupd 4*8(%[max]), %%ymm4   \n\t"
                "\n\t"
                "vmovupd 4*8(%[mat]), %%ymm5   \n\t"
                "\n\t"
                "vminpd %%ymm5, %%ymm3, %%ymm3 \n\t"
                "vmaxpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmovupd %%ymm3, 4*8(%[min])   \n\t"
                "vmovupd %%ymm4, 4*8(%[max])   \n\t"
                "\n\t"
                "subq $-8*8, %[min]            \n\t"
                "subq $-8*8, %[max]            \n\t"
                "subq $-8*8, %[mat]            \n\t"
                "\n\t"
                :[min]"=r"(min_vals), [max]"=r"(max_vals), [mat]"=r"(mat_t)
                :"0"(min_vals), "1"(max_vals), "2"(mat_t)
            );
        }

        shift = (indices_diff[i]-1)*n_cols*8;
        __asm__ __volatile__ (
            "addq %[shift], %[mat]             \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift)
            :"0"(mat_t), "1"(shift)
        );

        __asm__ __volatile__ (
            "subq %[back], %[min] \n\t"
            "subq %[back], %[max] \n\t"
            :[min]"=r"(min_vals), [max]"=r"(max_vals), [back]"=r"(back)
            :"0"(min_vals), "1"(max_vals), "2"(back)
        );
        i++;
    }
}


void get_matrix_minmax_loop_08z_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices_diff, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=8, c_unroll = n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift;

    shift = indices_diff[0]*n_cols*8;
    __asm__ __volatile__ (
        "addq %[shift], %[mat]             \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift)
        :"0"(mat_t), "1"(shift)
    );

    back = n_cols * 8;
    i=1;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[min]), %%zmm0   \n\t"
                "vmovupd 0*8(%[max]), %%zmm1   \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat]), %%zmm2   \n\t"
                "\n\t"
                "vminpd %%zmm2, %%zmm0, %%zmm0 \n\t"
                "vmaxpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[min])   \n\t"
                "vmovupd %%zmm1, 0*8(%[max])   \n\t"
                "\n\t"
                "subq $-8*8, %[min]            \n\t"
                "subq $-8*8, %[max]            \n\t"
                "subq $-8*8, %[mat]            \n\t"
                "\n\t"
                :[min]"=r"(min_vals), [max]"=r"(max_vals), [mat]"=r"(mat_t)
                :"0"(min_vals), "1"(max_vals), "2"(mat_t)
            );
        }

        shift = (indices_diff[i]-1)*n_cols*8;
        __asm__ __volatile__ (
            "addq %[shift], %[mat]             \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift)
            :"0"(mat_t), "1"(shift)
        );

        __asm__ __volatile__ (
            "subq %[back], %[min] \n\t"
            "subq %[back], %[max] \n\t"
            :[min]"=r"(min_vals), [max]"=r"(max_vals), [back]"=r"(back)
            :"0"(min_vals), "1"(max_vals), "2"(back)
        );
        i++;
    }
}


void get_matrix_minmax_loop_16_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices_diff, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=16, c_unroll = n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift;

    shift = indices_diff[0]*n_cols*8;
    __asm__ __volatile__ (
        "addq %[shift], %[mat]             \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift)
        :"0"(mat_t), "1"(shift)
    );

    back = n_cols * 8;
    i=1;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[min]), %%ymm0   \n\t"
                "vmovupd 0*8(%[max]), %%ymm1   \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat]), %%ymm2   \n\t"
                "\n\t"
                "vminpd %%ymm2, %%ymm0, %%ymm0 \n\t"
                "vmaxpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[min])   \n\t"
                "vmovupd %%ymm1, 0*8(%[max])   \n\t"
                "\n\t"
                "\n\t"
                "vmovupd 4*8(%[min]), %%ymm3   \n\t"
                "vmovupd 4*8(%[max]), %%ymm4   \n\t"
                "\n\t"
                "vmovupd 4*8(%[mat]), %%ymm5   \n\t"
                "\n\t"
                "vminpd %%ymm5, %%ymm3, %%ymm3 \n\t"
                "vmaxpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmovupd %%ymm3, 4*8(%[min])   \n\t"
                "vmovupd %%ymm4, 4*8(%[max])   \n\t"
                "\n\t"
                "\n\t"
                "vmovupd 8*8(%[min]), %%ymm6   \n\t"
                "vmovupd 8*8(%[max]), %%ymm7   \n\t"
                "\n\t"
                "vmovupd 8*8(%[mat]), %%ymm8   \n\t"
                "\n\t"
                "vminpd %%ymm8, %%ymm6, %%ymm6 \n\t"
                "vmaxpd %%ymm8, %%ymm7, %%ymm7 \n\t"
                "\n\t"
                "vmovupd %%ymm6, 8*8(%[min])   \n\t"
                "vmovupd %%ymm7, 8*8(%[max])   \n\t"
                "\n\t"
                "\n\t"
                "vmovupd 12*8(%[min]), %%ymm9   \n\t"
                "vmovupd 12*8(%[max]), %%ymm10   \n\t"
                "\n\t"
                "vmovupd 12*8(%[mat]), %%ymm11   \n\t"
                "\n\t"
                "vminpd %%ymm11, %%ymm9, %%ymm9 \n\t"
                "vmaxpd %%ymm11, %%ymm10, %%ymm10 \n\t"
                "\n\t"
                "vmovupd %%ymm9, 12*8(%[min])   \n\t"
                "vmovupd %%ymm10, 12*8(%[max])   \n\t"
                "\n\t"
                "subq $-16*8, %[min]            \n\t"
                "subq $-16*8, %[max]            \n\t"
                "subq $-16*8, %[mat]            \n\t"
                "\n\t"
                :[min]"=r"(min_vals), [max]"=r"(max_vals), [mat]"=r"(mat_t)
                :"0"(min_vals), "1"(max_vals), "2"(mat_t)
            );
        }

        shift = (indices_diff[i]-1)*n_cols*8;
        __asm__ __volatile__ (
            "addq %[shift], %[mat]             \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift)
            :"0"(mat_t), "1"(shift)
        );

        __asm__ __volatile__ (
            "subq %[back], %[min] \n\t"
            "subq %[back], %[max] \n\t"
            :[min]"=r"(min_vals), [max]"=r"(max_vals), [back]"=r"(back)
            :"0"(min_vals), "1"(max_vals), "2"(back)
        );
        i++;
    }
}


void get_matrix_minmax_loop_16z_A(double *min_vals, double *max_vals, double *mat_t, int64_t *indices_diff, int64_t n_idxs, int64_t n_rows, int64_t n_cols){
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=16, c_unroll = n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift;

    shift = indices_diff[0]*n_cols*8;
    __asm__ __volatile__ (
        "addq %[shift], %[mat]             \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift)
        :"0"(mat_t), "1"(shift)
    );

    back = n_cols * 8;
    i=1;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[min]), %%zmm0   \n\t"
                "vmovupd 0*8(%[max]), %%zmm1   \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat]), %%zmm2   \n\t"
                "\n\t"
                "vminpd %%zmm2, %%zmm0, %%zmm0 \n\t"
                "vmaxpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[min])   \n\t"
                "vmovupd %%zmm1, 0*8(%[max])   \n\t"
                "\n\t"
                "vmovupd 8*8(%[min]), %%zmm3   \n\t"
                "vmovupd 8*8(%[max]), %%zmm4   \n\t"
                "\n\t"
                "vmovupd 8*8(%[mat]), %%zmm5   \n\t"
                "\n\t"
                "vminpd %%zmm5, %%zmm3, %%zmm3 \n\t"
                "vmaxpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm3, 8*8(%[min])   \n\t"
                "vmovupd %%zmm4, 8*8(%[max])   \n\t"
                "\n\t"
                "subq $-16*8, %[min]            \n\t"
                "subq $-16*8, %[max]            \n\t"
                "subq $-16*8, %[mat]            \n\t"
                "\n\t"
                :[min]"=r"(min_vals), [max]"=r"(max_vals), [mat]"=r"(mat_t)
                :"0"(min_vals), "1"(max_vals), "2"(mat_t)
            );
        }

        shift = (indices_diff[i]-1)*n_cols*8;
        __asm__ __volatile__ (
            "addq %[shift], %[mat]             \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift)
            :"0"(mat_t), "1"(shift)
        );

        __asm__ __volatile__ (
            "subq %[back], %[min] \n\t"
            "subq %[back], %[max] \n\t"
            :[min]"=r"(min_vals), [max]"=r"(max_vals), [back]"=r"(back)
            :"0"(min_vals), "1"(max_vals), "2"(back)
        );
        i++;
    }
}