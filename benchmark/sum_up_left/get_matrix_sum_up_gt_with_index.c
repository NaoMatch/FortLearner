#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111


void get_matrix_sum_up_gt_with_index_C(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double  r00, r02, r03, r15;
    int64_t r01, r04;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        r15 = y[idx];
        j=0;
        c_unroll=n_cols/c_unroll_size;
        j_base = idx*n_cols;
        while (c_unroll--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];

            r03 = mat_t[j_base+j];

            r04 = r03>r02;

            r00 += r15 * r04;
            r01 += r04;

            sum_vals[j] = r00;
            cnt_vals[j] = r01;

            j+=c_unroll_size;
        }
        i++;
    }
}

void get_matrix_sum_up_gt_with_index_C_02(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double  r00, r02, r03, r05, r06, r07, r15;
    int64_t r01, r04, r08, r09;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        r15 = y[idx];
        j=0;

        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;

        j_base = idx*n_cols;
        while (c_unroll--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];
            r03 = sum_vals[j+1];
            r04 = cnt_vals[j+1];
            r05 = thr_vals[j+1];

            r06 = mat_t[j_base+j];
            r07 = mat_t[j_base+j+1];

            r08 = r06>r02;
            r09 = r07>r05;

            r00 += r15 * r08;
            r01 += r08;
            r03 += r15 * r09;
            r04 += r09;

            sum_vals[j] = r00;
            cnt_vals[j] = r01;
            sum_vals[j+1] = r03;
            cnt_vals[j+1] = r04;

            j+=c_unroll_size;
        }

        while(c_remain--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];

            r06 = mat_t[j_base+j];

            r08 = r06>r02;

            r00 += r15 * r08;
            r01 += r08;

            sum_vals[j] = r00;
            cnt_vals[j] = r01;

            j++;
        }

        i++;
    }
}


void get_matrix_sum_up_gt_with_index_C_04(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double  r00, r02, r03, r05, r06, r08, r09, r11, r16;
    int64_t r01, r04, r07, r10, r12, r13, r14, r15;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        r16 = y[idx];
        j=0;

        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;

        j_base = idx*n_cols;
        while (c_unroll--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];
            r03 = sum_vals[j+1];
            r04 = cnt_vals[j+1];
            r05 = thr_vals[j+1];
            r06 = sum_vals[j+2];
            r07 = cnt_vals[j+2];
            r08 = thr_vals[j+2];
            r09 = sum_vals[j+3];
            r10 = cnt_vals[j+3];
            r11 = thr_vals[j+3];

            r12 = mat_t[j_base+j]   > r02;
            r13 = mat_t[j_base+j+1] > r05;
            r14 = mat_t[j_base+j+2] > r08;
            r15 = mat_t[j_base+j+3] > r11;

            r00 += r16 * r12;
            r01 += r12;
            r03 += r16 * r13;
            r04 += r13;
            r06 += r16 * r14;
            r07 += r14;
            r09 += r16 * r15;
            r10 += r15;

            sum_vals[j]   = r00;
            cnt_vals[j]   = r01;
            sum_vals[j+1] = r03;
            cnt_vals[j+1] = r04;
            sum_vals[j+2] = r06;
            cnt_vals[j+2] = r07;
            sum_vals[j+3] = r09;
            cnt_vals[j+3] = r10;

            j+=c_unroll_size;
        }

        while(c_remain--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];

            r12 = mat_t[j_base+j]   > r02;

            r00 += r16 * r12;
            r01 += r12;

            sum_vals[j] = r00;
            cnt_vals[j] = r01;

            j++;
        }
        i++;
    }
}

void get_matrix_sum_up_gt_with_index_C_08(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=8, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx, i, j, j_base;
    double  r00, r02, r03, r05, r06, r08, r09, r11, r16;
    int64_t r01, r04, r07, r10, r12, r13, r14, r15;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    i=0;
    while (i_unroll--){
        idx = indices[i]-1;
        r16 = y[idx];
        j=0;

        c_unroll=n_cols/c_unroll_size;
        c_remain=n_cols%c_unroll_size;

        j_base = idx*n_cols;
        while (c_unroll--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];
            r03 = sum_vals[j+1];
            r04 = cnt_vals[j+1];
            r05 = thr_vals[j+1];
            r06 = sum_vals[j+2];
            r07 = cnt_vals[j+2];
            r08 = thr_vals[j+2];
            r09 = sum_vals[j+3];
            r10 = cnt_vals[j+3];
            r11 = thr_vals[j+3];

            r12 = mat_t[j_base+j]   > r02;
            r13 = mat_t[j_base+j+1] > r05;
            r14 = mat_t[j_base+j+2] > r08;
            r15 = mat_t[j_base+j+3] > r11;

            r00 += r16 * r12;
            r01 += r12;
            r03 += r16 * r13;
            r04 += r13;
            r06 += r16 * r14;
            r07 += r14;
            r09 += r16 * r15;
            r10 += r15;

            sum_vals[j]   = r00;
            cnt_vals[j]   = r01;
            sum_vals[j+1] = r03;
            cnt_vals[j+1] = r04;
            sum_vals[j+2] = r06;
            cnt_vals[j+2] = r07;
            sum_vals[j+3] = r09;
            cnt_vals[j+3] = r10;

            r00 = sum_vals[j+4];
            r01 = cnt_vals[j+4];
            r02 = thr_vals[j+4];
            r03 = sum_vals[j+5];
            r04 = cnt_vals[j+5];
            r05 = thr_vals[j+5];
            r06 = sum_vals[j+6];
            r07 = cnt_vals[j+6];
            r08 = thr_vals[j+6];
            r09 = sum_vals[j+7];
            r10 = cnt_vals[j+7];
            r11 = thr_vals[j+7];

            r12 = mat_t[j_base+j+4]   > r02;
            r13 = mat_t[j_base+j+5] > r05;
            r14 = mat_t[j_base+j+6] > r08;
            r15 = mat_t[j_base+j+7] > r11;

            r00 += r16 * r12;
            r01 += r12;
            r03 += r16 * r13;
            r04 += r13;
            r06 += r16 * r14;
            r07 += r14;
            r09 += r16 * r15;
            r10 += r15;

            sum_vals[j+4]   = r00;
            cnt_vals[j+4]   = r01;
            sum_vals[j+5] = r03;
            cnt_vals[j+5] = r04;
            sum_vals[j+6] = r06;
            cnt_vals[j+6] = r07;
            sum_vals[j+7] = r09;
            cnt_vals[j+7] = r10;

            j+=c_unroll_size;
        }

        while(c_remain--){
            r00 = sum_vals[j];
            r01 = cnt_vals[j];
            r02 = thr_vals[j];

            r12 = mat_t[j_base+j]   > r02;

            r00 += r16 * r12;
            r01 += r12;

            sum_vals[j] = r00;
            cnt_vals[j] = r01;

            j++;
        }
        i++;
    }

}

void get_matrix_sum_up_gt_with_index_A_02(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift, shift_y, one=1, iii;
    double y_idx;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    shift   = indices_diff[0]*8*n_cols;
    __asm__ __volatile__ (
        "addq %[shift],   %[mat]     \n\t"
        "VPBROADCASTQ  %[o], %%ymm15 \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift), [o]"=r"(one)
        :"0"(mat_t), "1"(shift), "2"(one)
    );

    back = n_cols * 8;
    i=0;

    y += indices_diff[0];
    y_idx = *(y);
    // i_unroll--;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        // printf("%f \n", y_idx);
        __asm__ __volatile__ (
            "VBROADCASTSD  %[y_idx], %%ymm14 \n\t"
            :[y_idx]"=m"(y_idx)
            :
        );
        y += indices_diff[i+1];
        y_idx = *(y);

        while(c_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sum]), %%xmm0        \n\t"
                "MOVDQU 0*8(%[cnt]), %%xmm1        \n\t"
                "movupd 0*8(%[thr]), %%xmm2        \n\t"
                "movupd 0*8(%[mat]), %%xmm3        \n\t"
                "\n\t"
                "VCMPGTPD %%xmm2, %%xmm3,  %%xmm3  \n\t"
                "\n\t"
                "vandpd   %%xmm3, %%xmm14, %%xmm2  \n\t"
                "vPAND    %%xmm3, %%xmm15, %%xmm3  \n\t"
                "\n\t"
                "vaddpd   %%xmm2, %%xmm0,  %%xmm0  \n\t"
                "vPADDQ   %%xmm3, %%xmm1,  %%xmm1  \n\t"
                "\n\t"
                "movupd %%xmm0, 0*8(%[sum])        \n\t"
                "MOVDQU %%xmm1, 0*8(%[cnt])        \n\t"
                "\n\t"
                "subq $-2*8, %[sum]                \n\t"
                "subq $-2*8, %[cnt]                \n\t"
                "subq $-2*8, %[thr]                \n\t"
                "subq $-2*8, %[mat]                \n\t"
                "\n\t"
                :[sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [mat]"=r"(mat_t)
                :      "0"(sum_vals),       "1"(cnt_vals),       "2"(thr_vals),       "3"(mat_t)
            );
        }

        shift   = (indices_diff[i+1]-1)*8*n_cols;

        __asm__ __volatile__ (
            "addq %[shift],   %[mat] \n\t"
            "subq %[back],    %[sum] \n\t"
            "subq %[back],    %[cnt] \n\t"
            "subq %[back],    %[thr] \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift),
                [sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [back]"=r"(back)
            :      "0"(mat_t),         "1"(shift),
                "2"(sum_vals),       "3"(cnt_vals),       "4"(thr_vals),        "5"(back)
        );
        i++;
    }
}


void get_matrix_sum_up_gt_with_index_A_04(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift, shift_y, one=1, iii;
    double y_idx;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    shift   = indices_diff[0]*8*n_cols;
    __asm__ __volatile__ (
        "addq %[shift],   %[mat]     \n\t"
        "VPBROADCASTQ  %[o], %%ymm15 \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift), [o]"=r"(one)
        :"0"(mat_t), "1"(shift), "2"(one)
    );

    back = n_cols * 8;
    i=0;

    y += indices_diff[0];
    y_idx = *(y);
    // i_unroll--;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        // printf("%f \n", y_idx);
        __asm__ __volatile__ (
            "VBROADCASTSD  %[y_idx], %%ymm14 \n\t"
            :[y_idx]"=m"(y_idx)
            :
        );
        y += indices_diff[i+1];
        y_idx = *(y);

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sum]), %%ymm0        \n\t"
                "vMOVDQU 0*8(%[cnt]), %%ymm1        \n\t"
                "vmovupd 0*8(%[thr]), %%ymm2        \n\t"
                "vmovupd 0*8(%[mat]), %%ymm3        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm2, %%ymm3,  %%ymm3  \n\t"
                "\n\t"
                "vandpd   %%ymm3, %%ymm14, %%ymm2  \n\t"
                "vPAND    %%ymm3, %%ymm15, %%ymm3  \n\t"
                "\n\t"
                "vaddpd   %%ymm2, %%ymm0,  %%ymm0  \n\t"
                "vPADDQ   %%ymm3, %%ymm1,  %%ymm1  \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[sum])        \n\t"
                "vMOVDQU %%ymm1, 0*8(%[cnt])        \n\t"
                "\n\t"
                "subq $-4*8, %[sum]                \n\t"
                "subq $-4*8, %[cnt]                \n\t"
                "subq $-4*8, %[thr]                \n\t"
                "subq $-4*8, %[mat]                \n\t"
                "\n\t"
                :[sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [mat]"=r"(mat_t)
                :      "0"(sum_vals),       "1"(cnt_vals),       "2"(thr_vals),       "3"(mat_t)
            );
        }

        shift   = (indices_diff[i+1]-1)*8*n_cols;

        __asm__ __volatile__ (
            "addq %[shift],   %[mat] \n\t"
            "subq %[back],    %[sum] \n\t"
            "subq %[back],    %[cnt] \n\t"
            "subq %[back],    %[thr] \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift),
                [sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [back]"=r"(back)
            :      "0"(mat_t),         "1"(shift),
                "2"(sum_vals),       "3"(cnt_vals),       "4"(thr_vals),        "5"(back)
        );
        i++;
    }
}


void get_matrix_sum_up_gt_with_index_A_04x(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift, shift_y, one=1, iii;
    double y_idx;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    shift   = indices_diff[0]*8*n_cols;
    __asm__ __volatile__ (
        "addq %[shift],   %[mat]     \n\t"
        "VPBROADCASTQ  %[o], %%ymm15 \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift), [o]"=r"(one)
        :"0"(mat_t), "1"(shift), "2"(one)
    );

    back = n_cols * 8;
    i=0;

    y += indices_diff[0];
    y_idx = *(y);
    // i_unroll--;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        // printf("%f \n", y_idx);
        __asm__ __volatile__ (
            "VBROADCASTSD  %[y_idx], %%ymm14 \n\t"
            :[y_idx]"=m"(y_idx)
            :
        );
        y += indices_diff[i+1];
        y_idx = *(y);

        while(c_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sum]), %%xmm0        \n\t"
                "MOVDQU 0*8(%[cnt]), %%xmm1        \n\t"
                "movupd 0*8(%[thr]), %%xmm2        \n\t"
                "movupd 0*8(%[mat]), %%xmm3        \n\t"
                "\n\t"
                "VCMPGTPD %%xmm2, %%xmm3,  %%xmm3  \n\t"
                "\n\t"
                "vandpd   %%xmm3, %%xmm14, %%xmm2  \n\t"
                "vPAND    %%xmm3, %%xmm15, %%xmm3  \n\t"
                "\n\t"
                "vaddpd   %%xmm2, %%xmm0,  %%xmm0  \n\t"
                "vPADDQ   %%xmm3, %%xmm1,  %%xmm1  \n\t"
                "\n\t"
                "movupd %%xmm0, 0*8(%[sum])        \n\t"
                "MOVDQU %%xmm1, 0*8(%[cnt])        \n\t"
                "\n\t"
                "movupd 2*8(%[sum]), %%xmm4        \n\t"
                "MOVDQU 2*8(%[cnt]), %%xmm5        \n\t"
                "movupd 2*8(%[thr]), %%xmm6        \n\t"
                "movupd 2*8(%[mat]), %%xmm7        \n\t"
                "\n\t"
                "VCMPGTPD %%xmm6, %%xmm7,  %%xmm7  \n\t"
                "\n\t"
                "vandpd   %%xmm7, %%xmm14, %%xmm6  \n\t"
                "vPAND    %%xmm7, %%xmm15, %%xmm7  \n\t"
                "\n\t"
                "vaddpd   %%xmm6, %%xmm4,  %%xmm4  \n\t"
                "vPADDQ   %%xmm7, %%xmm5,  %%xmm5  \n\t"
                "\n\t"
                "movupd %%xmm4, 2*8(%[sum])        \n\t"
                "MOVDQU %%xmm5, 2*8(%[cnt])        \n\t"
                "\n\t"
                "subq $-4*8, %[sum]                \n\t"
                "subq $-4*8, %[cnt]                \n\t"
                "subq $-4*8, %[thr]                \n\t"
                "subq $-4*8, %[mat]                \n\t"
                "\n\t"
                :[sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [mat]"=r"(mat_t)
                :      "0"(sum_vals),       "1"(cnt_vals),       "2"(thr_vals),       "3"(mat_t)
            );
        }

        shift   = (indices_diff[i+1]-1)*8*n_cols;

        __asm__ __volatile__ (
            "addq %[shift],   %[mat] \n\t"
            "subq %[back],    %[sum] \n\t"
            "subq %[back],    %[cnt] \n\t"
            "subq %[back],    %[thr] \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift),
                [sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [back]"=r"(back)
            :      "0"(mat_t),         "1"(shift),
                "2"(sum_vals),       "3"(cnt_vals),       "4"(thr_vals),        "5"(back)
        );
        i++;
    }
}

void get_matrix_sum_up_gt_with_index_A_08(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){

    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=8, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift, shift_y, one=1, iii;
    double y_idx;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    shift   = indices_diff[0]*8*n_cols;
    __asm__ __volatile__ (
        "addq %[shift],   %[mat]     \n\t"
        "VPBROADCASTQ  %[o], %%ymm15 \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift), [o]"=r"(one)
        :"0"(mat_t), "1"(shift), "2"(one)
    );

    back = n_cols * 8;
    i=0;

    y += indices_diff[0];
    y_idx = *(y);
    // i_unroll--;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        // printf("%f \n", y_idx);
        __asm__ __volatile__ (
            "VBROADCASTSD  %[y_idx], %%ymm14 \n\t"
            :[y_idx]"=m"(y_idx)
            :
        );
        y += indices_diff[i+1];
        y_idx = *(y);

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sum]), %%ymm0        \n\t"
                "vMOVDQU 0*8(%[cnt]), %%ymm1        \n\t"
                "vmovupd 0*8(%[thr]), %%ymm2        \n\t"
                "vmovupd 0*8(%[mat]), %%ymm3        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm2, %%ymm3,  %%ymm3  \n\t"
                "\n\t"
                "vandpd   %%ymm3, %%ymm14, %%ymm2  \n\t"
                "vPAND    %%ymm3, %%ymm15, %%ymm3  \n\t"
                "\n\t"
                "vaddpd   %%ymm2, %%ymm0,  %%ymm0  \n\t"
                "vPADDQ   %%ymm3, %%ymm1,  %%ymm1  \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[sum])        \n\t"
                "vMOVDQU %%ymm1, 0*8(%[cnt])        \n\t"
                "\n\t"
                "vmovupd 4*8(%[sum]), %%ymm4        \n\t"
                "vMOVDQU 4*8(%[cnt]), %%ymm5        \n\t"
                "vmovupd 4*8(%[thr]), %%ymm6        \n\t"
                "vmovupd 4*8(%[mat]), %%ymm7        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm6, %%ymm7,  %%ymm7  \n\t"
                "\n\t"
                "vandpd   %%ymm7, %%ymm14, %%ymm6  \n\t"
                "vPAND    %%ymm7, %%ymm15, %%ymm7  \n\t"
                "\n\t"
                "vaddpd   %%ymm6, %%ymm4,  %%ymm4  \n\t"
                "vPADDQ   %%ymm7, %%ymm5,  %%ymm5  \n\t"
                "\n\t"
                "vmovupd %%ymm4, 4*8(%[sum])        \n\t"
                "vMOVDQU %%ymm5, 4*8(%[cnt])        \n\t"
                "\n\t"
                "subq $-8*8, %[sum]                \n\t"
                "subq $-8*8, %[cnt]                \n\t"
                "subq $-8*8, %[thr]                \n\t"
                "subq $-8*8, %[mat]                \n\t"
                "\n\t"
                :[sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [mat]"=r"(mat_t)
                :      "0"(sum_vals),       "1"(cnt_vals),       "2"(thr_vals),       "3"(mat_t)
            );
        }

        shift = (indices_diff[i+1]-1)*8*n_cols;

        __asm__ __volatile__ (
            "addq %[shift],   %[mat] \n\t"
            "subq %[back],    %[sum] \n\t"
            "subq %[back],    %[cnt] \n\t"
            "subq %[back],    %[thr] \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift),
                [sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [back]"=r"(back)
            :      "0"(mat_t),         "1"(shift),
                "2"(sum_vals),       "3"(cnt_vals),       "4"(thr_vals),        "5"(back)
        );
        i++;
    }


}

void get_matrix_sum_up_gt_with_index_A_08z(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){
}

void get_matrix_sum_up_gt_with_index_A_16(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){
        
    int64_t i_unroll_size=1, i_unroll=n_idxs/i_unroll_size, i_remain=n_idxs%i_unroll_size;
    int64_t c_unroll_size=16, c_unroll=n_cols/c_unroll_size, c_remain=n_cols%c_unroll_size;
    int64_t idx0, i, tot=0, back, shift, shift_y, one=1, iii;
    double y_idx;

    for(int64_t iii=0; iii<n_cols; iii++){
        sum_vals[iii]=0;
        cnt_vals[iii]=0;
    }

    shift   = indices_diff[0]*8*n_cols;
    __asm__ __volatile__ (
        "addq %[shift],   %[mat]     \n\t"
        "VPBROADCASTQ  %[o], %%ymm15 \n\t"
        :[mat]"=r"(mat_t), [shift]"=r"(shift), [o]"=r"(one)
        :"0"(mat_t), "1"(shift), "2"(one)
    );

    back = n_cols * 8;
    i=0;

    y += indices_diff[0];
    y_idx = *(y);
    // i_unroll--;
    while(i_unroll--){
        c_unroll = n_cols/c_unroll_size;
        c_remain = n_cols%c_unroll_size;

        // printf("%f \n", y_idx);
        __asm__ __volatile__ (
            "VBROADCASTSD  %[y_idx], %%ymm14 \n\t"
            :[y_idx]"=m"(y_idx)
            :
        );
        y += indices_diff[i+1];
        y_idx = *(y);

        while(c_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sum]), %%ymm0        \n\t"
                "vMOVDQU 0*8(%[cnt]), %%ymm1        \n\t"
                "vmovupd 0*8(%[thr]), %%ymm2        \n\t"
                "vmovupd 0*8(%[mat]), %%ymm3        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm2, %%ymm3,  %%ymm3  \n\t"
                "\n\t"
                "vandpd   %%ymm3, %%ymm14, %%ymm2  \n\t"
                "vPAND    %%ymm3, %%ymm15, %%ymm3  \n\t"
                "\n\t"
                "vaddpd   %%ymm2, %%ymm0,  %%ymm0  \n\t"
                "vPADDQ   %%ymm3, %%ymm1,  %%ymm1  \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[sum])        \n\t"
                "vMOVDQU %%ymm1, 0*8(%[cnt])        \n\t"
                "\n\t"
                "vmovupd 4*8(%[sum]), %%ymm4        \n\t"
                "vMOVDQU 4*8(%[cnt]), %%ymm5        \n\t"
                "vmovupd 4*8(%[thr]), %%ymm6        \n\t"
                "vmovupd 4*8(%[mat]), %%ymm7        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm6, %%ymm7,  %%ymm7  \n\t"
                "\n\t"
                "vandpd   %%ymm7, %%ymm14, %%ymm6  \n\t"
                "vPAND    %%ymm7, %%ymm15, %%ymm7  \n\t"
                "\n\t"
                "vaddpd   %%ymm6, %%ymm4,  %%ymm4  \n\t"
                "vPADDQ   %%ymm7, %%ymm5,  %%ymm5  \n\t"
                "\n\t"
                "vmovupd %%ymm4, 4*8(%[sum])        \n\t"
                "vMOVDQU %%ymm5, 4*8(%[cnt])        \n\t"
                "\n\t"
                "vmovupd 8*8(%[sum]), %%ymm8        \n\t"
                "vMOVDQU 8*8(%[cnt]), %%ymm9        \n\t"
                "vmovupd 8*8(%[thr]), %%ymm10        \n\t"
                "vmovupd 8*8(%[mat]), %%ymm11        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm10, %%ymm11,  %%ymm11  \n\t"
                "\n\t"
                "vandpd   %%ymm11, %%ymm14, %%ymm10  \n\t"
                "vPAND    %%ymm11, %%ymm15, %%ymm11  \n\t"
                "\n\t"
                "vaddpd   %%ymm10, %%ymm8,  %%ymm8  \n\t"
                "vPADDQ   %%ymm11, %%ymm9,  %%ymm9  \n\t"
                "\n\t"
                "vmovupd %%ymm8, 8*8(%[sum])        \n\t"
                "vMOVDQU %%ymm9, 8*8(%[cnt])        \n\t"
                "\n\t"
                "vmovupd 12*8(%[sum]), %%ymm12        \n\t"
                "vMOVDQU 12*8(%[cnt]), %%ymm13        \n\t"
                "vmovupd 12*8(%[thr]), %%ymm0        \n\t"
                "vmovupd 12*8(%[mat]), %%ymm1        \n\t"
                "\n\t"
                "VCMPGTPD %%ymm0, %%ymm1,  %%ymm1  \n\t"
                "\n\t"
                "vandpd   %%ymm1, %%ymm14, %%ymm0  \n\t"
                "vPAND    %%ymm1, %%ymm15, %%ymm1  \n\t"
                "\n\t"
                "vaddpd   %%ymm0, %%ymm12,  %%ymm12  \n\t"
                "vPADDQ   %%ymm1, %%ymm13,  %%ymm13  \n\t"
                "\n\t"
                "vmovupd %%ymm12, 12*8(%[sum])        \n\t"
                "vMOVDQU %%ymm13, 12*8(%[cnt])        \n\t"
                "\n\t"
                "subq $-16*8, %[sum]                \n\t"
                "subq $-16*8, %[cnt]                \n\t"
                "subq $-16*8, %[thr]                \n\t"
                "subq $-16*8, %[mat]                \n\t"
                "\n\t"
                :[sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [mat]"=r"(mat_t)
                :      "0"(sum_vals),       "1"(cnt_vals),       "2"(thr_vals),       "3"(mat_t)
            );
        }

        shift = (indices_diff[i+1]-1)*8*n_cols;

        __asm__ __volatile__ (
            "addq %[shift],   %[mat] \n\t"
            "subq %[back],    %[sum] \n\t"
            "subq %[back],    %[cnt] \n\t"
            "subq %[back],    %[thr] \n\t"
            :[mat]"=r"(mat_t), [shift]"=r"(shift),
                [sum]"=r"(sum_vals), [cnt]"=r"(cnt_vals), [thr]"=r"(thr_vals), [back]"=r"(back)
            :      "0"(mat_t),         "1"(shift),
                "2"(sum_vals),       "3"(cnt_vals),       "4"(thr_vals),        "5"(back)
        );
        i++;
    }
}

void get_matrix_sum_up_gt_with_index_A_16z(
    double *sum_vals, int64_t *cnt_vals, double *thr_vals, 
    double *mat_t, double *y, int64_t *indices_diff, 
    int64_t n_idxs, int64_t n_rows, int64_t n_cols){
}