#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111





void matrix_sqsum_row_01x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 

}




void matrix_sqsum_row_02x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r14 = mat_ptr[i  +j*n_samples];
            r15 = mat_ptr[i+1+j*n_samples];

            r14 *= r14;
            r15 *= r15;

            r00 += r14;
            r01 += r15;

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1] = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 
}

void matrix_sqsum_row_04x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            r12 = mat_ptr[i+0+j*n_samples];
            r13 = mat_ptr[i+1+j*n_samples];
            r14 = mat_ptr[i+2+j*n_samples];
            r15 = mat_ptr[i+3+j*n_samples];

            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r00 += r12;
            r01 += r13;
            r02 += r14;
            r03 += r15;

            matrix_sqsum_row[i+0] = r00;
            matrix_sqsum_row[i+1] = r01;
            matrix_sqsum_row[i+2] = r02;
            matrix_sqsum_row[i+3] = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 
}

void matrix_sqsum_row_08x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            r04 = matrix_sqsum_row[i+4];
            r05 = matrix_sqsum_row[i+5];
            r06 = matrix_sqsum_row[i+6];
            r07 = matrix_sqsum_row[i+7];
            r08 = mat_ptr[i+0+j*n_samples];
            r09 = mat_ptr[i+1+j*n_samples];
            r10 = mat_ptr[i+2+j*n_samples];
            r11 = mat_ptr[i+3+j*n_samples];
            r12 = mat_ptr[i+4+j*n_samples];
            r13 = mat_ptr[i+5+j*n_samples];
            r14 = mat_ptr[i+6+j*n_samples];
            r15 = mat_ptr[i+7+j*n_samples];

            r08 *= r08;
            r09 *= r09;
            r10 *= r10;
            r11 *= r11;
            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r00 += r08;
            r01 += r09;
            r02 += r10;
            r03 += r11;
            r04 += r12;
            r05 += r13;
            r06 += r14;
            r07 += r15;

            matrix_sqsum_row[i+0] = r00;
            matrix_sqsum_row[i+1] = r01;
            matrix_sqsum_row[i+2] = r02;
            matrix_sqsum_row[i+3] = r03;
            matrix_sqsum_row[i+4] = r04;
            matrix_sqsum_row[i+5] = r05;
            matrix_sqsum_row[i+6] = r06;
            matrix_sqsum_row[i+7] = r07;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 
}

void matrix_sqsum_row_16x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=16, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            for(k=0; k<s_unroll; k+=s_unroll/2){
                r00 = matrix_sqsum_row[i+k+0];
                r01 = matrix_sqsum_row[i+k+1];
                r02 = matrix_sqsum_row[i+k+2];
                r03 = matrix_sqsum_row[i+k+3];
                r04 = matrix_sqsum_row[i+k+4];
                r05 = matrix_sqsum_row[i+k+5];
                r06 = matrix_sqsum_row[i+k+6];
                r07 = matrix_sqsum_row[i+k+7];
                r08 = mat_ptr[i+k+0+j*n_samples];
                r09 = mat_ptr[i+k+1+j*n_samples];
                r10 = mat_ptr[i+k+2+j*n_samples];
                r11 = mat_ptr[i+k+3+j*n_samples];
                r12 = mat_ptr[i+k+4+j*n_samples];
                r13 = mat_ptr[i+k+5+j*n_samples];
                r14 = mat_ptr[i+k+6+j*n_samples];
                r15 = mat_ptr[i+k+7+j*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
                r04 += r12;
                r05 += r13;
                r06 += r14;
                r07 += r15;

                matrix_sqsum_row[i+k+0] = r00;
                matrix_sqsum_row[i+k+1] = r01;
                matrix_sqsum_row[i+k+2] = r02;
                matrix_sqsum_row[i+k+3] = r03;
                matrix_sqsum_row[i+k+4] = r04;
                matrix_sqsum_row[i+k+5] = r05;
                matrix_sqsum_row[i+k+6] = r06;
                matrix_sqsum_row[i+k+7] = r07;
                }
            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 
}

void matrix_sqsum_row_32x01_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=32, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            for(k=0; k<s_unroll; k+=s_unroll/4){
                r00 = matrix_sqsum_row[i+k+0];
                r01 = matrix_sqsum_row[i+k+1];
                r02 = matrix_sqsum_row[i+k+2];
                r03 = matrix_sqsum_row[i+k+3];
                r04 = matrix_sqsum_row[i+k+4];
                r05 = matrix_sqsum_row[i+k+5];
                r06 = matrix_sqsum_row[i+k+6];
                r07 = matrix_sqsum_row[i+k+7];
                r08 = mat_ptr[i+k+0+j*n_samples];
                r09 = mat_ptr[i+k+1+j*n_samples];
                r10 = mat_ptr[i+k+2+j*n_samples];
                r11 = mat_ptr[i+k+3+j*n_samples];
                r12 = mat_ptr[i+k+4+j*n_samples];
                r13 = mat_ptr[i+k+5+j*n_samples];
                r14 = mat_ptr[i+k+6+j*n_samples];
                r15 = mat_ptr[i+k+7+j*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
                r04 += r12;
                r05 += r13;
                r06 += r14;
                r07 += r15;

                matrix_sqsum_row[i+k+0] = r00;
                matrix_sqsum_row[i+k+1] = r01;
                matrix_sqsum_row[i+k+2] = r02;
                matrix_sqsum_row[i+k+3] = r03;
                matrix_sqsum_row[i+k+4] = r04;
                matrix_sqsum_row[i+k+5] = r05;
                matrix_sqsum_row[i+k+6] = r06;
                matrix_sqsum_row[i+k+7] = r07;
                }
            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r15 = mat_ptr[i+j*n_samples];

            r15 *= r15;

            r00 += r15;

            matrix_sqsum_row[i+0] = r00;

            i+=1;
        }

        j+=1;
    } 
}




void matrix_sqsum_row_01x02_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r14 = mat_ptr[i+0+(j+0)*n_samples];
            r15 = mat_ptr[i+0+(j+1)*n_samples];

            r14 *= r14;
            r15 *= r15;

            r14 += r15;
            r00 += r14;

            matrix_sqsum_row[i+0]   = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r14 = mat_ptr[i+0+(j+0)*n_samples];
            r15 = mat_ptr[i+0+(j+1)*n_samples];

            r14 *= r14;
            r15 *= r15;

            r14 += r15;
            r00 += r14;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&1){
                // printf("%ld \n", n_columns_unroll);
                n_samples_unroll=n_samples/s_unroll;
                n_samples_remain=n_samples%s_unroll;

                i=0;
                while(n_samples_unroll--){
                    // printf("%ld \n", n_samples_unroll);
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=s_unroll;
                }

                while(n_samples_remain--){
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=1;
                }

                j+=1;
        }
    }
}

void matrix_sqsum_row_01x04_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            r00 = matrix_sqsum_row[i+0];
            r12 = mat_ptr[i+0+(j+0)*n_samples];
            r13 = mat_ptr[i+0+(j+1)*n_samples];
            r14 = mat_ptr[i+0+(j+2)*n_samples];
            r15 = mat_ptr[i+0+(j+3)*n_samples];

            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r12 += r13;
            r14 += r15;

            r12 += r14;
            r00 += r12;

            matrix_sqsum_row[i+0]   = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r12 = mat_ptr[i+0+(j+0)*n_samples];
            r13 = mat_ptr[i+0+(j+1)*n_samples];
            r14 = mat_ptr[i+0+(j+2)*n_samples];
            r15 = mat_ptr[i+0+(j+3)*n_samples];

            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r12 += r13;
            r14 += r15;

            r12 += r14;
            r00 += r12;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
                // printf("%ld \n", n_columns_unroll);
                n_samples_unroll=n_samples/s_unroll;
                n_samples_remain=n_samples%s_unroll;

                i=0;
                while(n_samples_unroll--){
                    // printf("%ld \n", n_samples_unroll);
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=s_unroll;
                }

                while(n_samples_remain--){
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=1;
                }

                j+=1;
        }
    }
}

void matrix_sqsum_row_01x08_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            r00 = matrix_sqsum_row[i+0];
            r08 = mat_ptr[i+0+(j+0)*n_samples];
            r09 = mat_ptr[i+0+(j+1)*n_samples];
            r10 = mat_ptr[i+0+(j+2)*n_samples];
            r11 = mat_ptr[i+0+(j+3)*n_samples];
            r12 = mat_ptr[i+0+(j+4)*n_samples];
            r13 = mat_ptr[i+0+(j+5)*n_samples];
            r14 = mat_ptr[i+0+(j+6)*n_samples];
            r15 = mat_ptr[i+0+(j+7)*n_samples];

            r08 *= r08;
            r09 *= r09;
            r10 *= r10;
            r11 *= r11;
            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r08 += r09;
            r10 += r11;
            r12 += r13;
            r14 += r15;

            r08 += r10;
            r12 += r14;

            r08 += r12;
            r00 += r08;

            matrix_sqsum_row[i+0]   = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r08 = mat_ptr[i+0+(j+0)*n_samples];
            r09 = mat_ptr[i+0+(j+1)*n_samples];
            r10 = mat_ptr[i+0+(j+2)*n_samples];
            r11 = mat_ptr[i+0+(j+3)*n_samples];
            r12 = mat_ptr[i+0+(j+0)*n_samples];
            r13 = mat_ptr[i+0+(j+1)*n_samples];
            r14 = mat_ptr[i+0+(j+2)*n_samples];
            r15 = mat_ptr[i+0+(j+3)*n_samples];

            r08 *= r08;
            r09 *= r09;
            r10 *= r10;
            r11 *= r11;
            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r08 += r09;
            r10 += r11;
            r12 += r13;
            r14 += r15;

            r08 += r10;
            r12 += r14;

            r08 += r12;
            r00 += r08;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
                // printf("%ld \n", n_columns_unroll);
                n_samples_unroll=n_samples/s_unroll;
                n_samples_remain=n_samples%s_unroll;

                i=0;
                while(n_samples_unroll--){
                    // printf("%ld \n", n_samples_unroll);
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=s_unroll;
                }

                while(n_samples_remain--){
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=1;
                }

                j+=1;
        }
    }
}

void matrix_sqsum_row_01x16_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+0+(j+1)*n_samples];
                r10 = mat_ptr[i+0+(j+2)*n_samples];
                r11 = mat_ptr[i+0+(j+3)*n_samples];
                r12 = mat_ptr[i+0+(j+4)*n_samples];
                r13 = mat_ptr[i+0+(j+5)*n_samples];
                r14 = mat_ptr[i+0+(j+6)*n_samples];
                r15 = mat_ptr[i+0+(j+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+0+(j+1)*n_samples];
                r10 = mat_ptr[i+0+(j+2)*n_samples];
                r11 = mat_ptr[i+0+(j+3)*n_samples];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=8;
        }

        if(n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
                // printf("%ld \n", n_columns_unroll);
                n_samples_unroll=n_samples/s_unroll;
                n_samples_remain=n_samples%s_unroll;

                i=0;
                while(n_samples_unroll--){
                    // printf("%ld \n", n_samples_unroll);
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=s_unroll;
                }

                while(n_samples_remain--){
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=1;
                }

                j+=1;
        }
    }

}

void matrix_sqsum_row_01x32_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=1, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&16){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<16; k+=8){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                    r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r09;
                    r10 += r11;
                    r12 += r13;
                    r14 += r15;

                    r08 += r10;
                    r12 += r14;

                    r08 += r12;
                    r00 += r08;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<16; k+=8){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r11 = mat_ptr[i+0+(j+k+3)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+4)*n_samples];
                    r13 = mat_ptr[i+0+(j+k+5)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+6)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+7)*n_samples];

                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r09;
                    r10 += r11;
                    r12 += r13;
                    r14 += r15;

                    r08 += r10;
                    r12 += r14;

                    r08 += r12;
                    r00 += r08;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=16;
        }

        if(n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+0+(j+1)*n_samples];
                r10 = mat_ptr[i+0+(j+2)*n_samples];
                r11 = mat_ptr[i+0+(j+3)*n_samples];
                r12 = mat_ptr[i+0+(j+4)*n_samples];
                r13 = mat_ptr[i+0+(j+5)*n_samples];
                r14 = mat_ptr[i+0+(j+6)*n_samples];
                r15 = mat_ptr[i+0+(j+7)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+0+(j+1)*n_samples];
                r10 = mat_ptr[i+0+(j+2)*n_samples];
                r11 = mat_ptr[i+0+(j+3)*n_samples];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r09;
                r10 += r11;
                r12 += r13;
                r14 += r15;

                r08 += r10;
                r12 += r14;

                r08 += r12;
                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=8;
        }

        if(n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+0+(j+1)*n_samples];
                r14 = mat_ptr[i+0+(j+2)*n_samples];
                r15 = mat_ptr[i+0+(j+3)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;

                r12 += r14;
                r00 += r12;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
                // printf("%ld \n", n_columns_unroll);
                n_samples_unroll=n_samples/s_unroll;
                n_samples_remain=n_samples%s_unroll;

                i=0;
                while(n_samples_unroll--){
                    // printf("%ld \n", n_samples_unroll);
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=s_unroll;
                }

                while(n_samples_remain--){
                    r00 = matrix_sqsum_row[i+0];
                    r15 = mat_ptr[i+j*n_samples];

                    r15 *= r15;

                    r00 += r15;

                    matrix_sqsum_row[i+0] = r00;

                    i+=1;
                }

                j+=1;
        }
    }
}





void matrix_sqsum_row_02x02_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r12 = mat_ptr[i+0+(j+0)*n_samples];
            r13 = mat_ptr[i+1+(j+0)*n_samples];
            r14 = mat_ptr[i+0+(j+1)*n_samples];
            r15 = mat_ptr[i+1+(j+1)*n_samples];

            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r12 += r14;
            r13 += r15;
            r00 += r12;
            r01 += r13;

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r14 = mat_ptr[i+0+(j+0)*n_samples];
            r15 = mat_ptr[i+0+(j+1)*n_samples];

            r14 *= r14;
            r15 *= r15;

            r14 += r15;
            r00 += r14;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];

                r12 *= r12;
                r13 *= r13;

                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_02x04_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r08 = mat_ptr[i+0+(j+0)*n_samples];
            r09 = mat_ptr[i+1+(j+0)*n_samples];
            r10 = mat_ptr[i+0+(j+1)*n_samples];
            r11 = mat_ptr[i+1+(j+1)*n_samples];
            r12 = mat_ptr[i+0+(j+2)*n_samples];
            r13 = mat_ptr[i+1+(j+2)*n_samples];
            r14 = mat_ptr[i+0+(j+3)*n_samples];
            r15 = mat_ptr[i+1+(j+3)*n_samples];

            r08 *= r08;
            r09 *= r09;
            r10 *= r10;
            r11 *= r11;
            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r08 += r10;
            r09 += r11;
            r12 += r14;
            r13 += r15;

            r08 += r12;
            r09 += r13;

            r00 += r08;
            r01 += r09;

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r08 = mat_ptr[i+0+(j+0)*n_samples];
            r10 = mat_ptr[i+0+(j+1)*n_samples];
            r12 = mat_ptr[i+0+(j+2)*n_samples];
            r14 = mat_ptr[i+0+(j+3)*n_samples];

            r08 *= r08;
            r10 *= r10;
            r12 *= r12;
            r14 *= r14;

            r08 += r10;
            r12 += r14;

            r08 += r12;

            r00 += r08;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];
                r14 = mat_ptr[i+0+(j+1)*n_samples];
                r15 = mat_ptr[i+1+(j+1)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r14;
                r13 += r15;
                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];

                r12 *= r12;
                r13 *= r13;

                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_02x08_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;
            }
            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r11 = mat_ptr[i+1+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r13 = mat_ptr[i+1+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];
                r15 = mat_ptr[i+1+(j+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];
                r14 = mat_ptr[i+0+(j+1)*n_samples];
                r15 = mat_ptr[i+1+(j+1)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r14;
                r13 += r15;
                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];

                r12 *= r12;
                r13 *= r13;

                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_02x16_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;
            }
            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                for(k=0; k<8; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                    r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r10;
                    r09 += r11;
                    r12 += r14;
                    r13 += r15;

                    r08 += r12;
                    r09 += r13;

                    r00 += r08;
                    r01 += r09;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                    r08 *= r08;
                    r10 *= r10;
                    r12 *= r12;
                    r14 *= r14;

                    r08 += r10;
                    r12 += r14;

                    r08 += r12;

                    r00 += r08;
                }
                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r11 = mat_ptr[i+1+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r13 = mat_ptr[i+1+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];
                r15 = mat_ptr[i+1+(j+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];
                r14 = mat_ptr[i+0+(j+1)*n_samples];
                r15 = mat_ptr[i+1+(j+1)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r14;
                r13 += r15;
                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];

                r12 *= r12;
                r13 *= r13;

                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_02x32_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            for(k=0; k<c_unroll; k+=c_unroll/8){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/8){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;
            }
            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&16){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                for(k=0; k<16; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                    r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r10;
                    r09 += r11;
                    r12 += r14;
                    r13 += r15;

                    r08 += r12;
                    r09 += r13;

                    r00 += r08;
                    r01 += r09;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<16; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                    r08 *= r08;
                    r10 *= r10;
                    r12 *= r12;
                    r14 *= r14;

                    r08 += r10;
                    r12 += r14;

                    r08 += r12;

                    r00 += r08;
                }
                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=16;
        }

        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                for(k=0; k<8; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r11 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];
                    r15 = mat_ptr[i+1+(j+k+3)*n_samples];

                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r10;
                    r09 += r11;
                    r12 += r14;
                    r13 += r15;

                    r08 += r12;
                    r09 += r13;

                    r00 += r08;
                    r01 += r09;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=4){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+2)*n_samples];
                    r14 = mat_ptr[i+0+(j+k+3)*n_samples];

                    r08 *= r08;
                    r10 *= r10;
                    r12 *= r12;
                    r14 *= r14;

                    r08 += r10;
                    r12 += r14;

                    r08 += r12;

                    r00 += r08;
                }
                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r11 = mat_ptr[i+1+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r13 = mat_ptr[i+1+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];
                r15 = mat_ptr[i+1+(j+3)*n_samples];

                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r10;
                r09 += r11;
                r12 += r14;
                r13 += r15;

                r08 += r12;
                r09 += r13;

                r00 += r08;
                r01 += r09;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r10 = mat_ptr[i+0+(j+1)*n_samples];
                r12 = mat_ptr[i+0+(j+2)*n_samples];
                r14 = mat_ptr[i+0+(j+3)*n_samples];

                r08 *= r08;
                r10 *= r10;
                r12 *= r12;
                r14 *= r14;

                r08 += r10;
                r12 += r14;

                r08 += r12;

                r00 += r08;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];
                r14 = mat_ptr[i+0+(j+1)*n_samples];
                r15 = mat_ptr[i+1+(j+1)*n_samples];

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r14;
                r13 += r15;
                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r12 = mat_ptr[i+0+(j+0)*n_samples];
                r13 = mat_ptr[i+1+(j+0)*n_samples];

                r12 *= r12;
                r13 *= r13;

                r00 += r12;
                r01 += r13;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}





void matrix_sqsum_row_04x02_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            r08 = mat_ptr[i+0+(j+0)*n_samples];
            r09 = mat_ptr[i+1+(j+0)*n_samples];
            r10 = mat_ptr[i+2+(j+0)*n_samples];
            r11 = mat_ptr[i+3+(j+0)*n_samples];
            r12 = mat_ptr[i+0+(j+1)*n_samples];
            r13 = mat_ptr[i+1+(j+1)*n_samples];
            r14 = mat_ptr[i+2+(j+1)*n_samples];
            r15 = mat_ptr[i+3+(j+1)*n_samples];
            
            r08 *= r08;
            r09 *= r09;
            r10 *= r10;
            r11 *= r11;
            r12 *= r12;
            r13 *= r13;
            r14 *= r14;
            r15 *= r15;

            r08 += r12;
            r09 += r13;
            r10 += r14;
            r11 += r15;

            r00 += r08;
            r01 += r09;
            r02 += r10;
            r03 += r11;

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;
            matrix_sqsum_row[i+2]   = r02;
            matrix_sqsum_row[i+3]   = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r14 = mat_ptr[i+0+(j+0)*n_samples];
            r15 = mat_ptr[i+0+(j+1)*n_samples];

            r14 *= r14;
            r15 *= r15;

            r14 += r15;
            r00 += r14;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_04x04_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;
            matrix_sqsum_row[i+2]   = r02;
            matrix_sqsum_row[i+3]   = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                r12 = mat_ptr[i+0+(j+1)*n_samples];
                r13 = mat_ptr[i+1+(j+1)*n_samples];
                r14 = mat_ptr[i+2+(j+1)*n_samples];
                r15 = mat_ptr[i+3+(j+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_04x08_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;
            matrix_sqsum_row[i+2]   = r02;
            matrix_sqsum_row[i+3]   = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<4; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                r12 = mat_ptr[i+0+(j+1)*n_samples];
                r13 = mat_ptr[i+1+(j+1)*n_samples];
                r14 = mat_ptr[i+2+(j+1)*n_samples];
                r15 = mat_ptr[i+3+(j+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }

}

void matrix_sqsum_row_04x16_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            for(k=0; k<c_unroll; k+=c_unroll/8){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;
            matrix_sqsum_row[i+2]   = r02;
            matrix_sqsum_row[i+3]   = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/8){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<8; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<4; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                r12 = mat_ptr[i+0+(j+1)*n_samples];
                r13 = mat_ptr[i+1+(j+1)*n_samples];
                r14 = mat_ptr[i+2+(j+1)*n_samples];
                r15 = mat_ptr[i+3+(j+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_04x32_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            r00 = matrix_sqsum_row[i+0];
            r01 = matrix_sqsum_row[i+1];
            r02 = matrix_sqsum_row[i+2];
            r03 = matrix_sqsum_row[i+3];
            for(k=0; k<c_unroll; k+=c_unroll/16){
                r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;
            }

            matrix_sqsum_row[i+0]   = r00;
            matrix_sqsum_row[i+1]   = r01;
            matrix_sqsum_row[i+2]   = r02;
            matrix_sqsum_row[i+3]   = r03;

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/16){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&16){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<16; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<16; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=16;
        }

        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<8; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                for(k=0; k<4; k+=2){
                    r08 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }
            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                r12 = mat_ptr[i+0+(j+1)*n_samples];
                r13 = mat_ptr[i+1+(j+1)*n_samples];
                r14 = mat_ptr[i+2+(j+1)*n_samples];
                r15 = mat_ptr[i+3+(j+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                r00 = matrix_sqsum_row[i+0];
                r01 = matrix_sqsum_row[i+1];
                r02 = matrix_sqsum_row[i+2];
                r03 = matrix_sqsum_row[i+3];
                r08 = mat_ptr[i+0+(j+0)*n_samples];
                r09 = mat_ptr[i+1+(j+0)*n_samples];
                r10 = mat_ptr[i+2+(j+0)*n_samples];
                r11 = mat_ptr[i+3+(j+0)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+0]   = r00;
                matrix_sqsum_row[i+1]   = r01;
                matrix_sqsum_row[i+2]   = r02;
                matrix_sqsum_row[i+3]   = r03;

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}




void matrix_sqsum_row_08x02_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k, l;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            for(l=0; l<s_unroll; l+=s_unroll/2){
                r00 = matrix_sqsum_row[i+l+0];
                r01 = matrix_sqsum_row[i+l+1];
                r02 = matrix_sqsum_row[i+l+2];
                r03 = matrix_sqsum_row[i+l+3];
                r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                r12 = mat_ptr[i+l+0+(j+1)*n_samples];
                r13 = mat_ptr[i+l+1+(j+1)*n_samples];
                r14 = mat_ptr[i+l+2+(j+1)*n_samples];
                r15 = mat_ptr[i+l+3+(j+1)*n_samples];
                
                r08 *= r08;
                r09 *= r09;
                r10 *= r10;
                r11 *= r11;
                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r08 += r12;
                r09 += r13;
                r10 += r14;
                r11 += r15;

                r00 += r08;
                r01 += r09;
                r02 += r10;
                r03 += r11;

                matrix_sqsum_row[i+l+0]   = r00;
                matrix_sqsum_row[i+l+1]   = r01;
                matrix_sqsum_row[i+l+2]   = r02;
                matrix_sqsum_row[i+l+3]   = r03;
            }

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            r14 = mat_ptr[i+0+(j+0)*n_samples];
            r15 = mat_ptr[i+0+(j+1)*n_samples];

            r14 *= r14;
            r15 *= r15;

            r14 += r15;
            r00 += r14;

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_08x04_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k, l;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            for(l=0; l<s_unroll; l+=s_unroll/2){
                r00 = matrix_sqsum_row[i+l+0];
                r01 = matrix_sqsum_row[i+l+1];
                r02 = matrix_sqsum_row[i+l+2];
                r03 = matrix_sqsum_row[i+l+3];
                for(k=0; k<c_unroll; k+=c_unroll/2){
                    r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+l+0]   = r00;
                matrix_sqsum_row[i+l+1]   = r01;
                matrix_sqsum_row[i+l+2]   = r02;
                matrix_sqsum_row[i+l+3]   = r03;
            }

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/2){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                
                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_08x08_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k, l;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            for(l=0; l<s_unroll; l+=s_unroll/2){
                r00 = matrix_sqsum_row[i+l+0];
                r01 = matrix_sqsum_row[i+l+1];
                r02 = matrix_sqsum_row[i+l+2];
                r03 = matrix_sqsum_row[i+l+3];
                for(k=0; k<c_unroll; k+=c_unroll/4){
                    r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+l+0]   = r00;
                matrix_sqsum_row[i+l+1]   = r01;
                matrix_sqsum_row[i+l+2]   = r02;
                matrix_sqsum_row[i+l+3]   = r03;
            }

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/4){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                
                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<4; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_08x16_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k, l;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            for(l=0; l<s_unroll; l+=s_unroll/2){
                r00 = matrix_sqsum_row[i+l+0];
                r01 = matrix_sqsum_row[i+l+1];
                r02 = matrix_sqsum_row[i+l+2];
                r03 = matrix_sqsum_row[i+l+3];
                for(k=0; k<c_unroll; k+=c_unroll/8){
                    r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+l+0]   = r00;
                matrix_sqsum_row[i+l+1]   = r01;
                matrix_sqsum_row[i+l+2]   = r02;
                matrix_sqsum_row[i+l+3]   = r03;
            }

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/8){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                
                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<8; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<4; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}

void matrix_sqsum_row_08x32_C_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 
    int64_t i, j, k, l;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    j=0;
    while(n_columns_unroll--){
        // printf("%ld \n", n_columns_unroll);
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        i=0;
        while(n_samples_unroll--){
            // printf("%ld \n", n_samples_unroll);
            for(l=0; l<s_unroll; l+=s_unroll/2){
                r00 = matrix_sqsum_row[i+l+0];
                r01 = matrix_sqsum_row[i+l+1];
                r02 = matrix_sqsum_row[i+l+2];
                r03 = matrix_sqsum_row[i+l+3];
                for(k=0; k<c_unroll; k+=c_unroll/16){
                    r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;
                }

                matrix_sqsum_row[i+l+0]   = r00;
                matrix_sqsum_row[i+l+1]   = r01;
                matrix_sqsum_row[i+l+2]   = r02;
                matrix_sqsum_row[i+l+3]   = r03;
            }

            i+=s_unroll;
        }

        while(n_samples_remain--){
            r00 = matrix_sqsum_row[i+0];
            for(k=0; k<c_unroll; k+=c_unroll/16){
                r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                
                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;
            }

            matrix_sqsum_row[i+0]   = r00;

            i+=1;
        }

        j+=c_unroll;
    } 

    if (n_columns_remain>0){
        if(n_columns_remain&16){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<16; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<16; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=16;
        }

        if(n_columns_remain&8){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<8; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<8; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=8;
        }

        if(n_columns_remain&4){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    for(k=0; k<4; k+=2){
                        r08 = mat_ptr[i+l+0+(j+k+0)*n_samples];
                        r09 = mat_ptr[i+l+1+(j+k+0)*n_samples];
                        r10 = mat_ptr[i+l+2+(j+k+0)*n_samples];
                        r11 = mat_ptr[i+l+3+(j+k+0)*n_samples];
                        r12 = mat_ptr[i+l+0+(j+k+1)*n_samples];
                        r13 = mat_ptr[i+l+1+(j+k+1)*n_samples];
                        r14 = mat_ptr[i+l+2+(j+k+1)*n_samples];
                        r15 = mat_ptr[i+l+3+(j+k+1)*n_samples];
                        
                        r08 *= r08;
                        r09 *= r09;
                        r10 *= r10;
                        r11 *= r11;
                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r08 += r12;
                        r09 += r13;
                        r10 += r14;
                        r11 += r15;

                        r00 += r08;
                        r01 += r09;
                        r02 += r10;
                        r03 += r11;
                    }

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                for(k=0; k<4; k+=2){
                    r14 = mat_ptr[i+0+(j+k+0)*n_samples];
                    r15 = mat_ptr[i+0+(j+k+1)*n_samples];
                    
                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    r00 += r14;
                }

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=4;
        }

        if(n_columns_remain&2){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    r12 = mat_ptr[i+l+0+(j+1)*n_samples];
                    r13 = mat_ptr[i+l+1+(j+1)*n_samples];
                    r14 = mat_ptr[i+l+2+(j+1)*n_samples];
                    r15 = mat_ptr[i+l+3+(j+1)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;
                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r08 += r12;
                    r09 += r13;
                    r10 += r14;
                    r11 += r15;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];
                r15 = mat_ptr[i+0+(j+1)*n_samples];

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=2;
        }

        if(n_columns_remain&1){
            // printf("%ld \n", n_columns_unroll);
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            i=0;
            while(n_samples_unroll--){
                // printf("%ld \n", n_samples_unroll);
                for(l=0; l<s_unroll; l+=s_unroll/2){
                    r00 = matrix_sqsum_row[i+l+0];
                    r01 = matrix_sqsum_row[i+l+1];
                    r02 = matrix_sqsum_row[i+l+2];
                    r03 = matrix_sqsum_row[i+l+3];
                    r08 = mat_ptr[i+l+0+(j+0)*n_samples];
                    r09 = mat_ptr[i+l+1+(j+0)*n_samples];
                    r10 = mat_ptr[i+l+2+(j+0)*n_samples];
                    r11 = mat_ptr[i+l+3+(j+0)*n_samples];
                    
                    r08 *= r08;
                    r09 *= r09;
                    r10 *= r10;
                    r11 *= r11;

                    r00 += r08;
                    r01 += r09;
                    r02 += r10;
                    r03 += r11;

                    matrix_sqsum_row[i+l+0]   = r00;
                    matrix_sqsum_row[i+l+1]   = r01;
                    matrix_sqsum_row[i+l+2]   = r02;
                    matrix_sqsum_row[i+l+3]   = r03;
                }

                i+=s_unroll;
            }

            while(n_samples_remain--){
                r00 = matrix_sqsum_row[i+0];
                r14 = mat_ptr[i+0+(j+0)*n_samples];

                r14 *= r14;

                r00 += r14;

                matrix_sqsum_row[i+0]   = r00;

                i+=1;
            }

            j+=1;
        }
    }
}



void matrix_sqsum_row_02x01_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                "movupd 0*8(%[mat]),  %%xmm15     \n\t"
                "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                "movupd %%xmm0, 0*8(%[sq])        \n\t"
                "subq $-2*8, %[sq]                \n\t"
                "subq $-2*8, %[mat]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat]"=r"(mat_ptr)
                :     "0"(matrix_sqsum_row),       "1"(mat_ptr)
            );
        }

        if (n_samples_remain>0){
            double r00, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r15 = *(mat_ptr);

                r15 *= r15;
                r00 += r15;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr++;
            }
        }

        matrix_sqsum_row -= n_samples;
    }
}

void matrix_sqsum_row_04x01_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                "vmovupd 0*8(%[mat]),  %%ymm15     \n\t"
                "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                "subq $-4*8, %[sq]                \n\t"
                "subq $-4*8, %[mat]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat]"=r"(mat_ptr)
                :     "0"(matrix_sqsum_row),       "1"(mat_ptr)
            );
        }

        if (n_samples_remain>0){
            double r00, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r15 = *(mat_ptr);

                r15 *= r15;
                r00 += r15;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr++;
            }
        }

        matrix_sqsum_row -= n_samples;
    }
}

void matrix_sqsum_row_08x01_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                "vmovupd 0*8(%[mat]),  %%zmm15     \n\t"
                "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                "subq $-8*8, %[sq]                \n\t"
                "subq $-8*8, %[mat]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat]"=r"(mat_ptr)
                :     "0"(matrix_sqsum_row),       "1"(mat_ptr)
            );
        }

        if (n_samples_remain>0){
            double r00, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r15 = *(mat_ptr);

                r15 *= r15;
                r00 += r15;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr++;
            }
        }

        matrix_sqsum_row -= n_samples;
    }
}

void matrix_sqsum_row_16x01_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=16, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                "vmovupd 0*8(%[mat]),  %%zmm15     \n\t"
                "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 8*8(%[sq]),   %%zmm1      \n\t"
                "vmovupd 8*8(%[mat]),  %%zmm14     \n\t"
                "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                "vaddpd %%zmm14, %%zmm1,  %%zmm1  \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                "vmovupd %%zmm1, 8*8(%[sq])        \n\t"
                "subq $-16*8, %[sq]                \n\t"
                "subq $-16*8, %[mat]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat]"=r"(mat_ptr)
                :     "0"(matrix_sqsum_row),       "1"(mat_ptr)
            );
        }

        if (n_samples_remain>0){
            double r00, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r15 = *(mat_ptr);

                r15 *= r15;
                r00 += r15;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr++;
            }
        }

        matrix_sqsum_row -= n_samples;
    }
}

void matrix_sqsum_row_32x01_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=32, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=1, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                "vmovupd 0*8(%[mat]),  %%zmm15     \n\t"
                "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 8*8(%[sq]),   %%zmm1      \n\t"
                "vmovupd 8*8(%[mat]),  %%zmm14     \n\t"
                "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                "vaddpd %%zmm14, %%zmm1,  %%zmm1  \n\t"
                "\n\t"
                "vmovupd 16*8(%[sq]),   %%zmm2      \n\t"
                "vmovupd 16*8(%[mat]),  %%zmm13     \n\t"
                "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm2,  %%zmm2  \n\t"
                "\n\t"
                "vmovupd 24*8(%[sq]),   %%zmm3      \n\t"
                "vmovupd 24*8(%[mat]),  %%zmm12     \n\t"
                "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm3,  %%zmm3  \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                "vmovupd %%zmm1, 8*8(%[sq])        \n\t"
                "vmovupd %%zmm2, 16*8(%[sq])        \n\t"
                "vmovupd %%zmm3, 24*8(%[sq])        \n\t"
                "subq $-32*8, %[sq]                \n\t"
                "subq $-32*8, %[mat]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat]"=r"(mat_ptr)
                :     "0"(matrix_sqsum_row),       "1"(mat_ptr)
            );
        }

        if (n_samples_remain>0){
            double r00, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r15 = *(mat_ptr);

                r15 *= r15;
                r00 += r15;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr++;
            }
        }

        matrix_sqsum_row -= n_samples;
    }
}





void matrix_sqsum_row_02x02_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                "\n\t"
                "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd %%xmm0, 0*8(%[sq])        \n\t"
                "subq $-2*8, %[sq]                \n\t"
                "subq $-2*8, %[mat00]             \n\t"
                "subq $-2*8, %[mat01]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r14 = *(mat_ptr00);
                r15 = *(mat_ptr01);

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                
                r00 += r14;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += n_samples;
        mat_ptr01        += n_samples;
    }

    if (n_columns_remain>0){
        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_02x04_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                "\n\t"
                "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                "\n\t"
                "movupd %%xmm0, 0*8(%[sq])        \n\t"
                "subq $-2*8, %[sq]                \n\t"
                "subq $-2*8, %[mat00]             \n\t"
                "subq $-2*8, %[mat01]               \n\t"
                "subq $-2*8, %[mat02]               \n\t"
                "subq $-2*8, %[mat03]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r12 = *(mat_ptr00);
                r13 = *(mat_ptr01);
                r14 = *(mat_ptr02);
                r15 = *(mat_ptr03);

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;
                
                r12 += r14;
                r00 += r12;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 3*n_samples;
        mat_ptr01        += 3*n_samples;
        mat_ptr02        += 3*n_samples;
        mat_ptr03        += 3*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_02x08_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 8*n_samples;
            mat_ptr01 -= 8*n_samples;
            mat_ptr02 -= 8*n_samples;
            mat_ptr03 -= 8*n_samples;

            __asm__ __volatile__ (
                "movupd %%xmm0, 0*8(%[sq])         \n\t"
                "subq $-2*8, %[sq] \n\t"
                "subq $-2*8, %[mat00] \n\t"
                "subq $-2*8, %[mat01] \n\t"
                "subq $-2*8, %[mat02] \n\t"
                "subq $-2*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/2){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 7*n_samples;
        mat_ptr01        += 7*n_samples;
        mat_ptr02        += 7*n_samples;
        mat_ptr03        += 7*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    "subq $-2*8, %[mat02]               \n\t"
                    "subq $-2*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_02x16_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/4){
                __asm__ __volatile__ (
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 16*n_samples;
            mat_ptr01 -= 16*n_samples;
            mat_ptr02 -= 16*n_samples;
            mat_ptr03 -= 16*n_samples;

            __asm__ __volatile__ (
                "movupd %%xmm0, 0*8(%[sq])         \n\t"
                "subq $-2*8, %[sq] \n\t"
                "subq $-2*8, %[mat00] \n\t"
                "subq $-2*8, %[mat01] \n\t"
                "subq $-2*8, %[mat02] \n\t"
                "subq $-2*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/4){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 15*n_samples;
        mat_ptr01        += 15*n_samples;
        mat_ptr02        += 15*n_samples;
        mat_ptr03        += 15*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                        "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                        "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                        "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                        "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                        "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                        "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                        "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                        "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "movupd %%xmm0, 0*8(%[sq])         \n\t"
                    "subq $-2*8, %[sq] \n\t"
                    "subq $-2*8, %[mat00] \n\t"
                    "subq $-2*8, %[mat01] \n\t"
                    "subq $-2*8, %[mat02] \n\t"
                    "subq $-2*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    "subq $-2*8, %[mat02]               \n\t"
                    "subq $-2*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_02x32_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=2, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/8){
                __asm__ __volatile__ (
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 32*n_samples;
            mat_ptr01 -= 32*n_samples;
            mat_ptr02 -= 32*n_samples;
            mat_ptr03 -= 32*n_samples;

            __asm__ __volatile__ (
                "movupd %%xmm0, 0*8(%[sq])         \n\t"
                "subq $-2*8, %[sq] \n\t"
                "subq $-2*8, %[mat00] \n\t"
                "subq $-2*8, %[mat01] \n\t"
                "subq $-2*8, %[mat02] \n\t"
                "subq $-2*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/8){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 32*n_samples;
                mat_ptr01 -= 32*n_samples;
                mat_ptr02 -= 32*n_samples;
                mat_ptr03 -= 32*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 31*n_samples;
        mat_ptr01        += 31*n_samples;
        mat_ptr02        += 31*n_samples;
        mat_ptr03        += 31*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&16){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<16; k+=4){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                        "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                        "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                        "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                        "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                        "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                        "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                        "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                        "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                __asm__ __volatile__ (
                    "movupd %%xmm0, 0*8(%[sq])         \n\t"
                    "subq $-2*8, %[sq] \n\t"
                    "subq $-2*8, %[mat00] \n\t"
                    "subq $-2*8, %[mat01] \n\t"
                    "subq $-2*8, %[mat02] \n\t"
                    "subq $-2*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<16; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 16*n_samples;
                    mat_ptr01 -= 16*n_samples;
                    mat_ptr02 -= 16*n_samples;
                    mat_ptr03 -= 16*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 15*n_samples;
            mat_ptr01        += 15*n_samples;
            mat_ptr02        += 15*n_samples;
            mat_ptr03        += 15*n_samples;
        }

        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                        "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                        "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                        "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                        "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                        "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                        "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                        "\n\t"
                        "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                        "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                        "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "movupd %%xmm0, 0*8(%[sq])         \n\t"
                    "subq $-2*8, %[sq] \n\t"
                    "subq $-2*8, %[mat00] \n\t"
                    "subq $-2*8, %[mat01] \n\t"
                    "subq $-2*8, %[mat02] \n\t"
                    "subq $-2*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat02]),  %%xmm13     \n\t"
                    "vmulpd %%xmm13, %%xmm13, %%xmm13 \n\t"
                    "vaddpd %%xmm13, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat03]),  %%xmm12     \n\t"
                    "vmulpd %%xmm12, %%xmm12, %%xmm12 \n\t"
                    "vaddpd %%xmm12, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    "subq $-2*8, %[mat02]               \n\t"
                    "subq $-2*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat01]),  %%xmm14     \n\t"
                    "vmulpd %%xmm14, %%xmm14, %%xmm14 \n\t"
                    "vaddpd %%xmm14, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    "subq $-2*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[sq]),   %%xmm0      \n\t"
                    "\n\t"
                    "movupd 0*8(%[mat00]),  %%xmm15     \n\t"
                    "vmulpd %%xmm15, %%xmm15, %%xmm15 \n\t"
                    "vaddpd %%xmm15, %%xmm0,  %%xmm0  \n\t"
                    "\n\t"
                    "movupd %%xmm0, 0*8(%[sq])        \n\t"
                    "subq $-2*8, %[sq]                \n\t"
                    "subq $-2*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}





void matrix_sqsum_row_04x02_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                "subq $-4*8, %[sq]                \n\t"
                "subq $-4*8, %[mat00]             \n\t"
                "subq $-4*8, %[mat01]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r14 = *(mat_ptr00);
                r15 = *(mat_ptr01);

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                
                r00 += r14;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += n_samples;
        mat_ptr01        += n_samples;
    }

    if (n_columns_remain>0){
        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_04x04_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                "\n\t"
                "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                "subq $-4*8, %[sq]                \n\t"
                "subq $-4*8, %[mat00]             \n\t"
                "subq $-4*8, %[mat01]               \n\t"
                "subq $-4*8, %[mat02]               \n\t"
                "subq $-4*8, %[mat03]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r12 = *(mat_ptr00);
                r13 = *(mat_ptr01);
                r14 = *(mat_ptr02);
                r15 = *(mat_ptr03);

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;
                
                r12 += r14;
                r00 += r12;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 3*n_samples;
        mat_ptr01        += 3*n_samples;
        mat_ptr02        += 3*n_samples;
        mat_ptr03        += 3*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_04x08_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/2){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 8*n_samples;
            mat_ptr01 -= 8*n_samples;
            mat_ptr02 -= 8*n_samples;
            mat_ptr03 -= 8*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                "subq $-4*8, %[sq] \n\t"
                "subq $-4*8, %[mat00] \n\t"
                "subq $-4*8, %[mat01] \n\t"
                "subq $-4*8, %[mat02] \n\t"
                "subq $-4*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/2){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 7*n_samples;
        mat_ptr01        += 7*n_samples;
        mat_ptr02        += 7*n_samples;
        mat_ptr03        += 7*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    "subq $-4*8, %[mat02]               \n\t"
                    "subq $-4*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_04x16_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 16*n_samples;
            mat_ptr01 -= 16*n_samples;
            mat_ptr02 -= 16*n_samples;
            mat_ptr03 -= 16*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                "subq $-4*8, %[sq] \n\t"
                "subq $-4*8, %[mat00] \n\t"
                "subq $-4*8, %[mat01] \n\t"
                "subq $-4*8, %[mat02] \n\t"
                "subq $-4*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/4){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 15*n_samples;
        mat_ptr01        += 15*n_samples;
        mat_ptr02        += 15*n_samples;
        mat_ptr03        += 15*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                        "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                        "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                        "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                        "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                        "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                        "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                        "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                        "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                    "subq $-4*8, %[sq] \n\t"
                    "subq $-4*8, %[mat00] \n\t"
                    "subq $-4*8, %[mat01] \n\t"
                    "subq $-4*8, %[mat02] \n\t"
                    "subq $-4*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    "subq $-4*8, %[mat02]               \n\t"
                    "subq $-4*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_04x32_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=4, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 32*n_samples;
            mat_ptr01 -= 32*n_samples;
            mat_ptr02 -= 32*n_samples;
            mat_ptr03 -= 32*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                "subq $-4*8, %[sq] \n\t"
                "subq $-4*8, %[mat00] \n\t"
                "subq $-4*8, %[mat01] \n\t"
                "subq $-4*8, %[mat02] \n\t"
                "subq $-4*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/8){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 32*n_samples;
                mat_ptr01 -= 32*n_samples;
                mat_ptr02 -= 32*n_samples;
                mat_ptr03 -= 32*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 31*n_samples;
        mat_ptr01        += 31*n_samples;
        mat_ptr02        += 31*n_samples;
        mat_ptr03        += 31*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&16){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<16; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                        "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                        "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                        "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                        "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                        "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                        "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                        "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                        "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                    "subq $-4*8, %[sq] \n\t"
                    "subq $-4*8, %[mat00] \n\t"
                    "subq $-4*8, %[mat01] \n\t"
                    "subq $-4*8, %[mat02] \n\t"
                    "subq $-4*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<16; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 16*n_samples;
                    mat_ptr01 -= 16*n_samples;
                    mat_ptr02 -= 16*n_samples;
                    mat_ptr03 -= 16*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 15*n_samples;
            mat_ptr01        += 15*n_samples;
            mat_ptr02        += 15*n_samples;
            mat_ptr03        += 15*n_samples;
        }

        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                        "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                        "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                        "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                        "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                        "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                        "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                        "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                        "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%ymm0, 0*8(%[sq])         \n\t"
                    "subq $-4*8, %[sq] \n\t"
                    "subq $-4*8, %[mat00] \n\t"
                    "subq $-4*8, %[mat01] \n\t"
                    "subq $-4*8, %[mat02] \n\t"
                    "subq $-4*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%ymm13     \n\t"
                    "vmulpd %%ymm13, %%ymm13, %%ymm13 \n\t"
                    "vaddpd %%ymm13, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%ymm12     \n\t"
                    "vmulpd %%ymm12, %%ymm12, %%ymm12 \n\t"
                    "vaddpd %%ymm12, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    "subq $-4*8, %[mat02]               \n\t"
                    "subq $-4*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%ymm14     \n\t"
                    "vmulpd %%ymm14, %%ymm14, %%ymm14 \n\t"
                    "vaddpd %%ymm14, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    "subq $-4*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%ymm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%ymm15     \n\t"
                    "vmulpd %%ymm15, %%ymm15, %%ymm15 \n\t"
                    "vaddpd %%ymm15, %%ymm0,  %%ymm0  \n\t"
                    "\n\t"
                    "vmovupd %%ymm0, 0*8(%[sq])        \n\t"
                    "subq $-4*8, %[sq]                \n\t"
                    "subq $-4*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}





void matrix_sqsum_row_08x02_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=2, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                "subq $-8*8, %[sq]                \n\t"
                "subq $-8*8, %[mat00]             \n\t"
                "subq $-8*8, %[mat01]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r14 = *(mat_ptr00);
                r15 = *(mat_ptr01);

                r14 *= r14;
                r15 *= r15;

                r14 += r15;
                
                r00 += r14;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += n_samples;
        mat_ptr01        += n_samples;
    }

    if (n_columns_remain>0){
        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_08x04_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=4, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                "\n\t"
                "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                "subq $-8*8, %[sq]                \n\t"
                "subq $-8*8, %[mat00]             \n\t"
                "subq $-8*8, %[mat01]               \n\t"
                "subq $-8*8, %[mat02]               \n\t"
                "subq $-8*8, %[mat03]               \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                r12 = *(mat_ptr00);
                r13 = *(mat_ptr01);
                r14 = *(mat_ptr02);
                r15 = *(mat_ptr03);

                r12 *= r12;
                r13 *= r13;
                r14 *= r14;
                r15 *= r15;

                r12 += r13;
                r14 += r15;
                
                r12 += r14;
                r00 += r12;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 3*n_samples;
        mat_ptr01        += 3*n_samples;
        mat_ptr02        += 3*n_samples;
        mat_ptr03        += 3*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_08x08_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=8, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/2){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 8*n_samples;
            mat_ptr01 -= 8*n_samples;
            mat_ptr02 -= 8*n_samples;
            mat_ptr03 -= 8*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                "subq $-8*8, %[sq] \n\t"
                "subq $-8*8, %[mat00] \n\t"
                "subq $-8*8, %[mat01] \n\t"
                "subq $-8*8, %[mat02] \n\t"
                "subq $-8*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/2){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 7*n_samples;
        mat_ptr01        += 7*n_samples;
        mat_ptr02        += 7*n_samples;
        mat_ptr03        += 7*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    "subq $-8*8, %[mat02]               \n\t"
                    "subq $-8*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_08x16_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=16, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 16*n_samples;
            mat_ptr01 -= 16*n_samples;
            mat_ptr02 -= 16*n_samples;
            mat_ptr03 -= 16*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                "subq $-8*8, %[sq] \n\t"
                "subq $-8*8, %[mat00] \n\t"
                "subq $-8*8, %[mat01] \n\t"
                "subq $-8*8, %[mat02] \n\t"
                "subq $-8*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/4){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 15*n_samples;
        mat_ptr01        += 15*n_samples;
        mat_ptr02        += 15*n_samples;
        mat_ptr03        += 15*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                        "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                        "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                        "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                        "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                        "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                        "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                        "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                        "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                    "subq $-8*8, %[sq] \n\t"
                    "subq $-8*8, %[mat00] \n\t"
                    "subq $-8*8, %[mat01] \n\t"
                    "subq $-8*8, %[mat02] \n\t"
                    "subq $-8*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    "subq $-8*8, %[mat02]               \n\t"
                    "subq $-8*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

void matrix_sqsum_row_08x32_A_r8(double *mat_ptr, double matrix_sqsum_row[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll=8, n_samples_unroll=n_samples/s_unroll, n_samples_remain=n_samples%s_unroll; 
    int64_t c_unroll=32, n_columns_unroll=n_columns/c_unroll, n_columns_remain=n_columns%c_unroll; 

    int64_t n=n_samples, k;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;
    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(n_columns_unroll--){
        n_samples_unroll=n_samples/s_unroll;
        n_samples_remain=n_samples%s_unroll;

        while(n_samples_unroll--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                :[sq]"=r"(matrix_sqsum_row)
                :     "0"(matrix_sqsum_row)
            );

            for(k=0; k<c_unroll; k+=c_unroll/8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                );
                mat_ptr00 += 4*n_samples;
                mat_ptr01 += 4*n_samples;
                mat_ptr02 += 4*n_samples;
                mat_ptr03 += 4*n_samples;
            }
            mat_ptr00 -= 32*n_samples;
            mat_ptr01 -= 32*n_samples;
            mat_ptr02 -= 32*n_samples;
            mat_ptr03 -= 32*n_samples;

            __asm__ __volatile__ (
                "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                "subq $-8*8, %[sq] \n\t"
                "subq $-8*8, %[mat00] \n\t"
                "subq $-8*8, %[mat01] \n\t"
                "subq $-8*8, %[mat02] \n\t"
                "subq $-8*8, %[mat03] \n\t"
                :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
            );
        }

        if (n_samples_remain>0){
            double r00, r14, r15;
            double r12, r13;
            while(n_samples_remain--){
                r00 = *(matrix_sqsum_row);
                for(k=0; k<c_unroll; k+=c_unroll/8){
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 32*n_samples;
                mat_ptr01 -= 32*n_samples;
                mat_ptr02 -= 32*n_samples;
                mat_ptr03 -= 32*n_samples;

                *(matrix_sqsum_row) = r00;

                matrix_sqsum_row++;
                mat_ptr00++;
                mat_ptr01++;
                mat_ptr02++;
                mat_ptr03++;
            }
        }

        matrix_sqsum_row -= n_samples;
        mat_ptr00        += 31*n_samples;
        mat_ptr01        += 31*n_samples;
        mat_ptr02        += 31*n_samples;
        mat_ptr03        += 31*n_samples;
    }


    if (n_columns_remain>0){
        if (n_columns_remain&16){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<16; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                        "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                        "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                        "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                        "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                        "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                        "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                        "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                        "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 16*n_samples;
                mat_ptr01 -= 16*n_samples;
                mat_ptr02 -= 16*n_samples;
                mat_ptr03 -= 16*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                    "subq $-8*8, %[sq] \n\t"
                    "subq $-8*8, %[mat00] \n\t"
                    "subq $-8*8, %[mat01] \n\t"
                    "subq $-8*8, %[mat02] \n\t"
                    "subq $-8*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<16; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 16*n_samples;
                    mat_ptr01 -= 16*n_samples;
                    mat_ptr02 -= 16*n_samples;
                    mat_ptr03 -= 16*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 15*n_samples;
            mat_ptr01        += 15*n_samples;
            mat_ptr02        += 15*n_samples;
            mat_ptr03        += 15*n_samples;
        }

        if (n_columns_remain&8){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    :[sq]"=r"(matrix_sqsum_row)
                    :     "0"(matrix_sqsum_row)
                );

                for(k=0; k<8; k+=4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                        "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                        "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                        "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                        "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                        "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                        "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                        "\n\t"
                        "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                        "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                        "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                        :[mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                        :        "0"(mat_ptr00),         "1"(mat_ptr01),         "2"(mat_ptr02),         "3"(mat_ptr03)
                    );
                    mat_ptr00 += 4*n_samples;
                    mat_ptr01 += 4*n_samples;
                    mat_ptr02 += 4*n_samples;
                    mat_ptr03 += 4*n_samples;
                }
                mat_ptr00 -= 8*n_samples;
                mat_ptr01 -= 8*n_samples;
                mat_ptr02 -= 8*n_samples;
                mat_ptr03 -= 8*n_samples;

                __asm__ __volatile__ (
                    "vmovupd %%zmm0, 0*8(%[sq])         \n\t"
                    "subq $-8*8, %[sq] \n\t"
                    "subq $-8*8, %[mat00] \n\t"
                    "subq $-8*8, %[mat01] \n\t"
                    "subq $-8*8, %[mat02] \n\t"
                    "subq $-8*8, %[mat03] \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    for(k=0; k<8; k+=4){
                        r12 = *(mat_ptr00);
                        r13 = *(mat_ptr01);
                        r14 = *(mat_ptr02);
                        r15 = *(mat_ptr03);

                        r12 *= r12;
                        r13 *= r13;
                        r14 *= r14;
                        r15 *= r15;

                        r12 += r13;
                        r14 += r15;
                        
                        r12 += r14;
                        r00 += r12;
                        mat_ptr00 += 4*n_samples;
                        mat_ptr01 += 4*n_samples;
                        mat_ptr02 += 4*n_samples;
                        mat_ptr03 += 4*n_samples;
                    }
                    mat_ptr00 -= 8*n_samples;
                    mat_ptr01 -= 8*n_samples;
                    mat_ptr02 -= 8*n_samples;
                    mat_ptr03 -= 8*n_samples;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 7*n_samples;
            mat_ptr01        += 7*n_samples;
            mat_ptr02        += 7*n_samples;
            mat_ptr03        += 7*n_samples;
        }

        if (n_columns_remain&4){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat02]),  %%zmm13     \n\t"
                    "vmulpd %%zmm13, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat03]),  %%zmm12     \n\t"
                    "vmulpd %%zmm12, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    "subq $-8*8, %[mat02]               \n\t"
                    "subq $-8*8, %[mat03]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01), [mat02]"=r"(mat_ptr02), [mat03]"=r"(mat_ptr03)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01),         "3"(mat_ptr02),         "4"(mat_ptr03)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                double r12, r13;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r12 = *(mat_ptr00);
                    r13 = *(mat_ptr01);
                    r14 = *(mat_ptr02);
                    r15 = *(mat_ptr03);

                    r12 *= r12;
                    r13 *= r13;
                    r14 *= r14;
                    r15 *= r15;

                    r12 += r13;
                    r14 += r15;
                    
                    r12 += r14;
                    r00 += r12;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                    mat_ptr02++;
                    mat_ptr03++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += 3*n_samples;
            mat_ptr01        += 3*n_samples;
            mat_ptr02        += 3*n_samples;
            mat_ptr03        += 3*n_samples;

        }

        if (n_columns_remain&2){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat01]),  %%zmm14     \n\t"
                    "vmulpd %%zmm14, %%zmm14, %%zmm14 \n\t"
                    "vaddpd %%zmm14, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    "subq $-8*8, %[mat01]               \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00), [mat01]"=r"(mat_ptr01)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00),         "2"(mat_ptr01)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);
                    r15 = *(mat_ptr01);

                    r14 *= r14;
                    r15 *= r15;

                    r14 += r15;
                    
                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                    mat_ptr01++;
                }
            }

            matrix_sqsum_row -= n_samples;
            mat_ptr00        += n_samples;
            mat_ptr01        += n_samples;
        }

        if (n_columns_remain&1){
            n_samples_unroll=n_samples/s_unroll;
            n_samples_remain=n_samples%s_unroll;

            while(n_samples_unroll--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[sq]),   %%zmm0      \n\t"
                    "\n\t"
                    "vmovupd 0*8(%[mat00]),  %%zmm15     \n\t"
                    "vmulpd %%zmm15, %%zmm15, %%zmm15 \n\t"
                    "vaddpd %%zmm15, %%zmm0,  %%zmm0  \n\t"
                    "\n\t"
                    "vmovupd %%zmm0, 0*8(%[sq])        \n\t"
                    "subq $-8*8, %[sq]                \n\t"
                    "subq $-8*8, %[mat00]             \n\t"
                    :[sq]"=r"(matrix_sqsum_row), [mat00]"=r"(mat_ptr00)
                    :     "0"(matrix_sqsum_row),         "1"(mat_ptr00)
                );
            }

            if (n_samples_remain>0){
                double r00, r14, r15;
                while(n_samples_remain--){
                    r00 = *(matrix_sqsum_row);
                    r14 = *(mat_ptr00);

                    r14 *= r14;

                    r00 += r14;

                    *(matrix_sqsum_row) = r00;

                    matrix_sqsum_row++;
                    mat_ptr00++;
                }
            }

            matrix_sqsum_row -= n_samples;
        }
    }
}

