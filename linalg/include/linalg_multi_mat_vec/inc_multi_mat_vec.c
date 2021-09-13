#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111


void multi_mat_vec_01x01_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=1, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base;
    while(c_unroll--){
        r00 = *(vec);
        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r15 = *(res);

            r01 = *(mat_ptr);

            r15 += r01 * r00;

            *(res) = r15;

            mat_ptr += s_unroll_size;
            res     += s_unroll_size;
        }
        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_02x01_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    while(c_unroll--){
        r00 = *(vec);
        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r14 = *(res);
            r15 = *(res+1);

            r01 = *(mat_ptr);
            r02 = *(mat_ptr+1);

            r14 += r01 * r00;
            r15 += r02 * r00;

            *(res)   = r14;
            *(res+1) = r15;

            mat_ptr += s_unroll_size;
            res     += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r01 = *(mat_ptr);

            r15 += r01 * r00;

            *(res) = r15;

            mat_ptr += 1;
            res     += 1;
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_04x01_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    while(c_unroll--){
        r00 = *(vec);
        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r01 = *(mat_ptr);
            r02 = *(mat_ptr+1);
            r03 = *(mat_ptr+2);
            r04 = *(mat_ptr+3);

            r12 += r01 * r00;
            r13 += r02 * r00;
            r14 += r03 * r00;
            r15 += r04 * r00;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            mat_ptr += s_unroll_size;
            res     += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r01 = *(mat_ptr);

            r15 += r01 * r00;

            *(res) = r15;

            mat_ptr += 1;
            res     += 1;
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_08x01_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    while(c_unroll--){
        r00 = *(vec);
        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r01 = *(mat_ptr);
            r02 = *(mat_ptr+1);
            r03 = *(mat_ptr+2);
            r04 = *(mat_ptr+3);

            r12 += r01 * r00;
            r13 += r02 * r00;
            r14 += r03 * r00;
            r15 += r04 * r00;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            r12 = *(res+4);
            r13 = *(res+5);
            r14 = *(res+6);
            r15 = *(res+7);

            r01 = *(mat_ptr+4);
            r02 = *(mat_ptr+5);
            r03 = *(mat_ptr+6);
            r04 = *(mat_ptr+7);

            r12 += r01 * r00;
            r13 += r02 * r00;
            r14 += r03 * r00;
            r15 += r04 * r00;

            *(res+4) = r12;
            *(res+5) = r13;
            *(res+6) = r14;
            *(res+7) = r15;

            mat_ptr += s_unroll_size;
            res     += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r01 = *(mat_ptr);

            r15 += r01 * r00;

            *(res) = r15;

            mat_ptr += 1;
            res     += 1;
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_01x02_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=1, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);

            r02 *= r00;
            r03 *= r01;

            r02 += r03;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            res       += s_unroll_size;
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_02x02_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r14 = *(res);
            r15 = *(res+1);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);
            r04 = *(mat_ptr00+1);
            r05 = *(mat_ptr01+1);

            r02 *= r00;
            r03 *= r01;
            r04 *= r00;
            r05 *= r01;

            r02 += r03;
            r04 += r05;

            r14 += r02;
            r15 += r04;

            *(res) = r14;
            *(res+1) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);

            r02 *= r00;
            r03 *= r01;

            r02 += r03;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            res       += 1;
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r14 = *(res);
            r15 = *(res+1);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);

            r02 *= r00;
            r04 *= r00;

            r14 += r02;
            r15 += r04;

            *(res) = r14;
            *(res+1) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_04x02_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);
            r04 = *(mat_ptr00+1);
            r05 = *(mat_ptr01+1);
            r06 = *(mat_ptr00+2);
            r07 = *(mat_ptr01+2);
            r08 = *(mat_ptr00+3);
            r09 = *(mat_ptr01+3);

            r02 *= r00;
            r03 *= r01;
            r04 *= r00;
            r05 *= r01;
            r06 *= r00;
            r07 *= r01;
            r08 *= r00;
            r09 *= r01;

            r02 += r03;
            r04 += r05;
            r06 += r07;
            r08 += r09;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);

            r02 *= r00;
            r03 *= r01;

            r02 += r03;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            res       += 1;
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);
            r06 = *(mat_ptr00+2);
            r08 = *(mat_ptr00+3);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_08x02_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);
            r04 = *(mat_ptr00+1);
            r05 = *(mat_ptr01+1);
            r06 = *(mat_ptr00+2);
            r07 = *(mat_ptr01+2);
            r08 = *(mat_ptr00+3);
            r09 = *(mat_ptr01+3);

            r02 *= r00;
            r03 *= r01;
            r04 *= r00;
            r05 *= r01;
            r06 *= r00;
            r07 *= r01;
            r08 *= r00;
            r09 *= r01;

            r02 += r03;
            r04 += r05;
            r06 += r07;
            r08 += r09;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            r12 = *(res+4);
            r13 = *(res+5);
            r14 = *(res+6);
            r15 = *(res+7);

            r02 = *(mat_ptr00+4);
            r03 = *(mat_ptr01+4);
            r04 = *(mat_ptr00+5);
            r05 = *(mat_ptr01+5);
            r06 = *(mat_ptr00+6);
            r07 = *(mat_ptr01+6);
            r08 = *(mat_ptr00+7);
            r09 = *(mat_ptr01+7);

            r02 *= r00;
            r03 *= r01;
            r04 *= r00;
            r05 *= r01;
            r06 *= r00;
            r07 *= r01;
            r08 *= r00;
            r09 *= r01;

            r02 += r03;
            r04 += r05;
            r06 += r07;
            r08 += r09;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res+4) = r12;
            *(res+5) = r13;
            *(res+6) = r14;
            *(res+7) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);
            r03 = *(mat_ptr01);

            r02 *= r00;
            r03 *= r01;

            r02 += r03;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            res       += 1;
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);
            r06 = *(mat_ptr00+2);
            r08 = *(mat_ptr00+3);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            r12 = *(res+4);
            r13 = *(res+5);
            r14 = *(res+6);
            r15 = *(res+7);

            r02 = *(mat_ptr00+4);
            r04 = *(mat_ptr00+5);
            r06 = *(mat_ptr00+6);
            r08 = *(mat_ptr00+7);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res+4) = r12;
            *(res+5) = r13;
            *(res+6) = r14;
            *(res+7) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec       += 1;
        res       -= n_samples;
    }
}

void multi_mat_vec_01x04_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=1, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);
        r02 = *(vec+2);
        r03 = *(vec+3);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r15 = *(res);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;

            r04 += r05;
            r06 += r07;

            r04 += r06;

            r15 += r04;

            *(res) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            mat_ptr02 += s_unroll_size;
            mat_ptr03 += s_unroll_size;
            res       += s_unroll_size;
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_02x04_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);
        r02 = *(vec+2);
        r03 = *(vec+3);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r14 = *(res);
            r15 = *(res+1);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);
            r08 = *(mat_ptr00+1);
            r09 = *(mat_ptr01+1);
            r10 = *(mat_ptr02+1);
            r11 = *(mat_ptr03+1);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r14 += r04;
            r15 += r08;

            *(res) = r14;
            *(res+1) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            mat_ptr02 += s_unroll_size;
            mat_ptr03 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;

            r04 += r05;
            r06 += r07;

            r04 += r06;

            r15 += r04;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            mat_ptr02 += 1;
            mat_ptr03 += 1;
            res       += 1;
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r14 = *(res);
            r15 = *(res+1);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);

            r02 *= r00;
            r04 *= r00;

            r14 += r02;
            r15 += r04;

            *(res) = r14;
            *(res+1) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec += 1;
        res -= n_samples;
    }

}

void multi_mat_vec_04x04_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);
        r02 = *(vec+2);
        r03 = *(vec+3);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);
            r08 = *(mat_ptr00+1);
            r09 = *(mat_ptr01+1);
            r10 = *(mat_ptr02+1);
            r11 = *(mat_ptr03+1);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r12 += r04;
            r13 += r08;

            *(res) = r12;
            *(res+1) = r13;

            r04 = *(mat_ptr00+2);
            r05 = *(mat_ptr01+2);
            r06 = *(mat_ptr02+2);
            r07 = *(mat_ptr03+2);
            r08 = *(mat_ptr00+3);
            r09 = *(mat_ptr01+3);
            r10 = *(mat_ptr02+3);
            r11 = *(mat_ptr03+3);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r14 += r04;
            r15 += r08;

            *(res+2) = r14;
            *(res+3) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            mat_ptr02 += s_unroll_size;
            mat_ptr03 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;

            r04 += r05;
            r06 += r07;

            r04 += r06;

            r15 += r04;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            mat_ptr02 += 1;
            mat_ptr03 += 1;
            res       += 1;
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);
            r06 = *(mat_ptr00+2);
            r08 = *(mat_ptr00+3);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_08x04_N_C_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    double r00, r01, r02, r03;
    double r04, r05, r06, r07;
    double r08, r09, r10, r11;
    double r12, r13, r14, r15;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;

    while(c_unroll--){
        r00 = *(vec);
        r01 = *(vec+1);
        r02 = *(vec+2);
        r03 = *(vec+3);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);
            r08 = *(mat_ptr00+1);
            r09 = *(mat_ptr01+1);
            r10 = *(mat_ptr02+1);
            r11 = *(mat_ptr03+1);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r12 += r04;
            r13 += r08;

            *(res) = r12;
            *(res+1) = r13;

            r04 = *(mat_ptr00+2);
            r05 = *(mat_ptr01+2);
            r06 = *(mat_ptr02+2);
            r07 = *(mat_ptr03+2);
            r08 = *(mat_ptr00+3);
            r09 = *(mat_ptr01+3);
            r10 = *(mat_ptr02+3);
            r11 = *(mat_ptr03+3);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r14 += r04;
            r15 += r08;

            *(res+2) = r14;
            *(res+3) = r15;

            r12 = *(res+4);
            r13 = *(res+5);
            r14 = *(res+6);
            r15 = *(res+7);

            r04 = *(mat_ptr00+4);
            r05 = *(mat_ptr01+4);
            r06 = *(mat_ptr02+4);
            r07 = *(mat_ptr03+4);
            r08 = *(mat_ptr00+5);
            r09 = *(mat_ptr01+5);
            r10 = *(mat_ptr02+5);
            r11 = *(mat_ptr03+5);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r12 += r04;
            r13 += r08;

            *(res+4) = r12;
            *(res+5) = r13;

            r04 = *(mat_ptr00+6);
            r05 = *(mat_ptr01+6);
            r06 = *(mat_ptr02+6);
            r07 = *(mat_ptr03+6);
            r08 = *(mat_ptr00+7);
            r09 = *(mat_ptr01+7);
            r10 = *(mat_ptr02+7);
            r11 = *(mat_ptr03+7);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;
            r08 *= r00;
            r09 *= r01;
            r10 *= r02;
            r11 *= r03;

            r04 += r05;
            r06 += r07;
            r08 += r09;
            r10 += r11;

            r04 += r06;
            r08 += r10;

            r14 += r04;
            r15 += r08;

            *(res+6) = r14;
            *(res+7) = r15;

            mat_ptr00 += s_unroll_size;
            mat_ptr01 += s_unroll_size;
            mat_ptr02 += s_unroll_size;
            mat_ptr03 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r04 = *(mat_ptr00);
            r05 = *(mat_ptr01);
            r06 = *(mat_ptr02);
            r07 = *(mat_ptr03);

            r04 *= r00;
            r05 *= r01;
            r06 *= r02;
            r07 *= r03;

            r04 += r05;
            r06 += r07;

            r04 += r06;

            r15 += r04;

            *(res) = r15;

            mat_ptr00 += 1;
            mat_ptr01 += 1;
            mat_ptr02 += 1;
            mat_ptr03 += 1;
            res       += 1;
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec       += c_unroll_size;
        res       -= n_samples;
    }

    while(c_remain--){
        r00 = *(vec);

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            r12 = *(res);
            r13 = *(res+1);
            r14 = *(res+2);
            r15 = *(res+3);

            r02 = *(mat_ptr00);
            r04 = *(mat_ptr00+1);
            r06 = *(mat_ptr00+2);
            r08 = *(mat_ptr00+3);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res)   = r12;
            *(res+1) = r13;
            *(res+2) = r14;
            *(res+3) = r15;

            r12 = *(res+4);
            r13 = *(res+5);
            r14 = *(res+6);
            r15 = *(res+7);

            r02 = *(mat_ptr00+4);
            r04 = *(mat_ptr00+5);
            r06 = *(mat_ptr00+6);
            r08 = *(mat_ptr00+7);

            r02 *= r00;
            r04 *= r00;
            r06 *= r00;
            r08 *= r00;

            r12 += r02;
            r13 += r04;
            r14 += r06;
            r15 += r08;

            *(res+4) = r12;
            *(res+5) = r13;
            *(res+6) = r14;
            *(res+7) = r15;

            mat_ptr00 += s_unroll_size;
            res       += s_unroll_size;
        }

        s_remain_base = s_remain;
        while(s_remain_base--){
            r15 = *(res);

            r02 = *(mat_ptr00);

            r02 *= r00;

            r15 += r02;

            *(res) = r15;

            mat_ptr00 += 1;
            res       += 1;
        }

        vec       += 1;
        res       -= n_samples;
    }
}







void multi_mat_vec_01x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
}

void multi_mat_vec_02x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "movupd 0*8(%[r]), %%xmm1 \n\t"
                "movupd 0*8(%[m]), %%xmm2 \n\t"
                "\n\t"
                "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                "\n\t"
                "movupd %%xmm1, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-2*8, %[r] \n\t"
                "subq $-2*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            __asm__ __volatile__ (
                "movsd 0*8(%[r]), %%xmm1 \n\t"
                "movsd 0*8(%[m]), %%xmm2 \n\t"
                "\n\t"
                "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                "\n\t"
                "movsd %%xmm1, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-1*8, %[r] \n\t"
                "subq $-1*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_04x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;

    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]), %%ymm1 \n\t"
                "vmovupd 0*8(%[m]), %%ymm2 \n\t"
                "\n\t"
                "vmulpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                "vaddpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                "\n\t"
                "vmovupd %%ymm1, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-4*8, %[r] \n\t"
                "subq $-4*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]), %%xmm1 \n\t"
                    "movupd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movupd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r] \n\t"
                    "subq $-2*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]), %%xmm1 \n\t"
                    "movsd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movsd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r] \n\t"
                    "subq $-1*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_08x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]), %%zmm1 \n\t"
                "vmovupd 0*8(%[m]), %%zmm2 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                "vaddpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                "\n\t"
                "vmovupd %%zmm1, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-8*8, %[r] \n\t"
                "subq $-8*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%ymm1 \n\t"
                    "vmovupd 0*8(%[m]), %%ymm2 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                    "vaddpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                    "\n\t"
                    "vmovupd %%ymm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r] \n\t"
                    "subq $-4*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]), %%xmm1 \n\t"
                    "movupd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movupd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r] \n\t"
                    "subq $-2*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]), %%xmm1 \n\t"
                    "movsd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movsd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r] \n\t"
                    "subq $-1*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_16x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=16, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]), %%zmm1 \n\t"
                "vmovupd 8*8(%[r]), %%zmm2 \n\t"
                "vmovupd 0*8(%[m]), %%zmm3 \n\t"
                "vmovupd 8*8(%[m]), %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm1, %%zmm1 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm4, %%zmm4 \n\t"
                "vaddpd %%zmm4, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm1, 0*8(%[r]) \n\t"
                "vmovupd %%zmm2, 8*8(%[r]) \n\t"
                "\n\t"
                "subq $-16*8, %[r] \n\t"
                "subq $-16*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%zmm1 \n\t"
                    "vmovupd 0*8(%[m]), %%zmm2 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                    "vaddpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                    "\n\t"
                    "vmovupd %%zmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r] \n\t"
                    "subq $-8*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%ymm1 \n\t"
                    "vmovupd 0*8(%[m]), %%ymm2 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                    "vaddpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                    "\n\t"
                    "vmovupd %%ymm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r] \n\t"
                    "subq $-4*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]), %%xmm1 \n\t"
                    "movupd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movupd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r] \n\t"
                    "subq $-2*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]), %%xmm1 \n\t"
                    "movsd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movsd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r] \n\t"
                    "subq $-1*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}

void multi_mat_vec_32x01_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=32, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=1, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd  0*8(%[r]), %%zmm1 \n\t"
                "vmovupd  8*8(%[r]), %%zmm2 \n\t"
                "vmovupd 16*8(%[r]), %%zmm3 \n\t"
                "vmovupd 24*8(%[r]), %%zmm4 \n\t"
                "vmovupd  0*8(%[m]), %%zmm5 \n\t"
                "vmovupd  8*8(%[m]), %%zmm6 \n\t"
                "vmovupd 16*8(%[m]), %%zmm7 \n\t"
                "vmovupd 24*8(%[m]), %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                "vaddpd %%zmm5, %%zmm1, %%zmm1 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm3, %%zmm3 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm8, %%zmm8 \n\t"
                "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm1,  0*8(%[r]) \n\t"
                "vmovupd %%zmm2,  8*8(%[r]) \n\t"
                "vmovupd %%zmm3, 16*8(%[r]) \n\t"
                "vmovupd %%zmm4, 24*8(%[r]) \n\t"
                "\n\t"
                "subq $-32*8, %[r] \n\t"
                "subq $-32*8, %[m] \n\t"
                :[r]"=r"(res), [m]"=r"(mat_ptr)
                :    "0"(res),     "1"(mat_ptr)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&16){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%zmm1 \n\t"
                    "vmovupd 0*8(%[m]), %%zmm2 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                    "vaddpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                    "\n\t"
                    "vmovupd %%zmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]), %%zmm1 \n\t"
                    "vmovupd 8*8(%[m]), %%zmm2 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                    "vaddpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                    "\n\t"
                    "vmovupd %%zmm1, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r] \n\t"
                    "subq $-16*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%zmm1 \n\t"
                    "vmovupd 0*8(%[m]), %%zmm2 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm2, %%zmm2 \n\t"
                    "vaddpd %%zmm2, %%zmm1, %%zmm1 \n\t"
                    "\n\t"
                    "vmovupd %%zmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r] \n\t"
                    "subq $-8*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]), %%ymm1 \n\t"
                    "vmovupd 0*8(%[m]), %%ymm2 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm2, %%ymm2 \n\t"
                    "vaddpd %%ymm2, %%ymm1, %%ymm1 \n\t"
                    "\n\t"
                    "vmovupd %%ymm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r] \n\t"
                    "subq $-4*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]), %%xmm1 \n\t"
                    "movupd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movupd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r] \n\t"
                    "subq $-2*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]), %%xmm1 \n\t"
                    "movsd 0*8(%[m]), %%xmm2 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm2, %%xmm2 \n\t"
                    "vaddpd %%xmm2, %%xmm1, %%xmm1 \n\t"
                    "\n\t"
                    "movsd %%xmm1, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r] \n\t"
                    "subq $-1*8, %[m] \n\t"
                    :[r]"=r"(res), [m]"=r"(mat_ptr)
                    :    "0"(res),     "1"(mat_ptr)
                );
            }
        }

        vec += c_unroll_size;
        res -= n_samples;
    }
}






void multi_mat_vec_01x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
}

void multi_mat_vec_02x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            "VBROADCASTSD %[r01], %%ymm1 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "movupd 0*8(%[r]),   %%xmm2 \n\t"
                "movupd 0*8(%[m00]), %%xmm3 \n\t"
                "movupd 0*8(%[m01]), %%xmm4 \n\t"
                "\n\t"
                "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                "\n\t"
                "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                "vaddpd %%xmm4, %%xmm2, %%xmm2 \n\t"
                "\n\t"
                "movupd %%xmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-2*8, %[r]   \n\t"
                "subq $-2*8, %[m00] \n\t"
                "subq $-2*8, %[m01] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "movsd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    while(c_remain--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "movupd 0*8(%[r]),   %%xmm2 \n\t"
                "movupd 0*8(%[m00]), %%xmm3 \n\t"
                "\n\t"
                "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                "\n\t"
                "movupd %%xmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-2*8, %[r]   \n\t"
                "subq $-2*8, %[m00] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                :    "0"(res),       "1"(mat_ptr00)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_04x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            "VBROADCASTSD %[r01], %%ymm1 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                "vmovupd 0*8(%[m01]), %%ymm4 \n\t"
                "\n\t"
                "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                "\n\t"
                "vmulpd %%ymm1, %%ymm4, %%ymm4 \n\t"
                "vaddpd %%ymm4, %%ymm2, %%ymm2 \n\t"
                "\n\t"
                "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-4*8, %[r]   \n\t"
                "subq $-4*8, %[m00] \n\t"
                "subq $-4*8, %[m01] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "movupd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "movsd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    while(c_remain--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            :[r00]"=m"(*(vec))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                "\n\t"
                "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                "\n\t"
                "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-4*8, %[r]   \n\t"
                "subq $-4*8, %[m00] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                :    "0"(res),       "1"(mat_ptr00)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_08x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                "vaddpd %%zmm4, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-8*8, %[r]   \n\t"
                "subq $-8*8, %[m00] \n\t"
                "subq $-8*8, %[m01] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vmulpd %%ymm1, %%ymm4, %%ymm4 \n\t"
                    "vaddpd %%ymm4, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "movupd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "movsd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    while(c_remain--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-8*8, %[r]   \n\t"
                "subq $-8*8, %[m00] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                :    "0"(res),       "1"(mat_ptr00)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_16x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=16, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                "vaddpd %%zmm4, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                "vmovupd 8*8(%[m01]), %%zmm7 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                "\n\t"
                "subq $-16*8, %[r]   \n\t"
                "subq $-16*8, %[m00] \n\t"
                "subq $-16*8, %[m01] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&8){
                // printf("%ld \n", 8);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                    "vaddpd %%zmm4, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    "subq $-8*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vmulpd %%ymm1, %%ymm4, %%ymm4 \n\t"
                    "vaddpd %%ymm4, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "movupd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "movsd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }
        }

        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    while(c_remain--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                "\n\t"
                "subq $-16*8, %[r]   \n\t"
                "subq $-16*8, %[m00] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                :    "0"(res),       "1"(mat_ptr00)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&8){
                // printf("%ld \n", 8);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }
        }

        vec += 1;
        res -= n_samples;
    }
}

void multi_mat_vec_32x02_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=32, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=2, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr01 += n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                "vaddpd %%zmm4, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                "vmovupd 8*8(%[m01]), %%zmm7 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 16*8(%[r]),   %%zmm8 \n\t"
                "vmovupd 16*8(%[m00]), %%zmm9 \n\t"
                "vmovupd 16*8(%[m01]), %%zmm10 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm9, %%zmm9 \n\t"
                "vaddpd %%zmm9, %%zmm8, %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm10, %%zmm10 \n\t"
                "vaddpd %%zmm10, %%zmm8, %%zmm8 \n\t"
                "\n\t"
                "vmovupd %%zmm8, 16*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 24*8(%[r]),   %%zmm11 \n\t"
                "vmovupd 24*8(%[m00]), %%zmm12 \n\t"
                "vmovupd 24*8(%[m01]), %%zmm13 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm11, %%zmm11 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm11, %%zmm11 \n\t"
                "\n\t"
                "vmovupd %%zmm11, 24*8(%[r]) \n\t"
                "\n\t"
                "subq $-32*8, %[r]   \n\t"
                "subq $-32*8, %[m00] \n\t"
                "subq $-32*8, %[m01] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&16){
                // printf("%ld \n", 16);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                    "vaddpd %%zmm4, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                    "vmovupd 8*8(%[m01]), %%zmm7 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                    "vmulpd %%zmm1, %%zmm7, %%zmm7 \n\t"
                    "vaddpd %%zmm7, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                    "\n\t"
                    "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r]   \n\t"
                    "subq $-16*8, %[m00] \n\t"
                    "subq $-16*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&8){
                // printf("%ld \n", 8);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vmulpd %%zmm1, %%zmm4, %%zmm4 \n\t"
                    "vaddpd %%zmm4, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    "subq $-8*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vmulpd %%ymm1, %%ymm4, %%ymm4 \n\t"
                    "vaddpd %%ymm4, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "movupd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "movsd 0*8(%[m01]), %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vmulpd %%xmm1, %%xmm4, %%xmm4 \n\t"
                    "vaddpd %%xmm4, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }
        }
    
        mat_ptr00 += n_samples;
        mat_ptr01 += n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    while(c_remain--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                "\n\t"
                "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                "\n\t"
                "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 16*8(%[r]),   %%zmm8 \n\t"
                "vmovupd 16*8(%[m00]), %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm9, %%zmm9 \n\t"
                "vaddpd %%zmm9, %%zmm8, %%zmm8 \n\t"
                "\n\t"
                "vmovupd %%zmm8, 16*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 24*8(%[r]),   %%zmm11 \n\t"
                "vmovupd 24*8(%[m00]), %%zmm12 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm11, %%zmm11 \n\t"
                "\n\t"
                "vmovupd %%zmm11, 24*8(%[r]) \n\t"
                "\n\t"
                "subq $-32*8, %[r]   \n\t"
                "subq $-32*8, %[m00] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                :    "0"(res),       "1"(mat_ptr00)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&16){
                // printf("%ld \n", 16);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm5 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm6 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm5, %%zmm5 \n\t"
                    "\n\t"
                    "vmovupd %%zmm5, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r]   \n\t"
                    "subq $-16*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&8){
                // printf("%ld \n", 8);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm3 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm3, %%zmm3 \n\t"
                    "vaddpd %%zmm3, %%zmm2, %%zmm2 \n\t"
                    "\n\t"
                    "vmovupd %%zmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&4){
                // printf("%ld \n", 4);
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm2 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm3 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm3, %%ymm3 \n\t"
                    "vaddpd %%ymm3, %%ymm2, %%ymm2 \n\t"
                    "\n\t"
                    "vmovupd %%ymm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&2){
                // printf("%ld \n", 2);
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm2 \n\t"
                    "movupd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movupd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            if(s_remain_base&1){
                // printf("%ld \n", 1);
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm2 \n\t"
                    "movsd 0*8(%[m00]), %%xmm3 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm3, %%xmm3 \n\t"
                    "vaddpd %%xmm3, %%xmm2, %%xmm2 \n\t"
                    "\n\t"
                    "movsd %%xmm2, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }
        }
    
        vec += 1;
        res -= n_samples;
    }
}






void multi_mat_vec_01x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
}

void multi_mat_vec_02x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=2, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            "VBROADCASTSD %[r01], %%ymm1 \n\t"
            "VBROADCASTSD %[r02], %%ymm2 \n\t"
            "VBROADCASTSD %[r03], %%ymm3 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1)), [r02]"=m"(*(vec+2)), [r03]"=m"(*(vec+3))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "movupd 0*8(%[r]),   %%xmm4 \n\t"
                "movupd 0*8(%[m00]), %%xmm5 \n\t"
                "movupd 0*8(%[m01]), %%xmm6 \n\t"
                "movupd 0*8(%[m02]), %%xmm7 \n\t"
                "movupd 0*8(%[m03]), %%xmm8 \n\t"
                "\n\t"
                "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                "\n\t"
                "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                "\n\t"
                "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                "\n\t"
                "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                "\n\t"
                "movupd %%xmm4, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-2*8, %[r]   \n\t"
                "subq $-2*8, %[m00] \n\t"
                "subq $-2*8, %[m01] \n\t"
                "subq $-2*8, %[m02] \n\t"
                "subq $-2*8, %[m03] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "movupd 0*8(%[m02]), %%xmm7 \n\t"
                    "movupd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    "subq $-2*8, %[m02] \n\t"
                    "subq $-2*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm4 \n\t"
                    "movsd 0*8(%[m00]), %%xmm5 \n\t"
                    "movsd 0*8(%[m01]), %%xmm6 \n\t"
                    "movsd 0*8(%[m02]), %%xmm7 \n\t"
                    "movsd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movsd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    "subq $-1*8, %[m02] \n\t"
                    "subq $-1*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    if(c_remain>0){
        if(c_remain&2){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                "VBROADCASTSD %[r01], %%zmm1 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "movsd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        "subq $-1*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }
            }

            mat_ptr00 += n_samples;
            mat_ptr01 += n_samples;
            vec += 2;
            res -= n_samples;
        }

        if(c_remain&1){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }
            }

            vec += 1;
            res -= n_samples;
        }
    }
}

void multi_mat_vec_04x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=4, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%ymm0 \n\t"
            "VBROADCASTSD %[r01], %%ymm1 \n\t"
            "VBROADCASTSD %[r02], %%ymm2 \n\t"
            "VBROADCASTSD %[r03], %%ymm3 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1)), [r02]"=m"(*(vec+2)), [r03]"=m"(*(vec+3))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                "vmovupd 0*8(%[m02]), %%ymm7 \n\t"
                "vmovupd 0*8(%[m03]), %%ymm8 \n\t"
                "\n\t"
                "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmulpd %%ymm2, %%ymm7, %%ymm7 \n\t"
                "vaddpd %%ymm7, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmulpd %%ymm3, %%ymm8, %%ymm8 \n\t"
                "vaddpd %%ymm8, %%ymm4, %%ymm4 \n\t"
                "\n\t"
                "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-4*8, %[r]   \n\t"
                "subq $-4*8, %[m00] \n\t"
                "subq $-4*8, %[m01] \n\t"
                "subq $-4*8, %[m02] \n\t"
                "subq $-4*8, %[m03] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%ymm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%ymm8 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                    "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm2, %%ymm7, %%ymm7 \n\t"
                    "vaddpd %%ymm7, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm3, %%ymm8, %%ymm8 \n\t"
                    "vaddpd %%ymm8, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    "subq $-4*8, %[m02] \n\t"
                    "subq $-4*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "movupd 0*8(%[m02]), %%xmm7 \n\t"
                    "movupd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    "subq $-2*8, %[m02] \n\t"
                    "subq $-2*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm4 \n\t"
                    "movsd 0*8(%[m00]), %%xmm5 \n\t"
                    "movsd 0*8(%[m01]), %%xmm6 \n\t"
                    "movsd 0*8(%[m02]), %%xmm7 \n\t"
                    "movsd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movsd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    "subq $-1*8, %[m02] \n\t"
                    "subq $-1*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    if(c_remain>0){
        if(c_remain&2){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                "VBROADCASTSD %[r01], %%zmm1 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                    "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "movupd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        "subq $-2*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "movsd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        "subq $-1*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }
            }

            mat_ptr00 += n_samples;
            mat_ptr01 += n_samples;
            vec += 2;
            res -= n_samples;
        }

        if(c_remain&1){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }
            }

            vec += 1;
            res -= n_samples;
        }
    }
}

void multi_mat_vec_08x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=8, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            "VBROADCASTSD %[r02], %%zmm2 \n\t"
            "VBROADCASTSD %[r03], %%zmm3 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1)), [r02]"=m"(*(vec+2)), [r03]"=m"(*(vec+3))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                "\n\t"
                "subq $-8*8, %[r]   \n\t"
                "subq $-8*8, %[m00] \n\t"
                "subq $-8*8, %[m01] \n\t"
                "subq $-8*8, %[m02] \n\t"
                "subq $-8*8, %[m03] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%ymm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%ymm8 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                    "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm2, %%ymm7, %%ymm7 \n\t"
                    "vaddpd %%ymm7, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm3, %%ymm8, %%ymm8 \n\t"
                    "vaddpd %%ymm8, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    "subq $-4*8, %[m02] \n\t"
                    "subq $-4*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "movupd 0*8(%[m02]), %%xmm7 \n\t"
                    "movupd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    "subq $-2*8, %[m02] \n\t"
                    "subq $-2*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm4 \n\t"
                    "movsd 0*8(%[m00]), %%xmm5 \n\t"
                    "movsd 0*8(%[m01]), %%xmm6 \n\t"
                    "movsd 0*8(%[m02]), %%xmm7 \n\t"
                    "movsd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movsd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    "subq $-1*8, %[m02] \n\t"
                    "subq $-1*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    if(c_remain>0){
        if(c_remain&2){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                "VBROADCASTSD %[r01], %%zmm1 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    "subq $-8*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                        "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        "subq $-4*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "movupd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        "subq $-2*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "movsd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        "subq $-1*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }
            }

            mat_ptr00 += n_samples;
            mat_ptr01 += n_samples;
            vec += 2;
            res -= n_samples;
        }

        if(c_remain&1){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }
            }

            vec += 1;
            res -= n_samples;
        }
    }
}

void multi_mat_vec_16x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=16, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4, c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            "VBROADCASTSD %[r02], %%zmm2 \n\t"
            "VBROADCASTSD %[r03], %%zmm3 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1)), [r02]"=m"(*(vec+2)), [r03]"=m"(*(vec+3))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                "vmovupd 8*8(%[m02]), %%zmm12 \n\t"
                "vmovupd 8*8(%[m03]), %%zmm13 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                "\n\t"
                "subq $-16*8, %[r]   \n\t"
                "subq $-16*8, %[m00] \n\t"
                "subq $-16*8, %[m01] \n\t"
                "subq $-16*8, %[m02] \n\t"
                "subq $-16*8, %[m03] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                    "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                    "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    "subq $-8*8, %[m01] \n\t"
                    "subq $-8*8, %[m02] \n\t"
                    "subq $-8*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%ymm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%ymm8 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                    "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm2, %%ymm7, %%ymm7 \n\t"
                    "vaddpd %%ymm7, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm3, %%ymm8, %%ymm8 \n\t"
                    "vaddpd %%ymm8, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    "subq $-4*8, %[m02] \n\t"
                    "subq $-4*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "movupd 0*8(%[m02]), %%xmm7 \n\t"
                    "movupd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    "subq $-2*8, %[m02] \n\t"
                    "subq $-2*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm4 \n\t"
                    "movsd 0*8(%[m00]), %%xmm5 \n\t"
                    "movsd 0*8(%[m01]), %%xmm6 \n\t"
                    "movsd 0*8(%[m02]), %%xmm7 \n\t"
                    "movsd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movsd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    "subq $-1*8, %[m02] \n\t"
                    "subq $-1*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }
        }

        mat_ptr00 += 3*n_samples;
        mat_ptr01 += 3*n_samples;
        mat_ptr02 += 3*n_samples;
        mat_ptr03 += 3*n_samples;
        vec += c_unroll_size;
        res -= n_samples;
    }

    if(c_remain>0){
        if(c_remain&2){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                "VBROADCASTSD %[r01], %%zmm1 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                    "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                    "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r]   \n\t"
                    "subq $-16*8, %[m00] \n\t"
                    "subq $-16*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&8){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                        "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-8*8, %[r]   \n\t"
                        "subq $-8*8, %[m00] \n\t"
                        "subq $-8*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                        "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        "subq $-4*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "movupd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        "subq $-2*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "movsd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        "subq $-1*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }
            }

            mat_ptr00 += n_samples;
            mat_ptr01 += n_samples;
            vec += 2;
            res -= n_samples;
        }

        if(c_remain&1){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r]   \n\t"
                    "subq $-16*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&8){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-8*8, %[r]   \n\t"
                        "subq $-8*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }
            }

            vec += 1;
            res -= n_samples;
        }
    }
}

void multi_mat_vec_32x04_N_A_r8(double *mat_ptr, double vec[], double res[], int64_t n_samples, int64_t n_columns){
    int64_t s_unroll_size=32, s_unroll=n_samples/s_unroll_size, s_remain=n_samples%s_unroll_size;
    int64_t c_unroll_size=4,  c_unroll=n_columns/c_unroll_size, c_remain=n_columns%c_unroll_size;

    int64_t s_unroll_base, s_remain_base;
    double *mat_ptr00, *mat_ptr01, *mat_ptr02, *mat_ptr03;

    mat_ptr00 = mat_ptr;
    mat_ptr01 = mat_ptr;
    mat_ptr02 = mat_ptr;
    mat_ptr03 = mat_ptr;
    mat_ptr01 += 1*n_samples;
    mat_ptr02 += 2*n_samples;
    mat_ptr03 += 3*n_samples;
    while(c_unroll--){
        __asm__ __volatile__ (
            "VBROADCASTSD %[r00], %%zmm0 \n\t"
            "VBROADCASTSD %[r01], %%zmm1 \n\t"
            "VBROADCASTSD %[r02], %%zmm2 \n\t"
            "VBROADCASTSD %[r03], %%zmm3 \n\t"
            :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1)), [r02]"=m"(*(vec+2)), [r03]"=m"(*(vec+3))
            :
        );

        s_unroll_base = s_unroll;
        while(s_unroll_base--){
            __asm__ __volatile__ (
                "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                "vmovupd 8*8(%[m02]), %%zmm12 \n\t"
                "vmovupd 8*8(%[m03]), %%zmm13 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 16*8(%[r]),   %%zmm4 \n\t"
                "vmovupd 16*8(%[m00]), %%zmm5 \n\t"
                "vmovupd 16*8(%[m01]), %%zmm6 \n\t"
                "vmovupd 16*8(%[m02]), %%zmm7 \n\t"
                "vmovupd 16*8(%[m03]), %%zmm8 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                "\n\t"
                "vmovupd %%zmm4, 16*8(%[r]) \n\t"
                "\n\t"
                "vmovupd 24*8(%[r]),   %%zmm9 \n\t"
                "vmovupd 24*8(%[m00]), %%zmm10 \n\t"
                "vmovupd 24*8(%[m01]), %%zmm11 \n\t"
                "vmovupd 24*8(%[m02]), %%zmm12 \n\t"
                "vmovupd 24*8(%[m03]), %%zmm13 \n\t"
                "\n\t"
                "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm2, %%zmm12, %%zmm12 \n\t"
                "vaddpd %%zmm12, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmulpd %%zmm3, %%zmm13, %%zmm13 \n\t"
                "vaddpd %%zmm13, %%zmm9, %%zmm9 \n\t"
                "\n\t"
                "vmovupd %%zmm9, 24*8(%[r]) \n\t"
                "\n\t"
                "subq $-32*8, %[r]   \n\t"
                "subq $-32*8, %[m00] \n\t"
                "subq $-32*8, %[m01] \n\t"
                "subq $-32*8, %[m02] \n\t"
                "subq $-32*8, %[m03] \n\t"
                :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
            );
        }

        s_remain_base = s_remain;
        if(s_remain_base>0){
            if(s_remain_base&16){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                    "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                    "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                    "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                    "vmovupd 8*8(%[m02]), %%zmm12 \n\t"
                    "vmovupd 8*8(%[m03]), %%zmm13 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                    "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm2, %%zmm12, %%zmm12 \n\t"
                    "vaddpd %%zmm12, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm3, %%zmm13, %%zmm13 \n\t"
                    "vaddpd %%zmm13, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-16*8, %[r]   \n\t"
                    "subq $-16*8, %[m00] \n\t"
                    "subq $-16*8, %[m01] \n\t"
                    "subq $-16*8, %[m02] \n\t"
                    "subq $-16*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&8){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%zmm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%zmm8 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm2, %%zmm7, %%zmm7 \n\t"
                    "vaddpd %%zmm7, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm3, %%zmm8, %%zmm8 \n\t"
                    "vaddpd %%zmm8, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-8*8, %[r]   \n\t"
                    "subq $-8*8, %[m00] \n\t"
                    "subq $-8*8, %[m01] \n\t"
                    "subq $-8*8, %[m02] \n\t"
                    "subq $-8*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&4){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                    "vmovupd 0*8(%[m02]), %%ymm7 \n\t"
                    "vmovupd 0*8(%[m03]), %%ymm8 \n\t"
                    "\n\t"
                    "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                    "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                    "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm2, %%ymm7, %%ymm7 \n\t"
                    "vaddpd %%ymm7, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmulpd %%ymm3, %%ymm8, %%ymm8 \n\t"
                    "vaddpd %%ymm8, %%ymm4, %%ymm4 \n\t"
                    "\n\t"
                    "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-4*8, %[r]   \n\t"
                    "subq $-4*8, %[m00] \n\t"
                    "subq $-4*8, %[m01] \n\t"
                    "subq $-4*8, %[m02] \n\t"
                    "subq $-4*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&2){
                __asm__ __volatile__ (
                    "movupd 0*8(%[r]),   %%xmm4 \n\t"
                    "movupd 0*8(%[m00]), %%xmm5 \n\t"
                    "movupd 0*8(%[m01]), %%xmm6 \n\t"
                    "movupd 0*8(%[m02]), %%xmm7 \n\t"
                    "movupd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movupd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-2*8, %[r]   \n\t"
                    "subq $-2*8, %[m00] \n\t"
                    "subq $-2*8, %[m01] \n\t"
                    "subq $-2*8, %[m02] \n\t"
                    "subq $-2*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }

            if(s_remain_base&1){
                __asm__ __volatile__ (
                    "movsd 0*8(%[r]),   %%xmm4 \n\t"
                    "movsd 0*8(%[m00]), %%xmm5 \n\t"
                    "movsd 0*8(%[m01]), %%xmm6 \n\t"
                    "movsd 0*8(%[m02]), %%xmm7 \n\t"
                    "movsd 0*8(%[m03]), %%xmm8 \n\t"
                    "\n\t"
                    "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                    "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                    "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm2, %%xmm7, %%xmm7 \n\t"
                    "vaddpd %%xmm7, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "vmulpd %%xmm3, %%xmm8, %%xmm8 \n\t"
                    "vaddpd %%xmm8, %%xmm4, %%xmm4 \n\t"
                    "\n\t"
                    "movsd %%xmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-1*8, %[r]   \n\t"
                    "subq $-1*8, %[m00] \n\t"
                    "subq $-1*8, %[m01] \n\t"
                    "subq $-1*8, %[m02] \n\t"
                    "subq $-1*8, %[m03] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01), [m02]"=r"(mat_ptr02), [m03]"=r"(mat_ptr03)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01),       "3"(mat_ptr02),       "4"(mat_ptr03)
                );
            }
        }

        // if(c_unroll>0){
            mat_ptr00 += 3*n_samples;
            mat_ptr01 += 3*n_samples;
            mat_ptr02 += 3*n_samples;
            mat_ptr03 += 3*n_samples;
        // }else{
        //     if(c_remain==2){
        //         mat_ptr00 += 3*n_samples;
        //         mat_ptr01 += 3*n_samples;
        //     }
        //     if(c_remain==1){
        //         mat_ptr00 += 3*n_samples;
        //     }
        // }
        vec += c_unroll_size;
        res -= n_samples;
    }

    if(c_remain>0){
        if(c_remain&2){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                "VBROADCASTSD %[r01], %%zmm1 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                    "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                    "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 16*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 16*8(%[m00]), %%zmm5 \n\t"
                    "vmovupd 16*8(%[m01]), %%zmm6 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                    "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 16*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 24*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 24*8(%[m00]), %%zmm10 \n\t"
                    "vmovupd 24*8(%[m01]), %%zmm11 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                    "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 24*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-32*8, %[r]   \n\t"
                    "subq $-32*8, %[m00] \n\t"
                    "subq $-32*8, %[m01] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                    :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&16){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                        "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                        "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                        "vmovupd 8*8(%[m01]), %%zmm11 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                        "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                        "\n\t"
                        "vmulpd %%zmm1, %%zmm11, %%zmm11 \n\t"
                        "vaddpd %%zmm11, %%zmm9, %%zmm9 \n\t"
                        "\n\t"
                        "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-16*8, %[r]   \n\t"
                        "subq $-16*8, %[m00] \n\t"
                        "subq $-16*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&8){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%zmm6 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmulpd %%zmm1, %%zmm6, %%zmm6 \n\t"
                        "vaddpd %%zmm6, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-8*8, %[r]   \n\t"
                        "subq $-8*8, %[m00] \n\t"
                        "subq $-8*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "vmovupd 0*8(%[m01]), %%ymm6 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmulpd %%ymm1, %%ymm6, %%ymm6 \n\t"
                        "vaddpd %%ymm6, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        "subq $-4*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "movupd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        "subq $-2*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "movsd 0*8(%[m01]), %%xmm6 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "vmulpd %%xmm1, %%xmm6, %%xmm6 \n\t"
                        "vaddpd %%xmm6, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        "subq $-1*8, %[m01] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00), [m01]"=r"(mat_ptr01)
                        :    "0"(res),       "1"(mat_ptr00),       "2"(mat_ptr01)
                    );
                }
            }

            mat_ptr00 += n_samples;
            mat_ptr01 += n_samples;
            vec += 2;
            res -= n_samples;
        }

        if(c_remain&1){
            __asm__ __volatile__ (
                "VBROADCASTSD %[r00], %%zmm0 \n\t"
                :[r00]"=m"(*(vec)), [r01]"=m"(*(vec+1))
                :
            );

            s_unroll_base = s_unroll;
            while(s_unroll_base--){
                __asm__ __volatile__ (
                    "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 16*8(%[r]),   %%zmm4 \n\t"
                    "vmovupd 16*8(%[m00]), %%zmm5 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                    "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                    "\n\t"
                    "vmovupd %%zmm4, 16*8(%[r]) \n\t"
                    "\n\t"
                    "vmovupd 24*8(%[r]),   %%zmm9 \n\t"
                    "vmovupd 24*8(%[m00]), %%zmm10 \n\t"
                    "\n\t"
                    "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                    "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                    "\n\t"
                    "vmovupd %%zmm9, 24*8(%[r]) \n\t"
                    "\n\t"
                    "subq $-32*8, %[r]   \n\t"
                    "subq $-32*8, %[m00] \n\t"
                    :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                    :    "0"(res),       "1"(mat_ptr00)
                );
            }

            s_remain_base = s_remain;
            if(s_remain_base>0){
                if(s_remain_base&16){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "vmovupd 8*8(%[r]),   %%zmm9 \n\t"
                        "vmovupd 8*8(%[m00]), %%zmm10 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm10, %%zmm10 \n\t"
                        "vaddpd %%zmm10, %%zmm9, %%zmm9 \n\t"
                        "\n\t"
                        "vmovupd %%zmm9, 8*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-16*8, %[r]   \n\t"
                        "subq $-16*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&8){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%zmm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%zmm5 \n\t"
                        "\n\t"
                        "vmulpd %%zmm0, %%zmm5, %%zmm5 \n\t"
                        "vaddpd %%zmm5, %%zmm4, %%zmm4 \n\t"
                        "\n\t"
                        "vmovupd %%zmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-8*8, %[r]   \n\t"
                        "subq $-8*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&4){
                    __asm__ __volatile__ (
                        "vmovupd 0*8(%[r]),   %%ymm4 \n\t"
                        "vmovupd 0*8(%[m00]), %%ymm5 \n\t"
                        "\n\t"
                        "vmulpd %%ymm0, %%ymm5, %%ymm5 \n\t"
                        "vaddpd %%ymm5, %%ymm4, %%ymm4 \n\t"
                        "\n\t"
                        "vmovupd %%ymm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-4*8, %[r]   \n\t"
                        "subq $-4*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&2){
                    __asm__ __volatile__ (
                        "movupd 0*8(%[r]),   %%xmm4 \n\t"
                        "movupd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movupd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-2*8, %[r]   \n\t"
                        "subq $-2*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }

                if(s_remain_base&1){
                    __asm__ __volatile__ (
                        "movsd 0*8(%[r]),   %%xmm4 \n\t"
                        "movsd 0*8(%[m00]), %%xmm5 \n\t"
                        "\n\t"
                        "vmulpd %%xmm0, %%xmm5, %%xmm5 \n\t"
                        "vaddpd %%xmm5, %%xmm4, %%xmm4 \n\t"
                        "\n\t"
                        "movsd %%xmm4, 0*8(%[r]) \n\t"
                        "\n\t"
                        "subq $-1*8, %[r]   \n\t"
                        "subq $-1*8, %[m00] \n\t"
                        :[r]"=r"(res), [m00]"=r"(mat_ptr00)
                        :    "0"(res),       "1"(mat_ptr00)
                    );
                }
            }

            vec += 1;
            res -= n_samples;
        }
    }
}
