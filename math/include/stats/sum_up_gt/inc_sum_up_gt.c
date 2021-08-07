#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define ALIGN_SIZE  32
#define ALIGN_CHECK 0x1f // 00001111

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t i;
    for (i=0; i<n; i++){
        if(y[i] > v){
            res += x[i];
        }
    }
    return(res);
}

double sum_up_gt_loop_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    int64_t i;
    for (i=0; i<n; i++){
        if(y[i] > v){
            res += x[i];
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_02_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00=0, r01=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }
        i+=unroll_size;
    }

    res = r00 + r01;
    if (n_remain>0){
        if(y[i] > v){
            res += x[i];
        }        
    }
    return(res);
}

double sum_up_gt_loop_02_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    double r00=0e0, r01=0e0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }
        i+=unroll_size;
    }

    res = r00 + r01;
    if (n_remain>0){
        if(y[i] > v){
            res += x[i];
        }        
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_04_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00=0, r01=0, r02=0, r03=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }
        i+=unroll_size;
    }

    r00 += r01;
    r02 += r03;

    r00 += r02;
    res = r00;

    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_04_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    double r00=0, r01=0, r02=0, r03=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }
        i+=unroll_size;
    }

    r00 += r01;
    r02 += r03;

    r00 += r02;
    res = r00;

    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_08_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00=0, r01=0, r02=0, r03=0;
    int64_t r04=0, r05=0, r06=0, r07=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }

        if(y[i+4] > v){
            r04 += x[i+4];
        }

        if(y[i+5] > v){
            r05 += x[i+5];
        }

        if(y[i+6] > v){
            r06 += x[i+6];
        }

        if(y[i+7] > v){
            r07 += x[i+7];
        }
        i+=unroll_size;
    }

    r00 += r01;
    r02 += r03;
    r04 += r05;
    r06 += r07;

    r00 += r02;
    r04 += r06;

    r00 += r04;
    res = r00;

    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_08_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    double r00=0, r01=0, r02=0, r03=0;
    double r04=0, r05=0, r06=0, r07=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }

        if(y[i+4] > v){
            r04 += x[i+4];
        }

        if(y[i+5] > v){
            r05 += x[i+5];
        }

        if(y[i+6] > v){
            r06 += x[i+6];
        }

        if(y[i+7] > v){
            r07 += x[i+7];
        }
        i+=unroll_size;
    }

    r00 += r01;
    r02 += r03;
    r04 += r05;
    r06 += r07;

    r00 += r02;
    r04 += r06;

    r00 += r04;
    res = r00;

    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_16_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00=0, r01=0, r02=0, r03=0;
    int64_t r04=0, r05=0, r06=0, r07=0;
    int64_t r08=0, r09=0, r10=0, r11=0;
    int64_t r12=0, r13=0, r14=0, r15=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }

        if(y[i+4] > v){
            r04 += x[i+4];
        }

        if(y[i+5] > v){
            r05 += x[i+5];
        }

        if(y[i+6] > v){
            r06 += x[i+6];
        }

        if(y[i+7] > v){
            r07 += x[i+7];
        }

        if(y[i+8] > v){
            r08 += x[i+8];
        }

        if(y[i+9] > v){
            r09 += x[i+9];
        }

        if(y[i+10] > v){
            r10 += x[i+10];
        }

        if(y[i+11] > v){
            r11 += x[i+11];
        }

        if(y[i+12] > v){
            r12 += x[i+12];
        }

        if(y[i+13] > v){
            r13 += x[i+13];
        }

        if(y[i+14] > v){
            r14 += x[i+14];
        }

        if(y[i+15] > v){
            r15 += x[i+15];
        }
        i+=unroll_size;
    }


    r00 += r01;
    r02 += r03;
    r04 += r05;
    r06 += r07;
    r08 += r09;
    r10 += r11;
    r12 += r13;
    r14 += r15;

    r00 += r02;
    r04 += r06;
    r08 += r10;
    r12 += r14;

    r00 += r04;
    r08 += r12;

    r00 += r08;

    res = r00;


    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_16_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    double r00=0, r01=0, r02=0, r03=0;
    double r04=0, r05=0, r06=0, r07=0;
    double r08=0, r09=0, r10=0, r11=0;
    double r12=0, r13=0, r14=0, r15=0;

    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        if(y[i] > v){
            r00 += x[i];
        }

        if(y[i+1] > v){
            r01 += x[i+1];
        }

        if(y[i+2] > v){
            r02 += x[i+2];
        }

        if(y[i+3] > v){
            r03 += x[i+3];
        }

        if(y[i+4] > v){
            r04 += x[i+4];
        }

        if(y[i+5] > v){
            r05 += x[i+5];
        }

        if(y[i+6] > v){
            r06 += x[i+6];
        }

        if(y[i+7] > v){
            r07 += x[i+7];
        }

        if(y[i+8] > v){
            r08 += x[i+8];
        }

        if(y[i+9] > v){
            r09 += x[i+9];
        }

        if(y[i+10] > v){
            r10 += x[i+10];
        }

        if(y[i+11] > v){
            r11 += x[i+11];
        }

        if(y[i+12] > v){
            r12 += x[i+12];
        }

        if(y[i+13] > v){
            r13 += x[i+13];
        }

        if(y[i+14] > v){
            r14 += x[i+14];
        }

        if(y[i+15] > v){
            r15 += x[i+15];
        }
        i+=unroll_size;
    }


    r00 += r01;
    r02 += r03;
    r04 += r05;
    r06 += r07;
    r08 += r09;
    r10 += r11;
    r12 += r13;
    r14 += r15;

    r00 += r02;
    r04 += r06;
    r08 += r10;
    r12 += r14;

    r00 += r04;
    r08 += r12;

    r00 += r08;

    res = r00;


    if (n_remain>0){
        while(n_remain--){
            if(y[i] > v){
                res += x[i];
            }        
            i+=1;
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_branchless_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t tmp;
    int64_t i;
    for (i=0; i<n; i++){
        tmp = y[i]>v?1:0;
        res += x[i]*tmp;
    }
    return(res);
}

double sum_up_gt_loop_branchless_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    int64_t tmp;
    int64_t i;
    for (i=0; i<n; i++){
        tmp = y[i]>v?1:0;
        res += x[i]*tmp;
    }
    return(res);
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_branchless_02_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06=0, r07=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r06 += r01 * r02;

        r03 = y[i+1];
        r04 = x[i+1];
        r05 = r03>v?1:0;
        r07 += r04 * r05;
        i+=unroll_size;
    }

    res = r06 + r07;
    if(n_remain>0){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        res += r01 * r02;
    }
    return(res);
}

double sum_up_gt_loop_branchless_02_C_r8(double x[], double y[], double v, int64_t n){
    double res=0e0;
    double r00, r01, r03, r04;
    double r06=0e0, r07=0e0;
    int64_t r02, r05;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;
    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r06 += r01 * r02;

        r03 = y[i+1];
        r04 = x[i+1];
        r05 = r03>v?1:0;
        r07 += r04 * r05;
        i+=unroll_size;
    }

    res = r06 + r07;
    if(n_remain>0){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        res += r01 * r02;
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_branchless_04_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00, r01, r02, r03;
    int64_t r04, r05, r06, r07;
    int64_t r08, r09, r10, r11;
    int64_t r12=0, r13=0, r14=0, r15=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r12 += r01 * r02;

        r03 = y[i+1];
        r04 = x[i+1];
        r05 = r03>v?1:0;
        r13 += r04 * r05;

        r06 = y[i+2];
        r07 = x[i+2];
        r08 = r06>v?1:0;
        r14 += r07 * r08;

        r09 = y[i+3];
        r10 = x[i+3];
        r11 = r09>v?1:0;
        r15 += r10 * r11;
        i+=unroll_size;
    }

    res = r12 + r13 + r14 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_branchless_04_C_r8(double x[], double y[], double v, int64_t n){
    double res=0;
    double r00, r01, r03;
    double r04, r06, r07, r09, r10;
    int64_t r02, r05, r08, r11;
    double r12=0, r13=0, r14=0, r15=0;

    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r12 += r01 * r02;

        r03 = y[i+1];
        r04 = x[i+1];
        r05 = r03>v?1:0;
        r13 += r04 * r05;

        r06 = y[i+2];
        r07 = x[i+2];
        r08 = r06>v?1:0;
        r14 += r07 * r08;

        r09 = y[i+3];
        r10 = x[i+3];
        r11 = r09>v?1:0;
        r15 += r10 * r11;
        i+=unroll_size;
    }

    res = r12 + r13 + r14 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_branchless_08_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00, r01, r02, r03=0;
    int64_t r04, r05, r06, r07=0;
    int64_t r08, r09, r10, r11=0;
    int64_t r12, r13, r14, r15=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+1];
        r05 = x[i+1];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+2];
        r09 = x[i+2];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+3];
        r13 = x[i+3];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+4];
        r01 = x[i+4];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+5];
        r05 = x[i+5];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+6];
        r09 = x[i+6];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+7];
        r13 = x[i+7];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        i+=unroll_size;
    }

    res = r03 + r07 + r11 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_branchless_08_C_r8(double x[], double y[], double v, int64_t n){
    double res=0;
    double r00, r01, r03=0;
    double r04, r05, r07=0;
    double r08, r09, r11=0;
    double r12, r13, r15=0;
    int64_t r02, r06, r10, r14;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+1];
        r05 = x[i+1];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+2];
        r09 = x[i+2];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+3];
        r13 = x[i+3];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+4];
        r01 = x[i+4];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+5];
        r05 = x[i+5];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+6];
        r09 = x[i+6];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+7];
        r13 = x[i+7];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        i+=unroll_size;
    }

    res = r03 + r07 + r11 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_branchless_16_C_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res=0;
    int64_t r00, r01, r02, r03=0;
    int64_t r04, r05, r06, r07=0;
    int64_t r08, r09, r10, r11=0;
    int64_t r12, r13, r14, r15=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+1];
        r05 = x[i+1];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+2];
        r09 = x[i+2];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+3];
        r13 = x[i+3];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+4];
        r01 = x[i+4];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+5];
        r05 = x[i+5];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+6];
        r09 = x[i+6];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+7];
        r13 = x[i+7];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+8];
        r01 = x[i+8];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+9];
        r05 = x[i+9];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+10];
        r09 = x[i+10];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+11];
        r13 = x[i+11];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+12];
        r01 = x[i+12];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+13];
        r05 = x[i+13];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+14];
        r09 = x[i+14];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+15];
        r13 = x[i+15];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        i+=unroll_size;
    }

    res = r03 + r07 + r11 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}

double sum_up_gt_loop_branchless_16_C_r8(double x[], double y[], double v, int64_t n){
    double res=0;
    double r00, r01, r03=0;
    double r04, r05, r07=0;
    double r08, r09, r11=0;
    double r12, r13, r15=0;
    int64_t r02, r06, r10, r14;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    int64_t i=0;

    while(n_unroll--){
        r00 = y[i];
        r01 = x[i];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+1];
        r05 = x[i+1];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+2];
        r09 = x[i+2];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+3];
        r13 = x[i+3];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+4];
        r01 = x[i+4];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+5];
        r05 = x[i+5];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+6];
        r09 = x[i+6];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+7];
        r13 = x[i+7];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+8];
        r01 = x[i+8];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+9];
        r05 = x[i+9];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+10];
        r09 = x[i+10];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+11];
        r13 = x[i+11];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        r00 = y[i+12];
        r01 = x[i+12];
        r02 = r00>v?1:0;
        r03 += r01 * r02;

        r04 = y[i+13];
        r05 = x[i+13];
        r06 = r04>v?1:0;
        r07 += r05 * r06;

        r08 = y[i+14];
        r09 = x[i+14];
        r10 = r08>v?1:0;
        r11 += r09 * r10;

        r12 = y[i+15];
        r13 = x[i+15];
        r14 = r12>v?1:0;
        r15 += r13 * r14;

        i+=unroll_size;
    }

    res = r03 + r07 + r11 + r15;
    if(n_remain>0){
        while(n_remain--){
            r00 = y[i];
            r01 = x[i];
            r02 = r00>v?1:0;
            res += r01 * r02;
            i+=1;
        }
    }
    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_02_A_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_i8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor    %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "VPCMPEQB %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm14      \n\t" // broadcast
        "MOVDQU 0*8(%[x]),   %%xmm0       \n\t" // load x
        "MOVDQU 0*8(%[y]),   %%xmm1       \n\t" // load y
        "VPCMPGTQ %%xmm14, %%xmm1, %%xmm1 \n\t"
        "subq $-2*8, %[x]                 \n\t"
        "subq $-2*8, %[y]                 \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "PXOR   %%xmm13,   %%xmm1         \n\t"
            "PAND   %%xmm1,    %%xmm0         \n\t"
            "PADDQ  %%xmm0,    %%xmm15        \n\t"
            "MOVDQU 0*8(%[x]), %%xmm0         \n\t"
            "MOVDQU 0*8(%[y]), %%xmm1         \n\t"
            "\n\t"
            "subq $-2*8, %[x]                 \n\t"
            "subq $-2*8, %[y]                 \n\t"
            "VPCMPGTQ %%xmm14, %%xmm1, %%xmm1 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "PXOR     %%xmm13,   %%xmm1        \n\t"
        "PAND  %%xmm1, %%xmm0    \n\t"
        "PADDQ %%xmm0, %%xmm15   \n\t"
        "\n\t"
        "MOVDQU %%xmm15, %%xmm12 \n\t"
        "PSRLDQ $1*8,    %%xmm12 \n\t"
        "PADDQ  %%xmm12, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0         \n\t"
                "MOVQ    0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm14,  %%xmm1, %%xmm1 \n\t"
                "PXOR     %%xmm13,   %%xmm1        \n\t"
                "PAND     %%xmm1,    %%xmm0        \n\t"
                "PADDQ    %%xmm0,    %%xmm15       \n\t"
                "subq $-1*8, %[x]                  \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

double sum_up_gt_loop_02_A_r8(double x[], double y[], double v, int64_t n){
    double res;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_r8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm14     \n\t" // broadcast
        "movupd 0*8(%[x]),   %%xmm0      \n\t" // load x
        "movupd 0*8(%[y]),   %%xmm1      \n\t" // load y
        "CMPLEPD %%xmm14,    %%xmm1      \n\t" // x > v
        "subq $-2*8, %[x]                \n\t"
        "subq $-2*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "ANDPD  %%xmm1,    %%xmm0  \n\t"
            "addpd  %%xmm0,    %%xmm15 \n\t"
            "movupd 0*8(%[x]), %%xmm0  \n\t"
            "movupd 0*8(%[y]), %%xmm1  \n\t"
            "\n\t"
            "subq $-2*8, %[x]          \n\t"
            "subq $-2*8, %[y]          \n\t"
            "CMPLEPD %%xmm14, %%xmm1   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "ANDPD %%xmm1, %%xmm0    \n\t"
        "addpd %%xmm0, %%xmm15   \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm13 \n\t"
        "PSRLDQ $1*8,   %%xmm13  \n\t"
        "addpd  %%xmm13, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        __asm__ __volatile__ (
            "movsd   0*8(%[x]), %%xmm0  \n\t"
            "movsd   0*8(%[y]), %%xmm1  \n\t"
            "CMPLEPD %%xmm14,   %%xmm1  \n\t" // x > v
            "ANDPD   %%xmm1,    %%xmm0  \n\t"
            "addpd   %%xmm0,    %%xmm15 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_04_A_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_i8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor    %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "VPCMPEQB %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm14      \n\t" // broadcast
        "vMOVDQU 0*8(%[x]),   %%ymm0       \n\t" // load x
        "vMOVDQU 0*8(%[y]),   %%ymm1       \n\t" // load y
        "VPCMPGTQ %%ymm14, %%ymm1, %%ymm1 \n\t"
        "subq $-4*8, %[x]                 \n\t"
        "subq $-4*8, %[y]                 \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPXOR   %%ymm13, %%ymm1,  %%ymm1  \n\t"
            "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
            "VPADDQ  %%ymm0,  %%ymm15, %%ymm15 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0         \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1         \n\t"
            "\n\t"
            "subq $-4*8, %[x]                 \n\t"
            "subq $-4*8, %[y]                 \n\t"
            "VPCMPGTQ %%ymm14, %%ymm1, %%ymm1 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPXOR   %%ymm13, %%ymm1,  %%ymm1  \n\t"
        "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
        "VPADDQ  %%ymm0,  %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "PADDQ  %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7,  %%xmm15 \n\t"
        "PSRLDQ $1*8,    %%xmm15 \n\t"
        "PADDQ  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ     0*8(%[x]), %%xmm0         \n\t"
                "MOVQ     0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm14,   %%xmm1, %%xmm1 \n\t"
                "PXOR     %%xmm13,   %%xmm1         \n\t"
                "PAND     %%xmm1,    %%xmm0         \n\t"
                "PADDQ    %%xmm0,    %%xmm15        \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

double sum_up_gt_loop_04_A_r8(double x[], double y[], double v, int64_t n){
    double res;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_r8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15      \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm14          \n\t" // broadcast
        "vmovupd  0*8(%[x]), %%ymm0           \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1           \n\t" // load y
        "VCMPLEPD %%ymm14,   %%ymm1,   %%ymm1 \n\t" // x > v
        "subq $-4*8, %[x]                     \n\t"
        "subq $-4*8, %[y]                     \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
            "vmovupd 0*8(%[x]),%%ymm0         \n\t"
            "vmovupd 0*8(%[y]),%%ymm1         \n\t"
            "\n\t"
            "subq $-4*8, %[x]                 \n\t"
            "subq $-4*8, %[y]                 \n\t"
            "VCMPLEPD %%ymm14, %%ymm1, %%ymm1 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "addpd %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "addpd  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "CMPLEPD %%xmm14,   %%xmm1  \n\t"
                "ANDPD   %%xmm1,    %%xmm0  \n\t"
                "addpd   %%xmm0,    %%xmm15 \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_08_A_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_i8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor    %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor    %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "VPCMPEQB %%ymm12, %%ymm12, %%ymm12  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13       \n\t" // broadcast
        "\n\t"
        "vMOVDQU 0*8(%[x]),   %%ymm0       \n\t" // load x
        "vMOVDQU 0*8(%[y]),   %%ymm1       \n\t" // load y
        "VPCMPGTQ %%ymm13, %%ymm1, %%ymm1  \n\t"
        "VPXOR    %%ymm12, %%ymm1, %%ymm1  \n\t"
        "\n\t"
        "vMOVDQU 4*8(%[x]),   %%ymm2       \n\t" // load x
        "vMOVDQU 4*8(%[y]),   %%ymm3       \n\t" // load y
        "VPCMPGTQ %%ymm13, %%ymm3, %%ymm3  \n\t"
        "VPXOR    %%ymm12, %%ymm3, %%ymm3  \n\t"
        "subq $-8*8, %[x]                 \n\t"
        "subq $-8*8, %[y]                 \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
            "VPADDQ  %%ymm0,  %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0         \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1         \n\t"
            "\n\t"
            "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
            "VPADDQ  %%ymm2,  %%ymm15, %%ymm15 \n\t"
            "vMOVDQU 4*8(%[x]), %%ymm2         \n\t"
            "vMOVDQU 4*8(%[y]), %%ymm3         \n\t"
            "\n\t"
            "subq $-8*8, %[x]                 \n\t"
            "subq $-8*8, %[y]                 \n\t"
            "\n\t"
            "VPCMPGTQ %%ymm13, %%ymm1, %%ymm1 \n\t"
            "VPXOR    %%ymm12, %%ymm1, %%ymm1 \n\t"
            "VPCMPGTQ %%ymm13, %%ymm3, %%ymm3 \n\t"
            "VPXOR    %%ymm12, %%ymm3, %%ymm3 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
        "VPADDQ  %%ymm0,  %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
        "VPADDQ  %%ymm2,  %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPADDQ  %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "PADDQ  %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7,  %%xmm15 \n\t"
        "PSRLDQ $1*8,    %%xmm15 \n\t"
        "PADDQ  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ     0*8(%[x]), %%xmm0         \n\t"
                "MOVQ     0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm13,   %%xmm1, %%xmm1 \n\t"
                "PXOR     %%xmm12,   %%xmm1         \n\t"
                "PAND     %%xmm1,    %%xmm0         \n\t"
                "PADDQ    %%xmm0,    %%xmm15        \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

double sum_up_gt_loop_08_A_r8(double x[], double y[], double v, int64_t n){
    double res;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_r8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15      \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14      \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13          \n\t" // broadcast
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0           \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1           \n\t" // load y
        "VCMPLEPD %%ymm13,   %%ymm1,   %%ymm1 \n\t" // x > v
        "\n\t"
        "vmovupd  4*8(%[x]), %%ymm2           \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3           \n\t" // load y
        "VCMPLEPD %%ymm13,   %%ymm3,   %%ymm3 \n\t" // x > v
        "subq $-8*8, %[x]                     \n\t"
        "subq $-8*8, %[y]                     \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vaddpd  %%ymm0, %%ymm14, %%ymm14 \n\t"
            "vmovupd 0*8(%[x]),%%ymm0         \n\t"
            "vmovupd 0*8(%[y]),%%ymm1         \n\t"
            "\n\t"
            "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vaddpd  %%ymm2, %%ymm15, %%ymm15 \n\t"
            "vmovupd 4*8(%[x]),%%ymm2         \n\t"
            "vmovupd 4*8(%[y]),%%ymm3         \n\t"
            "\n\t"
            "subq $-8*8, %[x]                 \n\t"
            "subq $-8*8, %[y]                 \n\t"
            "VCMPLEPD %%ymm13, %%ymm1, %%ymm1 \n\t"
            "VCMPLEPD %%ymm13, %%ymm3, %%ymm3 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vaddpd  %%ymm0, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
        "vaddpd  %%ymm2, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "addpd %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "addpd  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "CMPLEPD %%xmm13,   %%xmm1  \n\t"
                "ANDPD   %%xmm1,    %%xmm0  \n\t"
                "addpd   %%xmm0,    %%xmm15 \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_16_A_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_i8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor    %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor    %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor    %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor    %%ymm12, %%ymm12, %%ymm12  \n\t" // zero clear
        "VPCMPEQB %%ymm10, %%ymm10, %%ymm10  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11       \n\t" // broadcast
        "\n\t"
        "vMOVDQU  0*8(%[x]),   %%ymm0       \n\t" // load x
        "vMOVDQU  0*8(%[y]),   %%ymm1       \n\t" // load y
        "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1  \n\t"
        "VPXOR    %%ymm10, %%ymm1, %%ymm1  \n\t"
        "\n\t"
        "vMOVDQU  4*8(%[x]),   %%ymm2       \n\t" // load x
        "vMOVDQU  4*8(%[y]),   %%ymm3       \n\t" // load y
        "VPCMPGTQ %%ymm11, %%ymm3, %%ymm3  \n\t"
        "VPXOR    %%ymm10, %%ymm3, %%ymm3  \n\t"
        "\n\t"
        "vMOVDQU  8*8(%[x]),   %%ymm4       \n\t" // load x
        "vMOVDQU  8*8(%[y]),   %%ymm5       \n\t" // load y
        "VPCMPGTQ %%ymm11, %%ymm5, %%ymm5  \n\t"
        "VPXOR    %%ymm10, %%ymm5, %%ymm5  \n\t"
        "\n\t"
        "vMOVDQU 12*8(%[x]),   %%ymm6       \n\t" // load x
        "vMOVDQU 12*8(%[y]),   %%ymm7       \n\t" // load y
        "VPCMPGTQ %%ymm11, %%ymm7, %%ymm7  \n\t"
        "VPXOR    %%ymm10, %%ymm7, %%ymm7  \n\t"
        "\n\t"
        "subq $-16*8, %[x]                 \n\t"
        "subq $-16*8, %[y]                 \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
            "VPADDQ  %%ymm0,  %%ymm12, %%ymm12 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0         \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1         \n\t"
            "\n\t"
            "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
            "VPADDQ  %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "vMOVDQU 4*8(%[x]), %%ymm2         \n\t"
            "vMOVDQU 4*8(%[y]), %%ymm3         \n\t"
            "\n\t"
            "VPAND   %%ymm5,  %%ymm4,  %%ymm4  \n\t"
            "VPADDQ  %%ymm4,  %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 8*8(%[x]), %%ymm4         \n\t"
            "vMOVDQU 8*8(%[y]), %%ymm5         \n\t"
            "\n\t"
            "VPAND   %%ymm7,  %%ymm6,  %%ymm6  \n\t"
            "VPADDQ  %%ymm6,  %%ymm15, %%ymm15 \n\t"
            "vMOVDQU 12*8(%[x]), %%ymm6        \n\t"
            "vMOVDQU 12*8(%[y]), %%ymm7        \n\t"
            "\n\t"
            "subq $-16*8, %[x]                 \n\t"
            "subq $-16*8, %[y]                 \n\t"
            "\n\t"
            "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1 \n\t"
            "VPXOR    %%ymm10, %%ymm1, %%ymm1 \n\t"
            "VPCMPGTQ %%ymm11, %%ymm3, %%ymm3 \n\t"
            "VPXOR    %%ymm10, %%ymm3, %%ymm3 \n\t"
            "VPCMPGTQ %%ymm11, %%ymm5, %%ymm5 \n\t"
            "VPXOR    %%ymm10, %%ymm5, %%ymm5 \n\t"
            "VPCMPGTQ %%ymm11, %%ymm7, %%ymm7 \n\t"
            "VPXOR    %%ymm10, %%ymm7, %%ymm7 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
        "VPADDQ  %%ymm0,  %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
        "VPADDQ  %%ymm2,  %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "VPAND   %%ymm5,  %%ymm4,  %%ymm4  \n\t"
        "VPADDQ  %%ymm4,  %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VPAND   %%ymm7,  %%ymm6,  %%ymm6  \n\t"
        "VPADDQ  %%ymm6,  %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VPADDQ  %%ymm12, %%ymm13, %%ymm13 \n\t"
        "VPADDQ  %%ymm14, %%ymm15, %%ymm15 \n\t"
        "VPADDQ  %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "PADDQ  %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7,  %%xmm15 \n\t"
        "PSRLDQ $1*8,    %%xmm15 \n\t"
        "PADDQ  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ     0*8(%[x]), %%xmm0         \n\t"
                "MOVQ     0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm11,   %%xmm1, %%xmm1 \n\t"
                "PXOR     %%xmm10,   %%xmm1         \n\t"
                "PAND     %%xmm1,    %%xmm0         \n\t"
                "PADDQ    %%xmm0,    %%xmm15        \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

double sum_up_gt_loop_16_A_r8(double x[], double y[], double v, int64_t n){
    double res;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_r8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15      \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14      \n\t" // zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13      \n\t" // zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12      \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11          \n\t" // broadcast
        "\n\t"
        "vmovupd  0*8(%[x]), %%ymm0           \n\t" // load x
        "vmovupd  0*8(%[y]), %%ymm1           \n\t" // load y
        "VCMPLEPD %%ymm11,   %%ymm1,   %%ymm1 \n\t" // x > v
        "\n\t"
        "vmovupd  4*8(%[x]), %%ymm2           \n\t" // load x
        "vmovupd  4*8(%[y]), %%ymm3           \n\t" // load y
        "VCMPLEPD %%ymm11,   %%ymm3,   %%ymm3 \n\t" // x > v
        "\n\t"
        "vmovupd  8*8(%[x]), %%ymm4           \n\t" // load x
        "vmovupd  8*8(%[y]), %%ymm5           \n\t" // load y
        "VCMPLEPD %%ymm11,   %%ymm5,   %%ymm5 \n\t" // x > v
        "\n\t"
        "vmovupd 12*8(%[x]), %%ymm6           \n\t" // load x
        "vmovupd 12*8(%[y]), %%ymm7           \n\t" // load y
        "VCMPLEPD %%ymm11,   %%ymm7,   %%ymm7 \n\t" // x > v
        "subq $-16*8, %[x]                     \n\t"
        "subq $-16*8, %[y]                     \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v)
        :"0"(x), "1"(y), "2"(v)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
            "vmovupd 0*8(%[x]),%%ymm0         \n\t"
            "vmovupd 0*8(%[y]),%%ymm1         \n\t"
            "\n\t"
            "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vmovupd 4*8(%[x]),%%ymm2         \n\t"
            "vmovupd 4*8(%[y]),%%ymm3         \n\t"
            "\n\t"
            "vandpd  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vaddpd  %%ymm4, %%ymm14, %%ymm14 \n\t"
            "vmovupd 8*8(%[x]),%%ymm4         \n\t"
            "vmovupd 8*8(%[y]),%%ymm5         \n\t"
            "\n\t"
            "vandpd  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vaddpd  %%ymm6, %%ymm15, %%ymm15 \n\t"
            "vmovupd 12*8(%[x]),%%ymm6        \n\t"
            "vmovupd 12*8(%[y]),%%ymm7        \n\t"
            "\n\t"
            "subq $-16*8, %[x]                \n\t"
            "subq $-16*8, %[y]                \n\t"
            "VCMPLEPD %%ymm11, %%ymm1, %%ymm1 \n\t"
            "VCMPLEPD %%ymm11, %%ymm3, %%ymm3 \n\t"
            "VCMPLEPD %%ymm11, %%ymm5, %%ymm5 \n\t"
            "VCMPLEPD %%ymm11, %%ymm7, %%ymm7 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
        "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
        "\n\t"
        "vandpd  %%ymm5, %%ymm4,  %%ymm4  \n\t"
        "vaddpd  %%ymm4, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vandpd  %%ymm7, %%ymm6,  %%ymm6  \n\t"
        "vaddpd  %%ymm6, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "vaddpd %%ymm12, %%ymm13, %%ymm13 \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "addpd %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "addpd  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "CMPLEPD %%xmm11,   %%xmm1  \n\t"
                "ANDPD   %%xmm1,    %%xmm0  \n\t"
                "addpd   %%xmm0,    %%xmm15 \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
int64_t sum_up_gt_loop_32_A_i8(int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t res;
    int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        res = sum_up_gt_loop_C_i8(x, y, v, n);
        return (res);
    }

    __asm__ __volatile__ (
        "vpxor    %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor    %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor    %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor    %%ymm12, %%ymm12, %%ymm12  \n\t" // zero clear
        "VPCMPEQB %%ymm10, %%ymm10, %%ymm10  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11       \n\t" // broadcast
        :[v]"=r"(v)
        :"0"(v)
    );

    while(n_unroll--){
        __asm__ __volatile__(
            "vMOVDQU 0*8(%[x]), %%ymm0         \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1 \n\t"
            "VPXOR    %%ymm10, %%ymm1, %%ymm1 \n\t"
            "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
            "VPADDQ  %%ymm0,  %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vMOVDQU 4*8(%[x]), %%ymm2         \n\t"
            "vMOVDQU 4*8(%[y]), %%ymm3         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm3, %%ymm3 \n\t"
            "VPXOR    %%ymm10, %%ymm3, %%ymm3 \n\t"
            "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
            "VPADDQ  %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "vMOVDQU 8*8(%[x]), %%ymm4         \n\t"
            "vMOVDQU 8*8(%[y]), %%ymm5         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm5, %%ymm5 \n\t"
            "VPXOR    %%ymm10, %%ymm5, %%ymm5 \n\t"
            "VPAND   %%ymm5,  %%ymm4,  %%ymm4  \n\t"
            "VPADDQ  %%ymm4,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vMOVDQU 12*8(%[x]), %%ymm6        \n\t"
            "vMOVDQU 12*8(%[y]), %%ymm7        \n\t"
            "VPCMPGTQ %%ymm11, %%ymm7, %%ymm7 \n\t"
            "VPXOR    %%ymm10, %%ymm7, %%ymm7 \n\t"
            "VPAND   %%ymm7,  %%ymm6,  %%ymm6  \n\t"
            "VPADDQ  %%ymm6,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vMOVDQU 16*8(%[x]), %%ymm0         \n\t"
            "vMOVDQU 16*8(%[y]), %%ymm1         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1 \n\t"
            "VPXOR    %%ymm10, %%ymm1, %%ymm1 \n\t"
            "VPAND   %%ymm1,  %%ymm0,  %%ymm0  \n\t"
            "VPADDQ  %%ymm0,  %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vMOVDQU 20*8(%[x]), %%ymm2         \n\t"
            "vMOVDQU 20*8(%[y]), %%ymm3         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm3, %%ymm3 \n\t"
            "VPXOR    %%ymm10, %%ymm3, %%ymm3 \n\t"
            "VPAND   %%ymm3,  %%ymm2,  %%ymm2  \n\t"
            "VPADDQ  %%ymm2,  %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "vMOVDQU 24*8(%[x]), %%ymm4         \n\t"
            "vMOVDQU 24*8(%[y]), %%ymm5         \n\t"
            "VPCMPGTQ %%ymm11, %%ymm5, %%ymm5 \n\t"
            "VPXOR    %%ymm10, %%ymm5, %%ymm5 \n\t"
            "VPAND   %%ymm5,  %%ymm4,  %%ymm4  \n\t"
            "VPADDQ  %%ymm4,  %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vMOVDQU 28*8(%[x]), %%ymm6        \n\t"
            "vMOVDQU 28*8(%[y]), %%ymm7        \n\t"
            "VPCMPGTQ %%ymm11, %%ymm7, %%ymm7 \n\t"
            "VPXOR    %%ymm10, %%ymm7, %%ymm7 \n\t"
            "VPAND   %%ymm7,  %%ymm6,  %%ymm6  \n\t"
            "VPADDQ  %%ymm6,  %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-32*8, %[x]                 \n\t"
            "subq $-32*8, %[y]                 \n\t"
            "\n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "VPADDQ  %%ymm12, %%ymm13, %%ymm13 \n\t"
        "VPADDQ  %%ymm14, %%ymm15, %%ymm15 \n\t"
        "VPADDQ  %%ymm13, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "PADDQ  %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7,  %%xmm15 \n\t"
        "PSRLDQ $1*8,    %%xmm15 \n\t"
        "PADDQ  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ     0*8(%[x]), %%xmm0         \n\t"
                "MOVQ     0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm11,   %%xmm1, %%xmm1 \n\t"
                "PXOR     %%xmm10,   %%xmm1         \n\t"
                "PAND     %%xmm1,    %%xmm0         \n\t"
                "PADDQ    %%xmm0,    %%xmm15        \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}

double sum_up_gt_loop_32_A_r8(double x[], double y[], double v, int64_t n){
    double res;
    int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15      \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14      \n\t" // zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13      \n\t" // zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12      \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11          \n\t" // broadcast
        :[v]"=r"(v)
        :"0"(v)
    );

    while(n_unroll--){
        __asm__ __volatile__(
            "vmovupd  0*8(%[x]),%%ymm0        \n\t"
            "vmovupd  0*8(%[y]),%%ymm1        \n\t"
            "VCMPLEPD %%ymm11, %%ymm1, %%ymm1 \n\t"
            "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vmovupd  4*8(%[x]),%%ymm2        \n\t"
            "vmovupd  4*8(%[y]),%%ymm3        \n\t"
            "VCMPLEPD %%ymm11, %%ymm3, %%ymm3 \n\t"
            "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "vmovupd  8*8(%[x]),%%ymm4        \n\t"
            "vmovupd  8*8(%[y]),%%ymm5        \n\t"
            "VCMPLEPD %%ymm11, %%ymm5, %%ymm5 \n\t"
            "vandpd  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vaddpd  %%ymm4, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmovupd 12*8(%[x]),%%ymm6        \n\t"
            "vmovupd 12*8(%[y]),%%ymm7        \n\t"
            "VCMPLEPD %%ymm11, %%ymm7, %%ymm7 \n\t"
            "vandpd  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vaddpd  %%ymm6, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "vmovupd 16*8(%[x]),%%ymm0        \n\t"
            "vmovupd 16*8(%[y]),%%ymm1        \n\t"
            "VCMPLEPD %%ymm11, %%ymm1, %%ymm1 \n\t"
            "vandpd  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vaddpd  %%ymm0, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vmovupd 20*8(%[x]),%%ymm2        \n\t"
            "vmovupd 20*8(%[y]),%%ymm3        \n\t"
            "VCMPLEPD %%ymm11, %%ymm3, %%ymm3 \n\t"
            "vandpd  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "\n\t"
            "vmovupd 24*8(%[x]),%%ymm4        \n\t"
            "vmovupd 24*8(%[y]),%%ymm5        \n\t"
            "VCMPLEPD %%ymm11, %%ymm5, %%ymm5 \n\t"
            "vandpd  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vaddpd  %%ymm4, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmovupd 28*8(%[x]),%%ymm6        \n\t"
            "vmovupd 28*8(%[y]),%%ymm7        \n\t"
            "VCMPLEPD %%ymm11, %%ymm7, %%ymm7 \n\t"
            "vandpd  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vaddpd  %%ymm6, %%ymm15, %%ymm15 \n\t"
            "\n\t"
            "subq $-32*8, %[x]                \n\t"
            "subq $-32*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vaddpd %%ymm12, %%ymm13, %%ymm13 \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vaddpd %%ymm14, %%ymm15, %%ymm15 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm6 \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm7 \n\t"
        "addpd %%xmm6, %%xmm7   \n\t"
        "movupd %%xmm7, %%xmm15 \n\t"
        "PSRLDQ $1*8,   %%xmm15 \n\t"
        "addpd  %%xmm7, %%xmm15 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "CMPLEPD %%xmm11,   %%xmm1  \n\t"
                "ANDPD   %%xmm1,    %%xmm0  \n\t"
                "addpd   %%xmm0,    %%xmm15 \n\t"
                "subq $-1*8, %[x]           \n\t"
                "subq $-1*8, %[y]           \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[r] \n\t"
        :[r]"=m"(res)
        :
	);

    return(res);
}











































// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01;
    int64_t unroll_size=1, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }
    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01;
    int64_t unroll_size=1, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }
    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_02_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;
    int64_t r00, r01, r02=0;
    int64_t r03=0;
    int64_t r04, r05, r06=0;
    int64_t r07=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06;
    tmp_cnt = r03 + r07;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_02_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;
    double r00, r01, r02=0;
    int64_t r03=0;
    double r04, r05, r06=0;
    int64_t r07=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06;
    tmp_cnt = r03 + r07;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_04_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;
    int64_t r00, r01, r02=0;
    int64_t r03=0;
    int64_t r04, r05, r06=0;
    int64_t r07=0;
    int64_t r08, r09, r10=0;
    int64_t r11=0;
    int64_t r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_04_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;
    double r00, r01, r02=0;
    int64_t r03=0;
    double r04, r05, r06=0;
    int64_t r07=0;
    double r08, r09, r10=0;
    int64_t r11=0;
    double r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_08_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;
    int64_t r00, r01, r02=0;
    int64_t r03=0;
    int64_t r04, r05, r06=0;
    int64_t r07=0;
    int64_t r08, r09, r10=0;
    int64_t r11=0;
    int64_t r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+4);
        r01 = *(y+4);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+5);
        r05 = *(y+5);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+6);
        r09 = *(y+6);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+7);
        r13 = *(y+7);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_08_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;
    double r00, r01, r02=0;
    int64_t r03=0;
    double r04, r05, r06=0;
    int64_t r07=0;
    double r08, r09, r10=0;
    int64_t r11=0;
    double r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+4);
        r01 = *(y+4);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+5);
        r05 = *(y+5);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+6);
        r09 = *(y+6);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+7);
        r13 = *(y+7);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_16_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;
    int64_t r00, r01, r02=0;
    int64_t r03=0;
    int64_t r04, r05, r06=0;
    int64_t r07=0;
    int64_t r08, r09, r10=0;
    int64_t r11=0;
    int64_t r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+4);
        r01 = *(y+4);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+5);
        r05 = *(y+5);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+6);
        r09 = *(y+6);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+7);
        r13 = *(y+7);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+8);
        r01 = *(y+8);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+9);
        r05 = *(y+9);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+10);
        r09 = *(y+10);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+11);
        r13 = *(y+11);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+12);
        r01 = *(y+12);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+13);
        r05 = *(y+13);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+14);
        r09 = *(y+14);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+15);
        r13 = *(y+15);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_16_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;
    double r00, r01, r02=0;
    int64_t r03=0;
    double r04, r05, r06=0;
    int64_t r07=0;
    double r08, r09, r10=0;
    int64_t r11=0;
    double r12, r13, r14=0;
    int64_t r15=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+1);
        r05 = *(y+1);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+2);
        r09 = *(y+2);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+3);
        r13 = *(y+3);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+4);
        r01 = *(y+4);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+5);
        r05 = *(y+5);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+6);
        r09 = *(y+6);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+7);
        r13 = *(y+7);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+8);
        r01 = *(y+8);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+9);
        r05 = *(y+9);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+10);
        r09 = *(y+10);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+11);
        r13 = *(y+11);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }

        r00 = *(x+12);
        r01 = *(y+12);
        if (r01 > v){
            r02 += r00;
            r03 ++;
        }

        r04 = *(x+13);
        r05 = *(y+13);
        if (r05 > v){
            r06 += r04;
            r07 ++;
        }

        r08 = *(x+14);
        r09 = *(y+14);
        if (r09 > v){
            r10 += r08;
            r11 ++;
        }

        r12 = *(x+15);
        r13 = *(y+15);
        if (r13 > v){
            r14 += r12;
            r15 ++;
        }
        x+=unroll_size;
        y+=unroll_size;
    }

    tmp_sum = r02 + r06 + r10 + r14;
    tmp_cnt = r03 + r07 + r11 + r15;
    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        if (r01 > v){
            tmp_sum += r00;
            tmp_cnt ++;
        }
        x+=1;
        y+=1;
    }

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}






























// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_branchless_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01;
    int64_t r02;
    int64_t unroll_size=1, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        tmp_sum += r00 * r02;
        tmp_cnt += r02;

        x+=unroll_size;
        y+=unroll_size;
    }
    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_branchless_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01;
    int64_t r02;
    int64_t unroll_size=1, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        tmp_sum += r00 * r02;
        tmp_cnt += r02;

        x+=unroll_size;
        y+=unroll_size;
    }
    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_branchless_02_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01, r03=0;
    int64_t r02, r04=0;
    int64_t r05, r06, r08=0;
    int64_t r07, r09=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08;
    *count_left = r04 + r09;
}

void count_and_sum_up_gt_loop_branchless_02_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01, r03=0;
    int64_t r02, r04=0;
    double r05, r06, r08=0;
    int64_t r07, r09=0;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08;
    *count_left = r04 + r09;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_branchless_04_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01, r03=0;
    int64_t r02, r04=0;
    int64_t r05, r06, r08=0;
    int64_t r07, r09=0;
    int64_t r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}

void count_and_sum_up_gt_loop_branchless_04_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01, r03=0;
    int64_t r02, r04=0;
    double r05, r06, r08=0;
    int64_t r07, r09=0;
    double r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_branchless_08_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01, r03=0;
    int64_t r02, r04=0;
    int64_t r05, r06, r08=0;
    int64_t r07, r09=0;
    int64_t r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+4);
        r06 = *(y+4);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+5);
        r11 = *(y+5);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+6);
        r01 = *(y+6);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+7);
        r06 = *(y+7);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}

void count_and_sum_up_gt_loop_branchless_08_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01, r03=0;
    int64_t r02, r04=0;
    double r05, r06, r08=0;
    int64_t r07, r09=0;
    double r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+4);
        r06 = *(y+4);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+5);
        r11 = *(y+5);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+6);
        r01 = *(y+6);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+7);
        r06 = *(y+7);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_branchless_16_C_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum=0;
    int64_t tmp_cnt=0;
    int64_t r00, r01, r03=0;
    int64_t r02, r04=0;
    int64_t r05, r06, r08=0;
    int64_t r07, r09=0;
    int64_t r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+4);
        r06 = *(y+4);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+5);
        r11 = *(y+5);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+6);
        r01 = *(y+6);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+7);
        r06 = *(y+7);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+8);
        r11 = *(y+8);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+9);
        r01 = *(y+9);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+10);
        r06 = *(y+10);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+11);
        r11 = *(y+11);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+12);
        r01 = *(y+12);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+13);
        r06 = *(y+13);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+14);
        r11 = *(y+14);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+15);
        r01 = *(y+15);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}

void count_and_sum_up_gt_loop_branchless_16_C_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum=0;
    int64_t tmp_cnt=0;
    double r00, r01, r03=0;
    int64_t r02, r04=0;
    double r05, r06, r08=0;
    int64_t r07, r09=0;
    double r10, r11, r13=0;
    int64_t r12, r14=0;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;

    while(n_unroll--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+1);
        r06 = *(y+1);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+2);
        r11 = *(y+2);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+3);
        r01 = *(y+3);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+4);
        r06 = *(y+4);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+5);
        r11 = *(y+5);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+6);
        r01 = *(y+6);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+7);
        r06 = *(y+7);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+8);
        r11 = *(y+8);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+9);
        r01 = *(y+9);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+10);
        r06 = *(y+10);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+11);
        r11 = *(y+11);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+12);
        r01 = *(y+12);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        r05 = *(x+13);
        r06 = *(y+13);
        r07 = r06 > v;
        r08 += r05 * r07;
        r09 += r07;

        r10 = *(x+14);
        r11 = *(y+14);
        r12 = r11 > v;
        r13 += r10 * r12;
        r14 += r12;

        r00 = *(x+15);
        r01 = *(y+15);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;

        x+=unroll_size;
        y+=unroll_size;
    }

    while(n_remain--){
        r00 = *(x);
        r01 = *(y);
        r02 = r01 > v;
        r03 += r00 * r02;
        r04 += r02;
        x+=1;
        y+=1;
    }

    *sum_left   = r03 + r08 + r13;
    *count_left = r04 + r09 + r14;
}


































// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_02_A_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13      \n\t" // broadcast
        "VPBROADCASTQ  %[o], %%ymm12      \n\t" // zero clear
        "MOVDQU 0*8(%[x]),   %%xmm0       \n\t" // load x
        "MOVDQU 0*8(%[y]),   %%xmm1       \n\t" // load y
        "VPCMPGTQ %%xmm13, %%xmm1, %%xmm1 \n\t"
        "subq $-2*8, %[x]                 \n\t"
        "subq $-2*8, %[y]                 \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "\n\t"
            "vPAND  %%xmm1,    %%xmm0,  %%xmm0 \n\t"
            "vPAND  %%xmm1,    %%xmm12, %%xmm1 \n\t"
            "vPADDQ %%xmm0,    %%xmm15, %%xmm15\n\t"
            "vPADDQ %%xmm1,    %%xmm14, %%xmm14\n\t"
            "MOVDQU 0*8(%[x]), %%xmm0          \n\t"
            "MOVDQU 0*8(%[y]), %%xmm1          \n\t"
            "\n\t"
            "subq $-2*8, %[x]                  \n\t"
            "subq $-2*8, %[y]                  \n\t"
            "VPCMPGTQ %%xmm13, %%xmm1, %%xmm1  \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vPAND  %%xmm1,    %%xmm0,  %%xmm0 \n\t"
        "vPAND  %%xmm1,    %%xmm12, %%xmm1 \n\t"
        "vPADDQ %%xmm0,    %%xmm15, %%xmm15\n\t"
        "vPADDQ %%xmm1,    %%xmm14, %%xmm14\n\t"
        "\n\t"
        "MOVDQU %%xmm15, %%xmm0  \n\t"
        "PSRLDQ $1*8,    %%xmm0  \n\t"
        "PADDQ  %%xmm0 , %%xmm15 \n\t"
        "\n\t"
        "MOVDQU %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $1*8,    %%xmm1  \n\t"
        "PADDQ  %%xmm1 , %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0         \n\t"
                "MOVQ    0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm13,  %%xmm1, %%xmm1 \n\t"
                "vPAND  %%xmm1,    %%xmm0,  %%xmm0 \n\t"
                "vPAND  %%xmm1,    %%xmm12, %%xmm1 \n\t"
                "vPADDQ %%xmm0,    %%xmm15, %%xmm15\n\t"
                "vPADDQ %%xmm1,    %%xmm14, %%xmm14\n\t"
                "subq $-1*8, %[x]                  \n\t"
                "subq $-1*8, %[y]                  \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[s] \n\t"
        "MOVQ  %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left = tmp_sum;
    *count_left = tmp_cnt;

}

void count_and_sum_up_gt_loop_02_A_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=2, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13     \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm12     \n\t" // broadcast one for counter
        "\n\t"
        "movupd 0*8(%[x]),   %%xmm0      \n\t" // load x
        "movupd 0*8(%[y]),   %%xmm1      \n\t" // load y
        "VCMPGTPD  %%xmm13,    %%xmm1,    %%xmm1      \n\t" // x > v
        "subq $-2*8, %[x]                \n\t"
        "subq $-2*8, %[y]                \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD  %%xmm1, %%xmm0,  %%xmm0  \n\t"
            "vPAND   %%xmm1, %%xmm12, %%xmm2  \n\t"
            "addpd  %%xmm0,    %%xmm15 \n\t"
            "PADDQ  %%xmm2,    %%xmm14 \n\t"
            "movupd 0*8(%[x]), %%xmm0  \n\t"
            "movupd 0*8(%[y]), %%xmm1  \n\t"
            "\n\t"
            "subq $-2*8, %[x]          \n\t"
            "subq $-2*8, %[y]          \n\t"
            "VCMPGTPD  %%xmm13, %%xmm1, %%xmm1   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD %%xmm1, %%xmm0,  %%xmm0 \n\t"
        "vPAND  %%xmm1, %%xmm12, %%xmm2 \n\t"
        "addpd  %%xmm0, %%xmm15         \n\t"
        "PADDQ  %%xmm2, %%xmm14         \n\t"
        "\n\t"
        "movupd %%xmm15, %%xmm13 \n\t"
        "PSRLDQ $1*8,    %%xmm13 \n\t"
        "addpd  %%xmm13, %%xmm15 \n\t"
        "\n\t"
        "MOVDQU %%xmm14, %%xmm13 \n\t"
        "PSRLDQ $1*8,    %%xmm13 \n\t"
        "PADDQ  %%xmm13, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        __asm__ __volatile__ (
            "movsd   0*8(%[x]), %%xmm0  \n\t"
            "movsd   0*8(%[y]), %%xmm1  \n\t"
            "VCMPGTPD  %%xmm14,   %%xmm1,   %%xmm1  \n\t" // x > v
            "vANDPD  %%xmm1, %%xmm0,  %%xmm0  \n\t"
            "vPAND   %%xmm1, %%xmm12, %%xmm2  \n\t"
            "addpd   %%xmm0, %%xmm15 \n\t"
            "PADDQ   %%xmm2, %%xmm14 \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_04_A_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13        \n\t" // broadcast
        "VPBROADCASTQ  %[o], %%ymm12        \n\t" // zero clear
        "vMOVDQU 0*8(%[x]),  %%ymm0         \n\t" // load x
        "vMOVDQU 0*8(%[y]),  %%ymm1         \n\t" // load y
        "VPCMPGTQ %%ymm13,   %%ymm1, %%ymm1 \n\t"
        "subq $-4*8, %[x]                   \n\t"
        "subq $-4*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "\n\t"
            "vPAND  %%ymm1,    %%ymm0,  %%ymm0  \n\t"
            "vPAND  %%ymm1,    %%ymm12, %%ymm1  \n\t"
            "vPADDQ %%ymm0,    %%ymm15, %%ymm15 \n\t"
            "vPADDQ %%ymm1,    %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1          \n\t"
            "\n\t"
            "subq $-4*8, %[x]                   \n\t"
            "subq $-4*8, %[y]                   \n\t"
            "VPCMPGTQ %%ymm13, %%ymm1, %%ymm1   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vPAND  %%ymm1,    %%ymm0,  %%ymm0 \n\t"
        "vPAND  %%ymm1,    %%ymm12, %%ymm1 \n\t"
        "vPADDQ %%ymm0,    %%ymm15, %%ymm15\n\t"
        "vPADDQ %%ymm1,    %%ymm14, %%ymm14\n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm15        \n\t"
        "MOVDQU %%xmm15, %%xmm0  \n\t"
        "PSRLDQ $1*8,    %%xmm0  \n\t"
        "PADDQ  %%xmm0 , %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14        \n\t"
        "MOVDQU %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $1*8,    %%xmm1  \n\t"
        "PADDQ  %%xmm1 , %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0         \n\t"
                "MOVQ    0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm13,  %%xmm1, %%xmm1 \n\t"
                "vPAND  %%xmm1,    %%xmm0,  %%xmm0 \n\t"
                "vPAND  %%xmm1,    %%xmm12, %%xmm1 \n\t"
                "vPADDQ %%xmm0,    %%xmm15, %%xmm15\n\t"
                "vPADDQ %%xmm1,    %%xmm14, %%xmm14\n\t"
                "subq $-1*8, %[x]                  \n\t"
                "subq $-1*8, %[y]                  \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[s] \n\t"
        "MOVQ  %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_04_A_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=4, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm13     \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm12     \n\t" // broadcast one for counter
        "\n\t"
        "vmovupd 0*8(%[x]),   %%ymm0         \n\t" // load x
        "vmovupd 0*8(%[y]),   %%ymm1         \n\t" // load y
        "VCMPGTPD %%ymm13,    %%ymm1, %%ymm1 \n\t" // x > v
        "subq $-4*8, %[x]                   \n\t"
        "subq $-4*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vANDPD  %%ymm1, %%ymm12, %%ymm2  \n\t"
            "vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm2, %%ymm14, %%ymm14 \n\t"
            "vmovupd 0*8(%[x]), %%ymm0  \n\t"
            "vmovupd 0*8(%[y]), %%ymm1  \n\t"
            "\n\t"
            "subq $-4*8, %[x]          \n\t"
            "subq $-4*8, %[y]          \n\t"
            "VCMPGTPD %%ymm13, %%ymm1, %%ymm1   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vANDPD  %%ymm1, %%ymm12, %%ymm2  \n\t"
        "vaddpd  %%ymm0, %%ymm15, %%ymm15 \n\t"
        "vPADDQ  %%ymm2, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm15 \n\t"
        "movupd %%xmm15, %%xmm11 \n\t"
        "PSRLDQ $1*8,    %%xmm11 \n\t"
        "addpd  %%xmm11, %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14 \n\t"
        "MOVDQU %%xmm14, %%xmm10 \n\t"
        "PSRLDQ $1*8,    %%xmm10 \n\t"
        "PADDQ  %%xmm10, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "VCMPGTPD %%xmm13,   %%xmm1,   %%xmm1  \n\t" // x > v
                "vANDPD  %%xmm1, %%xmm0,  %%xmm0  \n\t"
                "vANDPD  %%xmm1, %%xmm12, %%xmm2  \n\t"
                "addpd   %%xmm0, %%xmm15 \n\t"
                "PADDQ   %%xmm2, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                "subq $-1*8, %[y] \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_08_A_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11        \n\t" // broadcast
        "VPBROADCASTQ  %[o], %%ymm10        \n\t" // zero clear
        "\n\t"
        "vMOVDQU 0*8(%[x]),  %%ymm0         \n\t" // load x
        "vMOVDQU 0*8(%[y]),  %%ymm1         \n\t" // load y
        "VPCMPGTQ %%ymm11,   %%ymm1, %%ymm1 \n\t"
        "\n\t"
        "vMOVDQU 4*8(%[x]),  %%ymm2         \n\t" // load x
        "vMOVDQU 4*8(%[y]),  %%ymm3         \n\t" // load y
        "VPCMPGTQ %%ymm11,   %%ymm3, %%ymm3 \n\t"
        "subq $-8*8, %[x]                   \n\t"
        "subq $-8*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "\n\t"
            "vPAND  %%ymm1,    %%ymm0,  %%ymm0  \n\t"
            "vPAND  %%ymm1,    %%ymm10, %%ymm1  \n\t"
            "vPADDQ %%ymm0,    %%ymm15, %%ymm15 \n\t"
            "vPADDQ %%ymm1,    %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1          \n\t"
            "\n\t"
            "vPAND  %%ymm3,    %%ymm2,  %%ymm2  \n\t"
            "vPAND  %%ymm3,    %%ymm10, %%ymm3  \n\t"
            "vPADDQ %%ymm2,    %%ymm13, %%ymm13 \n\t"
            "vPADDQ %%ymm3,    %%ymm12, %%ymm12 \n\t"
            "vMOVDQU 4*8(%[x]), %%ymm2          \n\t"
            "vMOVDQU 4*8(%[y]), %%ymm3          \n\t"
            "\n\t"
            "subq $-8*8, %[x]                   \n\t"
            "subq $-8*8, %[y]                   \n\t"
            "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1   \n\t"
            "VPCMPGTQ %%ymm11, %%ymm3, %%ymm3   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vPAND  %%ymm1,    %%ymm0,  %%ymm0 \n\t"
        "vPAND  %%ymm1,    %%ymm10, %%ymm1 \n\t"
        "vPADDQ %%ymm0,    %%ymm15, %%ymm15\n\t"
        "vPADDQ %%ymm1,    %%ymm14, %%ymm14\n\t"
        "\n\t"
        "vPAND  %%ymm3,    %%ymm2,  %%ymm2  \n\t"
        "vPAND  %%ymm3,    %%ymm10, %%ymm3  \n\t"
        "vPADDQ %%ymm2,    %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm3,    %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vPADDQ %%ymm13, %%ymm15, %%ymm15   \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14   \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm15        \n\t"
        "MOVDQU %%xmm15, %%xmm0  \n\t"
        "PSRLDQ $1*8,    %%xmm0  \n\t"
        "PADDQ  %%xmm0 , %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0,  %%xmm1, %%xmm14        \n\t"
        "MOVDQU %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $1*8,    %%xmm1  \n\t"
        "PADDQ  %%xmm1 , %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0         \n\t"
                "MOVQ    0*8(%[y]), %%xmm1         \n\t"
                "VPCMPGTQ %%xmm11,  %%xmm1, %%xmm1 \n\t"
                "vPAND  %%xmm1,    %%xmm0,  %%xmm0 \n\t"
                "vPAND  %%xmm1,    %%xmm10, %%xmm1 \n\t"
                "vPADDQ %%xmm0,    %%xmm15, %%xmm15\n\t"
                "vPADDQ %%xmm1,    %%xmm14, %%xmm14\n\t"
                "subq $-1*8, %[x]                  \n\t"
                "subq $-1*8, %[y]                  \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[s] \n\t"
        "MOVQ  %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_08_A_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=8, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t" // sum, zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11     \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm10     \n\t" // broadcast one for counter
        "\n\t"
        "vmovupd 0*8(%[x]),   %%ymm0         \n\t" // load x
        "vmovupd 0*8(%[y]),   %%ymm1         \n\t" // load y
        "VCMPGTPD %%ymm11,    %%ymm1, %%ymm1 \n\t" // x > v
        "\n\t"
        "vmovupd 4*8(%[x]),   %%ymm2         \n\t" // load x
        "vmovupd 4*8(%[y]),   %%ymm3         \n\t" // load y
        "VCMPGTPD %%ymm11,    %%ymm3, %%ymm3 \n\t" // x > v
        "subq $-8*8, %[x]                   \n\t"
        "subq $-8*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vANDPD  %%ymm1, %%ymm10, %%ymm1  \n\t"
            "vaddpd  %%ymm0, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm1, %%ymm12, %%ymm12 \n\t"
            "vmovupd 0*8(%[x]), %%ymm0  \n\t"
            "vmovupd 0*8(%[y]), %%ymm1  \n\t"
            "\n\t"
            "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vANDPD  %%ymm3, %%ymm10, %%ymm3  \n\t"
            "vaddpd  %%ymm2, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm3, %%ymm14, %%ymm14 \n\t"
            "vmovupd 4*8(%[x]), %%ymm2  \n\t"
            "vmovupd 4*8(%[y]), %%ymm3  \n\t"
            "\n\t"
            "subq $-8*8, %[x]          \n\t"
            "subq $-8*8, %[y]          \n\t"
            "VCMPGTPD %%ymm11, %%ymm1, %%ymm1   \n\t"
            "VCMPGTPD %%ymm11, %%ymm3, %%ymm3   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vANDPD  %%ymm1, %%ymm10, %%ymm1  \n\t"
        "vaddpd  %%ymm0, %%ymm13, %%ymm13 \n\t"
        "vPADDQ  %%ymm1, %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
        "vANDPD  %%ymm3, %%ymm10, %%ymm3  \n\t"
        "vaddpd  %%ymm2, %%ymm15, %%ymm15 \n\t"
        "vPADDQ  %%ymm3, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm15 \n\t"
        "movupd %%xmm15, %%xmm9 \n\t"
        "PSRLDQ $1*8,    %%xmm9 \n\t"
        "addpd  %%xmm9, %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14 \n\t"
        "MOVDQU %%xmm14, %%xmm8 \n\t"
        "PSRLDQ $1*8,    %%xmm8 \n\t"
        "PADDQ  %%xmm8, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "VCMPGTPD %%xmm11,   %%xmm1,   %%xmm1  \n\t" // x > v
                "vANDPD  %%xmm1, %%xmm0,  %%xmm0  \n\t"
                "vANDPD  %%xmm1, %%xmm10, %%xmm2  \n\t"
                "addpd   %%xmm0, %%xmm15 \n\t"
                "PADDQ   %%xmm2, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                "subq $-1*8, %[y] \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}


// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_16_A_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15  \n\t" // zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14  \n\t" // zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13  \n\t" // zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12  \n\t" // zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm11        \n\t" // broadcast
        "VPBROADCASTQ  %[o], %%ymm10        \n\t" // zero clear
        "VPBROADCASTQ  %[v], %%ymm9        \n\t" // broadcast
        "VPBROADCASTQ  %[o], %%ymm8        \n\t" // zero clear
        "\n\t"
        "vMOVDQU 0*8(%[x]),  %%ymm0         \n\t" // load x
        "vMOVDQU 0*8(%[y]),  %%ymm1         \n\t" // load y
        "VPCMPGTQ %%ymm11,   %%ymm1, %%ymm1 \n\t"
        "\n\t"
        "vMOVDQU 4*8(%[x]),  %%ymm2         \n\t" // load x
        "vMOVDQU 4*8(%[y]),  %%ymm3         \n\t" // load y
        "VPCMPGTQ %%ymm9,    %%ymm3, %%ymm3 \n\t"
        "\n\t"
        "vMOVDQU 8*8(%[x]),  %%ymm4         \n\t" // load x
        "vMOVDQU 8*8(%[y]),  %%ymm5         \n\t" // load y
        "VPCMPGTQ %%ymm11,   %%ymm5, %%ymm5 \n\t"
        "\n\t"
        "vMOVDQU 12*8(%[x]), %%ymm6         \n\t" // load x
        "vMOVDQU 12*8(%[y]), %%ymm7         \n\t" // load y
        "VPCMPGTQ %%ymm9,    %%ymm7, %%ymm7 \n\t"
        "subq $-16*8, %[x]                   \n\t"
        "subq $-16*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "\n\t"
            "vPAND  %%ymm1,    %%ymm0,  %%ymm0  \n\t"
            "vPAND  %%ymm1,    %%ymm10, %%ymm1  \n\t"
            "vPADDQ %%ymm0,    %%ymm15, %%ymm15 \n\t"
            "vPADDQ %%ymm1,    %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 0*8(%[x]), %%ymm0          \n\t"
            "vMOVDQU 0*8(%[y]), %%ymm1          \n\t"
            "\n\t"
            "vPAND  %%ymm3,    %%ymm2,  %%ymm2  \n\t"
            "vPAND  %%ymm3,    %%ymm8,  %%ymm3  \n\t"
            "vPADDQ %%ymm2,    %%ymm13, %%ymm13 \n\t"
            "vPADDQ %%ymm3,    %%ymm12, %%ymm12 \n\t"
            "vMOVDQU 4*8(%[x]), %%ymm2          \n\t"
            "vMOVDQU 4*8(%[y]), %%ymm3          \n\t"
            "\n\t"
            "vPAND  %%ymm5,    %%ymm4,  %%ymm4  \n\t"
            "vPAND  %%ymm5,    %%ymm10, %%ymm5  \n\t"
            "vPADDQ %%ymm4,    %%ymm15, %%ymm15 \n\t"
            "vPADDQ %%ymm5,    %%ymm14, %%ymm14 \n\t"
            "vMOVDQU 8*8(%[x]), %%ymm4          \n\t"
            "vMOVDQU 8*8(%[y]), %%ymm5          \n\t"
            "\n\t"
            "vPAND  %%ymm7,    %%ymm6,  %%ymm6  \n\t"
            "vPAND  %%ymm7,    %%ymm8,  %%ymm7  \n\t"
            "vPADDQ %%ymm6,    %%ymm13, %%ymm13 \n\t"
            "vPADDQ %%ymm7,    %%ymm12, %%ymm12 \n\t"
            "vMOVDQU 12*8(%[x]), %%ymm6         \n\t"
            "vMOVDQU 12*8(%[y]), %%ymm7         \n\t"
            "\n\t"
            "subq $-16*8, %[x]                  \n\t"
            "subq $-16*8, %[y]                  \n\t"
            "VPCMPGTQ %%ymm11, %%ymm1, %%ymm1   \n\t"
            "VPCMPGTQ %%ymm9,  %%ymm3, %%ymm3   \n\t"
            "VPCMPGTQ %%ymm11, %%ymm5, %%ymm5   \n\t"
            "VPCMPGTQ %%ymm9,  %%ymm7, %%ymm7   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vPAND  %%ymm1,  %%ymm0,  %%ymm0  \n\t"
        "vPAND  %%ymm1,  %%ymm10, %%ymm1  \n\t"
        "vPADDQ %%ymm0,  %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm1,  %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vPAND  %%ymm3,  %%ymm2,  %%ymm2  \n\t"
        "vPAND  %%ymm3,  %%ymm8,  %%ymm3  \n\t"
        "vPADDQ %%ymm2,  %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm3,  %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vPAND  %%ymm5,  %%ymm4,  %%ymm4  \n\t"
        "vPAND  %%ymm5,  %%ymm10, %%ymm5  \n\t"
        "vPADDQ %%ymm4,  %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm5,  %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vPAND  %%ymm7,  %%ymm6,  %%ymm6  \n\t"
        "vPAND  %%ymm7,  %%ymm8,  %%ymm7  \n\t"
        "vPADDQ %%ymm6,  %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm7,  %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vPADDQ %%ymm13, %%ymm15, %%ymm15   \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14   \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm15        \n\t"
        "MOVDQU %%xmm15, %%xmm0  \n\t"
        "PSRLDQ $1*8,    %%xmm0  \n\t"
        "PADDQ  %%xmm0 , %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0,  %%xmm1, %%xmm14       \n\t"
        "MOVDQU %%xmm14, %%xmm1  \n\t"
        "PSRLDQ $1*8,    %%xmm1  \n\t"
        "PADDQ  %%xmm1 , %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0           \n\t"
                "MOVQ    0*8(%[y]), %%xmm1           \n\t"
                "VPCMPGTQ %%xmm11,  %%xmm1,  %%xmm1  \n\t"
                "vPAND  %%xmm1,     %%xmm0,  %%xmm0  \n\t"
                "vPAND  %%xmm1,     %%xmm10, %%xmm1  \n\t"
                "vPADDQ %%xmm0,     %%xmm15, %%xmm15 \n\t"
                "vPADDQ %%xmm1,     %%xmm14, %%xmm14 \n\t"
                "subq $-1*8, %[x]                    \n\t"
                "subq $-1*8, %[y]                    \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ  %%xmm15, %[s] \n\t"
        "MOVQ  %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_16_A_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=16, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t" // sum, zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t" // counter, zero clear
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t" // sum, zero clear
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm9     \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm8     \n\t" // broadcast one for counter
        "\n\t"
        "vmovupd  0*8(%[x]),  %%ymm0         \n\t" // load x
        "vmovupd  0*8(%[y]),  %%ymm1         \n\t" // load y
        "VCMPGTPD  %%ymm9,     %%ymm1, %%ymm1 \n\t" // x > v
        "\n\t"
        "vmovupd  4*8(%[x]),  %%ymm2         \n\t" // load x
        "vmovupd  4*8(%[y]),  %%ymm3         \n\t" // load y
        "VCMPGTPD  %%ymm9,     %%ymm3, %%ymm3 \n\t" // x > v
        "\n\t"
        "vmovupd  8*8(%[x]),  %%ymm4         \n\t" // load x
        "vmovupd  8*8(%[y]),  %%ymm5         \n\t" // load y
        "VCMPGTPD  %%ymm9,     %%ymm5, %%ymm5 \n\t" // x > v
        "\n\t"
        "vmovupd 12*8(%[x]),  %%ymm6         \n\t" // load x
        "vmovupd 12*8(%[y]),  %%ymm7         \n\t" // load y
        "VCMPGTPD  %%ymm9,     %%ymm7, %%ymm7 \n\t" // x > v
        "subq $-16*8, %[x]                   \n\t"
        "subq $-16*8, %[y]                   \n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    n_unroll--;
    while(n_unroll--){
        __asm__ __volatile__(
            "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
            "vaddpd  %%ymm0, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
            "vmovupd  0*8(%[x]), %%ymm0  \n\t"
            "vmovupd  0*8(%[y]), %%ymm1  \n\t"
            "\n\t"
            "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
            "vmovupd  4*8(%[x]), %%ymm2  \n\t"
            "vmovupd  4*8(%[y]), %%ymm3  \n\t"
            "\n\t"
            "vANDPD  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
            "vaddpd  %%ymm4, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
            "vmovupd  8*8(%[x]), %%ymm4  \n\t"
            "vmovupd  8*8(%[y]), %%ymm5  \n\t"
            "\n\t"
            "vANDPD  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
            "vaddpd  %%ymm6, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
            "vmovupd 12*8(%[x]), %%ymm6  \n\t"
            "vmovupd 12*8(%[y]), %%ymm7  \n\t"
            "\n\t"
            "subq $-16*8, %[x]          \n\t"
            "subq $-16*8, %[y]          \n\t"
            "VCMPGTPD  %%ymm9, %%ymm1, %%ymm1   \n\t"
            "VCMPGTPD  %%ymm9, %%ymm3, %%ymm3   \n\t"
            "VCMPGTPD  %%ymm9, %%ymm5, %%ymm5   \n\t"
            "VCMPGTPD  %%ymm9, %%ymm7, %%ymm7   \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
        "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
        "vaddpd  %%ymm0, %%ymm11, %%ymm11 \n\t"
        "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
        "\n\t"
        "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
        "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
        "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
        "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
        "\n\t"
        "vANDPD  %%ymm5, %%ymm4,  %%ymm4  \n\t"
        "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
        "vaddpd  %%ymm4, %%ymm15, %%ymm15 \n\t"
        "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "vANDPD  %%ymm7, %%ymm6,  %%ymm6  \n\t"
        "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
        "vaddpd  %%ymm6, %%ymm11, %%ymm11 \n\t"
        "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
        "\n\t"
        "vaddpd %%ymm11, %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm10, %%ymm12, %%ymm12 \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm15 \n\t"
        "movupd %%xmm15, %%xmm0 \n\t"
        "PSRLDQ $1*8,    %%xmm0 \n\t"
        "addpd  %%xmm0, %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14 \n\t"
        "MOVDQU %%xmm14, %%xmm1 \n\t"
        "PSRLDQ $1*8,    %%xmm1 \n\t"
        "PADDQ  %%xmm1, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "VCMPGTPD  %%xmm9,    %%xmm1,    %%xmm1  \n\t" // x > v
                "vANDPD  %%xmm1, %%xmm0, %%xmm0  \n\t"
                "vANDPD  %%xmm1, %%xmm8, %%xmm1  \n\t"
                "addpd   %%xmm0, %%xmm15 \n\t"
                "PADDQ   %%xmm1, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                "subq $-1*8, %[y] \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
// --------------------------------------------------------------------------------------
void count_and_sum_up_gt_loop_32_A_i8(int64_t *sum_left, int64_t *count_left, int64_t x[], int64_t y[], int64_t v, int64_t n){
    int64_t tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_i8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t" // sum, zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t" // counter, zero clear
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t" // sum, zero clear
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm9      \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm8      \n\t" // broadcast one for counter
        "\n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    while(n_unroll--){
        __asm__ __volatile__(
            "vMOVDQU  0*8(%[x]), %%ymm0       \n\t"
            "vMOVDQU  0*8(%[y]), %%ymm1       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm1, %%ymm1  \n\t"
            "vPAND   %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
            "vPADDQ  %%ymm0, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vMOVDQU  4*8(%[x]), %%ymm2       \n\t"
            "vMOVDQU  4*8(%[y]), %%ymm3       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm3, %%ymm3  \n\t"
            "vPAND   %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
            "vPADDQ  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vMOVDQU  8*8(%[x]), %%ymm4       \n\t"
            "vMOVDQU  8*8(%[y]), %%ymm5       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm5, %%ymm5  \n\t"
            "vPAND   %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
            "vPADDQ  %%ymm4, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vMOVDQU 12*8(%[x]), %%ymm6       \n\t"
            "vMOVDQU 12*8(%[y]), %%ymm7       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm7, %%ymm7  \n\t"
            "vPAND   %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
            "vPADDQ  %%ymm6, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vMOVDQU 16*8(%[x]), %%ymm0       \n\t"
            "vMOVDQU 16*8(%[y]), %%ymm1       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm1, %%ymm1  \n\t"
            "vPAND   %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
            "vPADDQ  %%ymm0, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vMOVDQU 20*8(%[x]), %%ymm2       \n\t"
            "vMOVDQU 20*8(%[y]), %%ymm3       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm3, %%ymm3  \n\t"
            "vPAND   %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
            "vPADDQ  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vMOVDQU 24*8(%[x]), %%ymm4       \n\t"
            "vMOVDQU 24*8(%[y]), %%ymm5       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm5, %%ymm5  \n\t"
            "vPAND   %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
            "vPADDQ  %%ymm4, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vMOVDQU 28*8(%[x]), %%ymm6       \n\t"
            "vMOVDQU 28*8(%[y]), %%ymm7       \n\t"
            "VPCMPGTQ %%ymm9, %%ymm7, %%ymm7  \n\t"
            "vPAND   %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
            "vPADDQ  %%ymm6, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "subq $-32*8, %[x]                \n\t"
            "subq $-32*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vPADDQ %%ymm11, %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm10, %%ymm12, %%ymm12 \n\t"
        "vPADDQ %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm15 \n\t"
        "MOVDQU %%xmm15, %%xmm0 \n\t"
        "PSRLDQ $1*8,    %%xmm0 \n\t"
        "PADDQ  %%xmm0, %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14 \n\t"
        "MOVDQU %%xmm14, %%xmm1 \n\t"
        "PSRLDQ $1*8,    %%xmm1 \n\t"
        "PADDQ  %%xmm1, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "MOVQ    0*8(%[x]), %%xmm0  \n\t"
                "MOVQ    0*8(%[y]), %%xmm1  \n\t"
                "VPCMPGTQ %%xmm9, %%xmm1, %%xmm1  \n\t" // x > v
                "vANDPD   %%xmm1, %%xmm0, %%xmm0  \n\t"
                "vANDPD   %%xmm1, %%xmm8, %%xmm1  \n\t"
                "PADDQ    %%xmm0, %%xmm15 \n\t"
                "PADDQ    %%xmm1, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                "subq $-1*8, %[y] \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "MOVQ   %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}

void count_and_sum_up_gt_loop_32_A_r8(double *sum_left, int64_t *count_left, double x[], double y[], double v, int64_t n){
    double tmp_sum;
    int64_t tmp_cnt;

    int64_t o=1;
    int64_t unroll_size=32, n_unroll=n/unroll_size, n_remain=n%unroll_size;
    if(n<unroll_size){
        count_and_sum_up_gt_loop_C_r8(sum_left, count_left, x, y, v, n);
        return;
    }

    __asm__ __volatile__ (
        "vpxor %%ymm15, %%ymm15, %%ymm15 \n\t" // sum, zero clear
        "vpxor %%ymm14, %%ymm14, %%ymm14 \n\t" // counter, zero clear
        "vpxor %%ymm13, %%ymm13, %%ymm13 \n\t" // sum, zero clear
        "vpxor %%ymm12, %%ymm12, %%ymm12 \n\t" // counter, zero clear
        "vpxor %%ymm11, %%ymm11, %%ymm11 \n\t" // sum, zero clear
        "vpxor %%ymm10, %%ymm10, %%ymm10 \n\t" // counter, zero clear
        "\n\t"
        "VPBROADCASTQ  %[v], %%ymm9      \n\t" // broadcast threshold
        "VPBROADCASTQ  %[o], %%ymm8      \n\t" // broadcast one for counter
        "\n\t"
        :[x]"=r"(x), [y]"=r"(y), [v]"=r"(v), [o]"=r"(o)
        :"0"(x), "1"(y), "2"(v), "3"(o)
    );

    while(n_unroll--){
        __asm__ __volatile__(
            "vmovupd  0*8(%[x]), %%ymm0       \n\t"
            "vmovupd  0*8(%[y]), %%ymm1       \n\t"
            "VCMPGTPD %%ymm9, %%ymm1, %%ymm1  \n\t"
            "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
            "vaddpd  %%ymm0, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vmovupd  4*8(%[x]), %%ymm2       \n\t"
            "vmovupd  4*8(%[y]), %%ymm3       \n\t"
            "VCMPGTPD %%ymm9, %%ymm3, %%ymm3  \n\t"
            "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vmovupd  8*8(%[x]), %%ymm4       \n\t"
            "vmovupd  8*8(%[y]), %%ymm5       \n\t"
            "VCMPGTPD %%ymm9, %%ymm5, %%ymm5  \n\t"
            "vANDPD  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
            "vaddpd  %%ymm4, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmovupd 12*8(%[x]), %%ymm6       \n\t"
            "vmovupd 12*8(%[y]), %%ymm7       \n\t"
            "VCMPGTPD %%ymm9, %%ymm7, %%ymm7  \n\t"
            "vANDPD  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
            "vaddpd  %%ymm6, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vmovupd 16*8(%[x]), %%ymm0       \n\t"
            "vmovupd 16*8(%[y]), %%ymm1       \n\t"
            "VCMPGTPD %%ymm9, %%ymm1, %%ymm1  \n\t"
            "vANDPD  %%ymm1, %%ymm0,  %%ymm0  \n\t"
            "vPAND   %%ymm1, %%ymm8,  %%ymm1  \n\t"
            "vaddpd  %%ymm0, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm1, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "vmovupd 20*8(%[x]), %%ymm2       \n\t"
            "vmovupd 20*8(%[y]), %%ymm3       \n\t"
            "VCMPGTPD %%ymm9, %%ymm3, %%ymm3  \n\t"
            "vANDPD  %%ymm3, %%ymm2,  %%ymm2  \n\t"
            "vPAND   %%ymm3, %%ymm8,  %%ymm3  \n\t"
            "vaddpd  %%ymm2, %%ymm13, %%ymm13 \n\t"
            "vPADDQ  %%ymm3, %%ymm12, %%ymm12 \n\t"
            "\n\t"
            "vmovupd 24*8(%[x]), %%ymm4       \n\t"
            "vmovupd 24*8(%[y]), %%ymm5       \n\t"
            "VCMPGTPD %%ymm9, %%ymm5, %%ymm5  \n\t"
            "vANDPD  %%ymm5, %%ymm4,  %%ymm4  \n\t"
            "vPAND   %%ymm5, %%ymm8,  %%ymm5  \n\t"
            "vaddpd  %%ymm4, %%ymm15, %%ymm15 \n\t"
            "vPADDQ  %%ymm5, %%ymm14, %%ymm14 \n\t"
            "\n\t"
            "vmovupd 28*8(%[x]), %%ymm6       \n\t"
            "vmovupd 28*8(%[y]), %%ymm7       \n\t"
            "VCMPGTPD %%ymm9, %%ymm7, %%ymm7  \n\t"
            "vANDPD  %%ymm7, %%ymm6,  %%ymm6  \n\t"
            "vPAND   %%ymm7, %%ymm8,  %%ymm7  \n\t"
            "vaddpd  %%ymm6, %%ymm11, %%ymm11 \n\t"
            "vPADDQ  %%ymm7, %%ymm10, %%ymm10 \n\t"
            "\n\t"
            "subq $-32*8, %[x]                \n\t"
            "subq $-32*8, %[y]                \n\t"
            :[x]"=r"(x), [y]"=r"(y)
            :"0"(x), "1"(y)
        );
    }

    __asm__ __volatile__(
        "vaddpd %%ymm11, %%ymm13, %%ymm13 \n\t"
        "vPADDQ %%ymm10, %%ymm12, %%ymm12 \n\t"
        "vaddpd %%ymm13, %%ymm15, %%ymm15 \n\t"
        "vPADDQ %%ymm12, %%ymm14, %%ymm14 \n\t"
        "\n\t"
        "VEXTRACTF64X2  $0,  %%ymm15, %%xmm0   \n\t"
        "VEXTRACTF64X2  $1,  %%ymm15, %%xmm1   \n\t"
        "vaddpd %%xmm0, %%xmm1, %%xmm15 \n\t"
        "movupd %%xmm15, %%xmm0 \n\t"
        "PSRLDQ $1*8,    %%xmm0 \n\t"
        "addpd  %%xmm0, %%xmm15 \n\t"
        "\n\t"
        "VEXTRACTI64X2  $0,  %%ymm14, %%xmm0   \n\t"
        "VEXTRACTI64X2  $1,  %%ymm14, %%xmm1   \n\t"
        "vPADDQ %%xmm0, %%xmm1, %%xmm14 \n\t"
        "MOVDQU %%xmm14, %%xmm1 \n\t"
        "PSRLDQ $1*8,    %%xmm1 \n\t"
        "PADDQ  %%xmm1, %%xmm14 \n\t"
        ::
    );

    if (n_remain>0){
        while(n_remain--){
            __asm__ __volatile__ (
                "movsd   0*8(%[x]), %%xmm0  \n\t"
                "movsd   0*8(%[y]), %%xmm1  \n\t"
                "VCMPGTPD %%xmm9,    %%xmm1,    %%xmm1  \n\t" // x > v
                "vANDPD  %%xmm1, %%xmm0, %%xmm0  \n\t"
                "vANDPD  %%xmm1, %%xmm8, %%xmm1  \n\t"
                "addpd   %%xmm0, %%xmm15 \n\t"
                "PADDQ   %%xmm1, %%xmm14 \n\t"
                "subq $-1*8, %[x] \n\t"
                "subq $-1*8, %[y] \n\t"
                :[x]"=r"(x), [y]"=r"(y)
                :"0"(x), "1"(y)
            );
        }
    }

	__asm__ __volatile__(
        "movsd  %%xmm15, %[s] \n\t"
        "MOVQ   %%xmm14, %[c] \n\t"
        :[s]"=m"(tmp_sum), [c]"=m"(tmp_cnt)
        :
	);

    *sum_left   = tmp_sum;
    *count_left = tmp_cnt;
}
