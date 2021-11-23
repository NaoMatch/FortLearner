#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>

#define MYBLAS_PANEL_M  256
#define MYBLAS_PANEL_N  256
#define MYBLAS_PANEL_K  256

#define MYBLAS_BLOCK_M  128
#define MYBLAS_BLOCK_N   64
#define MYBLAS_BLOCK_K  128

#define MYBLAS_TILE_M    32
#define MYBLAS_TILE_N    32
#define MYBLAS_TILE_K    32

#define MIN(x,y) (x<y?x:y)

typedef struct {
    size_t i2;
    size_t k2;
    size_t j2;
    size_t lda;
    size_t ldb;
    size_t ldc;
    size_t lda2;
    size_t ldb2;
    size_t ldc2;
    size_t M2;
    size_t K2;
    size_t N2;
    size_t M;
    size_t K;
    size_t N;
} info_copy;

void a2_l2_cache_copy_005(double* A, double* A2, info_copy* info){

    size_t lda = info->lda;
    size_t k2 = info->k2;
    size_t i2 = info->i2;
    size_t M2 = info->M2;
    size_t K2 = info->K2;
    size_t M = info->M;
    size_t K = info->K;

    A = A + lda*k2 + i2;
    for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
    size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
        for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
        size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
            for( size_t i =i1; i < i1+M1; i++ ){
            for( size_t k =k1; k < k1+K1; k++ ){
                (*A2) = (*A);
                A += lda ;
                A2++;
            }
            A  = A  - lda *K1 + 1;  
            }
        // move to next tile head, row
        }
    A = A  - M2 + lda *K1;  // move to next tile head, column
    }
    A2= A2 - M2*K2; // return to head 
    A = A  - lda*K2; // return to block head 
    A = A  - lda*k2 - i2; // return to head 
}

void b2_l2_cache_copy_005(double* B, double* B2, info_copy* info){
    size_t ldb = info->ldb;
    size_t ldb2 = info->ldb2;
    size_t k2 = info->k2;
    size_t j2 = info->j2;
    size_t N2 = info->N2;
    size_t K2 = info->K2;
    size_t N = info->N;
    size_t K = info->K;

    B = B + ldb*j2 + k2;
    for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
        size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
        for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
            for( size_t j =j1; j < j1+N1; j++ ){
                for( size_t k =k1; k < k1+K1; k++ ){
                    (*B2)=(*B);
                    B++;
                    B2++;
                }
                B  = B  - K1 + ldb ; // move to next column
                B2 = B2 - K1 + ldb2;    
            }
            B  = B  - ldb *N1 + K1; // next tile head, row
            B2 = B2 - ldb2*N1 + K1; 
        }
        B  = B  - K2 + ldb *N1; // next tile head, column
        B2 = B2 - K2 + ldb2*N1; 
        }
    B  = B  - ldb *N2;
    B2 = B2 - ldb2*N2;
    B  = B  - ldb*j2 - k2; // return to head 
}

void c_l1_calc_005(double* C, double* A2, double* B2, info_copy* info){

    size_t ldc = info->ldc;
    size_t ldb2 = info->ldb2;
    size_t i2 = info->i2;
    size_t k2 = info->k2;
    size_t j2 = info->j2;
    size_t M2 = info->M2;
    size_t K2 = info->K2;
    size_t N2 = info->N2;
    size_t M = info->M;
    size_t K = info->K;
    size_t N = info->N;
    double AB;

    for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
    for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
        B2 = B2 + ldb2*(j1-j2) + (k1-k2); // move to tile head
    for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){

        size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
        size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
        size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);

        C  = C  + ldc*j1 + i1;            // move to tile head
        A2 = A2 + M2*(k1-k2) + K1*(i1-i2); // move to tile head
        for( size_t j =j1; j < j1+N1; j++ ){
            for( size_t i =i1; i < i1+M1; i++ ){
                AB=0e0;
                for( size_t k =k1; k < k1+K1; k++ ){
                    AB = AB + (*A2)*(*B2);
                    A2++;
                    B2++;
                }
                *C = (*C) + AB;
                B2 = B2 - K1; // return to column head
                C++;
            }
            A2= A2- M1*K1; // return to head
            B2= B2+ ldb2; // move to next column
            C = C - M1 + ldc; // return to head
        }
        B2 = B2 - ldb2*N1; // return to head

        C = C - ldc*N1; // return to tile head
        C = C - ldc*j1 - i1; // return to head

        A2 = A2 - M2*(k1-k2) - K1*(i1-i2); // return to head
    }
    B2 = B2 - ldb2*(j1-j2) - (k1-k2); // return to head
    }}    
}

void a2_l2_cache_copy_006(double* A, double* A2, info_copy* info){

    size_t lda = info->lda;
    size_t k2 = info->k2;
    size_t i2 = info->i2;
    size_t M2 = info->M2;
    size_t K2 = info->K2;
    size_t M = info->M;
    size_t K = info->K;

    A = A + lda*k2 + i2;
    for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
    size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
        for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
        size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
            for( size_t i =i1; i < i1+M1; i++ ){
            for( size_t k =k1; k < k1+K1; k++ ){
                (*A2) = (*A);
                A += lda ;
                A2++;
            }
            A  = A  - lda *K1 + 1;  
            }
        // move to next tile head, row
        }
    A = A  - M2 + lda *K1;  // move to next tile head, column
    }
    A2= A2 - M2*K2; // return to head 
    A = A  - lda*K2; // return to block head 
    A = A  - lda*k2 - i2; // return to head 
}

void b2_l2_cache_copy_006(double* B, double* B2, info_copy* info){
    size_t ldb = info->ldb;
    size_t ldb2 = info->ldb2;
    size_t k2 = info->k2;
    size_t j2 = info->j2;
    size_t N2 = info->N2;
    size_t K2 = info->K2;
    size_t N = info->N;
    size_t K = info->K;

    B = B + ldb*j2 + k2;
    for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
        size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
        for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
            for( size_t j =j1; j < j1+N1; j++ ){
                for( size_t k =k1; k < k1+K1; k++ ){
                    (*B2)=(*B);
                    B++;
                    B2++;
                }
                B  = B  - K1 + ldb ; // move to next column
                B2 = B2 - K1 + ldb2;    
            }
            B  = B  - ldb *N1 + K1; // next tile head, row
            B2 = B2 - ldb2*N1 + K1; 
        }
        B  = B  - K2 + ldb *N1; // next tile head, column
        B2 = B2 - K2 + ldb2*N1; 
        }
    B  = B  - ldb *N2;
    B2 = B2 - ldb2*N2;
    B  = B  - ldb*j2 - k2; // return to head 
}

void c_l1_calc_006(double* C, double* A2, double* B2, info_copy* info){

    size_t ldc = info->ldc;
    size_t ldb2 = info->ldb2;
    size_t i2 = info->i2;
    size_t k2 = info->k2;
    size_t j2 = info->j2;
    size_t M2 = info->M2;
    size_t K2 = info->K2;
    size_t N2 = info->N2;
    size_t M = info->M;
    size_t K = info->K;
    size_t N = info->N;
    double AB;

    for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
    for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
        B2 = B2 + ldb2*(j1-j2) + (k1-k2); // move to tile head
    for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){

        size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
        size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
        size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);

        C  = C  + ldc*j1 + i1;            // move to tile head
        A2 = A2 + M2*(k1-k2) + K1*(i1-i2); // move to tile head
        for( size_t j =j1; j < j1+N1; j++ ){
            for( size_t i =i1; i < i1+M1; i++ ){
                AB=0e0;
                for( size_t k =k1; k < k1+K1; k++ ){
                    AB = AB + (*A2)*(*B2);
                    A2++;
                    B2++;
                }
                *C = (*C) + AB;
                B2 = B2 - K1; // return to column head
                C++;
            }
            A2= A2- M1*K1; // return to head
            B2= B2+ ldb2; // move to next column
            C = C - M1 + ldc; // return to head
        }
        B2 = B2 - ldb2*N1; // return to head

        C = C - ldc*N1; // return to tile head
        C = C - ldc*j1 - i1; // return to head

        A2 = A2 - M2*(k1-k2) - K1*(i1-i2); // return to head
    }
    B2 = B2 - ldb2*(j1-j2) - (k1-k2); // return to head
    }}    
}

void c_l1_calc_008(double* C, double* A2, double* B2, info_copy* info){

    size_t ldc = info->ldc;
    size_t ldb2 = info->ldb2;
    size_t i2 = info->i2;
    size_t k2 = info->k2;
    size_t j2 = info->j2;
    size_t M2 = info->M2;
    size_t K2 = info->K2;
    size_t N2 = info->N2;
    size_t M = info->M;
    size_t K = info->K;
    size_t N = info->N;
    double AB;
    double* c_buf = calloc(16, sizeof(double));
    size_t len_buf=16;

    for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
    for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
        B2 = B2 + ldb2*(j1-j2) + (k1-k2); // move to tile head
    for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){

        size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
        size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
        size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);

        C  = C  + ldc*j1 + i1;            // move to tile head
        A2 = A2 + M2*(k1-k2) + K1*(i1-i2); // move to tile head

        size_t j=N1;
        if(j>>2){
            size_t jmod4=j>>2;
            while(jmod4--){

                size_t i=M1;
                if(i>>2){
                    size_t imod4=i>>2;
                    while(imod4--){

                        for(size_t len=0; len<len_buf; len++) c_buf[len]=0e0;

                        size_t k=K1;
                        size_t cnt_a=0;
                        size_t cnt_b=0;
                        if (k>>3){
                            size_t kmod8=k>>3;
                            while(kmod8--){
                                for(size_t jj=0; jj<4; jj++){
                                    for(size_t ii=0; ii<4; ii++){   
                                        for(size_t kk=0; kk<8; kk++){
                                            c_buf[ii+4*jj] = c_buf[ii+4*jj] + (*(A2+kk+K1*ii))*(*(B2+kk+K1*jj));
                                        }
                                    }
                                }
                                A2+=8;
                                B2+=8;
                            }
                        }
                        // printf("hoge \n");

                        A2 = A2 - K1;
                        B2 = B2 - K1; // return to column head
                        A2 = A2 + 4*K1;

                        for(size_t jj=0; jj<4; jj++){
                            for(size_t ii=0; ii<4; ii++){
                                *C = (*C) + c_buf[ii+4*jj];
                                C=C+1;
                            }
                            C=C-4;
                            C=C+ldc;
                        }
                    }
                }
                A2= A2- M1*K1; // return to head
                B2= B2+ ldb2; // move to next column
                C = C - M1 + ldc; // return to head
            }
        }
        B2 = B2 - ldb2*N1; // return to head

        C = C - ldc*N1; // return to tile head
        C = C - ldc*j1 - i1; // return to head

        A2 = A2 - M2*(k1-k2) - K1*(i1-i2); // return to head
    }
    B2 = B2 - ldb2*(j1-j2) - (k1-k2); // return to head
    }}    
}

// naive
void my_dgemm_000(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    double AB;
    int64_t i, j, k; 

    for (j=0; j<N; j++){
        for (i=0; i<M; i++){
            AB=0e0;
            for (k=0; k<K; k++){
                AB += (*A)*(*B);
                A += M;
                B ++;
            }
            (*C) += AB;
            A -= M * K - 1;
            B -= K;
            C ++;
        }
        A = A - M;
        B = B + K;
    }
}

// cache blocking
void my_dgemm_001(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    double AB;
    int64_t i, i1, i2, i3; 
    int64_t j, j1, j2, j3; 
    int64_t k, k1, k2, k3;

    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    for( size_t j2=j3; j2<MIN(j3+MYBLAS_PANEL_N,N); j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
    for( size_t j1=j2; j1<MIN(j2+MYBLAS_BLOCK_N,N); j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
    for( size_t j =j1; j <MIN(j1+MYBLAS_TILE_N ,N); j++ ){
        for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
        for( size_t i2=i3; i2<MIN(i3+MYBLAS_PANEL_M,M); i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
        for( size_t i1=i2; i1<MIN(i2+MYBLAS_BLOCK_M,M); i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
        for( size_t i =i1; i <MIN(i1+MYBLAS_TILE_M ,M); i++ ){
            AB=0e0;
            for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
            for( size_t k2=k3; k2<MIN(k3+MYBLAS_PANEL_K,K); k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
            for( size_t k1=k2; k1<MIN(k2+MYBLAS_BLOCK_K,K); k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            for( size_t k =k1; k <MIN(k1+MYBLAS_TILE_K ,K); k++ ){
                AB = AB + (*A)*(*B);
                A += M;
                B++;
            }}}}
            *C=(*C) + AB;
            A = A - M*K + 1;
            B = B - K;
            C++;
        }}}}
        A = A - M;
        B = B + K;
        C = C - M + M;
    }}}}
}

// cache copy
void my_dgemm_002(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    double AB;
    int64_t i, i1, i2, i3; 
    int64_t j, j1, j2, j3; 
    int64_t k, k1, k2, k3;

    double*   A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double*   B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
    for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
        size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);

        // L2 cache
        for( size_t j2=j3; j2<j3+N3; j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
        for( size_t i2=i3; i2<i3+M3; i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
        for( size_t k2=k3; k2<k3+K3; k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
            size_t M2 = MIN(MYBLAS_BLOCK_M ,M-i2);
            size_t N2 = MIN(MYBLAS_BLOCK_N ,N-j2);
            size_t K2 = MIN(MYBLAS_BLOCK_K ,K-k2);

            // On Cache Copy
            A = A + lda*k2 + i2;
            for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
            size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
                size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                for( size_t i =i1; i < i1+M1; i++ ){
                for( size_t k =k1; k < k1+K1; k++ ){
                    (*A2) = (*A);
                    A += lda ;
                    A2+= lda2;
                }
                A  = A  - lda *K1 + 1;
                A2 = A2 - lda2*K1 + 1;
                }
                A = A - M1 + lda *K1; // next tile head, column
                A2= A2- M1 + lda2*K1;
            }
            A = A  - lda *K2 + M1; // next tile head, row
            A2= A2 - lda2*K2 + M1;
            }
            A = A  - M2;
            A2= A2 - M2;
            A = A - lda*k2 - i2; // return to head

            B = B + ldb*j2 + k2;
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
                    size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                    for( size_t j =j1; j < j1+N1; j++ ){
                        for( size_t k =k1; k < k1+K1; k++ ){
                            (*B2)=(*B);
                            B++;
                            B2++;
                        }
                        B  = B  - K1 + ldb ;
                        B2 = B2 - K1 + ldb2;    
                    }
                    B  = B  - ldb *N1 + K1; // next tile head, row
                    B2 = B2 - ldb2*N1 + K1; 
                }
                B  = B  - K2 + ldb *N1; // next tile head, column
                B2 = B2 - K2 + ldb2*N1; 
                }
            B  = B  - ldb *N2;
            B2 = B2 - ldb2*N2;
            B  = B  - ldb*j2 - k2; // return to head 


            // L1 cache
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
            for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){

                C = C + ldc*j1 + i1;
                
                A2 = A2 + lda2*(k1-k2) + (i1-i2);
                B2 = B2 + ldb2*(j1-j2) + (k1-k2);

                size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                for( size_t j =j1; j < j1+N1; j++ ){
                    for( size_t i =i1; i < i1+M1; i++ ){
                    AB=0e0;
                    for( size_t k =k1; k < k1+K1; k++ ){
                        AB = AB + (*A2)*(*B2);
                        A2+= lda2;
                        B2++;
                    }
                    *C = (*C) + AB;
                    A2 = A2 - lda2*K1 + 1;
                    B2 = B2 - K1;
                    C++;
                    }
                    A2= A2- M1;
                    B2= B2+ ldb2;
                    C = C - M1 + ldc;
                }
                B2 = B2 - ldb2*N1;

                C = C - ldc*N1;
                C = C - ldc*j1 - i1;

                A2 = A2 - lda2*(k1-k2) - (i1-i2);
                B2 = B2 - ldb2*(j1-j2) - (k1-k2);

            }}}

        }}}

    }}}

    free(A2);
}

// loop exchange
void my_dgemm_003(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    double AB;
    int64_t i, i1, i2, i3; 
    int64_t j, j1, j2, j3; 
    int64_t k, k1, k2, k3;

    double*   A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double*   B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
    for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
        size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);

        // L2 cache
        for( size_t j2=j3; j2<j3+N3; j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
        for( size_t i2=i3; i2<i3+M3; i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
        for( size_t k2=k3; k2<k3+K3; k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
            size_t M2 = MIN(MYBLAS_BLOCK_M ,M-i2);
            size_t N2 = MIN(MYBLAS_BLOCK_N ,N-j2);
            size_t K2 = MIN(MYBLAS_BLOCK_K ,K-k2);

            // On Cache Copy
            A = A + lda*k2 + i2;
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
                size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
                    for( size_t i =i1; i < i1+M1; i++ ){
                    for( size_t k =k1; k < k1+K1; k++ ){
                        (*A2) = (*A);
                        A += lda ;
                        A2+= lda2;
                    }
                    A  = A  - lda *K1 + 1;  
                    A2 = A2 - lda2*K1 + 1;  
                    }
                // move to next tile head, row
                }
            A = A  - M2 + lda *K1;  // move to next tile head, column
            A2= A2 - M2 + lda2*K1;
            }
            A = A  - lda *K2; // return to block head 
            A2= A2 - lda2*K2; // return to head 
            A = A - lda*k2 - i2; // return to head 

            B = B + ldb*j2 + k2;
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
                    size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                    for( size_t j =j1; j < j1+N1; j++ ){
                        for( size_t k =k1; k < k1+K1; k++ ){
                            (*B2)=(*B);
                            B++;
                            B2++;
                        }
                        B  = B  - K1 + ldb ; // move to next column
                        B2 = B2 - K1 + ldb2;    
                    }
                    B  = B  - ldb *N1 + K1; // next tile head, row
                    B2 = B2 - ldb2*N1 + K1; 
                }
                B  = B  - K2 + ldb *N1; // next tile head, column
                B2 = B2 - K2 + ldb2*N1; 
                }
            B  = B  - ldb *N2;
            B2 = B2 - ldb2*N2;
            B  = B  - ldb*j2 - k2; // return to head 


            // L1 cache
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
                B2 = B2 + ldb2*(j1-j2) + (k1-k2); // move to tile head
            for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
                C  = C + ldc*j1 + i1; // move to tile head
                A2 = A2 + lda2*(k1-k2) + (i1-i2); // move to tile head

                size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                for( size_t j =j1; j < j1+N1; j++ ){
                    for( size_t i =i1; i < i1+M1; i++ ){
                        AB=0e0;
                        for( size_t k =k1; k < k1+K1; k++ ){
                            AB = AB + (*A2)*(*B2);
                            A2+= lda2;
                            B2++;
                        }
                        *C = (*C) + AB;
                        A2 = A2 - lda2*K1 + 1; // move to next row
                        B2 = B2 - K1; // return to column head
                        C++;
                    }
                    A2= A2- M1; // return to head
                    B2= B2+ ldb2; // move to next column
                    C = C - M1 + ldc; // return to head
                }
                B2 = B2 - ldb2*N1; // return to head

                C = C - ldc*N1; // return to tile head
                C = C - ldc*j1 - i1; // return to head

                A2 = A2 - lda2*(k1-k2) - (i1-i2); // return to head
            }
            B2 = B2 - ldb2*(j1-j2) - (k1-k2); // return to head
            }}

        }}}

    }}}

    free(A2);
}

// serialize
void my_dgemm_004(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    double AB;
    int64_t i, i1, i2, i3; 
    int64_t j, j1, j2, j3; 
    int64_t k, k1, k2, k3;

    double*   A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double*   B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
    for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
        size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);

        // L2 cache
        for( size_t j2=j3; j2<j3+N3; j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
        for( size_t i2=i3; i2<i3+M3; i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
        for( size_t k2=k3; k2<k3+K3; k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
            size_t M2 = MIN(MYBLAS_BLOCK_M ,M-i2);
            size_t N2 = MIN(MYBLAS_BLOCK_N ,N-j2);
            size_t K2 = MIN(MYBLAS_BLOCK_K ,K-k2);

            // On Cache Copy
            A = A + lda*k2 + i2;
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){
                size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
                    for( size_t i =i1; i < i1+M1; i++ ){
                    for( size_t k =k1; k < k1+K1; k++ ){
                        (*A2) = (*A);
                        A += lda ;
                        A2++;
                    }
                    A  = A  - lda *K1 + 1;  
                    }
                // move to next tile head, row
                }
            A = A  - M2 + lda *K1;  // move to next tile head, column
            }
            A2= A2 - M2*K2; // return to head 
            A = A  - lda*K2; // return to block head 
            A = A  - lda*k2 - i2; // return to head 

            B = B + ldb*j2 + k2;
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
                    size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);
                    for( size_t j =j1; j < j1+N1; j++ ){
                        for( size_t k =k1; k < k1+K1; k++ ){
                            (*B2)=(*B);
                            B++;
                            B2++;
                        }
                        B  = B  - K1 + ldb ; // move to next column
                        B2 = B2 - K1 + ldb2;    
                    }
                    B  = B  - ldb *N1 + K1; // next tile head, row
                    B2 = B2 - ldb2*N1 + K1; 
                }
                B  = B  - K2 + ldb *N1; // next tile head, column
                B2 = B2 - K2 + ldb2*N1; 
                }
            B  = B  - ldb *N2;
            B2 = B2 - ldb2*N2;
            B  = B  - ldb*j2 - k2; // return to head 


            // L1 cache
            for( size_t k1=k2; k1<k2+K2; k1+=MIN(K-k1,MYBLAS_TILE_K ) ){
            for( size_t j1=j2; j1<j2+N2; j1+=MIN(N-j1,MYBLAS_TILE_N ) ){
                B2 = B2 + ldb2*(j1-j2) + (k1-k2); // move to tile head
            for( size_t i1=i2; i1<i2+M2; i1+=MIN(M-i1,MYBLAS_TILE_M ) ){

                size_t M1 = MIN(MYBLAS_TILE_M ,M-i1);
                size_t N1 = MIN(MYBLAS_TILE_N ,N-j1);
                size_t K1 = MIN(MYBLAS_TILE_K ,K-k1);

                C  = C  + ldc*j1 + i1;            // move to tile head
                A2 = A2 + M2*(k1-k2) + K1*(i1-i2); // move to tile head
                for( size_t j =j1; j < j1+N1; j++ ){
                    for( size_t i =i1; i < i1+M1; i++ ){
                        AB=0e0;
                        for( size_t k =k1; k < k1+K1; k++ ){
                            AB = AB + (*A2)*(*B2);
                            A2++;
                            B2++;
                        }
                        *C = (*C) + AB;
                        B2 = B2 - K1; // return to column head
                        C++;
                    }
                    A2= A2- M1*K1; // return to head
                    B2= B2+ ldb2; // move to next column
                    C = C - M1 + ldc; // return to head
                }
                B2 = B2 - ldb2*N1; // return to head

                C = C - ldc*N1; // return to tile head
                C = C - ldc*j1 - i1; // return to head

                A2 = A2 - M2*(k1-k2) - K1*(i1-i2); // return to head
            }
            B2 = B2 - ldb2*(j1-j2) - (k1-k2); // return to head
            }}

        }}}

    }}}

    free(A2);
}

// kernelize
void my_dgemm_005(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    int64_t i1, i2, i3; 
    int64_t j1, j2, j3; 
    int64_t k1, k2, k3;

    double*   A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double*   B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
    for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
        size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);

        // L2 cache
        for( size_t j2=j3; j2<j3+N3; j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
        for( size_t i2=i3; i2<i3+M3; i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
        for( size_t k2=k3; k2<k3+K3; k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
            size_t M2 = MIN(MYBLAS_BLOCK_M ,M-i2);
            size_t N2 = MIN(MYBLAS_BLOCK_N ,N-j2);
            size_t K2 = MIN(MYBLAS_BLOCK_K ,K-k2);

            // On Cache Copy
            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
            a2_l2_cache_copy_005(A, A2, &info);
            b2_l2_cache_copy_005(B, B2, &info);

            // L1 cache
            c_l1_calc_005(C, A2, B2, &info);
        }}}

    }}}

    free(A2);
    free(B2);
}

// loop interchange 2
void my_dgemm_006(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    int64_t i1, i2, i3; 
    int64_t j1, j2, j3; 
    int64_t k1, k2, k3;

    double*   A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double*   B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);

        for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);

            for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
            size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);

                // L2 cache
                for( size_t k2=k3; k2<k3+K3; k2+=MIN(K-k2,MYBLAS_BLOCK_K) ){
                size_t K2 = MIN(MYBLAS_BLOCK_K ,K-k2);

                    for( size_t i2=i3; i2<i3+M3; i2+=MIN(M-i2,MYBLAS_BLOCK_M) ){
                    size_t M2 = MIN(MYBLAS_BLOCK_M ,M-i2);
                    info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,K2,M,K,N};
                    a2_l2_cache_copy_006(A, A2, &info);

                        for( size_t j2=j3; j2<j3+N3; j2+=MIN(N-j2,MYBLAS_BLOCK_N) ){
                            size_t N2 = MIN(MYBLAS_BLOCK_N ,N-j2);
                            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};

                            // On Cache Copy
                            b2_l2_cache_copy_006(B, B2, &info);

                            // L1 cache
                            c_l1_calc_006(C, A2, B2, &info);
                        } // L2 j
                    } // L2 i
                } // L2 k

            } // L3 k
        } // L3 i
    } // L3 j

    free(A2);
    free(B2);
}


void my_dgemm_007(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    int64_t i1, i2, i3; 
    int64_t j1, j2, j3; 
    int64_t k1, k2, k3;

    double* A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double* B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
    size_t N3B = N3/MYBLAS_BLOCK_N+(N3%MYBLAS_BLOCK_N>0?1:0); // number of full blocks 
    size_t N3R = N3%MYBLAS_BLOCK_N==0?MYBLAS_BLOCK_N:N3%MYBLAS_BLOCK_N;

        for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t M3B = M3/MYBLAS_BLOCK_M+(M3%MYBLAS_BLOCK_M>0?1:0); // number of full blocks 
        size_t M3R = M3%MYBLAS_BLOCK_M==0?MYBLAS_BLOCK_M:M3%MYBLAS_BLOCK_M;

            for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
            size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);
            size_t K3B = K3/MYBLAS_BLOCK_K+(K3%MYBLAS_BLOCK_K>0?1:0); // number of full blocks 
            size_t K3R = K3%MYBLAS_BLOCK_K==0?MYBLAS_BLOCK_K:K3%MYBLAS_BLOCK_K;

                size_t l2 = K3B; // block index
                size_t k2 = k3;  // element index
                while( l2-- ){  
                    size_t K2 = MYBLAS_BLOCK_K; if( l2==0 ){ K2=K3R; }

                    size_t m2n2 = M3B*N3B; // m2n2=m2*N3B+n2
                    // size_t m2 = M3B-1;
                    size_t n2 = N3B-1;
                    size_t i2 = i3;  // element index
                    size_t j2 = j3;  // element index
                    size_t M2 = MYBLAS_BLOCK_M; if( i2+M2 >= i3+M3 ){ M2=M3R; }
                    size_t N2 = MYBLAS_BLOCK_N; if( j2+N2 >= j3+N3 ){ N2=N3R; }

                    // First Blocks 
                    {
                        // On L2-Cache Copy for A
                        info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                        a2_l2_cache_copy_006(A, A2, &info);

                        // On Cache Copy
                        b2_l2_cache_copy_006(B, B2, &info);

                        // L1 cache
                        c_l1_calc_006(C, A2, B2, &info);

                        m2n2--;
                    }

                    while( m2n2-- ){
                        if( n2 > 0 ){ 
                            n2--;
                            if( j2+N2 < j3+N3 ){ j2+=N2; }else{ j2=j3; }
                            N2 = MYBLAS_BLOCK_N; if( j2+N2 >= j3+N3 ){ N2=N3R; }

                            // On L2-Cache Copy for B
                            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                            b2_l2_cache_copy_006(B, B2, &info);

                            // L1 cache
                            c_l1_calc_006(C, A2, B2, &info);
                        }else{ 
                            // m2--;
                            i2 += M2;
                            M2 = MYBLAS_BLOCK_M; if( i2+M2 >= i3+M3 ){ M2=M3R; }

                            // On L2-Cache Copy for A
                            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                            a2_l2_cache_copy_006(A, A2, &info);
                            n2 = N3B-1;

                            // L1 cache
                            c_l1_calc_006(C, A2, B2, &info);
                        }
                    }
                    k2 += K2;
                }

            } // L3 k
        } // L3 i
    } // L3 j

    free(A2);
    free(B2);
}


void my_dgemm_008(double* A, double* B, double* C, int64_t M, int64_t N, int64_t K){
    // compute c = a*b 
    // a: m x k
    // b: k x n
    // c: m x n
    int64_t i1, i2, i3; 
    int64_t j1, j2, j3; 
    int64_t k1, k2, k3;

    double* A2 = calloc( MYBLAS_BLOCK_M*MYBLAS_BLOCK_K, sizeof(double) );
    double* B2 = calloc( MYBLAS_BLOCK_K*MYBLAS_BLOCK_N, sizeof(double) );

    // leading dimension
    size_t  lda = M;
    size_t  ldb = K;
    size_t  ldc = M;
    size_t  lda2 = MYBLAS_BLOCK_M;
    size_t  ldb2 = MYBLAS_BLOCK_K;
    size_t  ldc2 = MYBLAS_BLOCK_M;

    // L3 cache
    for( size_t j3=0 ; j3<N; j3+=MIN(N-j3,MYBLAS_PANEL_N) ){
    size_t N3 = MIN(MYBLAS_PANEL_N ,N-j3);
    size_t N3B = N3/MYBLAS_BLOCK_N+(N3%MYBLAS_BLOCK_N>0?1:0); // number of full blocks 
    size_t N3R = N3%MYBLAS_BLOCK_N==0?MYBLAS_BLOCK_N:N3%MYBLAS_BLOCK_N;

        for( size_t i3=0 ; i3<M; i3+=MIN(M-i3,MYBLAS_PANEL_M) ){
        size_t M3 = MIN(MYBLAS_PANEL_M ,M-i3);
        size_t M3B = M3/MYBLAS_BLOCK_M+(M3%MYBLAS_BLOCK_M>0?1:0); // number of full blocks 
        size_t M3R = M3%MYBLAS_BLOCK_M==0?MYBLAS_BLOCK_M:M3%MYBLAS_BLOCK_M;

            for( size_t k3=0 ; k3<K; k3+=MIN(K-k3,MYBLAS_PANEL_K) ){
            size_t K3 = MIN(MYBLAS_PANEL_K ,K-k3);
            size_t K3B = K3/MYBLAS_BLOCK_K+(K3%MYBLAS_BLOCK_K>0?1:0); // number of full blocks 
            size_t K3R = K3%MYBLAS_BLOCK_K==0?MYBLAS_BLOCK_K:K3%MYBLAS_BLOCK_K;

                size_t l2 = K3B; // block index
                size_t k2 = k3;  // element index
                while( l2-- ){  
                    size_t K2 = MYBLAS_BLOCK_K; if( l2==0 ){ K2=K3R; }

                    size_t m2n2 = M3B*N3B; // m2n2=m2*N3B+n2
                    // size_t m2 = M3B-1;
                    size_t n2 = N3B-1;
                    size_t i2 = i3;  // element index
                    size_t j2 = j3;  // element index
                    size_t M2 = MYBLAS_BLOCK_M; if( i2+M2 >= i3+M3 ){ M2=M3R; }
                    size_t N2 = MYBLAS_BLOCK_N; if( j2+N2 >= j3+N3 ){ N2=N3R; }

                    // First Blocks 
                    {
                        // On L2-Cache Copy for A
                        info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                        a2_l2_cache_copy_006(A, A2, &info);

                        // On Cache Copy
                        b2_l2_cache_copy_006(B, B2, &info);

                        // L1 cache
                        c_l1_calc_008(C, A2, B2, &info);

                        m2n2--;
                    }

                    while( m2n2-- ){
                        if( n2 > 0 ){ 
                            n2--;
                            if( j2+N2 < j3+N3 ){ j2+=N2; }else{ j2=j3; }
                            N2 = MYBLAS_BLOCK_N; if( j2+N2 >= j3+N3 ){ N2=N3R; }

                            // On L2-Cache Copy for B
                            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                            b2_l2_cache_copy_006(B, B2, &info);

                            // L1 cache
                            c_l1_calc_008(C, A2, B2, &info);
                        }else{ 
                            // m2--;
                            i2 += M2;
                            M2 = MYBLAS_BLOCK_M; if( i2+M2 >= i3+M3 ){ M2=M3R; }

                            // On L2-Cache Copy for A
                            info_copy info = {i2,k2,j2,lda,ldb,ldc,lda2,ldb2,ldc2,M2,K2,N2,M,K,N};
                            a2_l2_cache_copy_006(A, A2, &info);
                            n2 = N3B-1;

                            // L1 cache
                            c_l1_calc_008(C, A2, B2, &info);
                        }
                    }
                    k2 += K2;
                }

            } // L3 k
        } // L3 i
    } // L3 j

    free(A2);
    free(B2);
}










