module mod_euclidean_distance
    use :: mod_kinds
    implicit none


contains

    subroutine euclidean_distance_mv( & 
        m, k,                         &
        distances,                    &
        mat, mat_row_sq_norm,         & 
        vec, vec_sq_norm)
        implicit none

        ! Output, distances = mat_row_sq_norm + vec_sq_norm - 2 * gemv(mat * vec)
        integer(i64), intent(in) :: m, k
        real(r64), intent(inout) :: distances(m)

        ! Input matrix
        real(r64), intent(in)    :: mat(m, k), mat_row_sq_norm(m)

        ! Input vector
        real(r64), intent(in)    :: vec(k), vec_sq_norm

        distances(:) = mat_row_sq_norm(:) + vec_sq_norm

        call dgemv(                          &
                "N", m, k, -2.0_r64,         & 
                mat, m, vec, 1_i64, 1.0_r64, &
                distances, 1_i64             &
            )
    end subroutine euclidean_distance_mv

    subroutine euclidean_distance_mm( & 
        m, k, l,                      &
        mat_dist,                     &
        mat_A, mat_A_row_sq_norm,     & 
        mat_B, mat_B_row_sq_norm)
        implicit none

        ! Output, distances = mat_A_row_sq_norm + mat_B_row_sq_norm - 2 * gemm(mat_A * mat_B)
        integer(i64), intent(in) :: m, k, l
        real(r64), intent(inout) :: mat_dist(m, l)

        ! Input matrix
        real(r64), intent(in)    :: mat_A(m, k), mat_A_row_sq_norm(m)
        real(r64), intent(in)    :: mat_B(l, k), mat_B_row_sq_norm(l)

        mat_dist(:,:) =                                     & 
            spread(mat_A_row_sq_norm, dim=2, ncopies=l)     &
             + spread(mat_B_row_sq_norm, dim=1, ncopies=m)

        call dgemm( 'N', 'T',                & ! op(A), op(B)
                    m,  l,  k,               & ! M, N, K
                    -2.0_r64,                & ! alpha
                    mat_A, m,                & ! A, LDA  (=m)
                    mat_B, l,                & ! B, LDB  (=l)
                    1.0_r64,                 & ! beta  (既存値を保持)
                    mat_dist, m )              ! C, LDC  (=m)
    end subroutine euclidean_distance_mm

    subroutine euclidean_distance_mm_chunk( &
            n_rows, k, l,           &  ! 行数, 列数, クラスタ数
            i_start, i_end, cs,     &  ! 1-based 行範囲, chunk_size (=cs)
            dist_buf,               &  ! (cs,l) 出力
            mat_A, mat_A_row_sq_norm, &
            mat_B, mat_B_row_sq_norm)   ! (l,k),(l)
        implicit none
        ! ----------------------- dummy args ---------------------------------
        integer(i64), intent(in)  :: n_rows, k, l
        integer(i64), intent(in)  :: i_start, i_end, cs
        real(r64)   , intent(out) :: dist_buf(cs, l)
        real(r64)   , intent(in)  :: mat_A(n_rows, k)
        real(r64)   , intent(in)  :: mat_A_row_sq_norm(n_rows)
        real(r64)   , intent(in)  :: mat_B(l, k)
        real(r64)   , intent(in)  :: mat_B_row_sq_norm(l)
        ! ----------------------- local vars ---------------------------------
        integer(i64) :: j
        ! ----------------------- ① |a_i|² + |b_j|² --------------------------
        do j = 1, l
            dist_buf(:, j) = mat_A_row_sq_norm(i_start:i_end) + mat_B_row_sq_norm(j)
        end do

        ! ----------------------- ② −2 * A_chunk * Bᵀ -----------------------
        call dgemm('N','T', cs, l, k,                   & ! Aそのまま, B転置
                -2.0_r64,                            & ! α = −2
                mat_A(i_start,1), n_rows,            & ! A, LDA (=n_rows)
                mat_B,             l,                & ! B, LDB (=l)
                1.0_r64,                             & ! β = 1 (既存値保持)
                dist_buf,        cs)                  ! C, LDC (=cs)
    end subroutine euclidean_distance_mm_chunk

    subroutine euclidean_distance_mm_bnorm( & 
            m, k, l,                      &  ! m=rows(A), k=cols, l=rows(B)
            mat_dist,                     &  ! (m,l)   : 出力
            mat_A,                        &  ! (m,k)
            mat_B, mat_B_row_sq_norm)       ! (l,k) , (l)
        implicit none
        integer(i64), intent(in) :: m, k, l
        real(r64),  intent(inout) :: mat_dist(m, l)
        real(r64),  intent(in)    :: mat_A(m, k)
        real(r64),  intent(in)    :: mat_B(l, k), mat_B_row_sq_norm(l)

        ! ---- ①  |b_j|^2 を各列にブロードキャスト -----------------------------
        mat_dist(:,:) = spread(mat_B_row_sq_norm, dim=1, ncopies=m)

        ! ---- ②  -2 * (A  *  B^T) を加算 -------------------------------------
        call dgemm('N','T',            &  ! Aそのまま, B転置
                m, l, k,            &  ! M, N, K
                -2.0_r64,            &  ! alpha
                mat_A, m,           &  ! A, LDA
                mat_B, l,           &  ! B, LDB
                1.0_r64,            &  ! beta  (既存 |b_j|² を残す)
                mat_dist, m)           ! C, LDC
    end subroutine euclidean_distance_mm_bnorm

    subroutine euclidean_distance_mm_bnorm_chunk( &
            n_rows, k, l,           &  ! 行数 (=LDA), 列数, クラスタ数
            i_start, i_end, cs,     &  ! 1‑based 行範囲, chunk_size (=cs)
            dist_buf,               &  ! (cs,l)  出力: 距離
            mat_A,                  &  ! (n_rows,k) = X
            mat_B, mat_B_row_sq_norm)  ! (l,k), (l) = centroids, |b_j|²
        implicit none
        ! -------------------- dummy args -----------------------------------
        integer(i64), intent(in)    :: n_rows, k, l
        integer(i64), intent(in)    :: i_start, i_end, cs
        real(r64)   , intent(out)   :: dist_buf(cs, l)
        real(r64)   , intent(in)    :: mat_A(n_rows, k)
        real(r64)   , intent(in)    :: mat_B(l, k), mat_B_row_sq_norm(l)
        ! -------------------- local vars -----------------------------------
        integer(i64) :: j
        ! -------------------- sanity checks --------------------------------
        do j = 1, l
            dist_buf(:, j) = mat_B_row_sq_norm(j)
        end do
        ! -------------------- ②  -2 * A_chunk * Bᵀ ------------------------
        call dgemm('N','T', cs, l, k,                   &
                -2.0_r64, mat_A(i_start,1), n_rows,  & ! A, LDA = n_rows
                mat_B,             l,                & ! B, LDB = l
                1.0_r64, dist_buf,      cs)            ! C, LDC = cs
    end subroutine euclidean_distance_mm_bnorm_chunk

end module mod_euclidean_distance
