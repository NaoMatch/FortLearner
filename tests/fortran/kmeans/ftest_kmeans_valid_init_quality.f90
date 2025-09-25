program ftest_valid_init_quality
    use mod_kinds
    use mod_kmeans, only : kmeans, new_kmeans
    use mod_random_seed, only : fix_random_seed
    implicit none
    integer(i64), parameter :: n=500, d=4, k=3, SEED=12345_i64
    real(r64), allocatable  :: X(:,:), c_rand(:,:), c_pp(:,:)
    real(r64) :: sse_rand, sse_pp
    type(kmeans) :: dummy

    ! ---- データ生成 ----
    allocate(X(n,d)); call fix_random_seed(SEED); call random_number(X)

    ! ---- 初期化 ----
    allocate(c_rand(k,d), c_pp(k,d))
    dummy = new_kmeans(n_clusters=k)
    call dummy%initialize_clusters_random(c_rand, k, X, n, d)
    call dummy%initialize_clusters_kmeanspp(c_pp , k, X, n, d)

    ! ---- SSE 計算 ----
    sse_rand = compute_sse(X, c_rand)
    sse_pp   = compute_sse(X, c_pp)

    write(*,*) "k-means++ SSE =", sse_pp, " and random SSE =", sse_rand
    if (sse_pp < sse_rand) then
        stop 0       ! PASSED
    else
        write(*,*) "FAILED: k-means++ SSE =", sse_pp, " >= random SSE =", sse_rand
        error stop 99
    end if

contains

function compute_sse(X, centroids) result(sse)
    !-------------------------------------------------
    !   X          : n_rows × n_cols
    !   centroids  : k × n_cols
    !   返り値 sse : Σ‖xi − μc(i)‖²
    !-------------------------------------------------
    use mod_kinds
    real(r64), intent(in) :: X(:,:), centroids(:,:)
    real(r64)             :: sse
    integer               :: i, j, idx
    real(r64)             :: dist2, best

    sse = 0.0_r64
    do i = 1, size(X,1)
        best = huge(1.0_r64)
        do idx = 1, size(centroids,1)
            dist2 = 0.0_r64
            do j = 1, size(X,2)
                dist2 = dist2 + (X(i,j) - centroids(idx,j))**2
            end do
            if (dist2 < best) best = dist2
        end do
        sse = sse + best
    end do
end function compute_sse

end program