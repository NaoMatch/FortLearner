program test_kmeans_valid_random_state_consistency
    use :: mod_kinds
    use :: mod_kmeans,      only : kmeans, new_kmeans
    use :: mod_random_seed, only : fix_random_seed
    implicit none
    integer(i64),  parameter :: n = 500, d = 4, k = 3
    integer(i64),  parameter :: SEED_ALGO = 42_i64
    integer(i64),  parameter :: MAX_ITER  = 100
    integer(i64),  parameter :: SEED_DATA = 12345_i64
    real(r64),  allocatable :: X(:,:)
    integer(i64), allocatable :: lab1(:,:), lab2(:,:)
    real(r64),    allocatable :: cen1(:,:), cen2(:,:)
    type(kmeans) :: m1, m2
    logical      :: pass
    real(r64),   parameter  :: EPS = 1.0e-12_r64

    ! ---- データ生成 ----
    allocate(X(n,d))
    call fix_random_seed(SEED_DATA)
    call random_number(X)

    ! ---- 学習１ ----
    m1 = new_kmeans(k, random_state = SEED_ALGO, max_iter = MAX_ITER)
    call m1%fit(X)
    lab1 = m1%predict(X)
    cen1 = m1%centroids(:,:)

    ! ---- 学習２（同パラメータで再学習） ----
    m2 = new_kmeans(k, random_state = SEED_ALGO, max_iter = MAX_ITER)
    call m2%fit(X)
    lab2 = m2%predict(X)
    cen2 = m2%centroids(:,:)

    ! ---- 判定 ----
    pass = all(lab1 == lab2) .and. all(abs(cen1 - cen2) < EPS)

    if (pass) then
        print*, "LabelDiff: ", count(lab1 /= lab2), " CentroidDiff: ", sum((cen1 - cen2)**2.0_r64)
        stop 0
    else
        write(*,*) "FAIL: deterministic test mismatch"
        error stop 1
    end if
end program test_kmeans_valid_random_state_consistency