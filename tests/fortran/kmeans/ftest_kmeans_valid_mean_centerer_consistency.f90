program test_kmeans_valid_mean_centerer_consistency
    use :: mod_kinds
    use :: mod_kmeans,      only : kmeans, new_kmeans
    use :: mod_random_seed, only : fix_random_seed
    use :: mod_mean_centerer
    implicit none
    integer(i64),  parameter :: n = 500, d = 4, k = 3
    integer(i64),  parameter :: SEED_ALGO = 42_i64
    integer(i64),  parameter :: MAX_ITER  = 100
    integer(i64),  parameter :: SEED_DATA = 12345_i64
    real(r64),  allocatable :: X(:,:), X_(:,:)
    integer(i64), allocatable :: lab1(:,:), lab2(:,:)
    real(r64),    allocatable :: cen1(:,:), cen2(:,:)
    type(kmeans) :: m1, m2
    type(mean_centerer) :: centerer
    logical      :: pass

    ! ---- データ生成 ----
    allocate(X(n,d), X_(n,d))
    call fix_random_seed(SEED_DATA)
    call random_number(X)

    ! ---- Mean Centerer ----
    centerer = new_mean_centerer(stable=.true.)
    call centerer%fit(X)
    X_ = X
    call centerer%transform(X_)

    ! ---- 学習１ ----
    m1 = new_kmeans(k, random_state = SEED_ALGO, max_iter = MAX_ITER)
    call m1%fit(X_)
    lab1 = m1%predict(X_)
    cen1 = m1%centroids(:,:)

    ! ---- 学習２（同パラメータで再学習） ----
    m2 = new_kmeans(k, random_state = SEED_ALGO, max_iter = MAX_ITER)
    call m2%fit(X_)
    lab2 = m2%predict(X_)
    cen2 = m2%centroids(:,:)

    ! ---- 判定 ----
    pass = all(lab1 == lab2)
    print*, count(lab1 /= lab2), sum(X), sum(X_)

    if (pass) then
        print*, "LabelDiff: ", count(lab1 /= lab2)
        stop 0
    else
        write(*,*) "FAIL: deterministic test mismatch"
        error stop 1
    end if
end program test_kmeans_valid_mean_centerer_consistency