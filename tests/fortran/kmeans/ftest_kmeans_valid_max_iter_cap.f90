program ftest_valid_max_iter_cap
    use mod_kinds
    use mod_kmeans      , only : kmeans, new_kmeans
    use mod_random_seed , only : fix_random_seed
    implicit none
    integer(i64), parameter :: n=400, d=4, k=4, SEED=13579_i64
    integer(i64), parameter :: MAX_IT=5
    real(r64),    allocatable :: X(:,:)
    type(kmeans)  :: km

    allocate(X(n,d))
    call fix_random_seed(SEED); call random_number(X)

    km = new_kmeans(k, random_state=SEED, max_iter=MAX_IT, tol=0.000000000000000001_r64, print_log=.true.)
    call km%fit(X)
    if (km%n_iter_ /= MAX_IT) error stop 99   ! n_iter は public 変数想定
end program
