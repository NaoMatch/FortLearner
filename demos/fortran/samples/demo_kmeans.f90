program demo_kmeans

    use :: mod_kinds
    use :: mod_kmeans
    use :: mod_mean_centerer
    implicit none

    integer(i64) :: n=100, d=10, c=4
    real(r64), allocatable :: X(:,:)
    integer(i64), allocatable :: labels(:,:)
    type(kmeans) :: km
    type(mean_centerer) :: centerer

    allocate(X(n,d))
    call random_number(X)

    centerer = new_mean_centerer()
    call centerer%fit(X)
    call centerer%transform(X)

    km = new_kmeans(n_clusters=c, random_state=1_i64, n_init=1_r64, chunk_size=10_r64)
    call km%fit(X)
    labels = km%predict(X)


end program demo_kmeans
