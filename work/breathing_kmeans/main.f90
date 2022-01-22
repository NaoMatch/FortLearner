program main
    use mod_kmeans
    use mod_breathing_kmeans
    implicit none
    
    type(kmeans) :: km
    type(breathing_kmeans) :: bkm

    integer(kind=8), allocatable :: labels_km(:), labels_bkm(:)
    real(kind=8), allocatable :: x(:,:)
    real(kind=8) :: score_km, score_bkm

    km = kmeans(n_clusters=50_8)
    bkm = breathing_kmeans(n_clusters=50_8, n_clusters_breathing_in=5_8)
    
    allocate(x(1000,5))
    call RANDOM_NUMBER(x)

    call km%fit(x)
    call bkm%fit(x)

    score_km = km%score(x)
    score_bkm = bkm%score(x)

    print*, score_km
    print*, score_bkm
    print*, (1d0 - score_bkm/score_km)*100d0

    labels_km = km%predict(x)
    labels_bkm = bkm%predict(x)



end program main
