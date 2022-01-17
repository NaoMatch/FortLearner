program main
    use mod_kmeans
    use mod_breathing_kmeans
    implicit none
    
    type(kmeans) :: km
    type(breathing_kmeans) :: bkm

    real(kind=8), allocatable :: x(:,:)

    bkm = breathing_kmeans(n_clusters=10_8, n_clusters_breathing_in=2_8)
    
    allocate(x(100,2))
    call RANDOM_NUMBER(x)

    call bkm%fit(x)



end program main
