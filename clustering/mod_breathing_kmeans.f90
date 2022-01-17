module mod_breathing_kmeans
    use mod_kmeans
    use mod_hyperparameter
    use mod_sort
    use mod_common
    use mod_stats
    implicit none

    type breathing_kmeans
        type(kmeans) :: km
        integer(kind=8) :: n_samples, n_columns
        type(hparam_breathing_kmeans) :: hparam
    contains
        procedure :: fit => fit_breathing_kmeans
        procedure :: calc_rmse_per_centers
        ! procedure :: calc_utilities_per_centers
        procedure :: breathing_in
        procedure :: breathing_out
    end type breathing_kmeans

    interface breathing_kmeans
        module procedure :: new_breathing_kmeans
    end interface breathing_kmeans

contains

    function new_breathing_kmeans(n_clusters, n_clusters_breathing_in)
        implicit none
        type(breathing_kmeans)    :: new_breathing_kmeans
        integer(kind=8), optional :: n_clusters
        integer(kind=8), optional :: n_clusters_breathing_in

        new_breathing_kmeans%hparam%algo_name = "breathing_kmeans"
        if (present(n_clusters))              new_breathing_kmeans%hparam%n_clusters              = n_clusters
        if (present(n_clusters_breathing_in)) new_breathing_kmeans%hparam%n_clusters_breathing_in = n_clusters_breathing_in
    end function new_breathing_kmeans


    subroutine fit_breathing_kmeans(this, x)
        implicit none
        class(breathing_kmeans)  :: this
        real(kind=8), intent(in) :: x(:,:)

        integer(kind=8) :: c, n_new_centers
        integer(kind=8) :: x_shape(2)
        integer(kind=8), ALLOCATABLE :: indices(:)
        real(kind=8), ALLOCATABLE :: rmses(:)
        real(kind=8) :: rmse_best_
        real(kind=8), ALLOCATABLE :: centers_best_(:,:)
        real(kind=8), ALLOCATABLE :: new_cluster_centers(:,:)

        x_shape(:) = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)
        this%km = kmeans(n_clusters=this%hparam%n_clusters)
        call this%km%fit(x)

        n_new_centers = this%hparam%n_clusters_breathing_in
        rmse_best_ = this%km%score(x)
        centers_best_ = this%km%cluster_centers(:,:)

        do while (n_new_centers > 0_8)
            new_cluster_centers = this%breathing_in(x, n_new_centers)
            this%km = kmeans(n_clusters=this%hparam%n_clusters+n_new_centers)
            call this%km%fit(x, new_cluster_centers)




            n_new_centers = n_new_centers - 1
        end do
    end subroutine fit_breathing_kmeans


    function calc_rmse_per_centers(this, x)
        implicit none
        class(breathing_kmeans)  :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), ALLOCATABLE :: calc_rmse_per_centers(:)

        integer(kind=8) :: n, lbl
        integer(kind=8), allocatable :: labels(:), counters(:), uniq_labels(:)

        allocate(calc_rmse_per_centers(this%km%hparam%n_clusters))
        calc_rmse_per_centers(:) = 0d0

        labels = this%km%predict(x)

        do n=1, size(x, dim=1), 1
            lbl = labels(n)
            calc_rmse_per_centers(lbl) = calc_rmse_per_centers(lbl) &
                + sum((x(n,:) - this%km%cluster_centers(:,lbl))**2d0)
        end do
    end function calc_rmse_per_centers


    function breathing_in(this, x, n_new_centers)
        implicit none
        class(breathing_kmeans)     :: this
        real(kind=8), intent(in)    :: x(:,:)
        integer(kind=8), intent(in) :: n_new_centers
        real(kind=8), allocatable   :: breathing_in(:,:)
        real(kind=8), allocatable   :: tmp_breathing_in(:,:)

        real(kind=8) :: eps, sum_rmse
        real(kind=8), ALLOCATABLE :: rmses(:)
        real(kind=8), ALLOCATABLE :: offsets(:)
        integer(kind=8), ALLOCATABLE :: indices(:)
        integer(kind=8) :: i, cluster_idx, n_clusters

        if (allocated(breathing_in)) deallocate(breathing_in)

        eps = 0.01

        rmses = this%calc_rmse_per_centers(x)
        sum_rmse = sum(rmses(:)) / dble(size(x, dim=1))
        allocate(indices(size(rmses)))
        do i=1, size(rmses), 1
            indices(i) = i
        end do
        call quick_argsort(rmses, indices, size(rmses)+0_8)

        allocate(tmp_breathing_in(this%n_columns, size(rmses)+n_new_centers))
        tmp_breathing_in(:,1:size(rmses)) = this%km%cluster_centers(:,1:size(rmses))

        allocate(offsets(size(x, dim=2)))
        call RANDOM_NUMBER(offsets)
        offsets = offsets - 0.5d0

        n_clusters = size(rmses)+1
        do i=size(rmses), size(rmses)-n_new_centers+1, -1
            cluster_idx = indices(i)
            tmp_breathing_in(:,n_clusters) = this%km%cluster_centers(:,cluster_idx) + &
                eps * sum_rmse * offsets
            n_clusters = n_clusters + 1
        end do
        print*, shape(tmp_breathing_in)

        allocate(breathing_in(size(rmses)+n_new_centers, this%n_columns))
        breathing_in(:,:) = transpose(tmp_breathing_in)
    end function breathing_in


    function breathing_out(this, x)
        implicit none
        class(breathing_kmeans)     :: this
        real(kind=8), intent(in)    :: x(:,:)

    end function breathing_out

end module mod_breathing_kmeans
