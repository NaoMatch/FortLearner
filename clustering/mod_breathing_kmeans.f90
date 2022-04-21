module mod_breathing_kmeans
    use mod_base_kmeans
    use mod_kmeans
    use mod_hyperparameter
    use mod_sort
    use mod_common
    use mod_const
    use mod_stats
    implicit none

    type breathing_kmeans
        character(len=256) :: algo_name = "breathing_kmeans" !< algorithm name

        type(kmeans) :: km
        integer(kind=8) :: n_samples, n_columns
        type(hparam_breathing_kmeans) :: hparam
    contains
        procedure :: fit => fit_breathing_kmeans
        procedure :: predict => predict_breathing_kmeans
        procedure :: calc_rmse_per_centers

        procedure :: breathing_in
        procedure :: breathing_out
        procedure :: utility
        procedure :: score

        procedure :: dump     => dump_breathing_kmeans
        procedure :: load     => load_breathing_kmeans
    end type breathing_kmeans

    interface breathing_kmeans
        module procedure :: new_breathing_kmeans
    end interface breathing_kmeans

contains

    subroutine dump_breathing_kmeans(this, file_name)
        class(breathing_kmeans) :: this
        character(len=*) :: file_name
        call this%km%dump(file_name=file_name)
    end subroutine dump_breathing_kmeans
    
    !> A subroutine to load 'kmeans' object
    !> If not fitted, cannot load.
    !! \param file_name loading file name
    subroutine load_breathing_kmeans(this, file_name)
        implicit none
        class(breathing_kmeans)    :: this
        character(len=*) :: file_name
        call this%km%load(file_name=file_name)
    end subroutine load_breathing_kmeans


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
        real(kind=8) :: rmse_best_, rmse_new_
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
            ! Breathing in training
            new_cluster_centers = this%breathing_in(x, n_new_centers)
            this%km = kmeans(n_clusters=this%hparam%n_clusters+n_new_centers)
            call this%km%fit(x, new_cluster_centers)

            ! Breathing out training
            new_cluster_centers = this%breathing_out(x, n_remove_centers=n_new_centers)
            this%km = kmeans(n_clusters=this%hparam%n_clusters)
            call this%km%fit(x, new_cluster_centers)

            ! Check
            rmse_new_ = this%km%score(x)
            if (rmse_new_ <= rmse_best_*(1d0 - this%hparam%tolerance)) then
                rmse_best_ = rmse_new_
                centers_best_ = this%km%cluster_centers
            else
                n_new_centers = n_new_centers - 1
            end if
        end do
        this%km%cluster_centers = centers_best_
    end subroutine fit_breathing_kmeans


    function predict_breathing_kmeans(this, x)
        implicit none
        class(breathing_kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8), ALLOCATABLE :: predict_breathing_kmeans(:)
        predict_breathing_kmeans = this%km%predict(x)
    end function predict_breathing_kmeans



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

        allocate(breathing_in(size(rmses)+n_new_centers, this%n_columns))
        breathing_in(:,:) = transpose(tmp_breathing_in)
    end function breathing_in


    function breathing_out(this, x, n_remove_centers)
        implicit none
        class(breathing_kmeans)     :: this
        real(kind=8), intent(in)    :: x(:,:)
        real(kind=8), allocatable   :: breathing_out(:,:)
        integer(kind=8)             :: n_remove_centers
        
        real(kind=8), allocatable    :: utilities(:), c_sq_sum_row(:)
        real(kind=8), allocatable    :: distance_between_centroids(:,:)
        integer(kind=8), allocatable :: nearest_centroid_indices(:,:)
        integer(kind=8), allocatable :: labels(:)
        integer(kind=8) :: i, j, c_idx, c_idx_near

        integer(kind=8), allocatable :: Freeze(:)
        integer(kind=8), allocatable :: Remove(:)
        logical(kind=1), allocatable :: Freeze_bool(:)
        logical(kind=1), allocatable :: Remove_bool(:)

        allocate(Freeze(0), Remove(0))
        allocate(Freeze_bool(this%km%hparam%n_clusters))
        allocate(Remove_bool(this%km%hparam%n_clusters))

        allocate( breathing_out(this%hparam%n_clusters, this%n_columns) )
        allocate( labels(this%km%hparam%n_clusters) )
        allocate( c_sq_sum_row(this%km%hparam%n_clusters) ); c_sq_sum_row(:) = 0d0
        allocate( distance_between_centroids(this%km%hparam%n_clusters, this%km%hparam%n_clusters) )
        allocate( nearest_centroid_indices(this%km%hparam%n_clusters, this%km%hparam%n_clusters) )

        do i=1, this%km%hparam%n_clusters, 1
            labels(i) = i
            do j=1, this%km%hparam%n_clusters, 1
                nearest_centroid_indices(i,j) = j
            end do
        end do

        utilities = this%utility(x)
        call quick_argsort(utilities, labels, this%km%hparam%n_clusters)

        c_sq_sum_row(:) = sum(this%km%cluster_centers(:,:)**2d0, dim=1)
        distance_between_centroids(:,:) = matmul(transpose(this%km%cluster_centers), this%km%cluster_centers)
        distance_between_centroids(:,:) = -2d0*distance_between_centroids(:,:) + &
                        spread(c_sq_sum_row, dim=2, ncopies=this%km%hparam%n_clusters) + &
                        spread(c_sq_sum_row, dim=1, ncopies=this%km%hparam%n_clusters)

        do i=1, this%km%hparam%n_clusters, 1
            call quick_argsort(distance_between_centroids(i,:), nearest_centroid_indices(i,:), this%km%hparam%n_clusters)
        end do

        Freeze_bool(:) = f_
        Remove_bool(:) = f_
        do i=1, this%km%hparam%n_clusters, 1
            c_idx = labels(i)
            if ( .not. Freeze_bool(c_idx) ) then
                Remove_bool(c_idx) = t_
                c_idx_near = nearest_centroid_indices(c_idx,2)
                if (count(Freeze_bool) + n_remove_centers .lt. this%km%hparam%n_clusters) then
                    Freeze_bool(c_idx_near) = t_
                end if

                if ( count(Remove_bool) .eq. n_remove_centers ) exit
            end if
        end do

        do i=1, this%km%hparam%n_clusters, 1
            if ( .not. Remove_bool(i) ) then
                Freeze = [Freeze, i]
            end if
        end do
        breathing_out(:,:) = transpose(this%km%cluster_centers(:,Freeze))
    end function breathing_out

    function utility(this, x)
        class(breathing_kmeans)  :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8), allocatable :: utility(:)

        real(kind=8), allocatable :: utility_all(:)
        integer(kind=8) :: x_shape(2), n_samples, n_columns, n_clusters, n, top_n
        logical(kind=4) :: left_align
        integer(kind=8), ALLOCATABLE :: labels(:), label(:)
        real(kind=8), ALLOCATABLE :: dist_all(:,:)
        real(kind=8), ALLOCATABLE :: x_sq_sum_row(:), c_sq_sum_row(:)

        x_shape(:) = shape(x)
        n_samples  = x_shape(1)
        n_columns  = x_shape(2)
        n_clusters = this%km%hparam%n_clusters

        allocate( utility_all(n_samples) )
        allocate( dist_all(n_samples, n_clusters) )
        allocate( x_sq_sum_row(n_samples) );  x_sq_sum_row(:) = 0d0
        allocate( c_sq_sum_row(n_clusters) ); c_sq_sum_row(:) = 0d0
        call matrix_sqsum_row(x, x_sq_sum_row, n_samples, n_columns, parallel=f_)
        c_sq_sum_row(:) = sum(this%km%cluster_centers(:,:)**2d0, dim=1)
        dist_all(:,:) = MATMUL(x, this%km%cluster_centers)
        dist_all(:,:) = -2d0*dist_all(:,:) + &
                        spread(x_sq_sum_row, dim=2, ncopies=n_clusters) + &
                        spread(c_sq_sum_row, dim=1, ncopies=n_samples)

        top_n = 2_8
        left_align = f_
        labels = minloc(dist_all, dim=2)
        do n=1, n_samples, 1
            call quick_sort(dist_all(n,:), n_clusters)
        end do
        utility_all(:) = dist_all(:,2) - dist_all(:,1)
        call groupby_sum(label, labels, utility, utility_all, n_samples)
    end function utility

    function score(this, x)
        implicit none
        class(breathing_kmeans) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8) :: score
        score = this%km%score(x)
    end function score 




end module mod_breathing_kmeans
