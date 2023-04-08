module mod_local_outlier_factor
    use mod_const
    use mod_kdtree
    use mod_balltree
    use mod_nearest_neighbour
    use mod_hyperparameter
    implicit none

    type local_outlier_factor
        type(hparam_local_outlier_factor) :: hparam
        type(kdtree) :: kdt
        type(balltree) :: blt
        type(brute_force_search) :: brf

        real(kind=8), allocatable :: x_(:,:) ! training data
        type(neighbor_results)    :: res_    ! neighbor distance and index
        real(kind=8), allocatable :: lrd_(:) ! local reachability density
        real(kind=8), allocatable :: scores_(:) ! anomaly score
        real(kind=8), allocatable :: scores_scaled(:) ! scaled anomaly score
    contains
        procedure :: fit => fit_local_outlier_factor
        procedure :: compute_reachability_density
        procedure :: score_samples
    end type local_outlier_factor
    
    interface local_outlier_factor
        module procedure new_local_outlier_factor
    end interface local_outlier_factor

contains

    function new_local_outlier_factor(n_neighbors, algorithm, min_samples_in_leaf, kernel) result(obj)
        implicit none
        type(local_outlier_factor) :: obj
        integer(kind=8), optional, intent(in)  :: n_neighbors
        character(len=*), optional, intent(in) :: algorithm
        integer(kind=8), optional, intent(in)  :: min_samples_in_leaf
        character(len=*), optional, intent(in) :: kernel

        obj%hparam%algo_name = "local_outlier_factor"

        if ( present(n_neighbors) ) obj%hparam%n_neighbors = n_neighbors
        if ( present(algorithm) ) obj%hparam%algorithm = algorithm
        if ( present(min_samples_in_leaf) ) obj%hparam%min_samples_in_leaf = min_samples_in_leaf
        if ( present(kernel) ) obj%hparam%kernel = kernel

        call obj%hparam%validate_char_list("algorithm", obj%hparam%algorithm, neighbor_algorithm_list)
        call obj%hparam%validate_char_list("kernel", obj%hparam%kernel, kernel_list)
        obj%hparam%algorithm_int = obj%hparam%convert_char_to_int(obj%hparam%algorithm, neighbor_algorithm_list)
    end function new_local_outlier_factor


    subroutine fit_local_outlier_factor(this, x)
        implicit none
        class(local_outlier_factor) :: this
        real(kind=8), intent(in) :: x(:,:)
        real(kind=8) :: min_val, max_val, coef_
        integer(kind=8) :: n, n_samples

        if (this%hparam%algorithm_int == 1_8) then
            this%kdt = kdtree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call this%kdt%build_new(x)
            this%res_ = this%kdt%query(q=x, n_neighbors=this%hparam%n_neighbors+1)
        elseif (this%hparam%algorithm_int == 2_8) then
            this%blt = balltree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call this%blt%build(x)
            this%res_ = this%blt%query(q=x, n_neighbors=this%hparam%n_neighbors+1)
        elseif (this%hparam%algorithm_int == 3_8) then
            this%brf = brute_force_search()
            call this%brf%build(x)
            this%res_ = this%brf%query(q=x, n_neighbors=this%hparam%n_neighbors+1)
        end if

        call remove_nearest_neighbor(this%res_)

        n_samples = size(this%res_%indices)
        if (this%hparam%kernel=="linear") then
            ! skip
        elseif (this%hparam%kernel=="exponential") then
            coef_ = 1d0 / sqrt(2d0 * pi_)
            do n=1, n_samples, 1
                this%res_%distances(n)%dst = 2d0 * coef_ * ( exp(0.5d0) - exp(0.5d0 - this%res_%distances(n)%dst) )
            end do
        elseif (this%hparam%kernel=="epanechnikov") then
            coef_ = 3d0 / 4d0
            do n=1, n_samples, 1
                this%res_%distances(n)%dst = 2d0 * coef_ * ( 1d0 - (1d0 - this%res_%distances(n)%dst**2d0) )
            end do
        elseif (this%hparam%kernel=="tricubic") then
            coef_ = 71d0 / 80d0
            do n=1, n_samples, 1
                this%res_%distances(n)%dst = 2d0 * coef_ * ( 1d0 - (1d0 - this%res_%distances(n)%dst**3d0)**3d0 )
            end do
        end if

        call this%compute_reachability_density()

        this%scores_ = this%score_samples()
        min_val = minval(this%scores_)
        max_val = maxval(this%scores_)
        this%scores_scaled = (this%scores_-min_val) / (max_val-min_val)
    end subroutine fit_local_outlier_factor


    subroutine compute_reachability_density(this, res_, lrd_)
        implicit none
        class(local_outlier_factor), target :: this
        type(neighbor_results), target, optional :: res_
        real(kind=8), allocatable, optional :: lrd_(:)

        integer(kind=8) :: n_samples, n, idx, k
        real(kind=8) :: tmp_sum

        type(neighbor_results), pointer :: res_ptr
        real(kind=8), allocatable :: lrd_tmp(:)
        real(kind=8) :: core_dist, bet_dist

        
        res_ptr => this%res_
        if (present(res_)) res_ptr => res_

        n_samples = size(res_ptr%distances)
        allocate(lrd_tmp(n_samples))

        do n=1, n_samples, 1
            tmp_sum = 0d0
            do k=1, this%hparam%n_neighbors, 1
                idx = res_ptr%indices(n)%idx(k)
                bet_dist = res_ptr%distances(n)%dst(k)
                core_dist = this%res_%distances(idx)%dst(this%hparam%n_neighbors)
                tmp_sum = tmp_sum + maxval([bet_dist, core_dist])
            end do
            lrd_tmp(n) = tmp_sum/dble(this%hparam%n_neighbors)
        end do
        lrd_tmp = 1d0 / (lrd_tmp+1d-10)


        if (present(lrd_)) then
            allocate(lrd_, source=lrd_tmp)
        else
            if (allocated(this%lrd_)) deallocate(this%lrd_)
            allocate(this%lrd_, source=lrd_tmp)
        end if
    end subroutine compute_reachability_density


    function score_samples(this, x, normalize) result(scores)
        implicit none
        class(local_outlier_factor), target :: this
        real(kind=8), optional :: x(:,:)
        real(kind=8), allocatable :: scores(:)
        logical(kind=4), optional :: normalize

        type(neighbor_results), target:: res_
        real(kind=8), allocatable, target:: lrd_(:)
        type(neighbor_results), pointer :: res_ptr
        real(kind=8), pointer :: lrd_ptr(:)
        
        integer(kind=8) :: n_samples, n, idx, k
        real(kind=8) :: tmp_sum, min_val, max_val, coef_

        if (present(x)) then
            if (this%hparam%algorithm_int == 1_8) then
                res_ = this%kdt%query(q=x, n_neighbors=this%hparam%n_neighbors)
            elseif (this%hparam%algorithm_int == 2_8) then
                res_ = this%blt%query(q=x, n_neighbors=this%hparam%n_neighbors)
            elseif (this%hparam%algorithm_int == 3_8) then
                res_ = this%brf%query(q=x, n_neighbors=this%hparam%n_neighbors)
            end if

            n_samples = size(res_%indices)
            if (this%hparam%kernel=="linear") then
                ! skip
            elseif (this%hparam%kernel=="exponential") then
                coef_ = 1d0 / sqrt(2d0 * pi_)
                do n=1, n_samples, 1
                    res_%distances(n)%dst = 2d0 * coef_ * ( exp(0.5d0) - exp(0.5d0 - res_%distances(n)%dst) )
                end do
            elseif (this%hparam%kernel=="epanechnikov") then
                coef_ = 3d0 / 4d0
                do n=1, n_samples, 1
                    res_%distances(n)%dst = 2d0 * coef_ * ( 1d0 - (1d0 - res_%distances(n)%dst**2d0) )
                end do
            elseif (this%hparam%kernel=="tricubic") then
                coef_ = 71d0 / 80d0
                do n=1, n_samples, 1
                    res_%distances(n)%dst = 2d0 * coef_ * ( 1d0 - (1d0 - res_%distances(n)%dst**3d0)**3d0 )
                end do
            end if

            call this%compute_reachability_density(res_, lrd_)
    
            res_ptr => res_
        else
            res_ptr => this%res_
            allocate(lrd_, source=this%lrd_)
        end if

        n_samples = size(res_ptr%indices)
        allocate(scores(n_samples))

        do n=1, n_samples, 1
            tmp_sum = 0d0
            do k=1, this%hparam%n_neighbors, 1
                idx = res_ptr%indices(n)%idx(k)
                tmp_sum = tmp_sum + this%lrd_(idx)
            end do
            scores(n) = tmp_sum  / (lrd_(n)* this%hparam%n_neighbors)
        end do

        if (present(normalize)) then
            if (normalize) then
                min_val = minval(scores)
                max_val = maxval(scores)
                scores = (scores-min_val) / (max_val-min_val)
            end if
        end if
    end function score_samples


    subroutine remove_nearest_neighbor(res_)
        implicit none
        type(neighbor_results) :: res_

        integer(kind=8) :: n_samples, n

        n_samples = size(res_%indices)

        do n=1, n_samples, 1
            res_%indices(n)%idx = res_%indices(n)%idx(2:)
            res_%distances(n)%dst = res_%distances(n)%dst(2:)
        end do
    end subroutine remove_nearest_neighbor

end module mod_local_outlier_factor