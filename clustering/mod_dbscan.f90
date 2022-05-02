module mod_dbscan
    use mod_nearest_neighbour
    use mod_hyperparameter
    use mod_kdtree
    use mod_balltree
    use mod_brute_force_search
    implicit none

    type dbscan
        type(hparam_dbscan) :: hparam
        integer(kind=8), allocatable :: labels_(:)
    contains
        procedure :: fit => fit_dbscan
        procedure :: expanding_cluster
    end type dbscan

    !> An interface to create new 'dbscan' object.
    interface dbscan
        module procedure :: new_dbscan
    end interface dbscan

contains


    function new_dbscan(min_nearest_samples, min_radius, neighbour_algo, min_samples_in_leaf)
        implicit none
        type(dbscan)               :: new_dbscan
        type(dbscan)               :: tmp
        integer(kind=8), optional  :: min_nearest_samples
        real(kind=8), optional     :: min_radius
        character(len=*), optional :: neighbour_algo
        integer(kind=8), optional  :: min_samples_in_leaf

        character(len=256) :: neighbour_algo_list(3)

        tmp%hparam%algo_name = "dbscan"

        neighbour_algo_list(1) = "kdtree"
        neighbour_algo_list(2) = "balltree"
        neighbour_algo_list(3) = "brute_force_search"

        if ( present(min_nearest_samples) ) tmp%hparam%min_nearest_samples = min_nearest_samples
        if ( present(min_radius) ) tmp%hparam%min_radius = min_radius
        if ( present(neighbour_algo) ) tmp%hparam%neighbour_algo = neighbour_algo
        if ( present(min_samples_in_leaf) ) tmp%hparam%min_samples_in_leaf = min_samples_in_leaf

        tmp%hparam%neighbour_algo_int = tmp%hparam%convert_char_to_int(tmp%hparam%neighbour_algo, neighbour_algo_list)

        new_dbscan = tmp
    end function new_dbscan


    subroutine fit_dbscan(this, x)
        implicit none
        class(dbscan) :: this
        real(kind=8), intent(in) :: x(:,:)
        ! real(kind=8), intent(in) :: dst(:,:)
        ! integer(kind=8), intent(in) :: idx(:,:)

        integer(kind=8) :: xshape(2), n_samples, n_columns
        integer(kind=8) :: i, j, label
        type(kdtree)   :: ktree
        type(balltree) :: btree
        type(brute_force_search) :: brf
        type(neighbor_results), target  :: res
        type(neighbor_results), pointer :: res_ptr

        real(kind=8) :: diff_sum
        integer(kind=8) :: diff_count

        logical(kind=4), allocatable :: is_visited(:)

        if (allocated(this%labels_)) deallocate(this%labels_)
        xshape(:) = shape(x)
        n_samples = xshape(1)
        n_columns = xshape(2)
        allocate(this%labels_(n_samples))
        this%labels_(:) = -1

        if (this%hparam%neighbour_algo_int .eq. 1_8) then
            ktree = kdtree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call ktree%build(x)
            res = ktree%query(x, radius=this%hparam%min_radius)
        elseif (this%hparam%neighbour_algo_int .eq. 2_8) then
            btree = balltree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call btree%build(x)
            res = btree%query(x, radius=this%hparam%min_radius)
        else
            brf = brute_force_search()
            call brf%build(x)
            res = brf%query(x, radius=this%hparam%min_radius)
        end if
        res_ptr => res

        allocate(is_visited(n_samples))
        label = 1
        is_visited(:) = f_
        do i=1, n_samples, 1
            if ( is_visited(i) ) cycle
            if ( size(res%indices(i)%idx) >= this%hparam%min_nearest_samples ) then
                call this%expanding_cluster(i, label, is_visited, n_samples, res_ptr)
                label = label + 1
            end if
        end do
    end subroutine fit_dbscan


    recursive subroutine expanding_cluster(this, point_idx, label, is_visited, n_samples, res_ptr)
        implicit none
        class(dbscan) :: this
        integer(kind=8), intent(in) :: point_idx
        integer(kind=8), intent(in) :: label, n_samples
        logical(kind=4), intent(inout) :: is_visited(n_samples)
        type(neighbor_results), pointer, intent(in) :: res_ptr
        integer(kind=8) :: i, idx

        integer(kind=8), allocatable :: seeds(:)

        if (is_visited(point_idx)) return
        allocate(seeds(0))

        seeds = [seeds, res_ptr%indices(point_idx)%idx]
        do while ( size(seeds)>0 )
            idx = seeds(1)
            if (.not. is_visited(idx)) then
                is_visited(idx) = t_
                seeds = [seeds, res_ptr%indices(idx)%idx]
                if ( size(res_ptr%indices(idx)%idx) >= this%hparam%min_nearest_samples ) then
                    this%labels_(idx) = label
                end if
            end if
            seeds = [seeds(2:)]
        end do
    end subroutine expanding_cluster


end module mod_dbscan