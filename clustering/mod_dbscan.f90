module mod_dbscan
    use mod_nearest_neighbour
    use mod_hyperparameter
    use mod_kdtree
    use mod_balltree
    use mod_brute_force_search
    use mod_data_holder
    implicit none

    type dbscan
        type(hparam_dbscan) :: hparam
        integer(kind=8), allocatable :: labels_(:)
        logical(kind=4), allocatable :: is_visited(:)
        type(kdtree) :: ktree
        type(balltree) :: btree
        type(brute_force_search) :: brute
    contains
        procedure, pass :: fit_dbscan_x
        procedure, pass :: fit_dbscan_dholder
        generic :: fit => fit_dbscan_x, fit_dbscan_dholder
        procedure :: expanding
    end type dbscan

    !> An interface to create new 'dbscan' object.
    interface dbscan
        module procedure :: new_dbscan
    end interface dbscan

contains


    function new_dbscan(n_neighbors, radius, neighbour_algo, min_samples_in_leaf)
        implicit none
        type(dbscan)               :: new_dbscan
        type(dbscan)               :: tmp
        integer(kind=8), optional  :: n_neighbors
        real(kind=8), optional     :: radius
        character(len=*), optional :: neighbour_algo
        integer(kind=8), optional  :: min_samples_in_leaf

        character(len=256) :: neighbour_algo_list(3)

        tmp%hparam%algo_name = "dbscan"

        neighbour_algo_list(1) = "kdtree"
        neighbour_algo_list(2) = "balltree"
        neighbour_algo_list(3) = "brute_force_search"

        if ( present(n_neighbors) ) tmp%hparam%n_neighbors = n_neighbors
        if ( present(radius) ) tmp%hparam%radius = radius
        if ( present(neighbour_algo) ) tmp%hparam%neighbour_algo = neighbour_algo
        if ( present(min_samples_in_leaf) ) tmp%hparam%min_samples_in_leaf = min_samples_in_leaf

        tmp%hparam%neighbour_algo_int = tmp%hparam%convert_char_to_int(tmp%hparam%neighbour_algo, neighbour_algo_list)

        new_dbscan = tmp
    end function new_dbscan


    subroutine fit_dbscan_x(this, x)
        implicit none
        class(dbscan) :: this
        real(kind=8), intent(in) :: x(:,:)
        ! real(kind=8), intent(in) :: dst(:,:)
        ! integer(kind=8), intent(in) :: idx(:,:)

        integer(kind=8) :: xshape(2), n_samples, n_columns
        integer(kind=8) :: i, j, label
        type(kdtree), target   :: ktree
        type(balltree), target :: btree
        type(brute_force_search), target :: brf
        type(neighbor_results), target  :: res
        type(neighbor_results), pointer :: res_ptr

        real(kind=8) :: diff_sum
        real(kind=8), allocatable :: query(:,:)
        integer(kind=8) :: diff_count

        logical(kind=4), allocatable :: is_visited(:)
        logical(kind=4) :: go_next_cluster

        
        if (allocated(this%labels_)) deallocate(this%labels_)
        xshape(:) = shape(x)
        n_samples = xshape(1)
        n_columns = xshape(2)
        allocate(this%labels_(n_samples))
        allocate(query(1, n_columns))
        this%labels_(:) = -1

        if (this%hparam%neighbour_algo_int .eq. 1_8) then
            this%ktree = kdtree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call this%ktree%build(x)
            res = this%ktree%query(x, radius=this%hparam%radius)
        elseif (this%hparam%neighbour_algo_int .eq. 2_8) then
            this%btree = balltree(min_samples_in_leaf=this%hparam%min_samples_in_leaf)
            call this%btree%build(x)
            res = this%btree%query(x, radius=this%hparam%radius)
        else
            this%brute = brute_force_search()
            call this%brute%build(x)
            res = this%brute%query(x, radius=this%hparam%radius)
        end if
        res_ptr => res

        if (allocated(this%is_visited)) deallocate(this%is_visited)
        allocate(this%is_visited(n_samples)); this%is_visited(:) = f_
        label = 1
        do i=1, n_samples, 1
            if (this%is_visited(i)) cycle
            this%is_visited(i) = t_
            this%labels_(i) = label
            go_next_cluster = this%expanding(res_ptr, query_idx=i, label=label)
            if ( go_next_cluster ) then
                label = label + 1
            end if
        end do
    end subroutine fit_dbscan_x

    
    subroutine fit_dbscan_dholder(this, dholder)
        implicit none
        class(dbscan) :: this
        type(data_holder), intent(in) :: dholder
        call this%fit_dbscan_x(dholder%x_ptr%x_r8_ptr)
    end subroutine fit_dbscan_dholder

    function expanding(this, res_ptr, query_idx, label)
        implicit none
        logical(kind=4) :: expanding
        class(dbscan) :: this
        type(neighbor_results), pointer :: res_ptr
        integer(kind=8), intent(in) :: query_idx
        integer(kind=8), intent(in) :: label

        integer(kind=8), allocatable :: seeds(:), seeds_new(:)
        integer(kind=8) :: idx

        seeds = res_ptr%indices(query_idx)%idx
        if ( size(seeds) < this%hparam%n_neighbors ) then
            this%labels_(query_idx) = -1
            expanding = f_
            return
        end if

        do while (size(seeds)>0)
            idx = seeds(1)
            if ( .not. this%is_visited(idx) ) then
                this%is_visited(idx) = t_
                this%labels_(idx) = label
                seeds_new = res_ptr%indices(idx)%idx
                if (size(seeds_new) >= this%hparam%n_neighbors) then
                    seeds = [seeds, seeds_new]
                end if
            end if
            seeds = [seeds(2:)]
        end do
        expanding = t_
    end function expanding






end module mod_dbscan