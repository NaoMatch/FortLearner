module mod_balltree
    !$ use omp_lib
    use mod_const
    use mod_common
    use mod_linalg
    use mod_timer
    use mod_hyperparameter
    use mod_nearest_neighbour, only: neighbor_results, base_node_for_nearest_neighbor
    implicit none

    !> Type of Ball for BallTree.
    type, extends(base_node_for_nearest_neighbor) :: ball
        ! Ball Info
        integer(kind=8) :: split_algo_int !< ball splitting algorithm. 1='most_spread', 2='farthest_two', 3='1st_pc_axis'(not implemented)
        real(kind=8) :: radius !< ball radius
        real(kind=8) :: radius_sq !< ball radius square
        real(kind=8), allocatable :: center(:) !< ball center coordinate

        ! Additional Info
        real(kind=8), allocatable :: x(:,:) !< leaf node sample data
        real(kind=8), allocatable :: x_sq_sum(:) !< squared sum of x
        real(kind=8), allocatable :: point_a(:) !< farthes point
        real(kind=8), allocatable :: point_b(:) !< farthes point
        real(kind=8), allocatable :: b_minus_a(:) !< difference between a and b
        real(kind=8) :: a_sq_sum, ab_dot, dist_ab !< squared sum of a, dot product of a and b, distance between a and b.

        ! Parent and children node pointers
        type(ball), pointer :: ball_p_ptr => null() !< pointer to parent ball
        type(ball), pointer :: ball_l_ptr => null() !< pointer to child left ball
        type(ball), pointer :: ball_r_ptr => null() !< pointer to child right ball
    contains
        procedure :: init => init_ball
        procedure :: info => info_ball
        procedure :: split => split_ball

        procedure :: choose_farthest_two_points
    end type ball

    type balltree
        integer(kind=8) :: n_samples, n_columns !< input data shape
        character(len=256) :: split_algo = "most_spread" !< most_spread, farthest_two, 1st_pc_axis
        integer(kind=8)    :: split_algo_int = 1 !< most_spread, farthest_two, 1st_pc_axis
    
        type(ball), pointer :: root_ball_ptr !< pointer to root ball
        integer(kind=8)  :: min_samples_in_leaf=32_8 !< minimum number of samples in leaf ball

        integer(kind=8), allocatable :: x_sq_sum(:) !< squared sum of x by row
    contains
        procedure :: build => build_balltree
        procedure :: build_balltree_rec

        procedure :: query => query_balltree
        procedure :: query_balltree_n_neighbors_rec
        procedure :: query_balltree_radius_rec

        procedure :: dump => dump_balltree
        procedure :: dump_ball
        procedure :: load => load_balltree
        procedure :: load_ball
    end type balltree

    !> New 'balltree' constructor.
    interface balltree
        module procedure :: new_balltree
    end interface balltree


contains

    subroutine dump_balltree(this, file_name)
        implicit none
        class(balltree) :: this
        character(len=*), intent(in) ::file_name
        integer(kind=8) :: newunit
        open(newunit=newunit, file=file_name, form='unformatted', status='replace')
        write(newunit) this%n_samples
        write(newunit) this%n_columns
        write(newunit) this%split_algo
        write(newunit) this%split_algo_int
        call this%dump_ball(this%root_ball_ptr, newunit)
        write(newunit) this%min_samples_in_leaf
        write(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine dump_balltree

    recursive subroutine dump_ball(this, ball_ptr, newunit)
        implicit none
        class(balltree)                 :: this
        type(ball), pointer, intent(in) :: ball_ptr
        integer(kind=8), intent(in)     :: newunit

        logical(kind=4) :: is_terminal
        integer(kind=8) :: x_shape(2)
        type(ball), pointer :: ball_l_ptr, ball_r_ptr

        is_terminal = .not. associated(ball_ptr%ball_l_ptr)
        ! Ball Info
        write(newunit) is_terminal
        write(newunit) ball_ptr%is_root
        write(newunit) ball_ptr%is_leaf
        write(newunit) ball_ptr%is_left
        write(newunit) ball_ptr%depth
        write(newunit) ball_ptr%n_samples
        write(newunit) ball_ptr%n_columns
        write(newunit) ball_ptr%min_samples_in_leaf
        write(newunit) ball_ptr%split_algo_int
        write(newunit) ball_ptr%radius
        write(newunit) ball_ptr%radius_sq
        write(newunit) ball_ptr%center(:)
        
        if (is_terminal) then
            ! Additional Info
            x_shape(:) = shape(ball_ptr%x)
            write(newunit) x_shape(:)
            write(newunit) ball_ptr%indices(:)
            write(newunit) ball_ptr%x(:,:)
            write(newunit) ball_ptr%x_sq_sum(:)    
        else
            call this%dump_ball(ball_ptr%ball_l_ptr, newunit)
            call this%dump_ball(ball_ptr%ball_r_ptr, newunit)
        end if
    end subroutine dump_ball

    subroutine load_balltree(this, file_name)
        implicit none
        class(balltree)                 :: this
        character(len=*), intent(in) ::file_name
        integer(kind=8) :: newunit
        open(newunit=newunit, file=file_name, form='unformatted')
        read(newunit) this%n_samples
        read(newunit) this%n_columns
        read(newunit) this%split_algo
        read(newunit) this%split_algo_int
        if (associated(this%root_ball_ptr)) nullify(this%root_ball_ptr)
        allocate(this%root_ball_ptr)
        call this%load_ball(this%root_ball_ptr, newunit)
        read(newunit) this%min_samples_in_leaf
        read(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine load_balltree

    recursive subroutine load_ball(this, ball_ptr, newunit)
        implicit none
        class(balltree)                    :: this
        type(ball), pointer, intent(inout) :: ball_ptr
        integer(kind=8), intent(in)        :: newunit

        logical(kind=4) :: is_terminal
        integer(kind=8) :: x_shape(2)
        integer(kind=8), save :: i=0

        is_terminal = .not. associated(ball_ptr%ball_l_ptr)
        x_shape(:) = shape(ball_ptr%x)

        ! Ball Info
        read(newunit) is_terminal
        read(newunit) ball_ptr%is_root
        read(newunit) ball_ptr%is_leaf
        read(newunit) ball_ptr%is_left
        read(newunit) ball_ptr%depth
        read(newunit) ball_ptr%n_samples
        read(newunit) ball_ptr%n_columns
        read(newunit) ball_ptr%min_samples_in_leaf
        read(newunit) ball_ptr%split_algo_int
        read(newunit) ball_ptr%radius
        read(newunit) ball_ptr%radius_sq
        allocate(ball_ptr%center(this%n_columns))
        read(newunit) ball_ptr%center(:)
        
        allocate(ball_ptr%ball_l_ptr)
        allocate(ball_ptr%ball_r_ptr)
        allocate(ball_ptr%ball_l_ptr%ball_p_ptr)
        allocate(ball_ptr%ball_r_ptr%ball_p_ptr)
        ball_ptr%ball_l_ptr%ball_p_ptr => ball_ptr
        ball_ptr%ball_r_ptr%ball_p_ptr => ball_ptr

        if (is_terminal) then
            nullify(ball_ptr%ball_l_ptr)
            nullify(ball_ptr%ball_r_ptr)
            ! Additional Info
            read(newunit) x_shape(:)
            allocate(ball_ptr%indices(ball_ptr%n_samples))
            read(newunit) ball_ptr%indices(:)
            allocate(ball_ptr%x(x_shape(1), x_shape(2)))
            allocate(ball_ptr%x_sq_sum(x_shape(1)))
            read(newunit) ball_ptr%x(:,:)
            read(newunit) ball_ptr%x_sq_sum(:)    
        else
            call this%load_ball(ball_ptr%ball_l_ptr, newunit)
            call this%load_ball(ball_ptr%ball_r_ptr, newunit)
        end if
    end subroutine load_ball

    ! ----------------------------------------------------------------------------------
    ! For Ball Tree
    ! ----------------------------------------------------------------------------------

    !> New 'balltree' constructor.
    !! \param min_samples_in_leaf minimum number of samples in leaf ball. Must be greater than 1 and smaller than number of input data. 
    !! \param split_algo ball splitting algorithm. 'most_spread', 'farthest_two', '1st_pc_axis'(not implemented).
    function new_balltree(min_samples_in_leaf, split_algo)
        implicit none
        type(balltree) :: new_balltree
        integer(kind=8), optional :: min_samples_in_leaf
        character(len=*), optional :: split_algo

        character(len=256) :: algo_list(3)
        type(hparam_base)  :: tmp

        algo_list(1) = "most_spread"
        algo_list(2) = "farthest_two"
        algo_list(3) = "1st_pc_axis"

        if ( present(min_samples_in_leaf) ) new_balltree%min_samples_in_leaf = min_samples_in_leaf
        if ( present(split_algo) ) new_balltree%split_algo = split_algo

        new_balltree%split_algo_int = tmp%convert_char_to_int(new_balltree%split_algo, algo_list)
    end function new_balltree


    !> Building 'balltree'.
    !! \param x input data.
    subroutine build_balltree(this, x)
        implicit none
        class(balltree) :: this
        real(kind=8), target, intent(in) :: x(:,:)

        real(kind=8), pointer :: x_ptr(:,:)
        integer(kind=8) :: x_shape(2), ball_idx
        type(ball), target :: root_ball
        integer(kind=8), allocatable :: times(:)

        allocate(times(6))
        times(:) = 0

        x_ptr => x
        x_shape(:) = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        if ( ASSOCIATED(this%root_ball_ptr) ) nullify( this%root_ball_ptr )
        allocate( this%root_ball_ptr )
        call this%root_ball_ptr%init(t_, f_, 0_8, this%n_samples, this%n_columns, this%min_samples_in_leaf, &
            this%split_algo_int)

        ball_idx = 0_8
        call this%build_balltree_rec(this%root_ball_ptr, x_ptr, ball_idx, times)

        ! print*, '============================================================='
        ! print*, "Calculate Centroid    : ", times(1)
        ! print*, "Calculate Radius      : ", times(2)
        ! print*, "Get Most Spread Dim   : ", times(3)
        ! print*, "Get Pivot Position    : ", times(4)
        ! print*, "Initialize Child Balls: ", times(5)
        ! print*, "Allocate Indices      : ", times(6)
    end subroutine build_balltree

    
    !> Building 'balltree' recursively using in 'build_balltree'.
    !! \param ball_ptr pointer to current 'ball'
    !! \param x_ptr pointer to 'x'
    !! \param ball_idx 
    subroutine build_balltree_rec(this, ball_ptr, x_ptr, ball_idx, times)
        implicit none
        class(balltree) :: this
        type(ball), pointer, intent(inout) :: ball_ptr
        real(kind=8), pointer, intent(in)  :: x_ptr(:,:)
        integer(kind=8), intent(inout)     :: ball_idx
        integer(kind=8), intent(inout)     :: times(:)

        ball_ptr%idx = ball_idx

        call ball_ptr%split(x_ptr, times)

        if (this%min_samples_in_leaf > ball_ptr%n_samples .or. ball_ptr%radius .eq. 0d0) then
            ball_ptr%is_leaf = t_
            nullify(ball_ptr%ball_l_ptr)
            nullify(ball_ptr%ball_r_ptr)

            allocate( ball_ptr%x(ball_ptr%n_samples, ball_ptr%n_columns) )
            ball_ptr%x(:,:) = x_ptr(ball_ptr%indices,:)

            allocate( ball_ptr%x_sq_sum(ball_ptr%n_samples) )
            call matrix_sqsum_row(ball_ptr%x, ball_ptr%x_sq_sum, ball_ptr%n_samples, ball_ptr%n_columns, parallel=f_)
            ! call ball_ptr%info()
            return
        end if

        ball_idx = ball_idx + 1
        call this%build_balltree_rec(ball_ptr%ball_l_ptr, x_ptr, ball_idx, times)

        ball_idx = ball_idx + 1
        call this%build_balltree_rec(ball_ptr%ball_r_ptr, x_ptr, ball_idx, times)
    end subroutine build_balltree_rec


    function query_balltree(this, q, n_neighbors, radius)
        implicit none
        class(balltree)                  :: this
        real(kind=8), target, intent(in) :: q(:,:)
        integer(kind=8), optional        :: n_neighbors
        real(kind=8), optional           :: radius
        type(neighbor_results), target     :: query_balltree

        integer(kind=8) :: q_shape(2), n_samples, n_columns, n
        real(kind=8) :: radius_sq, q_sq_sum
        real(kind=8), pointer :: q_ptr(:,:)
        type(neighbor_results), pointer :: res_ptr
        real(kind=8), allocatable :: distances(:), q_i(:)
        integer(kind=8), allocatable :: indices(:)

        q_ptr => q
        q_shape(:) = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)

        res_ptr => query_balltree
        allocate( query_balltree%indices(n_samples) )
        allocate( query_balltree%distances(n_samples) )

        if (present(n_neighbors)) then
            do n=1, n_samples, 1
                allocate( query_balltree%indices(n)%idx(n_neighbors) )
                allocate( query_balltree%distances(n)%dst(n_neighbors) )
            end do

            allocate(q_i(n_columns))
            !$omp parallel num_threads(4)
            !$omp do private(n, distances, indices, q_i, q_sq_sum)
            do n=1, n_samples, 1
                allocate( distances(n_neighbors), indices(n_neighbors) )
                distances(:) = huge(0d0)
                indices(:) = -2_8
                q_i(:) = q_ptr(n,:)
                q_sq_sum = sum( q_i(:)**2d0 )
                call this%query_balltree_n_neighbors_rec(distances, indices, &
                    this%root_ball_ptr, q_i, q_sq_sum, n_samples, n_columns, n_neighbors)
                query_balltree%distances(n)%dst = sqrt(distances(:))
                query_balltree%indices(n)%idx = indices(:)
                deallocate( distances, indices )
            end do
            !$omp end do
            !$omp end parallel
        elseif (present(radius)) then
            radius_sq = radius**2d0

            do n=1, n_samples, 1
                allocate( query_balltree%indices(n)%idx(0) )
                allocate( query_balltree%distances(n)%dst(0) )
            end do

            allocate(q_i(n_columns))
            !!$omp parallel num_threads(4)
            !!$omp do private(n, distances, indices, q_i, q_sq_sum)
            do n=1, n_samples, 1
                allocate( distances(0), indices(0) )
                q_i(:) = q_ptr(n,:)
                q_sq_sum = sum( q_i(:)**2d0 )
                call this%query_balltree_radius_rec(distances, indices, &
                    this%root_ball_ptr, q_i, q_sq_sum, n_samples, n_columns, radius_sq)
                query_balltree%distances(n)%dst = [query_balltree%distances(n)%dst, sqrt(distances(:))]
                query_balltree%indices(n)%idx   = [query_balltree%indices(n)%idx, indices(:)]
                call quick_argsort(query_balltree%distances(n)%dst, query_balltree%indices(n)%idx, &
                        size(query_balltree%indices(n)%idx)+0_8)
                deallocate( distances, indices )
            end do
            !!$omp end do
            !!$omp end parallel
        end if
    end function query_balltree


    recursive subroutine query_balltree_n_neighbors_rec(this, distances, indices, root_ball_ptr, &
        q_i, q_sq_sum, n_samples, n_columns, n_neighbors)
        implicit none
        class(balltree)                   :: this
        real(kind=8), intent(inout)       :: distances(n_neighbors)
        integer(kind=8), intent(inout)    :: indices(n_neighbors)
        type(ball), pointer               :: root_ball_ptr
        real(kind=8), intent(in)          :: q_i(n_columns), q_sq_sum
        integer(kind=8), intent(in)       :: n_samples, n_columns
        integer(kind=8), intent(in)       :: n_neighbors

        type(ball), pointer :: ball_near, ball_far

        real(kind=8) :: distance_q_and_c !< distance between query and ball center
        real(kind=8) :: distance_q_and_c_l, distance_q_and_c_r !< distance between query and ball center left and right
        real(kind=8), allocatable :: distance_q_and_s(:) !< distance between query and leaf ball samples
        real(kind=8), allocatable :: tmp_d(:)
        integer(kind=8), allocatable :: tmp_i(:)
        integer(kind=8) :: i, n


        distance_q_and_c = sum( (q_i(:) - root_ball_ptr%center(:))**2d0 ) 

        if ( distance_q_and_c .gt. root_ball_ptr%radius_sq + maxval(distances(:)) ) then
            return
        elseif ( root_ball_ptr%is_leaf ) then
            allocate( distance_q_and_s(root_ball_ptr%n_samples) )
            distance_q_and_s(:) = 0d0
            call multi_mat_vec(root_ball_ptr%x, q_i, distance_q_and_s, &
                root_ball_ptr%n_samples, root_ball_ptr%n_columns)
            distance_q_and_s(:) = -2d0*distance_q_and_s(:) &
                + root_ball_ptr%x_sq_sum(:) + q_sq_sum
            if ( maxval(distances(:)) < minval(distance_q_and_s(:)) ) then
                ! skip
            else
                allocate(tmp_i(root_ball_ptr%n_samples))
                do i=1, root_ball_ptr%n_samples
                    tmp_i(i) = root_ball_ptr%indices(i)
                end do
                tmp_d = distances(:)

                tmp_d = [distance_q_and_s, distances]
                tmp_i = [tmp_i, indices]
                n = minval((/size(tmp_d)+0_8, n_neighbors/))
                call quick_argsort(tmp_d, tmp_i, size(tmp_d)+0_8)
                distances(1:n) = tmp_d(1:n)
                indices(1:n) = tmp_i(1:n)
            end if
        else
            distance_q_and_c_l = sum( (q_i(:) - root_ball_ptr%ball_l_ptr%center(:))**2d0 )
            distance_q_and_c_r = sum( (q_i(:) - root_ball_ptr%ball_r_ptr%center(:))**2d0 )
            if ( distance_q_and_c_l < distance_q_and_c_r ) then
                ball_near => root_ball_ptr%ball_l_ptr
                ball_far  => root_ball_ptr%ball_r_ptr
            else
                ball_near => root_ball_ptr%ball_r_ptr
                ball_far  => root_ball_ptr%ball_l_ptr
            end if

            call this%query_balltree_n_neighbors_rec(distances, indices, ball_near, &
                    q_i, q_sq_sum, n_samples, n_columns, n_neighbors)
            call this%query_balltree_n_neighbors_rec(distances, indices, ball_far, &
                    q_i, q_sq_sum, n_samples, n_columns, n_neighbors)
        end if
    end subroutine query_balltree_n_neighbors_rec


    recursive subroutine query_balltree_radius_rec(this, distances, indices, root_ball_ptr, &
        q_i, q_sq_sum, n_samples, n_columns, radius_sq)
        implicit none
        class(balltree)                   :: this
        real(kind=8), allocatable, intent(inout)       :: distances(:)
        integer(kind=8), allocatable, intent(inout)    :: indices(:)
        type(ball), pointer               :: root_ball_ptr
        real(kind=8), intent(in)          :: q_i(n_columns), q_sq_sum
        integer(kind=8), intent(in)       :: n_samples, n_columns
        real(kind=8), intent(in)          :: radius_sq

        type(ball), pointer :: ball_near, ball_far

        real(kind=8) :: distance_q_and_c !< distance between query and ball center
        real(kind=8) :: distance_q_and_c_l, distance_q_and_c_r !< distance between query and ball center left and right
        real(kind=8), allocatable :: distance_q_and_s(:) !< distance between query and leaf ball samples
        real(kind=8), allocatable :: tmp_d(:)
        integer(kind=8), allocatable :: tmp_i(:)
        integer(kind=8) :: i, n, count_in_ball


        distance_q_and_c = sum( (q_i(:) - root_ball_ptr%center(:))**2d0 )

        if ( distance_q_and_c - root_ball_ptr%radius_sq .gt. radius_sq ) then
            return
        elseif ( root_ball_ptr%is_leaf ) then
            allocate( distance_q_and_s(root_ball_ptr%n_samples) )
            distance_q_and_s(:) = 0d0
            call multi_mat_vec(root_ball_ptr%x, q_i, distance_q_and_s, &
                root_ball_ptr%n_samples, root_ball_ptr%n_columns)
            distance_q_and_s(:) = -2d0*distance_q_and_s(:) &
                + root_ball_ptr%x_sq_sum(:) + q_sq_sum
            if ( radius_sq < minval(distance_q_and_s(:)) ) then
                ! skip
            else
                allocate(tmp_i(root_ball_ptr%n_samples))
                do i=1, root_ball_ptr%n_samples
                    tmp_i(i) = root_ball_ptr%indices(i)
                end do
                count_in_ball = count(distance_q_and_s <= radius_sq)

                if ( count_in_ball > 0 ) then
                    call quick_argsort(distance_q_and_s, tmp_i, root_ball_ptr%n_samples)
                    distances = [distances, distance_q_and_s(1:count_in_ball)]
                    indices = [indices, tmp_i(1:count_in_ball)]
                end if
            end if
        else
            distance_q_and_c_l = sum( (q_i(:) - root_ball_ptr%ball_l_ptr%center(:))**2d0 )
            distance_q_and_c_r = sum( (q_i(:) - root_ball_ptr%ball_r_ptr%center(:))**2d0 )
            if ( distance_q_and_c_l < distance_q_and_c_r ) then
                ball_near => root_ball_ptr%ball_l_ptr
                ball_far  => root_ball_ptr%ball_r_ptr
            else
                ball_near => root_ball_ptr%ball_r_ptr
                ball_far  => root_ball_ptr%ball_l_ptr
            end if

            call this%query_balltree_radius_rec(distances, indices, ball_near, &
                    q_i, q_sq_sum, n_samples, n_columns, radius_sq)
            call this%query_balltree_radius_rec(distances, indices, ball_far, &
                    q_i, q_sq_sum, n_samples, n_columns, radius_sq)
        end if
    end subroutine query_balltree_radius_rec



    ! ----------------------------------------------------------------------------------
    ! For Ball
    ! ----------------------------------------------------------------------------------

    subroutine init_ball(this, is_root, is_leaf, depth, n_samples, n_columns, min_samples_in_leaf, &
        split_algo_int, parent_ball)
        implicit none
        class(ball) :: this
        logical(kind=4), intent(in) :: is_root, is_leaf
        integer(kind=8), intent(in) :: depth, n_samples, n_columns, min_samples_in_leaf, split_algo_int
        type(ball), intent(in), optional, target :: parent_ball

        integer(kind=8) :: n

        this%is_root = is_root
        this%is_leaf = is_leaf
        this%depth = depth
        this%n_samples = n_samples
        this%n_columns = n_columns
        this%min_samples_in_leaf = min_samples_in_leaf
        this%split_algo_int = split_algo_int

        this%radius = 0d0
        call ifdealloc(this%center)
        allocate(this%center(this%n_columns))
        this%center(:) = huge(0d0)

        call ifdealloc(this%indices)
        allocate(this%indices(this%n_samples))

        if (ASSOCIATED(this%ball_p_ptr)) nullify(this%ball_p_ptr)
        if (ASSOCIATED(this%ball_l_ptr)) nullify(this%ball_l_ptr)
        if (ASSOCIATED(this%ball_r_ptr)) nullify(this%ball_r_ptr)

        if (present(parent_ball)) then
            if (ASSOCIATED(this%ball_p_ptr)) deallocate(this%ball_p_ptr)
            allocate(this%ball_p_ptr)
            this%ball_p_ptr => parent_ball
        end if

        if (is_root) then
            do n=1, this%n_samples, 1
                this%indices(n) = n
            end do
        end if
    end subroutine init_ball


    !> Print Node information
    subroutine info_ball(this)
        implicit none
        class(ball) :: this
        integer(kind=8) :: fin

        print*, '============================================================='
        print*, '============================================================='
        print*, "ball%is_root                 : ", this%is_root
        print*, "ball%is_leaf                 : ", this%is_leaf
        print*, "ball%idx                     : ", this%idx
        print*, "ball%depth                   : ", this%depth
        print*, "ball%n_samples               : ", this%n_samples
        print*, "ball%n_columns               : ", this%n_columns
        print*, "ball%min_samples_in_leaf     : ", this%min_samples_in_leaf
        print*, "ball%split_algo_int          : ", this%split_algo_int

        print*, "ball%radius                  : ", this%radius
        if (allocated(this%center)) then
            fin = minval((/5, size(this%center)/))
            print*, "this%center                 : ", this%center(1:fin)
        end if

        print*, "allocated(ball%indices)      : ", allocated(this%indices)
        if (allocated(this%indices)) then
            fin = minval((/5, size(this%indices)/))
            print*, "this%indices                 : ", this%indices(1:fin)
        end if
        print*, "associated(ball%ball_p)      : ", associated(this%ball_p_ptr)
        print*, "associated(ball%ball_l)      : ", associated(this%ball_l_ptr)
        print*, "associated(ball%ball_r)      : ", associated(this%ball_r_ptr)
    end subroutine info_ball


    subroutine split_ball(this, x_ptr, times)
        implicit none
        class(ball) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)
        integer(kind=8), intent(inout) :: times(:)

        integer(kind=8) :: n, f, idx, p_idx_1st, p_idx_2nd, idx_a, idx_b
        real(kind=8), allocatable :: distance_from_center(:), tmp_vec(:), tmp_X(:,:), distance_from_center_base(:)
        real(kind=8), allocatable :: distance_from_1st(:), distance_from_2nd(:)
        real(kind=8) :: center_sq_sum, p_1st_sq_sum, p_2nd_sq_sum

        integer(kind=8) :: flg, cnt_l, cnt_r, max_spread_dim, med_idx
        integer(kind=8), ALLOCATABLE :: which_side(:)
        real(kind=8), ALLOCATABLE    :: min_vals(:), max_vals(:), center(:)
        real(kind=8) :: tmp_min, tmp_max, val
        real(kind=8), ALLOCATABLE :: project_dist(:)

        integer(kind=8) :: date_value1(8), date_value2(8)

        allocate(distance_from_center(this%n_samples), tmp_vec(this%n_samples), tmp_X(this%n_samples, this%n_columns))
        allocate(distance_from_center_base(this%n_samples))
        distance_from_center_base(:) = 0d0

        ! Calculate Centroid and Square Sum of Points ----------------------------------
        ! call date_and_time(values=date_value1)
        allocate( center(this%n_columns) )
        center(:) = 0d0
        !$omp parallel num_threads(2)
        !$omp do private(n, idx) reduction(+:center)
        do n=1, this%n_samples, 1
            idx = this%indices(n)
            center(:) = center(:) + x_ptr(idx,:)
            distance_from_center_base(n) = distance_from_center_base(n) + sum(x_ptr(idx,:)**2d0)
        end do
        !$omp end do
        !$omp end parallel
        this%center(:) = center(:)
        distance_from_center(:) = distance_from_center_base(:)
        this%center(:) = this%center(:) / dble(this%n_samples)
        ! call date_and_time(values=date_value2)
        ! ! times(1) = times(1) + time_diff(date_value1, date_value2)

        ! Choose Farthest Point From Centroid and Set Ball Radius
        ! call date_and_time(values=date_value1)        
        center_sq_sum = sum( this%center(:)**2d0 )
        call multi_mat_vec(x_ptr(this%indices(:),:), this%center, &
            tmp_vec, this%n_samples, this%n_columns, f_)
        distance_from_center(:) = distance_from_center(:) + center_sq_sum &
            -2d0 * tmp_vec(:)
        this%radius_sq = maxval(distance_from_center)
        this%radius = sqrt(this%radius_sq)
        ! call date_and_time(values=date_value2)
        ! ! times(2) = times(2) + time_diff(date_value1, date_value2)

        ! Fast Return - All points are dullicated.
        if ( this%radius_sq .eq. 0d0 ) return

        ! call date_and_time(values=date_value1)        
        ! Get Most Spread Dimension
        allocate( min_vals(this%n_columns) )
        allocate( max_vals(this%n_columns) )
        if ( this%split_algo_int .eq. 1_8 ) then
            !$omp parallel num_threads(2)
            !$omp do private(f, n, tmp_min, tmp_max, idx, val)
            do f=1, this%n_columns, 1
                tmp_min =   huge(0d0)
                tmp_max = - huge(0d0)
                do n=1, this%n_samples, 1
                    idx = this%indices(n)
                    val = x_ptr(idx,f)
                    tmp_min = minval((/tmp_min, val/))
                    tmp_max = maxval((/tmp_max, val/))
                end do
                min_vals(f) = tmp_min
                max_vals(f) = tmp_max
            end do
            !$omp end do
            !$omp end parallel
            max_spread_dim = maxloc( abs( max_vals - min_vals ), dim=1)
            do n=1, this%n_samples, 1
                idx = this%indices(n)
                tmp_vec(n) = x_ptr(idx, max_spread_dim)
            end do
        else if ( this%split_algo_int .eq. 2_8 ) then
            call this%choose_farthest_two_points(idx_a, idx_b, x_ptr)
            allocate( this%point_a(this%n_columns) )
            allocate( this%point_b(this%n_columns) )
            allocate( this%b_minus_a(this%n_columns) )
            this%point_a(:) = x_ptr(idx_a,:)
            this%point_b(:) = x_ptr(idx_b,:)
            this%b_minus_a(:) = this%point_b(:) - this%point_a(:)
            this%a_sq_sum = sum( this%point_a(:)**2d0 )
            this%ab_dot = sum( this%point_a(:) * this%point_b(:) )
            this%dist_ab = sqrt( sum( (this%point_a(:)-this%point_b(:))**2d0 ) )

            call multi_mat_vec(x_ptr(this%indices,:), this%b_minus_a, tmp_vec, &
                this%n_samples, this%n_columns)
            tmp_vec(:) = tmp_vec(:) + this%a_sq_sum - this%ab_dot
            tmp_vec(:) = tmp_vec(:) / this%dist_ab
        else
            stop "NotImplementedError"
        end if
        ! call date_and_time(values=date_value2)
        ! ! times(3) = times(3) + time_diff(date_value1, date_value2)

        ! Get Pivot
        ! call date_and_time(values=date_value1)        
        med_idx = this%n_samples / 2_8
        call quick_argsort(tmp_vec, this%indices, this%n_samples)
        cnt_l = med_idx
        cnt_r = this%n_samples - cnt_l
        med_idx = this%indices(med_idx)
        ! call date_and_time(values=date_value2)
        ! ! times(4) = times(4) + time_diff(date_value1, date_value2)

        ! call date_and_time(values=date_value1)        
        allocate(this%ball_l_ptr)
        call this%ball_l_ptr%init(f_, f_, this%depth+1, &
            cnt_l, this%n_columns, this%min_samples_in_leaf, this%split_algo_int, this)

        allocate(this%ball_r_ptr)
        call this%ball_r_ptr%init(f_, f_, this%depth+1, &
            cnt_r, this%n_columns, this%min_samples_in_leaf, this%split_algo_int, this)
        ! call date_and_time(values=date_value2)
        ! ! times(5) = times(5) + time_diff(date_value1, date_value2)

        ! call date_and_time(values=date_value1) 
        cnt_l = 1
        do n=1, this%ball_l_ptr%n_samples, 1
            idx = this%indices(n)
            this%ball_l_ptr%indices(cnt_l) = idx
            cnt_l = cnt_l + 1
        end do

        cnt_r = 1
        do n=this%ball_l_ptr%n_samples+1, this%n_samples, 1
            idx = this%indices(n)
            this%ball_r_ptr%indices(cnt_r) = idx
            cnt_r = cnt_r + 1
        end do
        ! call date_and_time(values=date_value2)
        ! ! times(6) = times(6) + time_diff(date_value1, date_value2)
    end subroutine split_ball


    subroutine choose_farthest_two_points(this, idx_a, idx_b, x_ptr)
        implicit none
        class(ball)                       :: this
        integer(kind=8), intent(inout)    :: idx_a, idx_b
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        integer(kind=8) :: max_loc
        real(kind=8) :: center_sq_sum, first_sq_sum
        real(kind=8), ALLOCATABLE :: distances(:), x(:,:), x_sq_sum(:)

        center_sq_sum = sum( this%center(:)**2d0 )

        allocate( distances(this%n_samples) )
        allocate( x(this%n_samples, this%n_columns) )
        allocate( x_sq_sum(this%n_samples) )

        x(:,:) = x_ptr(this%indices,:)
        call matrix_sqsum_row(x, x_sq_sum, this%n_samples, this%n_columns, parallel=f_)

        ! Choose Farthest Point From Center
        distances(:) = 0d0
        call multi_mat_vec(x, this%center, distances, this%n_samples, this%n_columns)
        distances(:) = -2d0 * distances(:) + x_sq_sum(:) + center_sq_sum
        max_loc = maxloc(distances, dim=1)
        idx_a = this%indices(max_loc)
        first_sq_sum = sum( x_ptr(idx_a,:)**2d0 )

        ! Chooese Farthest Pointe From idx_a
        distances(:) = 0d0
        call multi_mat_vec(x, x_ptr(idx_a,:), distances, this%n_samples, this%n_columns)
        distances(:) = -2d0 * distances(:) + x_sq_sum(:) + first_sq_sum
        max_loc = maxloc(distances, dim=1)
        idx_b = this%indices(max_loc)
    end subroutine choose_farthest_two_points


end module mod_balltree

