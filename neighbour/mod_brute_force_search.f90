!< Nearest Neighbor Search by Brute Force.
module mod_brute_force_search
    !$ use omp_lib
    use mod_const
    use mod_common
    use mod_linalg
    use mod_data_holder    
    use mod_nearest_neighbour, only: neighbor_results
    implicit none

    !< Brute Force Nearest Neighbor Search
    type brute_force_search
        integer(kind=8)           :: n_samples, n_columns !< input data shape
        real(kind=8), ALLOCATABLE :: x(:,:) !< input data
        real(kind=8), ALLOCATABLE :: x_sq_sum(:) !< squared sum of input data
    contains
        procedure, pass :: build_brute_force_search_x
        procedure, pass :: build_brute_force_search_dholder
        generic :: build => build_brute_force_search_x, build_brute_force_search_dholder
        procedure :: query => query_brute_force_search
        procedure :: dump => dump_brute_force_search
        procedure :: load => load_brute_force_search

        procedure :: query_brute_force_search_n_neighbors
        procedure :: query_brute_force_search_radius
    end type brute_force_search
    
    !> Constructor of 'brute_force_search'
    interface brute_force_search
        module procedure :: new_brute_force_search
    end interface brute_force_search

contains


    !> Create new 'brute_force_search'.
    !> No paramter.
    function new_brute_force_search()
        implicit none
        type(brute_force_search) :: new_brute_force_search
    end function new_brute_force_search


    !> Build 'brute_force_search'
    !> Store input data. Calculate and Store its squared sum of row.
    !! \param x 2-dim matrix. domain data
    subroutine build_brute_force_search_x(this, x)
        implicit none
        class(brute_force_search) :: this
        real(kind=8), intent(in)  :: x(:,:)

        integer(kind=8) :: x_shape(2)

        x_shape = shape(x)

        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        allocate(this%x(this%n_samples, this%n_columns))
        this%x(:,:) = x(:,:)

        allocate(this%x_sq_sum(this%n_samples))
        call matrix_sqsum_row(x, this%x_sq_sum, this%n_samples, this%n_columns, parallel=f_)
    end subroutine build_brute_force_search_x

    
    subroutine build_brute_force_search_dholder(this, dholder)
        implicit none
        class(brute_force_search) :: this
        type(data_holder), intent(in) :: dholder
        call this%build_brute_force_search_x(dholder%x_ptr%x_r8_ptr)
    end subroutine build_brute_force_search_dholder


    !> Query by 'brute_force_search'
    !! \param q query points
    !! \param n_neighbors optional number of nearest neighbors to be extracted
    !! \param radius optional search radius 
    function query_brute_force_search(this, q, n_neighbors, radius)
        implicit none
        class(brute_force_search) :: this
        real(kind=8), intent(in) :: q(:,:)
        integer(kind=8), OPTIONAL :: n_neighbors
        real(kind=8), OPTIONAL :: radius
        type(neighbor_results) :: query_brute_force_search

        integer(kind=8) :: n_samples, n_columns, q_shape(2)
        real(kind=8), allocatable :: q_sq_sum(:)

        q_shape = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)
        if (allocated(query_brute_force_search%indices)) deallocate(query_brute_force_search%indices)
        if (allocated(query_brute_force_search%distances)) deallocate(query_brute_force_search%distances)

        allocate(q_sq_sum(n_samples))
        call matrix_sqsum_row(q, q_sq_sum, n_samples, n_columns, parallel=f_)

        if ( present(n_neighbors) ) then
            call this%query_brute_force_search_n_neighbors(query_brute_force_search, &
                q, q_sq_sum, n_neighbors, n_samples, n_columns)
        elseif ( present(radius) ) then
            call this%query_brute_force_search_radius(query_brute_force_search, &
                q, q_sq_sum, radius, n_samples, n_columns)            
        end if
    end function query_brute_force_search


    !> Return top-K nearest neighbors
    !! \param res result
    !! \param q query points
    !! \param q_sq_sum squared sum of query points
    !! \param n_neighbors optional number of nearest neighbors to be extracted
    !! \param n_samples number of query points
    !! \param n_columns number of columns
    subroutine query_brute_force_search_n_neighbors(this, res, q, q_sq_sum, n_neighbors, n_samples, n_columns)
        implicit none
        class(brute_force_search)   :: this
        type(neighbor_results)        :: res
        real(kind=8), intent(in)    :: q(n_samples, n_columns)
        real(kind=8), intent(in)    :: q_sq_sum(n_samples)
        integer(kind=8), intent(in) :: n_neighbors, n_samples, n_columns

        integer(kind=8) :: n, c, idx
        real(kind=8), allocatable :: distance_matrix(:,:), tmp(:)
        integer(kind=8), allocatable :: indices(:)
        ! n_neighbor=1
        integer(kind=8), allocatable :: min_locs(:)

        allocate(res%indices(n_samples))
        allocate(res%distances(n_samples))
        do n=1, n_samples, 1
            allocate(res%indices(n)%idx(n_neighbors))
            allocate(res%distances(n)%dst(n_neighbors))
        end do

        allocate(distance_matrix(this%n_samples, n_samples))
        ! distance_matrix(:,:) = spread(this%x_sq_sum, dim=2, ncopies=n_samples) & 
        !                         + spread(q_sq_sum, dim=1, ncopies=this%n_samples)
        ! call dgemm("N", "N", & 
        !     this%n_samples, n_columns, this % n_columns, &
        !     -2d0, & 
        !     this%x, this%n_samples, &
        !     transpose(q), n_columns, &
        !     1d0, &
        !     distance_matrix, this%n_samples)

        distance_matrix(:,:) = 0d0
        !$omp parallel num_threads(4)
        !$omp do private(c)
        do c=1, n_samples, 1
            call multi_mat_vec(this%x(:,:), q(c,:), distance_matrix(:,c), this%n_samples, n_columns, parallel=f_)
            distance_matrix(:,c) = -2d0*distance_matrix(:,c) + this%x_sq_sum(:) + q_sq_sum(c)
        end do
        !$omp end do
        !$omp end parallel

        if (n_neighbors .eq. 1_8) then
            allocate(min_locs(n_samples))
            min_locs(:) = minloc(distance_matrix, dim=1)
            !$omp parallel num_threads(4)
            !$omp do private(c, idx)
            do c=1, n_samples, 1
                idx = min_locs(c)
                res%indices(c)%idx = idx
                res%distances(c)%dst = sqrt(abs(distance_matrix(idx,c)))
            end do
            !$omp end do
            !$omp end parallel
        else
            allocate(tmp(this%n_samples))
            allocate(indices(this%n_samples))
            !$omp parallel num_threads(4)
            !$omp do private(c, indices, tmp)
            do c=1, n_samples, 1
                do n=1, this%n_samples, 1
                    indices(n) = n
                end do
                tmp(:) = distance_matrix(:,c)
                call quick_argselect(tmp, indices, this%n_samples, n_neighbors)

                call quick_argsort(tmp(1:n_neighbors), indices(1:n_neighbors), n_neighbors)
                res%indices(c)%idx(:) = indices(1:n_neighbors)
                res%distances(c)%dst(:) = sqrt(abs(tmp(1:n_neighbors)))
            end do
            !$omp end do
            !$omp end parallel
        end if
    end subroutine query_brute_force_search_n_neighbors


    !> Return nearest neighbors in ball with 'radius'
    !! \param res result
    !! \param q query points
    !! \param q_sq_sum squared sum of query points
    !! \param radius search radius aroud query points
    !! \param n_samples number of query points
    !! \param n_columns number of columns
    subroutine query_brute_force_search_radius(this, res, q, q_sq_sum, radius, n_samples, n_columns)
        implicit none
        class(brute_force_search)   :: this
        type(neighbor_results)        :: res
        real(kind=8), intent(in)    :: q(n_samples, n_columns)
        real(kind=8), intent(in)    :: q_sq_sum(n_samples)
        real(kind=8), intent(in)    :: radius
        integer(kind=8), intent(in) :: n_samples, n_columns

        real(kind=8) :: r_sq
        integer(kind=8) :: n, c, idx, count_in_ball
        real(kind=8), allocatable :: distance_matrix(:,:), tmp(:)
        integer(kind=8), allocatable :: indices(:)
        ! n_neighbor=1
        integer(kind=8), allocatable :: min_locs(:)

        allocate(res%indices(n_samples))
        allocate(res%distances(n_samples))
        do n=1, n_samples, 1
            allocate(res%indices(n)%idx(0))
            allocate(res%distances(n)%dst(0))
        end do

        allocate(distance_matrix(this%n_samples, n_samples))
        ! distance_matrix(:,:) = spread(this%x_sq_sum, dim=2, ncopies=n_samples) & 
        !                         + spread(q_sq_sum, dim=1, ncopies=this%n_samples)
        ! call dgemm("N", "N", & 
        !     this%n_samples, n_columns, this % n_columns, &
        !     -2d0, & 
        !     this%x, this%n_samples, &
        !     transpose(q), n_columns, &
        !     1d0, &
        !     distance_matrix, this%n_samples)

        distance_matrix(:,:) = 0d0
        !$omp parallel num_threads(4)
        !$omp do private(c)
        do c=1, n_samples, 1
            call multi_mat_vec(this%x(:,:), q(c,:), distance_matrix(:,c), this%n_samples, n_columns, parallel=f_)
            distance_matrix(:,c) = -2d0*distance_matrix(:,c) + this%x_sq_sum(:) + q_sq_sum(c)
        end do
        !$omp end do
        !$omp end parallel

        allocate(tmp(this%n_samples))
        allocate(indices(this%n_samples))
        r_sq = radius**2d0
        !$omp parallel num_threads(4)
        !$omp do private(c, indices, count_in_ball, tmp, n)
        do c=1, n_samples, 1
            do n=1, this%n_samples, 1
                indices(n) = n
            end do
            tmp(:) = distance_matrix(:,c)

            count_in_ball = count( tmp(:) .le. r_sq )

            call quick_argsort(tmp, indices, size(tmp)+0_8)

            res%indices(c)%idx   = [res%indices(c)%idx,   indices(1:count_in_ball)]
            res%distances(c)%dst = [res%distances(c)%dst, sqrt(tmp(1:count_in_ball))]
        end do
        !$omp end do
        !$omp end parallel
    end subroutine query_brute_force_search_radius


    subroutine dump_brute_force_search(this, file_name)
        implicit none
        class(brute_force_search)    :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit

        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%n_samples
        write(newunit) this%n_columns
        write(newunit) this%x(:,:)
        write(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine dump_brute_force_search


    subroutine load_brute_force_search(this, file_name)
        implicit none
        class(brute_force_search)    :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit

        if (allocated(this%x)) deallocate(this%x)
        if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)

        open(newunit=newunit, file=file_name, form='unformatted')
        read(newunit) this%n_samples
        read(newunit) this%n_columns
        allocate(this%x(this%n_samples, this%n_columns))
        allocate(this%x_sq_sum(this%n_samples))
        read(newunit) this%x(:,:)
        read(newunit) this%x_sq_sum(:)
        close(newunit)
    end subroutine load_brute_force_search


end module mod_brute_force_search
