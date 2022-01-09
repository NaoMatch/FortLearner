module mod_lsh
    !$ use omp_lib
    use mod_const, only: t_, f_
    use mod_stats, only: groupby_count
    use mod_random, only: rand_normal, rand_integer
    use mod_linalg, only: multi_mat_vec, matrix_sqsum_row
    use mod_sort
    use mod_common
    use mod_common_type
    use mod_nearest_neighbour, only: neighbor_results
    use mod_hash
    use mod_hash_table
    use mod_math
    implicit none

    !> Locality-Sensitive-Hashing.
    type lsh
        integer(kind=8)                             :: n_hash_functions = 16_8 !< number of hash functions in each hash tables
        integer(kind=8)                             :: n_hash_tables    =  8_8 !< number of hash tables
        integer(kind=8)                             :: bit_length       =  8_8 !< lenght of bit for 'random_projection_pstable'.
        character(len=256)                          :: algorithm        =  "random_projection" !< hashing algorithm, 'random_projection' or 'random_projection_pstable'
        integer(kind=8)                             :: n_samples, n_columns !< input data shape
        real(kind=8)                                :: div_val !< 1/bit_length
        type(jagged_array_of_array_i8), allocatable :: array_per_table(:) !< 

        type(jagged_matrix_r8), allocatable         :: hash_functoin_sets(:) !< hash function set.

        type(hash_table), allocatable               :: hash_tables(:) !< 'hash_table' object to store encoded data and search.

        real(kind=8), allocatable :: x(:,:)
        real(kind=8), allocatable :: x_sq_sum(:)
    contains
        procedure :: build => build_lsh
        procedure :: query => query_lsh

        procedure :: build_lsh_random_projection
        procedure :: query_lsh_nearest_neighbor_random_projection
        procedure :: key2hash_rp
        procedure :: key2hash_fast_rp

        procedure :: build_lsh_random_projection_pstable
        procedure :: query_lsh_nearest_neighbor_random_projection_pstable
        procedure :: key2hash_rpp
        procedure :: key2hash_rpp_fast

        procedure :: init_hash_tables

        procedure :: dealloc_all
    end type lsh

    interface lsh
        procedure :: new_lsh
    end interface lsh

contains

    !> Deallocate all allocatable member of 'lsh'
    subroutine dealloc_all(this)
        implicit none
        class(lsh) :: this
        if ( allocated(this%array_per_table) ) deallocate(this%array_per_table)
        if ( allocated(this%hash_functoin_sets) ) deallocate(this%hash_functoin_sets)
        if ( allocated(this%x) ) deallocate(this%x)
        if ( allocated(this%x_sq_sum) ) deallocate(this%x_sq_sum)
    end subroutine dealloc_all


    !> Initialize weight of hash function
    !! n_columns number of columns of input data
    subroutine init_hash_tables(this)
        implicit none
        class(lsh) :: this

        integer(kind=8) :: t

        if (allocated(this%hash_functoin_sets)) deallocate(this%hash_functoin_sets)
        allocate(this%hash_functoin_sets(this%n_hash_tables))

        if (allocated(this%hash_tables)) deallocate(this%hash_tables)
        allocate(this%hash_tables(this%n_hash_tables))

        do t=1, this%n_hash_tables, 1
            allocate( this%hash_functoin_sets(t)%matrix(this%n_columns, this%n_hash_functions) )
            call rand_normal(this%hash_functoin_sets(t)%matrix, this%n_columns, this%n_hash_functions)

            if ( this%algorithm .eq. "random_projection_pstable" ) then
                allocate( this%hash_functoin_sets(t)%offsets(this%n_hash_functions) )
                call RANDOM_NUMBER( this%hash_functoin_sets(t)%offsets )
                this%hash_functoin_sets(t)%offsets(:) = &
                    this%hash_functoin_sets(t)%offsets(:) * this%bit_length
                this%div_val = 1d0 / dble(this%bit_length)
            end if
        end do
    end subroutine init_hash_tables


    !> Construct New 'lsh' object.
    !! \param n_hash_function number of hash functions in one hash table
    !! \param n_hash_tables number of hash tables
    function new_lsh(n_hash_functions, n_hash_tables, bit_length, algorithm)
        implicit none
        type(lsh)                  :: new_lsh
        integer(kind=8),  optional :: n_hash_functions
        integer(kind=8),  optional :: n_hash_tables
        integer(kind=8),  optional :: bit_length
        character(len=*), optional :: algorithm

        if ( present(n_hash_functions) ) new_lsh%n_hash_functions = n_hash_functions
        if ( present(n_hash_tables) )    new_lsh%n_hash_tables    = n_hash_tables
        if ( present(bit_length) )       new_lsh%bit_length       = bit_length
        if ( present(algorithm) )        new_lsh%algorithm        = algorithm

        if (new_lsh%n_hash_functions <  1_8) stop
        if (new_lsh%n_hash_functions > 63_8) stop

        if (new_lsh%n_hash_tables    < 1_8) stop
    end function new_lsh


    !> Build LSH.
    !! \param x input data
    subroutine build_lsh(this, x)
        implicit none
        class(lsh) :: this
        real(kind=8), target, intent(in) :: x(:,:)

        real(kind=8), pointer :: x_ptr(:,:)

        integer(kind=8) :: x_shape(2), n_samples, t, f, n, tmp

        real(kind=8), allocatable    :: x_x_hash_funcs(:,:)
        logical(kind=1), allocatable :: table_bool(:,:)
        integer(kind=8), allocatable :: hash_vals(:)

        call this%dealloc_all()

        x_ptr => x
        x_shape = shape(x)

        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        call this%init_hash_tables()

        if (this%algorithm .eq. "random_projection") then
            call this%build_lsh_random_projection(x_ptr)
        elseif ( this%algorithm .eq. "random_projection_pstable" ) then
            call this%build_lsh_random_projection_pstable(x_ptr)
        else
            stop "NotImplementedError"
        end if

        if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)
        allocate(this%x_sq_sum(this%n_samples))
        this%x = x
        call matrix_sqsum_row(this%x, this%x_sq_sum, this%n_samples, this%n_columns, parallel=t_)
    end subroutine build_lsh


    !> Query function for lsh.
    !! \param q query samples
    !! \param n_neighbors number of neighbors
    function query_lsh(this, q, n_neighbors)
        implicit none
        class(lsh)                       :: this
        real(kind=8), target, intent(in) :: q(:,:)
        integer(kind=8), intent(in)      :: n_neighbors
        type(neighbor_results)             :: query_lsh

        integer(kind=8) :: q_shape(2), n_samples, n_columns, n
        real(kind=8), pointer :: q_ptr(:,:)

        q_ptr => q
        q_shape = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)

        allocate(query_lsh%indices(n_samples))
        allocate(query_lsh%distances(n_samples))
        do n=1, n_samples, 1
            allocate( query_lsh%indices(n)%idx(n_neighbors) )
            allocate( query_lsh%distances(n)%dst(n_neighbors) )
        end do

        if (this%algorithm .eq. "random_projection") then
            call this%query_lsh_nearest_neighbor_random_projection(query_lsh, q_ptr, n_neighbors, n_samples, n_columns)
        elseif ( this%algorithm .eq. "random_projection_pstable" ) then
            call this%query_lsh_nearest_neighbor_random_projection_pstable(query_lsh, q_ptr, n_neighbors, n_samples, n_columns)
        else
            stop "NotImplementedError"
        end if
    end function query_lsh



    !> Build LSH by random projection algorithm.
    !! \param x_ptr pointer to input data
    subroutine build_lsh_random_projection(this, x_ptr)
        implicit none
        class(lsh) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        integer(kind=8), allocatable :: hash_vals(:), indices(:), uniq_hashes(:), uniq_hash_counters(:)

        integer(kind=8) :: u, t, f, n, tmp, count_unique_hash
        integer(kind=8) :: ini, fin

        allocate( hash_vals(this%n_samples) )
        allocate( indices(this%n_samples) )
        allocate(this%array_per_table(this%n_hash_tables))

        !$omp parallel num_threads(4)
        !$omp do private(t, indices, hash_vals, uniq_hashes, count_unique_hash, ini, fin, uniq_hash_counters)
        do t=1, this%n_hash_tables, 1
            ! print*, 1
            do n=1, this%n_samples, 1
                indices(n) = n
            end do

            ! print*, 2
            hash_vals(:) = this%key2hash_fast_rp(x_ptr, t, this%n_samples, this%n_columns)

            ! print*, 4
            call quick_argsort(hash_vals, indices, this%n_samples)
            call groupby_count(uniq_hashes, uniq_hash_counters, hash_vals, this%n_samples)
            count_unique_hash = size(uniq_hashes)

            ! print*, 5
            allocate( this%array_per_table(t)%arrays(count_unique_hash) )
            allocate( this%array_per_table(t)%hashval2id(count_unique_hash) )

            this%array_per_table(t)%hashval2id(:) = uniq_hashes(:)

            ! print*, 6
            ini = 1
            do u=1, count_unique_hash, 1
                fin = ini + uniq_hash_counters(u)-1
                allocate( this%array_per_table(t)%arrays(u)%indices(uniq_hash_counters(u)) )
                this%array_per_table(t)%arrays(u)%indices(:) = indices(ini:fin)
                ini = fin + 1
            end do
        end do
        !$omp end do
        !$omp end parallel
    end subroutine build_lsh_random_projection

    !> Query function for lsh by random projection algorithm.
    !! \param res neighbor results.
    !! \param q_ptr pointer to query samples
    !! \param n_neighbors number of neighbors
    !! \param n_samples number of query samples
    !! \param n_columns number of columns
    subroutine query_lsh_nearest_neighbor_random_projection(this, res, q_ptr, n_neighbors, n_samples, n_columns)
        implicit none
        class(lsh)                       :: this
        type(neighbor_results)             :: res
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in)      :: n_neighbors, n_samples, n_columns

        integer(kind=8) :: rand_idx
        integer(kind=8) :: c, t, f, n, tmp, id, uniq_ids, hash, u, i, q_i_enc
        integer(kind=8) :: n_candidates, n_uniq_candidates, min_loc
        integer(kind=8), ALLOCATABLE :: q2hash(:,:)
        real(kind=8) :: q_i_sq_sum
        real(kind=8), ALLOCATABLE :: q_i(:), d_i(:)
        logical(kind=1), allocatable :: q_i_bin(:)
        integer(kind=8), allocatable :: candidates(:), unique_candidates(:)
        real(kind=8), ALLOCATABLE :: x_candidates(:,:), x_sq_sum_candidates(:)

        allocate( q_i(n_columns) )
        allocate( q_i_bin(this%n_hash_functions) )

        !$omp parallel num_threads(4)
        !$omp do private(n, candidates, q_i, q_i_sq_sum, t, hash, id, n_candidates, unique_candidates) &
        !$omp private(n_uniq_candidates, d_i, min_loc, x_candidates, x_sq_sum_candidates)
        do n=1, n_samples, 1
            allocate( candidates(0) )
            q_i(:) = q_ptr(n,:)
            q_i_sq_sum = sum( q_i*q_i )

            do t=1, this%n_hash_tables, 1
                hash = this%key2hash_rp(q_i, t)

                id = linear_search(this%array_per_table(t)%hashval2id(:), &
                    size(this%array_per_table(t)%hashval2id(:))+0_8, hash)
                if (id .eq. -1) cycle

                candidates = [candidates, this%array_per_table(t)%arrays(id)%indices]
                call quick_sort(candidates, size(candidates)+0_8)
                call collect_unique_values(unique_candidates, candidates, size(candidates)+0_8)
                if ( size(unique_candidates) > 2_8*this%n_hash_tables ) exit
            end do

            n_uniq_candidates = size(unique_candidates)
            if ( n_uniq_candidates < n_neighbors ) then
                do i=1, n_neighbors-n_uniq_candidates, 1
                    call rand_integer(1_8, this%n_samples, rand_idx)
                    unique_candidates = [unique_candidates, rand_idx]
                end do
                n_uniq_candidates = n_neighbors
            end if
            allocate( d_i(n_uniq_candidates) )
            allocate( x_candidates(n_uniq_candidates, this%n_columns) )
            allocate( x_sq_sum_candidates(n_uniq_candidates) )

            x_candidates(:,:) = this%x(unique_candidates,:)
            x_sq_sum_candidates(:) = this%x_sq_sum(unique_candidates)
            d_i(:) = 0d0

            call multi_mat_vec(x_candidates, q_i, d_i, n_uniq_candidates, this%n_columns)
            d_i(:) = -2d0*d_i(:) + q_i_sq_sum + x_sq_sum_candidates(:)
            d_i(:) = relu(d_i)

            call quick_argselect(d_i, unique_candidates, n_uniq_candidates, n_neighbors)
            res%indices(n)%idx(1:n_neighbors)   = unique_candidates(1:n_neighbors)
            res%distances(n)%dst(1:n_neighbors) = sqrt(d_i(1:n_neighbors))
            deallocate( candidates, d_i, x_candidates, x_sq_sum_candidates )
        end do
        !$omp end do
        !$omp end parallel
    end subroutine query_lsh_nearest_neighbor_random_projection

    !> Convert one query sample to encoded value.
    !! \param q_i query sample
    !! \param table_idx index of hash talbes
    function key2hash_rp(this, q_i, table_idx)
        implicit none
        class(lsh) :: this
        real(kind=8), intent(in) :: q_i(:)
        integer(kind=8) :: key2hash_rp, table_idx
        logical(kind=1), ALLOCATABLE :: q_i_bin(:)
        integer(kind=8) :: t, f

        allocate(q_i_bin(this%n_hash_functions))
        q_i_bin(:) = f_
        do f=1, this%n_hash_functions, 1
            q_i_bin(f) = sum( this%hash_functoin_sets(table_idx)%matrix(:,f) * q_i(:) ) >= 0d0
        end do

        key2hash_rp = 0_8
        do f=1, this%n_hash_functions, 1
            if (q_i_bin(f)) key2hash_rp = ibset(key2hash_rp,f-1)
        end do
    end function key2hash_rp


    !> Convert query samples to encoded values.
    !> Faster than 'key2hash_rp'.
    !! \param q_i query sample
    !! \param table_idx index of hash talbes
    !! \param n_samples number of query samples
    !! \param n_columns number of columns
    function key2hash_fast_rp(this, q_ptr, table_idx, n_samples, n_columns)
        implicit none
        class(lsh) :: this
        integer(kind=8), allocatable :: key2hash_fast_rp(:)
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        integer(kind=8) :: table_idx
        integer(kind=8) :: n_samples, n_columns

        integer(kind=8) :: n, f, hash
        real(kind=8), allocatable :: project(:,:)
        logical(kind=1), allocatable :: project_logical(:,:)

        allocate(key2hash_fast_rp(n_samples))
        allocate(project(n_samples, this%n_hash_functions))
        allocate(project_logical(n_samples, this%n_hash_functions))

        ! call dgemm("N", "N", & 
        !     n_samples, this%n_hash_functions, n_columns, &
        !     1d0, & 
        !     q_ptr, n_samples, &
        !     this%hash_functoin_sets(table_idx)%matrix, n_columns, &
        !     0d0, &
        !     project, n_samples) 

        ! project(:,:) = 0d0
        ! do f=1, this%n_hash_functions, 1
        !     call multi_mat_vec(q_ptr, this%hash_functoin_sets(table_idx)%matrix(:,f), &
        !         project(:,f), n_samples, n_columns)
        ! end do

        project(:,:) = matmul(q_ptr, this%hash_functoin_sets(table_idx)%matrix)
        project_logical(:,:) = project(:,:) >= 0d0

        do n=1, n_samples, 1
            hash = 0_8
            do f=1, this%n_hash_functions, 1
                if (project_logical(n,f)) hash = ibset(hash,f-1)
            end do
            key2hash_fast_rp(n) = hash
        end do
    end function key2hash_fast_rp




    !> Build LSH by random projection with p-stable distribution algorithm.
    !! \param x_ptr pointer to input data
    subroutine build_lsh_random_projection_pstable(this, x_ptr)
        implicit none
        class(lsh) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        real(kind=8), allocatable    :: x2enc(:,:)
        logical(kind=1), allocatable :: enc2bin(:,:)
        integer(kind=8), allocatable :: flt2int(:,:)
        integer(kind=8), allocatable :: hash_vals(:), indices(:), uniq_hashes(:), uniq_hash_counters(:)

        integer(kind=8) :: u, t, f, n, tmp, count_unique_hash
        integer(kind=8) :: ini, fin
        real(kind=8) :: div_val

        allocate( x2enc(this%n_samples, this%n_hash_functions) )
        allocate( flt2int(this%n_samples, this%n_hash_functions) )
        allocate( enc2bin(this%n_samples, this%n_hash_functions) )
        allocate( hash_vals(this%n_samples) )
        allocate( indices(this%n_samples) )
        allocate(this%array_per_table(this%n_hash_tables))

        div_val = 1d0 / dble(this%bit_length)
        !$omp parallel num_threads(4)
        !$omp do private(t, n, flt2int)
        do t=1, this%n_hash_tables, 1
            ! Encoding To Hash Values
            ! do f=1, this%n_hash_functions, 1
            !     call multi_mat_vec(x_ptr, this%hash_functoin_sets(t)%matrix(:,f), &
            !         x2enc(:,f), this%n_samples, this%n_hash_functions, parallel=f_)
            !     x2enc(:,f) = x2enc(:,f) + this%hash_functoin_sets(t)%offsets(f)
            ! end do
            ! flt2int(:,:) = floor( x2enc(:,:) * div_val )
            ! do n=1, this%n_samples, 1
            !     flt2int(n,:) = this%key2hash_rpp(x_ptr(n,:), t)
            ! end do

            flt2int(:,:) = this%key2hash_rpp_fast(x_ptr, t, this%n_samples, this%n_columns)

            ! Create Hash Table
            call this%hash_tables(t)%create_table(flt2int)
        end do
        !$omp end do
        !$omp end parallel
    end subroutine build_lsh_random_projection_pstable

    !> Query function for lsh by random projection with p-stable distribution algorithm.
    !! \param res neighbor results.
    !! \param q_ptr pointer to query samples
    !! \param n_neighbors number of neighbors
    !! \param n_samples number of query samples
    !! \param n_columns number of columns
    subroutine query_lsh_nearest_neighbor_random_projection_pstable(this, res, q_ptr, n_neighbors, n_samples, n_columns)
        implicit none
        class(lsh)                       :: this
        type(neighbor_results)             :: res
        real(kind=8), target, intent(in) :: q_ptr(:,:)
        integer(kind=8), intent(in)      :: n_neighbors, n_samples, n_columns

        integer(kind=8) :: rand_idx
        integer(kind=8) :: c, t, f, n, tmp, id, uniq_ids, hash, u, i, q_i_enc
        integer(kind=8) :: n_candidates, n_uniq_candidates, min_loc, hash_q
        integer(kind=8), ALLOCATABLE :: q2hash(:,:)
        real(kind=8) :: q_i_sq_sum, div_val
        real(kind=8), ALLOCATABLE :: q_i(:), d_i(:)
        real(kind=8), ALLOCATABLE :: x_candidates(:,:), x_sq_sum_candidates(:)
        integer(kind=8), allocatable :: q_i_hash(:)
        integer(kind=8), allocatable :: candidates(:), unique_candidates(:), indices(:)

        allocate( q_i(n_columns) )
        allocate( q_i_hash(this%n_hash_functions) )

        !$omp parallel num_threads(4)
        !$omp do private(n, q_i, q_i_sq_sum, t, q_i_hash, indices, candidates, n_uniq_candidates) &
        !$omp private(unique_candidates, x_candidates, x_sq_sum_candidates, d_i, i)
        do n=1, n_samples, 1
            allocate( candidates(0) )
            ! fetch, preprocess
            q_i(:) = q_ptr(n,:)
            q_i_sq_sum = sum( q_i(:) * q_i(:) )

            ! Extract Nearest Neighbor Candidate Indices
            do t=1, this%n_hash_tables, 1
                q_i_hash(:) = this%key2hash_rpp(q_i, t)
                indices = this%hash_tables(t)%search(q_i_hash)
                candidates = [candidates, indices]

                call quick_sort(candidates, size(candidates)+0_8)
                call collect_unique_values(unique_candidates, candidates, size(candidates)+0_8)
                if ( size(unique_candidates) > 2_8*this%n_hash_tables ) exit
            end do

            ! Exception: No. unique candidates < No. neighbors
            !            Add Indices
            n_uniq_candidates = size(unique_candidates)
            if ( n_uniq_candidates < n_neighbors ) then
                do i=1, n_neighbors-n_uniq_candidates, 1
                    call rand_integer(1_8, this%n_samples, rand_idx)
                    unique_candidates = [unique_candidates, rand_idx]
                end do
                n_uniq_candidates = n_neighbors
            end if

            x_candidates = this%x(unique_candidates, :)
            x_sq_sum_candidates = this%x_sq_sum(unique_candidates)

            allocate( d_i(n_uniq_candidates) )
            d_i(:) = 0d0
            call multi_mat_vec(x_candidates, q_i, d_i, n_uniq_candidates, this%n_columns, parallel=f_)
            d_i(:) = - 2d0 * d_i(:) + x_sq_sum_candidates(:) + q_i_sq_sum
            d_i(:) = relu(d_i)

            call quick_argsort(d_i, unique_candidates, n_uniq_candidates)
            res%indices(n)%idx(1:n_neighbors)   = unique_candidates(1:n_neighbors)
            res%distances(n)%dst(1:n_neighbors) = sqrt(d_i(1:n_neighbors))
            deallocate( candidates, d_i )
        end do
        !$omp end do
        !$omp end parallel
    end subroutine query_lsh_nearest_neighbor_random_projection_pstable

    !> Convert one query sample to encoded value.
    !! \param q_i query sample
    !! \param table_idx index of hash talbes
    function key2hash_rpp(this, q_i, table_idx)
        implicit none
        class(lsh) :: this
        real(kind=8), intent(in) :: q_i(:)
        integer(kind=8), ALLOCATABLE :: key2hash_rpp(:)
        integer(kind=8) :: table_idx
        real(kind=8), ALLOCATABLE :: tmp(:)
        integer(kind=8) :: t, f

        allocate(key2hash_rpp(this%n_hash_functions))
        allocate(tmp(this%n_hash_functions))
        tmp(:) = 0d0

        do f=1, this%n_hash_functions, 1
            tmp(f) = sum( this%hash_functoin_sets(table_idx)%matrix(:,f)*q_i(:) )
            tmp(f) = tmp(f) + this%hash_functoin_sets(table_idx)%offsets(f)
        end do
        key2hash_rpp(:) = FLOOR( tmp(:) * this%div_val )
    end function key2hash_rpp

    !> Convert query samples to encoded values.
    !> Faster than 'key2hash_rpp'.
    !! \param q_i query sample
    !! \param table_idx index of hash talbes
    !! \param n_samples number of query samples
    !! \param n_columns number of columns
    function key2hash_rpp_fast(this, q_ptr, table_idx, n_samples, n_columns)
        implicit none
        class(lsh) :: this
        real(kind=8), pointer, intent(in) :: q_ptr(:,:)
        real(kind=8), ALLOCATABLE :: tmp(:,:)
        integer(kind=8), ALLOCATABLE :: key2hash_rpp_fast(:,:)
        integer(kind=8) :: table_idx, n_samples, n_columns

        allocate( tmp(n_samples, this%n_hash_functions) )
        allocate( key2hash_rpp_fast(n_samples, this%n_hash_functions) )

        tmp(:,:) = matmul(q_ptr, this%hash_functoin_sets(table_idx)%matrix)
        tmp(:,:) = tmp(:,:) + spread(this%hash_functoin_sets(table_idx)%offsets(:), &
            dim=1, ncopies=n_samples)

        key2hash_rpp_fast = floor(tmp * this%div_val)
    end function key2hash_rpp_fast

end module mod_lsh
