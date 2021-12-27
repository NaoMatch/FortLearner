module mod_lsh
    !$ use omp_lib
    use mod_const, only: t_, f_
    use mod_stats, only: groupby_count
    use mod_random, only: rand_normal
    use mod_linalg, only: multi_mat_vec, matrix_sqsum_row
    use mod_sort
    use mod_nearest_neighbour, only: kdtree_results
    implicit none

    type hash_function
        real(kind=8), allocatable :: weight(:)
    contains
        procedure :: init_weight
    end type hash_function

    type hash_table
        type(hash_function), allocatable :: hash_functions(:)
    contains
        procedure :: init_hash_functions
    end type hash_table

    type jagged_matrix_r8
        real(kind=8), allocatable :: matrix(:,:)
    end type jagged_matrix_r8

    type jagged_array_i8
        integer(kind=8), allocatable :: indices(:)
    end type jagged_array_i8

    type jagged_array_of_array_i8
        integer(kind=8), allocatable :: hashval2id(:)
        type(jagged_array_i8), ALLOCATABLE :: arrays(:)
    end type jagged_array_of_array_i8

    type lsh
        integer(kind=8)                             :: n_hash_functions = 16_8
        integer(kind=8)                             :: n_hash_tables    =  8_8
        character(len=256)                          :: algorithm        =  "random_projection"
        integer(kind=8)                             :: n_samples, n_columns
        type(hash_table), allocatable               :: hash_tables(:) ! n_columns, n_hash_functions, n_hash_tabless
        type(jagged_array_of_array_i8), allocatable :: array_per_table(:)

        type(jagged_matrix_r8), allocatable         :: hash_functoin_sets(:)

        real(kind=8), allocatable :: x(:,:)
        real(kind=8), allocatable :: x_sq_sum(:)
    contains
        procedure :: fit_lsh
        procedure :: fit_lsh_random_projection
        procedure :: query_lsh_nearest_neighbor_random_projection
        
        procedure :: fit_lsh_matmul
        procedure :: fit_lsh_matmul_random_projection
        procedure :: query_lsh_nearest_neighbor_random_projection_matmul

        procedure :: init_hash_tables
        procedure :: init_hash_tables_matmul


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
        if ( allocated(this%hash_tables) ) deallocate(this%hash_tables)
        if ( allocated(this%array_per_table) ) deallocate(this%array_per_table)
        if ( allocated(this%hash_functoin_sets) ) deallocate(this%hash_functoin_sets)
        if ( allocated(this%x) ) deallocate(this%x)
        if ( allocated(this%x_sq_sum) ) deallocate(this%x_sq_sum)
    end subroutine dealloc_all


    !> Initialize weight of hash function
    !! n_columns number of columns of input data
    subroutine init_weight(this, n_columns)
        implicit none
        class(hash_function)        :: this
        integer(kind=8), intent(in) :: n_columns
        allocate(this%weight(n_columns))
        call rand_normal(this%weight, n_columns)
    end subroutine init_weight

    subroutine init_hash_functions(this, n_hash_functions, n_columns)
        implicit none
        class(hash_table) :: this
        integer(kind=8), intent(in) :: n_hash_functions, n_columns

        integer(kind=8) :: n, m

        allocate(this%hash_functions(n_hash_functions))

        do n=1, n_hash_functions, 1
            call this%hash_functions(n)%init_weight(n_columns)
        end do
    end subroutine init_hash_functions

    subroutine init_hash_tables(this)
        implicit none
        class(lsh) :: this

        integer(kind=8) :: n, m

        allocate(this%hash_tables(this%n_hash_tables))

        do n=1, this%n_hash_tables, 1
            call this%hash_tables(n)%init_hash_functions(this%n_hash_functions, this%n_columns)
        end do
    end subroutine init_hash_tables

    subroutine init_hash_tables_matmul(this)
        implicit none
        class(lsh) :: this

        integer(kind=8) :: t

        if (allocated(this%hash_functoin_sets)) deallocate(this%hash_functoin_sets)
        allocate(this%hash_functoin_sets(this%n_hash_tables))

        do t=1, this%n_hash_tables, 1
            allocate( this%hash_functoin_sets(t)%matrix(this%n_columns, this%n_hash_functions) )
            call rand_normal(this%hash_functoin_sets(t)%matrix, this%n_columns, this%n_hash_functions)
        end do
    end subroutine init_hash_tables_matmul


    !> Construct New 'lsh' object.
    !! \param n_hash_function number of hash functions in one hash table
    !! \param n_hash_tables number of hash tables
    function new_lsh(n_hash_functions, n_hash_tables, algorithm)
        implicit none
        type(lsh)                  :: new_lsh
        integer(kind=8),  optional :: n_hash_functions
        integer(kind=8),  optional :: n_hash_tables
        character(len=*), optional :: algorithm

        if ( present(n_hash_functions) ) new_lsh%n_hash_functions = n_hash_functions
        if ( present(n_hash_tables) )    new_lsh%n_hash_tables    = n_hash_tables
        if ( present(algorithm) )        new_lsh%algorithm        = algorithm

        if (new_lsh%n_hash_functions <  1_8) stop
        if (new_lsh%n_hash_functions > 63_8) stop

        if (new_lsh%n_hash_tables    < 1_8) stop
    end function new_lsh




    subroutine fit_lsh(this, x)
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
            call this%fit_lsh_random_projection(x_ptr)
        end if

        allocate(this%x_sq_sum(this%n_samples))
        this%x = x
        call matrix_sqsum_row(this%x, this%x_sq_sum, this%n_samples, this%n_columns, parallel=t_)
    end subroutine fit_lsh


    subroutine fit_lsh_random_projection(this, x_ptr)
        class(lsh) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        real(kind=8), allocatable    :: x_x_hash_funcs(:,:)
        logical(kind=1), allocatable :: table_bool(:,:)
        integer(kind=8), allocatable :: hash_vals(:)

        integer(kind=8) :: u, t, f, n, tmp, uniq_count, ini, fin, i
        integer(kind=8), ALLOCATABLE :: uniq_vals(:), uniq_cnts(:), indices(:)

        allocate(x_x_hash_funcs(this%n_samples, this%n_hash_functions))
        allocate(table_bool(this%n_samples, this%n_hash_functions))
        allocate(hash_vals(this%n_samples))
        allocate(indices(this%n_samples))
        allocate(this%array_per_table(this%n_hash_tables))

        do t=1, this%n_hash_tables, 1
            do i=1, this%n_samples, 1
                indices(i) = i
            end do

            do f=1, this%n_hash_functions, 1
                call multi_mat_vec(x_ptr, this%hash_tables(t)%hash_functions(f)%weight, &
                    x_x_hash_funcs(:,f), this%n_samples, this%n_hash_functions)
            end do
            table_bool(:,:) = x_x_hash_funcs(:,:) >= 0d0
            hash_vals(:) = 0_8

            ! もしくは、(2^f, 2^(f-1), 2^(f-2))のベクトルを持っておいて、0と1の行列にかぶせて、
            ! sum(mask*pow2, dim=2)のほうが速いかも
            do n=1, this%n_samples, 1
                tmp = 0_8
                do f=1, this%n_hash_functions, 1
                    if (table_bool(n,f)) then
                        tmp = IBSET(tmp, f-1)
                    end if
                end do
                hash_vals(n) = tmp
            end do

            call quick_argsort(hash_vals, indices, this%n_samples)
            uniq_count = count_unique(hash_vals, this%n_samples)

            allocate(this%array_per_table(t)%arrays(uniq_count))
            call groupby_count(uniq_vals, uniq_cnts, hash_vals, this%n_samples)

            allocate( this%array_per_table(t)%hashval2id(uniq_count) )
            this%array_per_table(t)%hashval2id(:) = uniq_vals

            ini = 1
            do u=1, uniq_count, 1
                fin = ini + uniq_cnts(u)-1
                allocate( this%array_per_table(t)%arrays(u)%indices(uniq_cnts(u)) )
                this%array_per_table(t)%arrays(u)%indices(:) = indices(ini:fin)
                ini = fin + 1
            end do
        end do
    end subroutine fit_lsh_random_projection


    subroutine query_lsh_nearest_neighbor_random_projection(this, q)
        implicit none
        class(lsh)               :: this
        real(kind=8), target, intent(in) :: q(:,:)

        real(kind=8), pointer :: q_ptr(:,:)

        integer(kind=8) :: q_shape(2), n_samples, n_columns
        integer(kind=8) :: t, f, n, tmp, id, uniq_ids, hash, u, i
        integer(kind=8), ALLOCATABLE :: q2hash(:,:)
        real(kind=8) :: q_sq

        real(kind=8), allocatable    :: x_x_hash_funcs(:,:), q_sq_sum_rows(:), distances(:)
        logical(kind=1), allocatable :: table_bool(:,:)
        integer(kind=8), allocatable :: hash_vals(:), uniq_candidates(:)
        type(jagged_array_i8), ALLOCATABLE :: candidate_indices(:)

        q_ptr => q
        q_shape = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)

        allocate(q2hash(n_samples, this%n_hash_tables))
        allocate(x_x_hash_funcs(n_samples, this%n_hash_functions))
        allocate(table_bool(n_samples, this%n_hash_functions))
        allocate(hash_vals(n_samples))
        allocate(q_sq_sum_rows(n_samples))
        allocate(candidate_indices(n_samples))

        call matrix_sqsum_row(q, q_sq_sum_rows, n_samples, n_columns)

        do n=1, n_samples, 1
            allocate( candidate_indices(n)%indices(0) )
        end do

        do t=1, this%n_hash_tables, 1
            do f=1, this%n_hash_functions, 1
                call multi_mat_vec(q_ptr, this%hash_tables(t)%hash_functions(f)%weight, &
                    x_x_hash_funcs(:,f), n_samples, this%n_hash_functions)
            end do
            table_bool(:,:) = x_x_hash_funcs(:,:) >= 0d0
            hash_vals(:)    = 0_8

            do n=1, n_samples, 1
                tmp = 0_8
                do f=1, this%n_hash_functions, 1
                    if (table_bool(n,f)) then
                        tmp = IBSET(tmp, f-1) 
                    end if
                end do
                hash_vals(n) = tmp
            end do

            uniq_ids = size(this%array_per_table(t)%hashval2id)
            do n=1, n_samples, 1
                hash = hash_vals(n)
                id = -1_8
                do u=1, uniq_ids, 1
                    if (hash .eq. this%array_per_table(t)%hashval2id(u)) then
                        id = u; exit
                    end if
                end do
                if (id .eq. -1) cycle
                candidate_indices(n)%indices = [candidate_indices(n)%indices, this%array_per_table(t)%arrays(id)%indices]
            end do
        end do

        do n=1, n_samples, 1
            q_sq = q_sq_sum_rows(n)
            uniq_ids = size( candidate_indices(n)%indices )
            call collect_unique_values(uniq_candidates, candidate_indices(n)%indices, uniq_ids)
            uniq_ids = size( uniq_candidates )
            allocate(distances(uniq_ids))
            do i=1, uniq_ids, 1
                id = uniq_candidates(i)
                distances(i) = this%x_sq_sum(id) + q_sq - 2d0 * sum( q_ptr(n,:)*this%x(id,:) )
            end do

            id = minloc(distances,dim=1)
            if (id .eq. 0_8) then
                print*, "None"
            else
                print*, n, uniq_candidates(id), sqrt(minval(distances)), &
                    sum( q_ptr(n,:)*this%x(id,:) )
            end if
            deallocate(distances)
        end do
    end subroutine query_lsh_nearest_neighbor_random_projection





    subroutine fit_lsh_matmul(this, x)
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

        call this%init_hash_tables_matmul()

        if (this%algorithm .eq. "random_projection") then
            call this%fit_lsh_matmul_random_projection(x_ptr)
        end if

        if (allocated(this%x_sq_sum)) deallocate(this%x_sq_sum)
        allocate(this%x_sq_sum(this%n_samples))
        this%x = x
        call matrix_sqsum_row(this%x, this%x_sq_sum, this%n_samples, this%n_columns, parallel=t_)
    end subroutine fit_lsh_matmul


    subroutine fit_lsh_matmul_random_projection(this, x_ptr)
        implicit none
        class(lsh) :: this
        real(kind=8), pointer, intent(in) :: x_ptr(:,:)

        real(kind=8), allocatable    :: x2enc(:,:)
        logical(kind=1), allocatable :: enc2bin(:,:)
        integer(kind=8), allocatable :: hash_vals(:), indices(:), uniq_hashes(:), uniq_hash_counters(:)

        integer(kind=8) :: u, t, f, n, tmp, count_unique_hash
        integer(kind=8) :: ini, fin

        allocate( x2enc(this%n_samples, this%n_hash_functions) )
        allocate( enc2bin(this%n_samples, this%n_hash_functions) )
        allocate( hash_vals(this%n_samples) )
        allocate( indices(this%n_samples) )
        allocate(this%array_per_table(this%n_hash_tables))

        !$omp parallel num_threads(4)
        !$omp do private(indices, t, n, f, u, tmp, hash_vals, uniq_hashes, uniq_hash_counters, count_unique_hash, ini, fin)
        do t=1, this%n_hash_tables, 1
            ! print*, 1
            do n=1, this%n_samples, 1
                indices(n) = n
            end do

            ! x2enc = matmul( x_ptr, this%hash_functoin_sets(t)%matrix )
            ! print*, 2
            do f=1, this%n_hash_functions, 1
                call multi_mat_vec(x_ptr, this%hash_functoin_sets(t)%matrix(:,f), &
                    x2enc(:,f), this%n_samples, this%n_hash_functions, parallel=f_)
            end do
            enc2bin(:,:) = x2enc >= 0d0

            ! print*, 3
            do n=1, this%n_samples, 1
                tmp = 0_8
                do f=1, this%n_hash_functions, 1
                    if (enc2bin(n,f)) then
                        tmp = IBSET(tmp, f-1)
                    end if
                end do
                hash_vals(n) = tmp
            end do

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
    end subroutine fit_lsh_matmul_random_projection


    function query_lsh_nearest_neighbor_random_projection_matmul(this, q)
        implicit none
        class(lsh)               :: this
        real(kind=8), target, intent(in) :: q(:,:)
        type(kdtree_results)     :: query_lsh_nearest_neighbor_random_projection_matmul

        real(kind=8), pointer :: q_ptr(:,:)

        integer(kind=8) :: q_shape(2), n_samples, n_columns
        integer(kind=8) :: c, t, f, n, tmp, id, uniq_ids, hash, u, i, q_i_enc
        integer(kind=8) :: n_candidates, n_uniq_candidates, min_loc
        integer(kind=8), ALLOCATABLE :: q2hash(:,:)
        real(kind=8) :: q_i_sq_sum
        real(kind=8), ALLOCATABLE :: q_i(:), d_i(:)
        logical(kind=1), allocatable :: q_i_bin(:)
        integer(kind=8), allocatable :: candidates(:), unique_candidates(:)

        q_ptr => q
        q_shape = shape(q)
        n_samples = q_shape(1)
        n_columns = q_shape(2)

        allocate( q_i(n_columns) )
        allocate( q_i_bin(this%n_hash_functions) )

        allocate(query_lsh_nearest_neighbor_random_projection_matmul%indices(n_samples))
        allocate(query_lsh_nearest_neighbor_random_projection_matmul%distances(n_samples))
        do n=1, n_samples, 1
            allocate( query_lsh_nearest_neighbor_random_projection_matmul%indices(n)%idx(1) )
            allocate( query_lsh_nearest_neighbor_random_projection_matmul%distances(n)%dst(1) )
        end do

        !$omp parallel num_threads(4)
        !$omp do private(q_i, q_i_sq_sum, n, t, f, q_i_bin, hash, id, u, candidates, unique_candidates) &
        !$omp private(n_candidates, c, d_i)
        do n=1, n_samples, 1
            allocate( candidates(0) )
            q_i(:) = q(n,:)
            q_i_sq_sum = sum( q_i*q_i )

            do t=1, this%n_hash_tables, 1
                do f=1, this%n_hash_functions, 1
                    q_i_bin(f) = sum( this%hash_functoin_sets(t)%matrix(:,f) * q_i(:) ) >= 0d0
                end do

                hash = 0_8
                do f=1, this%n_hash_functions, 1
                    if (q_i_bin(f)) hash = ibset(hash,f-1)
                end do

                id = -1_8
                do u=1, size(this%array_per_table(t)%hashval2id), 1
                    if (hash .eq. this%array_per_table(t)%hashval2id(u)) then
                        id = u; exit
                    end if
                end do
                if (id .eq. -1) cycle

                candidates = [candidates, this%array_per_table(t)%arrays(id)%indices]
            end do

            n_candidates = size(candidates)
            call quick_sort(candidates, n_candidates)
            call collect_unique_values(unique_candidates, candidates, n_candidates)

            n_uniq_candidates = size(unique_candidates)
            allocate( d_i(n_uniq_candidates) )
            do c=1, n_uniq_candidates, 1
                id = unique_candidates(c)
                d_i(c) = this%x_sq_sum(id) + q_i_sq_sum - 2d0 * sum( this%x(id,:)*q_i(:) )
            end do

            min_loc = minloc(d_i, dim=1)
            query_lsh_nearest_neighbor_random_projection_matmul%indices(n)%idx(1)   = unique_candidates(min_loc)
            query_lsh_nearest_neighbor_random_projection_matmul%distances(n)%dst(1) = sqrt(d_i(min_loc))
            deallocate( candidates, d_i )
        end do
        !$omp end do
        !$omp end parallel

    end function query_lsh_nearest_neighbor_random_projection_matmul

end module mod_lsh
