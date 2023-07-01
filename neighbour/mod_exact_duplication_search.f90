module mod_exact_duplicate_search
    !$ use omp_lib
    use mod_nearest_neighbour, only: neighbor_results
    use mod_sort
    use mod_random
    use mod_hash
    use mod_common
    use mod_common_type
    use mod_data_holder    
    use mod_linalg, only: multi_mat_vec
    implicit none

    type, extends(jagged_vector_i8) ::  duplicate_index
    end type duplicate_index

    type exact_duplication_search
    contains
        procedure :: get_exact_duplication_row_indices => get_exact_duplication_row_indices_set_matrix_x_r8
        procedure :: get_exact_duplication_row_indices_set_matrix_x_r8_base
    end type exact_duplication_search

    interface exact_duplication_search
        module procedure :: new_exact_duplication_search
    end interface exact_duplication_search

contains


    !> Create new 'exact_duplication_search' object
    function new_exact_duplication_search()
        implicit none
        type(exact_duplication_search) :: new_exact_duplication_search
    end function new_exact_duplication_search


    function get_exact_duplication_row_indices_set_matrix_x_r8(this, x, check_raw_value) result(raw_duplicated_indices)
        implicit none
        class(exact_duplication_search) :: this
        real(kind=8), intent(in) :: x(:,:)
        logical(kind=4), optional :: check_raw_value

        logical(kind=4) :: check_raw_value_opt
        integer(kind=8) :: n_samples, n_columns
        type(duplicate_index), allocatable :: raw_duplicated_indices(:)
        type(duplicate_index), allocatable :: hash_duplicated_indices(:)
        type(duplicate_index), allocatable :: raw_idxs(:)
        integer(kind=8), allocatable :: indices(:)

        integer(kind=8) :: d, i, j, k, idx, counter
        integer(kind=8), allocatable :: collected_indices(:)
        real(kind=8), allocatable :: x_dup(:,:)

        check_raw_value_opt = f_
        if (present(check_raw_value)) check_raw_value_opt = check_raw_value
        
        ! First Hash value check.
        hash_duplicated_indices = this%get_exact_duplication_row_indices_set_matrix_x_r8_base(x, start_idx=1_8)

        ! No duplication. Fast return.
        if (size(hash_duplicated_indices) == 0_8) return

        ! Case 1. shape(x) = [n,1]
        if     (size(x, dim=2)==1_8) then
            return ! Fast return.
        else
            ! Second Hash value check.
            allocate(raw_duplicated_indices(size(hash_duplicated_indices)))
            counter = 0
            n_columns = size(x, dim=2)
            j = 1
            if (check_raw_value_opt) then
                do d=1, size(hash_duplicated_indices), 1
                    n_samples = size(hash_duplicated_indices(d)%vector)
                    allocate(x_dup(n_samples, n_columns))
                    do i=1, n_samples, 1
                        idx = hash_duplicated_indices(d)%vector(i)
                        x_dup(i,:) = x(idx,:)
                    end do
                    raw_idxs = raw_value_duplication_detection(x_dup, n_samples, n_columns)
                    do k=1, size(raw_idxs), 1
                        raw_duplicated_indices(j) = raw_idxs(k)
                        j = j + 1
                    end do
                    deallocate(x_dup)
                end do
            else
                do d=1, size(hash_duplicated_indices), 1
                    n_samples = size(hash_duplicated_indices(d)%vector)
                    allocate(x_dup(n_samples, n_columns))
                    do i=1, n_samples, 1
                        idx = hash_duplicated_indices(d)%vector(i)
                        x_dup(i,:) = x(idx,:)
                    end do
                    raw_idxs = this%get_exact_duplication_row_indices_set_matrix_x_r8_base(x_dup, start_idx=2_8)
                    do k=1, size(raw_idxs), 1
                        raw_duplicated_indices(j) = raw_idxs(k)
                        j = j + 1
                    end do
                    deallocate(x_dup)
                end do
            end if
        end if
        raw_duplicated_indices = raw_duplicated_indices(1:j-1)
    end function get_exact_duplication_row_indices_set_matrix_x_r8


    function raw_value_duplication_detection(x_subset, n_samples, n_columns) result(dup_index_sets)
        implicit none
        real(kind=8), intent(in) :: x_subset(n_samples, n_columns)
        integer(kind=8), intent(in) :: n_samples, n_columns
        type(duplicate_index), allocatable :: dup_index_sets(:)

        integer(kind=8) :: i, j, k, idx
        logical(kind=4) :: is_all_same
        type(duplicate_index) :: index_set
        logical(kind=4), allocatable :: is_stored(:)

        allocate(dup_index_sets(0))
        allocate(is_stored(n_samples)); is_stored = f_

        do i=1, n_samples, 1
            if (is_stored(i)) cycle
            allocate(index_set%vector(1))
            index_set%vector = i
            do j=i+1, n_samples, 1
                is_all_same = all(x_subset(i,:) == x_subset(j,:))
                if (is_all_same) index_set%vector = [index_set%vector, j]
            end do

            do k=1, size(index_set%vector), 1
                idx = index_set%vector(k)
                is_stored(idx) = t_
            end do

            if (size(index_set%vector)>1_8) dup_index_sets = [dup_index_sets, index_set]
            deallocate(index_set%vector)
        end do
    end function 


    function get_exact_duplication_row_indices_set_matrix_x_r8_base(this, x, start_idx) result(duplicated_indices)
        implicit none
        class(exact_duplication_search) :: this
        real(kind=8), intent(in) :: x(:,:)
        integer(kind=8) :: start_idx

        type(duplicate_index), allocatable :: duplicated_indices(:)
        integer(kind=8), allocatable :: collected_indices(:)

        integer(kind=8) :: n_samples, n_columns, i, n_unique, ini, fin, j
        integer(kind=8), allocatable :: hash_values(:), hash_indxs(:)

        n_samples = size(x, dim=1)
        n_columns = size(x, dim=2)
        
        allocate(hash_indxs(n_samples))
        do i=1, n_samples, 1
            hash_indxs(i) = i
        end do

        if (n_columns == 1_8) then
            allocate(hash_values(n_samples))
            do i=1, n_samples, 1
                hash_values(i) = transfer(x(i,:), 1_8)
            end do    
        else
            hash_values = one_at_a_time_hash(x, n_samples, n_columns, start_idx)
        end if

        call quick_argsort(hash_values, hash_indxs, n_samples)
        n_unique = count_unique(hash_values, n_samples)

        if (n_unique == n_samples) then
            allocate(duplicated_indices(0))
        else
            allocate(duplicated_indices(n_samples-n_unique))
            ini = 1
            j = 1
            do i=1, n_unique-1, 1
                fin = ini
                do while (t_)
                    if (hash_values(fin) .ne. hash_values(fin+1)) exit
                    fin = fin+1
                end do
                if (fin-ini >= 1_8) then
                    duplicated_indices(j)%vector = hash_indxs(ini:fin)
                    j = j+1
                end if
                ini = fin+1
            end do
            if (n_samples-ini >= 1_8) then 
                duplicated_indices(j)%vector = hash_indxs(ini:n_samples)
                j = j + 1
            end if
        end if
        duplicated_indices = duplicated_indices(1:j-1)
    end function get_exact_duplication_row_indices_set_matrix_x_r8_base

end module mod_exact_duplicate_search
