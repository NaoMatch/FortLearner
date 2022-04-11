program main_hash_table
    use mod_hash_table
    implicit none

    integer(kind=8) :: n_samples, n_columns
    integer(kind=8) :: min_val, max_val
    integer(kind=8), allocatable :: q(:)
    integer(kind=8), allocatable :: x(:,:)
    real(kind=8), allocatable    :: x_r8(:,:)

    type(hash_table) :: htable, htable2
    type(index_sets) :: dup_idx_sets, dup_idx_sets2
    integer(kind=8), allocatable  :: idx_sets(:), idx_sets2(:)

    min_val = 0
    max_val = 5

    n_samples = 10000
    n_columns = 3
    allocate(x(n_samples, n_columns))
    allocate(x_r8(n_samples, n_columns))

    call random_number(x_r8)
    x = x_r8 * (max_val-min_val) + min_val

    htable = hash_table()
    call htable%create_table(x)
    call htable%dump(file_name="htable.bin")    
    call htable2%load(file_name="htable.bin")

    q = x(1,:)
    print*, q(:)

    print*, '*********************************************************************************************'
    idx_sets = htable%search(q)
    print*, idx_sets
    
    print*, '*********************************************************************************************'
    idx_sets2 = htable2%search(q)
    print*, idx_sets2

end program main_hash_table