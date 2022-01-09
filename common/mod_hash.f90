module mod_hash
    !$ use omp_lib
    implicit none
    
    interface one_at_a_time_hash
        module procedure :: one_at_a_time_hash_i8
        module procedure :: one_at_a_time_hash_vec_i8
        module procedure :: one_at_a_time_hash_mat_i8
    end interface one_at_a_time_hash

contains

    function one_at_a_time_hash_i8(val)
        implicit none
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: one_at_a_time_hash_i8

        integer(kind=8) :: hash
        integer(kind=8) :: i

        hash = 0_8

        hash = hash + val
        hash = hash + ishft(hash, 10_8)
        hash = xor(hash, ishft(hash, -6_8))

        hash = hash + ishft(hash, 3_8)
        hash = xor(hash, ishft(hash, -11_8))
        hash = hash + ishft(hash, 15_8)

        one_at_a_time_hash_i8 = hash
    end function one_at_a_time_hash_i8

    function one_at_a_time_hash_vec_i8(vector, n_samples)
        implicit none
        integer(kind=8), intent(in) :: vector(n_samples)
        integer(kind=8), intent(in) :: n_samples
        integer(kind=8) :: one_at_a_time_hash_vec_i8

        integer(kind=8) :: hash
        integer(kind=8) :: i

        hash = 0_8

        do i=1, n_samples, 1
            hash = hash + vector(i)
            hash = hash + ishft(hash, 10_8)
            hash = xor(hash, ishft(hash, -6_8))
        end do
        hash = hash + ishft(hash, 3_8)
        hash = xor(hash, ishft(hash, -11_8))
        hash = hash + ishft(hash, 15_8)

        one_at_a_time_hash_vec_i8 = hash
    end function one_at_a_time_hash_vec_i8

    function one_at_a_time_hash_mat_i8(matrix, n_samples, n_columns)
        implicit none
        integer(kind=8), intent(in)  :: matrix(n_samples, n_columns)
        integer(kind=8), intent(in)  :: n_samples, n_columns
        integer(kind=8), ALLOCATABLE, target :: one_at_a_time_hash_mat_i8(:)
        integer(kind=8), pointer :: res_ptr(:)

        integer(kind=8) :: i, j, jj, hash
        integer(kind=8) :: j_unroll, j_remain, j_unroll_size
        integer(kind=8) :: buffer_hash(15)

        allocate( one_at_a_time_hash_mat_i8(n_samples) )
        one_at_a_time_hash_mat_i8(:) = 0_8

        res_ptr => one_at_a_time_hash_mat_i8

        do j=1, n_columns, 1
            do i=1, n_samples, 1
                hash = one_at_a_time_hash_mat_i8(i)
                hash = hash + matrix(i,j)
                hash = hash + ishft(hash, 10_8)
                hash = xor(hash, ishft(hash, -6_8))
                one_at_a_time_hash_mat_i8(i) = hash
            end do
        end do

        do i=1, n_samples, 1
            hash = one_at_a_time_hash_mat_i8(i)
            hash = hash + ishft(hash, 3_8)
            hash = xor(hash, ishft(hash, -11_8))
            hash = hash + ishft(hash, 15_8)
            one_at_a_time_hash_mat_i8(i) = hash
        end do
    end function one_at_a_time_hash_mat_i8

end module mod_hash