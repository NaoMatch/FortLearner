module mod_hash
    !$ use omp_lib
    implicit none
    
    interface one_at_a_time_hash
        module procedure :: one_at_a_time_hash_i8
        module procedure :: one_at_a_time_hash_vec_i8
        module procedure :: one_at_a_time_hash_mat_i8
        module procedure :: one_at_a_time_hash_r8
        module procedure :: one_at_a_time_hash_vec_r8
        module procedure :: one_at_a_time_hash_mat_r8            
    end interface one_at_a_time_hash
    
    interface xorshift64_hash
        module procedure :: xorshift64_hash_i8
        module procedure :: xorshift64_hash_vec_i8
    end interface xorshift64_hash

contains

    function one_at_a_time_hash_r8(val)
        implicit none
        real(kind=8), intent(in) :: val
        integer(kind=8) :: one_at_a_time_hash_r8

        integer(kind=8) :: val_i8
        val_i8 = transfer(val, 0_8)

        one_at_a_time_hash_r8 = one_at_a_time_hash_i8(val_i8)
    end function one_at_a_time_hash_r8

    function one_at_a_time_hash_vec_r8(vector, n_elements)
        implicit none
        real(kind=8), intent(in) :: vector(n_elements)
        integer(kind=8), intent(in) :: n_elements
        integer(kind=8) :: one_at_a_time_hash_vec_r8

        integer(kind=8) :: hash
        integer(kind=8) :: i

        hash = 0_8

        do i=1, n_elements, 1
            print*, transfer(vector(i), 0_8)
            hash = hash + transfer(vector(i), 0_8)
            hash = hash + ishft(hash, 10_8)
            hash = xor(hash, ishft(hash, -6_8))
        end do
        hash = hash + ishft(hash, 3_8)
        hash = xor(hash, ishft(hash, -11_8))
        hash = hash + ishft(hash, 15_8)

        one_at_a_time_hash_vec_r8 = hash
    end function one_at_a_time_hash_vec_r8

    function one_at_a_time_hash_mat_r8(matrix, n_samples, n_elements)
        implicit none
        real(kind=8), intent(in)  :: matrix(n_samples, n_elements)
        integer(kind=8), intent(in)  :: n_samples, n_elements
        integer(kind=8), ALLOCATABLE, target :: one_at_a_time_hash_mat_r8(:)
        integer(kind=8), pointer :: res_ptr(:)

        integer(kind=8) :: i, j, jj, hash
        integer(kind=8) :: j_unroll, j_remain, j_unroll_size
        integer(kind=8) :: buffer_hash(15)

        allocate( one_at_a_time_hash_mat_r8(n_samples) )
        one_at_a_time_hash_mat_r8(:) = 0_8

        res_ptr => one_at_a_time_hash_mat_r8

        do j=1, n_elements, 1
            do i=1, n_samples, 1
                print*, transfer(matrix(i,j), 0_8)
                hash = one_at_a_time_hash_mat_r8(i)
                hash = hash + transfer(matrix(i,j), 0_8)
                hash = hash + ishft(hash, 10_8)
                hash = xor(hash, ishft(hash, -6_8))
                one_at_a_time_hash_mat_r8(i) = hash
            end do
        end do

        do i=1, n_samples, 1
            hash = one_at_a_time_hash_mat_r8(i)
            hash = hash + ishft(hash, 3_8)
            hash = xor(hash, ishft(hash, -11_8))
            hash = hash + ishft(hash, 15_8)
            one_at_a_time_hash_mat_r8(i) = hash
        end do
    end function one_at_a_time_hash_mat_r8


    function one_at_a_time_hash_i8(val)
        implicit none
        integer(kind=8), intent(in) :: val
        integer(kind=8) :: one_at_a_time_hash_i8

        integer(kind=8) :: hash

        hash = 0_8

        hash = hash + val
        hash = hash + ishft(hash, 10_8)
        hash = xor(hash, ishft(hash, -6_8))

        hash = hash + ishft(hash, 3_8)
        hash = xor(hash, ishft(hash, -11_8))
        hash = hash + ishft(hash, 15_8)

        one_at_a_time_hash_i8 = hash
    end function one_at_a_time_hash_i8

    function one_at_a_time_hash_vec_i8(vector, n_elements)
        implicit none
        integer(kind=8), intent(in) :: vector(n_elements)
        integer(kind=8), intent(in) :: n_elements
        integer(kind=8) :: one_at_a_time_hash_vec_i8

        integer(kind=8) :: hash
        integer(kind=8) :: i

        hash = 0_8

        do i=1, n_elements, 1
            hash = hash + vector(i)
            hash = hash + ishft(hash, 10_8)
            hash = xor(hash, ishft(hash, -6_8))
        end do
        hash = hash + ishft(hash, 3_8)
        hash = xor(hash, ishft(hash, -11_8))
        hash = hash + ishft(hash, 15_8)

        one_at_a_time_hash_vec_i8 = hash
    end function one_at_a_time_hash_vec_i8

    function one_at_a_time_hash_mat_i8(matrix, n_samples, n_elements)
        implicit none
        integer(kind=8), intent(in)  :: matrix(n_samples, n_elements)
        integer(kind=8), intent(in)  :: n_samples, n_elements
        integer(kind=8), ALLOCATABLE, target :: one_at_a_time_hash_mat_i8(:)
        integer(kind=8), pointer :: res_ptr(:)

        integer(kind=8) :: i, j, jj, hash
        integer(kind=8) :: j_unroll, j_remain, j_unroll_size
        integer(kind=8) :: buffer_hash(15)

        allocate( one_at_a_time_hash_mat_i8(n_samples) )
        one_at_a_time_hash_mat_i8(:) = 0_8

        res_ptr => one_at_a_time_hash_mat_i8

        do j=1, n_elements, 1
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


    function xorshift64_hash_i8(key)
        implicit none
        integer(kind=8), intent(in) :: key
        integer(kind=8) :: xorshift64_hash_i8
        xorshift64_hash_i8 = key

        xorshift64_hash_i8 = ieor(xorshift64_hash_i8, ishft(xorshift64_hash_i8, 13))
        xorshift64_hash_i8 = ieor(xorshift64_hash_i8, ishft(xorshift64_hash_i8, -7))
        xorshift64_hash_i8 = ieor(xorshift64_hash_i8, ishft(xorshift64_hash_i8, 17))
    end function xorshift64_hash_i8


    function xorshift64_hash_vec_i8(keys, n_elements)
        implicit none
        integer(kind=8), intent(in) :: keys(n_elements)
        integer(kind=8) :: xorshift64_hash_vec_i8, n_elements, k
        xorshift64_hash_vec_i8 = 0

        do k=1, n_elements, 1
            xorshift64_hash_vec_i8 = xorshift64_hash_vec_i8 + keys(k)    
            xorshift64_hash_vec_i8 = ieor(xorshift64_hash_vec_i8, ishft(xorshift64_hash_vec_i8, 13))
            xorshift64_hash_vec_i8 = ieor(xorshift64_hash_vec_i8, ishft(xorshift64_hash_vec_i8, -7))
            xorshift64_hash_vec_i8 = ieor(xorshift64_hash_vec_i8, ishft(xorshift64_hash_vec_i8, 17))
        end do
    end function xorshift64_hash_vec_i8

end module mod_hash
