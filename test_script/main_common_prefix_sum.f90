program main_common_prefix_sum
    !$ use omp_lib
    use mod_timer
    use mod_common
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: time_naive, time_super_scalar

    integer(kind=8) :: n_samples, iter, i
    real(kind=8), allocatable :: x(:), c(:), c_(:)
    integer(kind=8), allocatable :: arr_sizes(:)


    arr_sizes = [ &
        4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32 , &
        33, 34, 36, 38, 39, 41, 43, 45, 47, 49, 51, 53, 56, 58, 61, 64, 66, 69, 72, 76, 79, 82, 86, 90, 94, 98, &
        103, 107, 112, 117, 122, 128, 133, 139, 145, 152, 158, 165, 173, 181, 189, 197, 206, 215, 224, 234, 245, &
        256, 267, 279, 291, 304, 317, 331, 346, 362, 378, 394, 412, 430, 449, 469, 490, 512, 534, 558, 583, 608, &
        635, 663, 693, 724, 756, 789, 824, 861, 899, 939, 980, 1024 &
    ]

    do iter=1, size(arr_sizes), 1
        n_samples = arr_sizes(iter)
        allocate(x(n_samples), c(n_samples), c_(n_samples))
        x = (/(i, i=1, n_samples)/)

        call cumsum_naive(x, c, n_samples)
        call prefix_sum(x, c_, n_samples)

        print*, n_samples, "sum(naive - simd) = ", sum(c-c_) 
        deallocate(x, c, c_)
    end do

contains

    subroutine cumsum_naive(x,  cumsum_x, n_samples)
        real(kind=8), intent(in) :: x(n_samples)
        real(kind=8), intent(inout) :: cumsum_x(n_samples)
        integer(kind=8), intent(in) :: n_samples
    
        integer(kind=8) :: i

        cumsum_x(1) = x(1)

        do i=2, n_samples
            cumsum_x(i) = cumsum_x(i-1) + x(i)
        end do
    end subroutine cumsum_naive

end program main_common_prefix_sum