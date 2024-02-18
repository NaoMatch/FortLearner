program main
    !$ use omp_lib
    use mod_timer
    use mod_common
    implicit none

    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: time_naive, time_super_scalar

    integer(kind=8) :: n_samples, i, iter, iter_counts, iter_counts_max
    integer(kind=4) :: n_samples_
    real(kind=8), allocatable :: x(:), c(:)
    integer(kind=8), allocatable :: arr_sizes(:)

    real(kind=8), allocatable :: a(:,:), b(:,:), d(:,:)

    real(kind=4), allocatable :: aaa(:),bbb(:)
    real(kind=8), allocatable :: aaa_(:),bbb_(:)


    iter_counts_max = 10000000000_8
    iter_counts_max = 5000000000_8
    iter_counts_max = 500000000_8
    ! iter_counts_max = 50000000_8
    ! iter_counts_max = 100000_8
    ! iter_counts_max = 1_8

    arr_sizes = [ &
        4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32 , &
        33, 34, 36, 38, 39, 41, 43, 45, 47, 49, 51, 53, 56, 58, 61, 64, 66, 69, 72, 76, 79, 82, 86, 90, 94, 98, &
        103, 107, 112, 117, 122, 128, 133, 139, 145, 152, 158, 165, 173, 181, 189, 197, 206, 215, 224, 234, 245, &
        256, 267, 279, 291, 304, 317, 331, 346, 362, 378, 394, 412, 430, 449, 469, 490, 512, 534, 558, 583, 608, &
        635, 663, 693, 724, 756, 789, 824, 861, 899, 939, 980, 1024 &
    ]

    ! arr_sizes = [256+15]

    do iter=1, size(arr_sizes), 1
        n_samples = arr_sizes(iter)
        print*, n_samples
        allocate(x(n_samples), c(n_samples))
        x = (/(i, i=1, n_samples)/)
        ! x = 1d0

        ! call prefix_sum_4elem(x, c, 0d0)
        ! print*, int(x)
        ! print*, int(c)
        ! stop

        iter_counts = maxval([int(iter_counts_max / n_samples, kind=kind(0_8)), 1_8])

        c = 0d0
        call date_and_time(values=date_value1)
        do i=1, iter_counts, 1
            call cumsum_naive(x, c, n_samples)
        end do
        call date_and_time(values=date_value2)
        time_naive = time_diff(date_value1, date_value2)
        print*, "cumsum naive                       :  ", &
            time_diff(date_value1, date_value2), time_diff(date_value1, date_value2)/dble(iter_counts), sum(c)

        ! c = 0d0
        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     call cumsum_ss(x, c, n_samples)
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_super_scalar = time_diff(date_value1, date_value2)
        ! print*, "cumsum super scalar                :  ", &
        !     time_diff(date_value1, date_value2), time_diff(date_value1, date_value2)/dble(iter_counts), sum(c)

        ! c = 0d0
        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     call prefix_sum(x, c, n_samples)
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_super_scalar = time_diff(date_value1, date_value2)
        ! print*, "cumsum simd                        :  ", &
        !     time_diff(date_value1, date_value2), time_diff(date_value1, date_value2)/dble(iter_counts), sum(c)
        ! ! print*, int(x)
        ! ! print*, int(c)

        c = 0d0
        call date_and_time(values=date_value1)
        do i=1, iter_counts, 1
            call prefix_sum(x, c, n_samples)
        end do
        call date_and_time(values=date_value2)
        time_super_scalar = time_diff(date_value1, date_value2)
        print*, "cumsum simd intel                  :  ", &
        time_diff(date_value1, date_value2), time_diff(date_value1, date_value2)/dble(iter_counts), sum(c)
        ! print*, int(x)
        ! print*, int(c)

        deallocate(x, c)
    end do


contains




    ! subroutine cumsum_naive_r4(x,  cumsum_x, n_samples)
    !     real(kind=4), intent(in) :: x(n_samples)
    !     real(kind=4), intent(inout) :: cumsum_x(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    
    !     integer(kind=8) :: i

    !     cumsum_x(1) = x(1)

    !     do i=2, n_samples
    !         cumsum_x(i) = cumsum_x(i-1) + x(i)
    !     end do
    ! end subroutine cumsum_naive_r4

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

    ! ! https://github.com/joelangeway/CumulativeSum/blob/548195d9c5b5e775ec5e8377884bc6b44a823a96/cumsum.c#L26
    ! subroutine cumsum_ss_r4(x,  cumsum_x, n_samples)
    !     real(kind=4), intent(in) :: x(n_samples)
    !     real(kind=4), intent(inout) :: cumsum_x(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    
    !     real(kind=4) :: s
    !     integer(kind=8) :: i, mod8
    !     real(kind=4) :: e0, e1, e2, e3, e4, e5, e6, e7

    !     mod8 = mod(n_samples, 8_8)

    !     if (n_samples>=8) then
    !         ! print*, mod8, n_samples
    !         s = 0d0
    !         do i=1, n_samples-mod8, 8
    !             ! print*, i, size(x)
    !             e0 = x(i+0) + s
    !             e1 = x(i+1)
    !             e2 = x(i+2)
    !             e3 = x(i+3)
    !             e4 = x(i+4)
    !             e5 = x(i+5)
    !             e6 = x(i+6)
    !             e7 = x(i+7)

    !             e1 = e1 + e0
    !             e3 = e3 + e2
    !             e5 = e5 + e4
    !             e7 = e7 + e6

    !             e2 = e2 + e1
    !             e3 = e3 + e1
    !             e6 = e6 + e5
    !             e7 = e7 + e5

    !             e4 = e4 + e3
    !             e5 = e5 + e3
    !             e6 = e6 + e3
    !             e7 = e7 + e3

    !             cumsum_x(i+0) = e0
    !             cumsum_x(i+1) = e1
    !             cumsum_x(i+2) = e2
    !             cumsum_x(i+3) = e3
    !             cumsum_x(i+4) = e4
    !             cumsum_x(i+5) = e5
    !             cumsum_x(i+6) = e6
    !             cumsum_x(i+7) = e7

    !             s = e7
    !         end do

    !         ! do i=n_samples-mod8+1, n_samples, 1
    !         !     s = s + x(i)
    !         !     c(i) = s
    !         ! end do
    !     else
    !         call cumsum_naive_r4(x, cumsum_x, n_samples)
    !     end if
    ! end subroutine cumsum_ss_r4

    ! subroutine cumsum_ss(x,  cumsum_x, n_samples)
    !     real(kind=8), intent(in) :: x(n_samples)
    !     real(kind=8), intent(inout) :: cumsum_x(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    
    !     real(kind=8) :: s
    !     integer(kind=8) :: i, mod8
    !     real(kind=8) :: e0, e1, e2, e3, e4, e5, e6, e7

    !     mod8 = mod(n_samples, 8_8)

    !     if (n_samples>=8) then
    !         ! print*, mod8, n_samples
    !         s = 0d0
    !         do i=1, n_samples-mod8, 8
    !             ! print*, i
    !             e0 = x(i+0) + s
    !             e1 = x(i+1)
    !             e2 = x(i+2)
    !             e3 = x(i+3)
    !             e4 = x(i+4)
    !             e5 = x(i+5)
    !             e6 = x(i+6)
    !             e7 = x(i+7)

    !             e1 = e1 + e0
    !             e3 = e3 + e2
    !             e5 = e5 + e4
    !             e7 = e7 + e6

    !             e2 = e2 + e1
    !             e3 = e3 + e1
    !             e6 = e6 + e5
    !             e7 = e7 + e5

    !             e4 = e4 + e3
    !             e5 = e5 + e3
    !             e6 = e6 + e3
    !             e7 = e7 + e3

    !             cumsum_x(i+0) = e0
    !             cumsum_x(i+1) = e1
    !             cumsum_x(i+2) = e2
    !             cumsum_x(i+3) = e3
    !             cumsum_x(i+4) = e4
    !             cumsum_x(i+5) = e5
    !             cumsum_x(i+6) = e6
    !             cumsum_x(i+7) = e7

    !             s = e7
    !         end do
    !         do i=n_samples-mod8+1, n_samples, 1
    !             s = s + x(i)
    !             cumsum_x(i) = s
    !         end do
    !     else
    !         call cumsum_naive(x, cumsum_x, n_samples)
    !     end if
    ! end subroutine cumsum_ss

end program main