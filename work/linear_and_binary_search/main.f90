program main
    use mod_timer
    use mod_common
    use mod_simple_tree
    implicit none
    
    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(kind=8) :: i, n_samples, step_size, idx, iter
    integer(kind=8) :: time_linear, time_binary, time_binary_branchless, time_binary_branchless_sift
    integer(kind=8) :: time_binary_branchless_step, time_binary_branchless_unroll
    integer(kind=8) :: time_binary_branchless_ordered
    real(kind=8) :: val

    real(kind=8), allocatable :: x(:), c(:), v(:), x_o(:)
    integer(kind=8), allocatable :: arr_sizes(:), step_sizes(:), o(:), pows(:)
    integer(kind=8) :: iter_counts_max, iter_counts, pow, counter
    type(simple_node) :: node
    type(simple_node) :: nodes(1)



    iter_counts_max = 10000000000_8
    iter_counts_max = 5000000000_8
    iter_counts_max = 500000000_8
    iter_counts_max = 50000000_8
    ! iter_counts_max = 100000_8
    ! iter_counts_max = 1_8

    arr_sizes = [ &
        4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32 , &
        33, 34, 36, 38, 39, 41, 43, 45, 47, 49, 51, 53, 56, 58, 61, 64, 66, 69, 72, 76, 79, 82, 86, 90, 94, 98, &
        103, 107, 112, 117, 122, 128, 133, 139, 145, 152, 158, 165, 173, 181, 189, 197, 206, 215, 224, 234, 245, &
        256, 267, 279, 291, 304, 317, 331, 346, 362, 378, 394, 412, 430, 449, 469, 490, 512, 534, 558, 583, 608, &
        635, 663, 693, 724, 756, 789, 824, 861, 899, 939, 980, 1024 &
    ]
    step_sizes = [ &
        4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8, 8, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 32, 32, 32, &
        32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, &
        64, 64, 64, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 256, 256, &
        256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 256, 512, 512, 512, 512, 512, 512, 512, &
        512, 512, 512, 512, 512, 512, 512, 512, 512, 1024 &
    ]
    pows = [ &
        2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, &
        5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, &
        7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, &
        9, 9, 9, 9, 10 &
    ]

    arr_sizes = [ &
        3_8, 4_8, 5_8, 7_8, 8_8, 10_8, 12_8, 15_8, 18_8, 21_8, 25_8, 31_8, 37_8, 44_8, 52_8, 63_8,  &
        75_8, 89_8, 106_8, 127_8, 151_8, 180_8, 214_8, 255_8, 303_8, 361_8, 429_8, 511_8, 607_8, 723_8, 860_8,  &
        1023_8, 1216_8, 1447_8, 1721_8, 2047_8, 2434_8, 2895_8, 3443_8, 4095_8, 4869_8, 5791_8, 6887_8, 8191_8,  &
        9740_8, 11584_8, 13776_8, 16383_8, 19482_8, 23169_8, 27553_8, 32767_8, 38966_8, 46339_8, 55107_8, 65535_8,  &
        77934_8, 92680_8, 110216_8, 131071_8, 155870_8, 185362_8, 220434_8, 262143_8, 311742_8, 370726_8, 440870_8,  &
        524287_8, 623486_8, 741454_8, 881742_8, 1048575_8 &
    ]
    step_sizes = [ &
        2_8, 4_8, 4_8, 4_8, 8_8, 8_8, 8_8, 8_8, 16_8, 16_8, 16_8, 16_8, 32_8, 32_8, 32_8, 32_8,  &
        64_8, 64_8, 64_8, 64_8, 128_8, 128_8, 128_8, 128_8, 256_8, 256_8, 256_8, 256_8, 512_8, 512_8, 512_8,  &
        512_8, 1024_8, 1024_8, 1024_8, 1024_8, 2048_8, 2048_8, 2048_8, 2048_8, 4096_8, 4096_8, 4096_8, 4096_8,  &
        8192_8, 8192_8, 8192_8, 8192_8, 16384_8, 16384_8, 16384_8, 16384_8, 32768_8, 32768_8, 32768_8, 32768_8,  &
        65536_8, 65536_8, 65536_8, 65536_8, 131072_8, 131072_8, 131072_8, 131072_8, 262144_8, 262144_8, 262144_8,  &
        262144_8, 524288_8, 524288_8, 524288_8, 524288_8 &
    ]

    pows = [ &
        1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9,  &
        9, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16,  &
        16, 16, 16, 17, 17, 17, 17, 18, 18, 18, 18, 19, 19, 19, 19 &
    ]

    arr_sizes = [ &
    3_8, 7_8, 15_8, 31_8, 63_8, 127_8, 255_8, 511_8, 1023_8, 2047_8, 4095_8, 8191_8, 16383_8, 32767_8, 65535_8,  &
    131071_8, 262143_8, 524287_8, 1048575_8 &
    ]
    step_sizes = [ &
    2_8, 4_8, 8_8, 16_8, 32_8, 64_8, 128_8, 256_8, 512_8, 1024_8, 2048_8, 4096_8, 8192_8, 16384_8, 32768_8,  &
    65536_8, 131072_8, 262144_8, 524288_8 &
    ]

    pows = [ &
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 &
    ]
    do iter=1, size(arr_sizes), 1
        n_samples = arr_sizes(iter)
        step_size = step_sizes(iter)
        pow = pows(iter)
        print*, n_samples, step_size, &
            find_nearest_power_of_two_below(n_samples), &
            find_nearest_power_of_two_above(n_samples)
    end do

    ! arr_sizes = [2047]
    ! step_sizes = [1024]
    ! pows = [10]

    open(10, file="linear_and_binary_search_right.log")
    do iter=1, size(arr_sizes), 1
        print*, '*********************************************************************************************'
        n_samples = arr_sizes(iter)
        step_size = step_sizes(iter)
        pow = pows(iter)

        iter_counts = maxval([int(iter_counts_max / log(n_samples+0d0), kind=kind(0_8)), 1_8])
        allocate(x(n_samples), v(iter_counts), x_o(n_samples), o(0))
        x = (/(i, i=1, n_samples)/)
        call random_number(v)
        v = (int(v*n_samples, kind=kind(0_8))+1_8) * 0.5
        ! v = 27

        print*, n_samples, step_size, pow, iter_counts


        ! counter = 0
        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = binary_search_right_unroll(x, n_samples, val, step_size)
        !     ! if (idx /= val) then
        !     !     print*, idx, val
        !     !     stop "idx = binary_search_right_unroll(x, n_samples, val, step_size)"
        !     ! end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_binary_branchless_unroll = time_diff(date_value1, date_value2)
        ! print*, "binary_search_right branchless unroll:  ", time_diff(date_value1, date_value2), idx, counter

        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = linear_search(x, n_samples, val)
        !     ! if (idx /= val) then
        !     !     stop "idx = linear_search(x, n_samples, val)"
        !     ! end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_linear = time_diff(date_value1, date_value2)
        ! print*, "linear_search                  :  ", time_diff(date_value1, date_value2), idx


        call date_and_time(values=date_value1)
        do i=1, iter_counts, 1
            val = v(i)
            idx = binary_search_right(x, n_samples, val)
            ! if (idx /= val) then
            !     stop "idx = binary_search_right(x, n_samples, val)"
            ! end if
        end do
        call date_and_time(values=date_value2)
        time_binary = time_diff(date_value1, date_value2)
        print*, "binary_search_right                  :  ", time_diff(date_value1, date_value2), idx

        call date_and_time(values=date_value1)
        do i=1, iter_counts, 1
            val = v(i)
            idx = binary_search_right_branchless(x, n_samples, val)
            ! if (idx /= val) then
            !     stop "idx = binary_search_right_branchless(x, n_samples, val)"
            ! end if
        end do
        call date_and_time(values=date_value2)
        time_binary_branchless = time_diff(date_value1, date_value2)
        print*, "binary_search_right branchless naive :  ", time_diff(date_value1, date_value2), idx

        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = binary_search_right_branchless_2(x, n_samples, val)
        !     ! if (idx /= val) then
        !     !     stop "idx = binary_search_right_branchless_2(x, n_samples, val)"
        !     ! end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_binary_branchless_sift = time_diff(date_value1, date_value2)
        ! print*, "binary_search_right branchless shift :  ", time_diff(date_value1, date_value2), idx

        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = binary_search_right_branchless_3(x, n_samples, val, step_size)
        !     ! if (idx /= val) then
        !     !     stop "idx = binary_search_right_branchless_3(x, n_samples, val, step_size)"
        !     ! end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_binary_branchless_step = time_diff(date_value1, date_value2)
        ! print*, "binary_search_right branchless step  :  ", time_diff(date_value1, date_value2), idx

        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = binary_search_right_branchless_4(x, n_samples, val)
        !     ! if (idx /= val) then
        !     !     stop "idx = binary_search_right_branchless_3(x, n_samples, val, step_size)"
        !     ! end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_binary_branchless_step = time_diff(date_value1, date_value2)
        ! print*, "binary_search_right branchless step4 :  ", time_diff(date_value1, date_value2), idx

        ! node = simple_node()
        ! call construct(node, x, n_samples)
        
        ! nodes(1) = node
        ! call extract_ordered_vector(nodes, o, 0_8)
        ! x_o = x(o)
        ! call date_and_time(values=date_value1)
        ! do i=1, iter_counts, 1
        !     val = v(i)
        !     idx = binary_search_ordered(x_o, o, n_samples, val, pow)
        !     if (idx /= val) then
        !         print*, idx, val
        !         stop "idx = binary_search_ordered(x, n_samples, val)"
        !     end if
        ! end do
        ! call date_and_time(values=date_value2)
        ! time_binary_branchless_ordered = time_diff(date_value1, date_value2)
        ! print*, "binary_search branchless ordered: ", time_diff(date_value1, date_value2), idx

        write(10, *), n_samples, time_linear/dble(iter_counts), ", ", time_binary/dble(iter_counts), ", ", &
            time_binary_branchless/dble(iter_counts), ", ", &
            time_binary_branchless_sift/dble(iter_counts), ", ", time_binary_branchless_step/dble(iter_counts), ", ", &
            time_binary_branchless_unroll/dble(iter_counts), ", ", time_binary_branchless_ordered/dble(iter_counts)
        deallocate(x, v, x_o, o)
    end do
    close(10)

    ! open(10, file="linear_and_binary_search_left.log")
    ! do iter=1, size(arr_sizes), 1
    !     print*, '*********************************************************************************************'
    !     n_samples = arr_sizes(iter)
    !     step_size = step_sizes(iter)
    !     pow = pows(iter)



    !     iter_counts = maxval([int(iter_counts_max / log(n_samples+0d0), kind=kind(0_8)), 1_8])
    !     allocate(x(n_samples), v(iter_counts), x_o(n_samples), o(0))
    !     x = (/(i, i=1, n_samples)/)
    !     call random_number(v)
    !     v = (int(v*n_samples, kind=kind(0_8))+1_8) * 0.5
    !     ! v = 27

    !     print*, n_samples, step_size, pow, iter_counts


    !     counter = 0
    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left_unroll(x, n_samples, val, step_size)
    !         ! if (idx /= val) then
    !         !     print*, idx, val
    !         !     stop "idx = binary_search_left_unroll(x, n_samples, val, step_size)"
    !         ! end if
    !         counter = counter + 1
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary_branchless_unroll = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left branchless unroll:  ", time_diff(date_value1, date_value2), idx, counter

    !     ! call date_and_time(values=date_value1)
    !     ! do i=1, iter_counts, 1
    !     !     val = v(i)
    !     !     idx = linear_search(x, n_samples, val)
    !     !     ! if (idx /= val) then
    !     !     !     stop "idx = linear_search(x, n_samples, val)"
    !     !     ! end if
    !     ! end do
    !     ! call date_and_time(values=date_value2)
    !     ! time_linear = time_diff(date_value1, date_value2)
    !     ! print*, "linear_search                  :  ", time_diff(date_value1, date_value2), idx


    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left(x, n_samples, val)
    !         ! if (idx /= val) then
    !         !     stop "idx = binary_search_left(x, n_samples, val)"
    !         ! end if
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left                  :  ", time_diff(date_value1, date_value2), idx

    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left_branchless(x, n_samples, val)
    !         ! if (idx /= val) then
    !         !     stop "idx = binary_search_left_branchless(x, n_samples, val)"
    !         ! end if
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary_branchless = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left branchless naive :  ", time_diff(date_value1, date_value2), idx

    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left_branchless_2(x, n_samples, val)
    !         ! if (idx /= val) then
    !         !     stop "idx = binary_search_left_branchless_2(x, n_samples, val)"
    !         ! end if
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary_branchless_sift = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left branchless shift :  ", time_diff(date_value1, date_value2), idx

    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left_branchless_3(x, n_samples, val, step_size)
    !         ! if (idx /= val) then
    !         !     stop "idx = binary_search_left_branchless_3(x, n_samples, val, step_size)"
    !         ! end if
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary_branchless_step = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left branchless step  :  ", time_diff(date_value1, date_value2), idx

    !     call date_and_time(values=date_value1)
    !     do i=1, iter_counts, 1
    !         val = v(i)
    !         idx = binary_search_left_branchless_4(x, n_samples, val)
    !         ! if (idx /= val) then
    !         !     stop "idx = binary_search_left_branchless_3(x, n_samples, val, step_size)"
    !         ! end if
    !     end do
    !     call date_and_time(values=date_value2)
    !     time_binary_branchless_step = time_diff(date_value1, date_value2)
    !     print*, "binary_search_left branchless step4 :  ", time_diff(date_value1, date_value2), idx

    !     ! node = simple_node()
    !     ! call construct(node, x, n_samples)
        
    !     ! nodes(1) = node
    !     ! call extract_ordered_vector(nodes, o, 0_8)
    !     ! x_o = x(o)
    !     ! call date_and_time(values=date_value1)
    !     ! do i=1, iter_counts, 1
    !     !     val = v(i)
    !     !     idx = binary_search_ordered(x_o, o, n_samples, val, pow)
    !     !     if (idx /= val) then
    !     !         print*, idx, val
    !     !         stop "idx = binary_search_ordered(x, n_samples, val)"
    !     !     end if
    !     ! end do
    !     ! call date_and_time(values=date_value2)
    !     ! time_binary_branchless_ordered = time_diff(date_value1, date_value2)
    !     ! print*, "binary_search branchless ordered: ", time_diff(date_value1, date_value2), idx

    !     write(10, *), n_samples, time_linear/dble(iter_counts), ", ", time_binary/dble(iter_counts), ", ", &
    !         time_binary_branchless/dble(iter_counts), ", ", &
    !         time_binary_branchless_sift/dble(iter_counts), ", ", time_binary_branchless_step/dble(iter_counts), ", ", &
    !         time_binary_branchless_unroll/dble(iter_counts), ", ", time_binary_branchless_ordered/dble(iter_counts)
    !     deallocate(x, v, x_o, o)
    ! end do
    ! close(10)


contains

    ! function get_nearest_smaller_pow_of_2(x) result(val)
    !     implicit none
    !     integer(kind=8), intent(in) :: x
    !     integer(kind=8) :: val

    !     integer(kind=8) :: tmp

    !     if (x<=0_8) goto 999

    !     if (x==1_8) then
    !         val = 1
    !         return
    !     end if

    !     if (x>=4611686018427387904_8) then
    !         val = 4611686018427387904_8
    !         return
    !     end if

    !     tmp = 2
    !     do while (.TRUE.)
    !         if (x==tmp) then
    !             val = tmp
    !             exit
    !         elseif (x<tmp) then
    !             val = ishft(tmp, -1)
    !             exit
    !         end if
    !         tmp = ishft(tmp, 1)
    !     end do

    !     return
    !     999 continue
    !     stop "'x' must be greater than 0."

    ! end function get_nearest_smaller_pow_of_2

    ! function get_nearest_greater_pow_of_2(x) result(val)
    !     implicit none
    !     integer(kind=8), intent(in) :: x
    !     integer(kind=8) :: val

    !     integer(kind=8) :: tmp

    !     if (x<=0_8 .or. x>4611686018427387904_8) goto 999

    !     if (x==1_8) then
    !         val = 1
    !         return
    !     end if

    !     tmp = 2
    !     do while (.TRUE.)
    !         if (x==tmp) then
    !             val = tmp
    !             exit
    !         elseif (x<tmp) then
    !             val = tmp
    !             exit
    !         end if
    !         tmp = ishft(tmp, 1)
    !     end do

    !     return
    !     999 continue
    !     stop "'x' must be greater than 0 or less eqaul 4611686018427387904_8."

    ! end function get_nearest_greater_pow_of_2

    ! function get_nearest_smaller_pow_of_2_ver2(x) result(val)
    !     implicit none
    !     integer(kind=8), intent(in) :: x
    !     integer(kind=8) :: val

    !     integer(kind=8) :: tmp

    !     if (x<=0_8) goto 999

    !     tmp = x - 1

    !     tmp = ior(tmp, (ishft(tmp,-1)))
    !     tmp = ior(tmp, (ishft(tmp,-2)))
    !     tmp = ior(tmp, (ishft(tmp,-4)))
    !     tmp = ior(tmp, (ishft(tmp,-8)))
    !     tmp = ior(tmp, (ishft(tmp,-16)))
    !     tmp = ior(tmp, (ishft(tmp,-32)))

    !     val = ishft(tmp + 1, -1)
    !     return
    !     999 continue
    !     stop "'x' must be greater than 0."
    ! end function get_nearest_smaller_pow_of_2_ver2

    ! function get_nearest_greater_pow_of_2_ver2(x) result(val)
    !     implicit none
    !     integer(kind=8), intent(in) :: x
    !     integer(kind=8) :: val

    !     integer(kind=8) :: tmp

    !     if (x<=0_8 .or. x>4611686018427387904_8) goto 999

    !     tmp = x - 1

    !     tmp = ior(tmp, (ishft(tmp,-1)))
    !     tmp = ior(tmp, (ishft(tmp,-2)))
    !     tmp = ior(tmp, (ishft(tmp,-4)))
    !     tmp = ior(tmp, (ishft(tmp,-8)))
    !     tmp = ior(tmp, (ishft(tmp,-16)))
    !     tmp = ior(tmp, (ishft(tmp,-32)))

    !     val = tmp + 1
    !     return
    !     999 continue
    !     stop "'x' must be greater than 0 or less eqaul 4611686018427387904_8."
    ! end function get_nearest_greater_pow_of_2_ver2


    ! !> Naive branchless binary search implementation 
    ! function binary_search_left_branchless(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: lo, hi, mid, flg, glf

    !     lo = 1
    !     hi = n_samples
    !     do while (lo .lt. hi)
    !         mid = (lo+hi)/2
    !         flg = vector(mid) .lt. value
    !         glf = 1-flg
    !         lo = (mid + 1)*flg + lo*glf
    !         hi = (mid)*glf + hi*flg
    !     end do     
    !     idx = lo   
    ! end function binary_search_left_branchless


    ! !> Branchless binary search implementation without div
    ! function binary_search_left_branchless_2(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: lo, hi, mid, flg, glf

    !     lo = 1
    !     hi = n_samples
    !     do while (lo .lt. hi)
    !         mid = ishft(lo+hi,-1)
    !         flg = vector(mid) .lt. value
    !         glf = 1-flg
    !         lo = (mid + 1)*flg + lo*glf
    !         hi = (mid)*glf + hi*flg
    !     end do     
    !     idx = lo   
    ! end function binary_search_left_branchless_2


    ! !> https://dirtyhandscoding.github.io/posts/performance-comparison-linear-search-vs-binary-search.html
    ! function binary_search_left_branchless_3(vector, n_samples, value, step) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, step
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: flg, step_

    !     idx = 0
    !     step_ = n_samples - step
    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)

    !     step_ = ishft(step,-1)
    !     do while (step_ > 0_8)
    !         flg = vector(idx+step_) .lt. value
    !         idx = (idx+step_)*flg + idx*(1-flg)
    !         step_ = ishft(step_,-1)
    !     end do   
    !     idx = idx + 1  
    ! end function binary_search_left_branchless_3

    ! function binary_search_left_branchless_4(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx, step

    !     integer(kind=8) :: flg, step_

    !     step = get_nearest_smaller_pow_of_2_ver2(n_samples)

    !     idx = 0
    !     step_ = n_samples - step
    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)

    !     step_ = ishft(step,-1)
    !     do while (step_ > 0_8)
    !         flg = vector(idx+step_) .lt. value
    !         idx = (idx+step_)*flg + idx*(1-flg)
    !         step_ = ishft(step_,-1)
    !     end do   
    !     idx = idx + 1  
    ! end function binary_search_left_branchless_4


    ! !> https://dirtyhandscoding.github.io/posts/performance-comparison-linear-search-vs-binary-search.html
    ! function binary_search_left_unroll(vector, n_samples, value, step) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, step
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: flg, step_

    !     idx = 0
    !     step_ = n_samples - step

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step,-1)


    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     flg = vector(idx+step_) .lt. value
    !     idx = (idx+step_)*flg + idx*(1-flg)
    !     step_ = ishft(step_,-1)

    !     idx = idx + 1  
    ! end function binary_search_left_unroll


    ! function binary_search_left_ordered(ordered_vector, order, n_samples, value, pow) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: ordered_vector(n_samples)
    !     integer(kind=8), intent(in) :: order(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, pow
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: i, flg, j

    !     i = 1
    !     j = 1
    !     ! print*, ordered_vector
    !     ! print*, order
    !     do while (j<=n_samples)
    !         ! print*, '------------------------------------------------------------------------------------------------'
    !         ! print*, '------------------------------------------------------------------------------------------------'
    !         ! print*, '------------------------------------------------------------------------------------------------'
    !         ! print*, "CHECK *** ordered_vector(j) == value ***", ordered_vector(j) == value
    !         if (ordered_vector(j) == value) then
    !             idx = order(j)
    !             exit
    !         else
    !             ! print*, "    CHECK *** ordered_vector(j) < value ***", ordered_vector(j) < value
    !             flg = ordered_vector(j) < value
    !             i = 2*i + flg
    !         end if
    !         ! print*, i, j

    !         if (i==n_samples) then
    !             ! print*, "             exit"
    !             j=n_samples
    !             exit
    !         elseif (i>n_samples) then
    !             ! print*, "             exit"
    !             j=n_samples
    !             exit
    !         end if

    !         j = i
    !     end do
    !     ! print*, '------------------------------------------------------------------------------------------------'
    !     ! idx = myfunc(i, n_samples)
    !     idx = order(j)
    !     ! print*, '*********************************************************************************************'
    !     ! print*, '*********************************************************************************************'
    !     ! print*, '*********************************************************************************************'
    !     ! print*, idx, i
    !     ! print*, '*********************************************************************************************'
    !     ! print*, '*********************************************************************************************'
    !     ! print*, '*********************************************************************************************'
    ! end function binary_search_left_ordered




    ! !> Naive branchless binary search implementation 
    ! function binary_search_right_branchless(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: lo, hi, mid, flg, glf

    !     lo = 1
    !     hi = n_samples
    !     do while (lo .lt. hi)
    !         mid = (lo+hi)/2
    !         flg = value .lt. vector(mid)
    !         glf = 1-flg
    !         lo = (mid + 1)*glf + lo*flg
    !         hi = (mid)*flg + hi*glf
    !     end do     
    !     idx = lo   
    ! end function binary_search_right_branchless


    ! !> Branchless binary search implementation without div
    ! function binary_search_right_branchless_2(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: lo, hi, mid, flg, glf

    !     lo = 1
    !     hi = n_samples
    !     do while (lo .lt. hi)
    !         mid = ishft(lo+hi,-1)
    !         flg = value .lt. vector(mid)
    !         glf = 1-flg
    !         lo = (mid + 1)*glf + lo*flg
    !         hi = (mid)*flg + hi*glf
    !     end do     
    !     idx = lo   
    ! end function binary_search_right_branchless_2


    ! !> https://dirtyhandscoding.github.io/posts/performance-comparison-linear-search-vs-binary-search.html
    ! function binary_search_right_branchless_3(vector, n_samples, value, step) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, step
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: flg, step_

    !     idx = 0
    !     step_ = n_samples - step
    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*(flg)

    !     step_ = ishft(step,-1)
    !     do while (step_ > 0_8)
    !         flg = value .lt. vector(idx+step_)
    !         idx = (idx+step_)*(1-flg) + idx*(flg)
    !         step_ = ishft(step_,-1)
    !     end do   
    !     idx = idx + 1  
    ! end function binary_search_right_branchless_3

    ! function binary_search_right_branchless_4(vector, n_samples, value) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx, step

    !     integer(kind=8) :: flg, step_

    !     step = get_nearest_smaller_pow_of_2_ver2(n_samples)

    !     idx = 0
    !     step_ = n_samples - step
    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*(flg)

    !     step_ = ishft(step,-1)
    !     do while (step_ > 0_8)
    !         flg = value .lt. vector(idx+step_)
    !         idx = (idx+step_)*(1-flg) + idx*(flg)
    !         step_ = ishft(step_,-1)
    !     end do   
    !     idx = idx + 1  
    ! end function binary_search_right_branchless_4


    ! !> https://dirtyhandscoding.github.io/posts/performance-comparison-linear-search-vs-binary-search.html
    ! function binary_search_right_unroll(vector, n_samples, value, step) result(idx)
    !     implicit none
    !     real(kind=8), intent(in) :: vector(n_samples)
    !     integer(kind=8), intent(in) :: n_samples, step
    !     real(kind=8), intent(in) :: value
    !     integer(kind=8) :: idx

    !     integer(kind=8) :: flg, step_

    !     idx = 0
    !     step_ = n_samples - step

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step,-1)


    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     flg = value .lt. vector(idx+step_)
    !     idx = (idx+step_)*(1-flg) + idx*flg
    !     step_ = ishft(step_,-1)

    !     idx = idx + 1  
    ! end function binary_search_right_unroll




end program main