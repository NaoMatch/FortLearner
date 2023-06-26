program main_common_hash_one_at_a_time_hash
    use mod_hash
    implicit none

    integer(kind=8) :: i
    integer(kind=8) :: res_1, res_2
    integer(kind=8) :: res_1_(2), res_2_(2)

    real(kind=8) :: x
    integer(kind=8) :: xi

    real(kind=8) :: xs(4)
    integer(kind=8) :: xsi(4)

    real(kind=8) :: ms(2,2)
    integer(kind=8) :: msi(2,2)


    print*, '*********************************************************************************************'
    x = 10d0
    xi = transfer(x, xi)
    res_1 = one_at_a_time_hash(x)
    res_2 = one_at_a_time_hash(xi)
    print*, "x = ", x, res_1
    print*, "xi = ", xi, res_2

    print*, '*********************************************************************************************'
    call random_number(xs)
    xsi = transfer(xs, xsi)
    res_1 = one_at_a_time_hash(xs, 4_8)
    res_2 = one_at_a_time_hash(xsi, 4_8)
    print*, "xs  = ", xs,  res_1
    print*, "xsi = ", xsi, res_2

    print*, '*********************************************************************************************'
    call random_number(ms)
    do i=1, 2, 1
        msi(i,:) = transfer(ms(i,:), [1_8, 1_8])
    end do
    res_1_ = one_at_a_time_hash(ms, 2_8, 2_8)
    res_2_ = one_at_a_time_hash(msi, 2_8, 2_8)
    print*, "ms  = ", ms,  res_1_
    print*, "msi = ", msi, res_2_

end program main_common_hash_one_at_a_time_hash