! gfortran ../common/mod_const.f90 ../common/mod_common.f90 main_most_left_bit_position.f90 -o main_most_left_bit_position.out
program main_most_left_bit_position
    use mod_common, only: most_left_bit_position
    implicit none

    integer(kind=8) :: val

    print*, "============================================================="
    val = 8
    print*, val
    print*, most_left_bit_position(val)


    print*, "============================================================="
    val = huge(0_8)
    print*, val
    print*, most_left_bit_position(val)


    print*, "============================================================="
    val = -8
    print*, val
    print*, most_left_bit_position(val)

end program main_most_left_bit_position
