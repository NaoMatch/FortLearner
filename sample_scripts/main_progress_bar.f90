! gfortran -fbounds-check ../common/mod_const.f90 ../common/mod_common.f90 main_progress_bar.f90 -o main_progress_bar.out
program main_progress_bar
    use mod_common, only: progress_bar
    implicit none
    
    integer(kind=4) :: i_i4, i_max_i4
    integer(kind=8) :: i_i8, i_max_i8

    i_max_i4 = 3
    do i_i4=1, i_max_i4, 1
        call progress_bar(i_i4, i_max_i4, 1)
        call sleep(1)
    end do

    ! i_max_i8 = 10
    ! do i_i8=1, i_max_i8, 1
    !     call progress_bar(i_i8, i_max_i8, 1_8)
    !     call sleep(1)
    ! end do

end program main_progress_bar
