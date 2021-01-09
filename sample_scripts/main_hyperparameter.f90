program main_hyperparameter
    use mod_hyperparameter
    implicit none
    
    type, extends(hparam_base) :: test_hparam
        integer(kind=8)    :: hparam_i
        real(kind=8)       :: hparam_r
        character(len=256) :: hparam_c
    end type test_hparam

    type(test_hparam)  :: hparam
    integer(kind=8)    :: lo_i, hi_i
    real(kind=8)       :: lo_r, hi_r
    character(len=256) :: preset_list(3), param
    integer(kind=8)    :: i

    print*, '============================================================='
    print*, "integer  "
    lo_i = 1
    hi_i = 10
    hparam % algo_name = "test"
    hparam % hparam_i = 8
    ! hparam % hparam_i = 11 ! <- Error!
    print*, "    min: ", lo_i
    print*, "    max: ", hi_i
    call hparam%validate_int_range("hparam_i", hparam%hparam_i, lo_i, hi_i)


    print*, '============================================================='
    print*, "real  "
    lo_r = -.3
    hi_r = 10000
    hparam % algo_name = "test"
    hparam % hparam_r = 8
    ! hparam % hparam_r = hi_r+1 ! <- Error!
    ! hparam % hparam_r = -lo_r-1 ! <- Error!
    print*, "    min: ", lo_r
    print*, "    max: ", hi_r
    call hparam%validate_real_range("hparam_r", hparam%hparam_r, lo_r, hi_r)

    print*, '============================================================='
    print*, "character  "
    preset_list(1) = "hoge"
    preset_list(2) = "fuga"
    preset_list(3) = "piyo"
    hparam % hparam_c = "hoge"
    ! hparam % hparam_c = "foobar" ! <- Error!
    print*, "    preset: "
    do i=1, size(preset_list), 1
        print*, i, trim(preset_list(i))
    end do
    call hparam%validate_char_list("hparam_c", hparam % hparam_c, preset_list)


end program main_hyperparameter
