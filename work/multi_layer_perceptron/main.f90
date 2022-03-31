program main
    use mod_variable
    use mod_my_model
    implicit none

    real(kind=8) :: x(3, 2)
    type(variable) :: var_out
    type(my_model) :: model

    call random_number(x)
    x(:,:) = 0.1d0
    model = my_model()
    print*, '*********************************************************************************************'
    var_out = model%forward(x)

    print*, '*********************************************************************************************'
    ! print*, var_out(1)%creator%vars_in(1)%creator%vars_in(1)%x
    ! print*, ""
    ! print*, var_out(1)%creators%vars_in(1)%x
    ! print*, ""
    ! print*, var_out(1)%x

end program main
