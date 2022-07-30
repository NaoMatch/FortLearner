module mod_var
    ! 型が一緒であれば継承元の関数をオーバーライドできる
    implicit none
    
    type variable
        real(kind=8), allocatable :: v(:,:)
    end type variable

    interface variable
        module procedure :: new_variable_s
        module procedure :: new_variable_m
    end interface variable

contains

    function new_variable_s(sclr)
        implicit none
        type(variable) :: new_variable_s
        real(kind=8), intent(in) :: sclr
        new_variable_s%v = reshape( (/sclr/), shape = [1,1] )
    end function new_variable_s
    
    function new_variable_m(mtrx)
        implicit none
        type(variable) :: new_variable_m
        real(kind=8), intent(in) :: mtrx(:,:)
        new_variable_m%v = mtrx(:,:)
    end function new_variable_m
    
end module mod_var