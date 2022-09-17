module mod_loss_function
    use mod_wengert_list
    use mod_activation_function
    include "./inc_use_activation_functions.f90"
    implicit none
    
    type, extends(activation_function_) :: loss_function
    end type loss_function 

    interface mean_squared_error
        module procedure mean_squared_error_nn
    end interface mean_squared_error

    interface mean_absolute_error
        module procedure mean_absolute_error_nn
    end interface mean_absolute_error

contains

    function mean_squared_error_nn(y_true, y_pred) result(loss)
        implicit none
        type(variable), intent(in) :: y_true, y_pred
        type(variable) :: loss
        loss = sum((y_true-y_pred)**2d0) / dble(y_true%sizes())
    end function mean_squared_error_nn

    function mean_absolute_error_nn(y_true, y_pred) result(loss)
        implicit none
        type(variable), intent(in) :: y_true, y_pred
        type(variable) :: loss
        loss = sum(abs(y_true-y_pred)) / dble(y_true%sizes())
    end function mean_absolute_error_nn

end module mod_loss_function