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

    interface cross_entropy_error
        module procedure cross_entropy_error_nn
    end interface cross_entropy_error

    interface binary_cross_entropy_error
        module procedure binary_cross_entropy_error_nn
    end interface binary_cross_entropy_error

contains

    function cross_entropy_error_nn(y_true, y_pred) result(loss)
        implicit none
        type(variable), intent(in) :: y_true, y_pred
        type(variable) :: loss(2)
        type(variable) :: h_logq, h_plogq, h_plogq_sum

        h_logq = log(y_pred)
        h_plogq = 0d0 - y_true * h_logq
        h_plogq_sum = sum(h_plogq)
        loss(1) = h_plogq_sum / dble(y_true%sizes())
        loss(2) = h_plogq
    end function cross_entropy_error_nn

    function binary_cross_entropy_error_nn(y_true, y_pred) result(loss)
        implicit none
        type(variable), intent(in) :: y_true, y_pred
        type(variable) :: loss
        type(variable) :: h_logq, h_logq1, h_logq2, h_plogq, h_plogq_sum

        h_logq1 = log(y_pred) * y_true
        ! h_logq2 = (1d0-y_true) * log(1d0 - y_pred) 
        h_logq2 = log(1d0 - y_pred) - log(1d0 - y_pred) * y_true
        h_logq = h_logq1 + h_logq2
        h_plogq = 0d0 - h_logq
        h_plogq_sum = sum(h_plogq)
        loss = h_plogq_sum / dble(y_true%sizes())
    end function binary_cross_entropy_error_nn

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