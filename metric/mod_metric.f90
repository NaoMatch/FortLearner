module mod_metric
    use mod_stats
    implicit none
    
    !> A interface to call root_mean_square_error_real32
    interface root_mean_square_error
        module procedure root_mean_square_error_real32
        module procedure root_mean_square_error_real64
        module procedure root_mean_square_error_int32
        module procedure root_mean_square_error_int64
    end interface root_mean_square_error

    !> A interface to call mean_square_error_real32
    interface mean_square_error
        module procedure mean_square_error_real32
        module procedure mean_square_error_real64
        module procedure mean_square_error_int32
        module procedure mean_square_error_int64
        module procedure mean_square_error_r8_r8_mat
    end interface mean_square_error

contains

    !> A function to compute Mean Squared Error.
    !! \return returns Mean Squared Error
    !! \param y_true ground-truth values
    !! \param y_pred predicted values
    function mean_square_error_real32(y_true, y_pred)
        implicit none
        real(kind=4), intent(in) :: y_true(:)
        real(kind=4), intent(in) :: y_pred(:)
        real(kind=4) :: mean_square_error_real32

        real(kind=4) :: tmp_sq_sum
        integer(kind=4) :: i, num_y_true, num_y_pred

        include "./include/mean_square_error/inc_mean_square_error_detail.f90"
        mean_square_error_real32 = tmp_sq_sum / dble(num_y_true)
    end function mean_square_error_real32
    include "./include/mean_square_error/inc_mean_square_error.f90"


    !> A function to compute Root Mean Squared Error.
    !! \return returns Root Mean Squared Error
    !! \param y_true ground-truth values
    !! \param y_pred predicted values
    function root_mean_square_error_real32(y_true, y_pred)
        implicit none
        real(kind=4), intent(in) :: y_true(:)
        real(kind=4), intent(in) :: y_pred(:)
        real(kind=4) :: root_mean_square_error_real32
        root_mean_square_error_real32 = sqrt(mean_square_error_real32(y_true, y_pred))
    end function root_mean_square_error_real32
    include "./include/root_mean_square_error/inc_root_mean_square_error.f90"

end module mod_metric
