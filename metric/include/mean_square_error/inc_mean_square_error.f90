function mean_square_error_real64(y_true, y_pred)
    implicit none
    real(kind=8), intent(in) :: y_true(:)
    real(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: mean_square_error_real64

    real(kind=8) :: tmp_sq_sum
    integer(kind=8) :: i, num_y_true, num_y_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_real64 = tmp_sq_sum / dble(num_y_true)
end function mean_square_error_real64

function mean_square_error_int32(y_true, y_pred)
    implicit none
    real(kind=4), intent(in) :: y_true(:)
    integer(kind=4), intent(in) :: y_pred(:)
    real(kind=4) :: mean_square_error_int32

    real(kind=4) :: tmp_sq_sum
    integer(kind=4) :: i, num_y_true, num_y_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_int32 = tmp_sq_sum / dble(num_y_true)
end function mean_square_error_int32

function mean_square_error_int64(y_true, y_pred)
    implicit none
    real(kind=8), intent(in) :: y_true(:)
    integer(kind=8), intent(in) :: y_pred(:)
    real(kind=8) :: mean_square_error_int64

    real(kind=8) :: tmp_sq_sum
    integer(kind=8) :: i, num_y_true, num_y_pred

    include "./include/mean_square_error/inc_mean_square_error_detail.f90"
    mean_square_error_int64 = tmp_sq_sum / dble(num_y_true)
end function mean_square_error_int64

function mean_square_error_r8_r8_mat(y_true, y_pred)
    implicit none
    real(kind=8), intent(in) :: y_true(:,:)
    real(kind=8), intent(in) :: y_pred(:,:)
    real(kind=8) :: mean_square_error_r8_r8_mat

    real(kind=8) :: tmp_sq_sum
    integer(kind=8) :: i, num_y_true, num_y_pred, n_outpts

    n_outpts = size(y_true(1,:))
    num_y_true = size(y_true(:,1))
    num_y_pred = size(y_pred(:,1))
    if (num_y_true .ne. num_y_pred) then
        stop "Size of y_true and y_pred missmatch."
    end if

    tmp_sq_sum = 0
    do i=1, num_y_true, 1
        tmp_sq_sum = tmp_sq_sum + sum((y_true(i,:)-y_pred(i,:)) ** 2.0)
    end do        
    mean_square_error_r8_r8_mat = tmp_sq_sum / dble(num_y_true) / dble(n_outpts)
end function mean_square_error_r8_r8_mat
