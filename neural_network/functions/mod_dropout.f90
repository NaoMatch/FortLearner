module mod_dropout
    use mod_config
    use mod_variable
    use mod_intrinsics
    implicit none

contains

    function dropout_(var_in, drop_ratio) result(var_out)
        implicit none
        type(variable) :: var_in
        real(kind=8), intent(in) :: drop_ratio
        type(variable) :: var_out
        real(kind=8), allocatable :: mask(:,:)
        integer(kind=8), allocatable :: mask_i8(:,:)

        if (is_train) then
            allocate(mask, source=vstack(var_in%id)%v)
            call random_number(mask)
            mask_i8 = mask > drop_ratio
            mask = dble(mask_i8) / (1d0 - drop_ratio)
            var_out = var_in * mask
        else
            var_out = var_in
        end if
    end function dropout_

end module mod_dropout