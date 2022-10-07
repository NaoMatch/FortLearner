module mod_dense
    use mod_random
    use mod_const
    use mod_wengert_list
    use mod_matmul
    use mod_addition
    use mod_multiplication
    implicit none

    type dense
        integer(kind=8) :: in_dim
        integer(kind=8) :: out_dim
        logical(kind=4) :: bias=t_
        logical(kind=4) :: double_weight=f_
        type(variable) :: w
        type(variable) :: w_d
        type(variable) :: g
        type(variable) :: b
    contains
        procedure :: init => init_dense
        procedure :: act  => act_dense
    end type dense

contains

    subroutine init_dense(this)
        implicit none
        class(dense) :: this
        real(kind=8), allocatable :: w(:,:), b(:), w_d(:,:)
        allocate(w(this%in_dim, this%out_dim))
        allocate(b(this%out_dim))
        call rand_normal_2d_r8(w, this%in_dim, this%out_dim)
        b = 0d0
        w = w / sqrt(this%out_dim+0d0)
        this%w = variable(w, is_learnable=t_)
        this%b = variable(b, is_learnable=t_)
        deallocate(w, b)
        if (this%double_weight) then
            allocate(w_d(this%in_dim, this%out_dim))
            call rand_normal_2d_r8(w_d, this%in_dim, this%out_dim)
            w_d = w_d / sqrt(this%out_dim+0d0)
            this%w_d = variable(w_d, is_learnable=t_)
            deallocate(w_d)
        end if
    end subroutine init_dense

    function act_dense(this, var) result(res)
        implicit none
        class(dense) :: this
        type(variable) :: var, res, wg
        if (this%bias) then
            if (this%double_weight) then
                wg = this%w * this%w_d
                res = matmul(var, wg) + this%b
            else
                res = matmul(var, this%w) + this%b
            end if
        else
            res = matmul(var, this%w)
        end if
    end function act_dense
    
end module mod_dense