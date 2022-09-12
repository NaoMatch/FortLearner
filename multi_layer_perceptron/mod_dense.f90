module mod_dense
    use mod_random
    use mod_const
    use mod_var
    use mod_wengert_list
    use mod_matmul
    use mod_addition
    implicit none

    type dense
        integer(kind=8) :: in_dim, out_dim
        logical(kind=4) :: bias
        type(variable_) :: w
        type(variable_) :: b
    contains
        procedure :: init => init_dense
        procedure :: act  => act_dense
    end type dense

contains

    subroutine init_dense(this)
        implicit none
        class(dense) :: this
        real(kind=8), allocatable :: w(:,:), b(:)
        allocate(w(this%in_dim, this%out_dim))
        allocate(b(this%out_dim))
        call rand_normal_2d_r8(w, this%in_dim, this%out_dim)
        b = 0d0
        this%w = variable_(w, is_learnable=t_)
        this%b = variable_(b, is_learnable=t_)
    end subroutine init_dense

    function act_dense(this, var) result(res)
        implicit none
        class(dense) :: this
        type(variable_) :: var, res
        res = matmul(var, this%w) + this%b
    end function act_dense
    
end module mod_dense