module mod_data_summary
    use :: mod_kinds, only : r64, i64
    use :: mod_dump_load
    implicit none
    
    type data_summary
        integer(i64) :: n_rows
        integer(i64) :: n_cols

        integer(i64) :: n_uniq_rows
        integer(i64), allocatable :: n_uniq_values(:)
    contains
        procedure :: dump => dump_data_summary
        procedure :: load => load_data_summary
    end type data_summary

    interface get_data_summary
        module procedure :: get_data_summary_r64
        module procedure :: get_data_summary_i64
    end interface get_data_summary

contains

    function get_data_summary_r64(X) result(summary)
        implicit none
        real(r64), intent(in) :: X(:,:)
        type(data_summary) :: summary
        
        summary%n_rows = size(X, dim=1)
        summary%n_cols = size(X, dim=2)
    end function get_data_summary_r64

    function get_data_summary_i64(X) result(summary)
        implicit none
        integer(i64), intent(in) :: X(:,:)
        type(data_summary) :: summary

        summary%n_rows = size(X, dim=1)
        summary%n_cols = size(X, dim=2)        
    end function get_data_summary_i64

    subroutine dump_data_summary(this, unit)
        implicit none
        class(data_summary), intent(in) :: this
        integer(i64), intent(in)        :: unit
        call dump_i64(unit, this%n_rows)
        call dump_i64(unit, this%n_cols)
    end subroutine 

    subroutine load_data_summary(this, unit)
        implicit none
        class(data_summary), intent(inout) :: this
        integer(i64), intent(in)           :: unit
        call load_i64(unit, this%n_rows)
        call load_i64(unit, this%n_cols)
    end subroutine 

end module mod_data_summary
