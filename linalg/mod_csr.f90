module mod_csr
    implicit none

    type csr_matrix
        integer(kind=8) :: start_index = 1
        integer(kind=8), allocatable :: rows(:)
        integer(kind=8), allocatable :: cols(:)
        real(kind=8), allocatable :: vals(:)

        integer(kind=8) :: n_rows, n_cols
        integer(kind=8) :: offset
    contains
        procedure :: delete
        procedure :: insert
    end type csr_matrix

    interface csr_matrix 
        procedure :: new_csr_matrix
    end interface csr_matrix 
    
contains

    function new_csr_matrix(n_columns, start_index)
        implicit none
        integer(kind=8) :: n_columns
        integer(kind=8), optional :: start_index
        type(csr_matrix) :: new_csr_matrix

        if (present(start_index)) new_csr_matrix%start_index = start_index

        allocate(new_csr_matrix%rows(1))
        allocate(new_csr_matrix%cols(0))
        allocate(new_csr_matrix%vals(0))
        new_csr_matrix%rows = new_csr_matrix%start_index
        new_csr_matrix%n_rows = 0
        new_csr_matrix%n_cols = n_columns

        new_csr_matrix%offset = new_csr_matrix%start_index - 1
    end function new_csr_matrix

    subroutine delete(this)
        implicit none
        class(csr_matrix) :: this

        if (allocated(this%rows)) deallocate(this%rows)
        if (allocated(this%cols)) deallocate(this%cols)
        if (allocated(this%vals)) deallocate(this%vals)

        allocate(this%rows(1))
        allocate(this%cols(0))
        allocate(this%vals(0))

        this%rows = this%start_index

        this%n_rows = 0
    end subroutine delete

    subroutine insert(this, cols, vals)
        implicit none
        class(csr_matrix) :: this
        integer(kind=8), intent(in) :: cols(:)
        real(kind=8), intent(in)    :: vals(:)

        integer(kind=8) :: row_id, n_elms

        if (maxval(cols) > this%n_cols) stop "Number of Column Missmatch."

        n_elms = size(cols)

        this%rows = [this%rows, n_elms+maxval(this%rows)]
        this%cols = [this%cols, cols + this%offset]
        this%vals = [this%vals, vals]

        this%n_rows = this%n_rows + 1
    end subroutine insert


end module mod_csr