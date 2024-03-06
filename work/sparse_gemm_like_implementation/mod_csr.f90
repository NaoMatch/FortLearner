module mod_csr
    implicit none

    type row_elems
        integer(kind=8) :: n
        integer(kind=8), allocatable :: cols(:)
        real(kind=8), allocatable    :: vals(:)
    end type row_elems
    
    type csr_matrix
        integer(kind=8), allocatable :: rows(:)
        type(row_elems), allocatable :: cols(:)

        integer(kind=8) :: n_rows, n_cols
    contains
        procedure :: delete
        procedure :: insert
    end type csr_matrix

    interface csr_matrix 
        procedure :: new_csr_matrix
    end interface csr_matrix 
    
    type csr_matrix_for_c
        integer(kind=8), allocatable :: rows(:)
        integer(kind=8), allocatable :: cols(:)
        real(kind=8), allocatable :: vals(:)

        integer(kind=8) :: n_rows, n_cols
    contains
        procedure :: delete => delete_csr_matrix_for_c
        procedure :: insert => insert_csr_matrix_for_c
    end type csr_matrix_for_c

    interface csr_matrix_for_c 
        procedure :: new_csr_matrix_for_c
    end interface csr_matrix_for_c 

contains
    
    function new_csr_matrix_for_c()
        implicit none
        type(csr_matrix_for_c) :: new_csr_matrix_for_c
        allocate(new_csr_matrix_for_c%rows(1))
        allocate(new_csr_matrix_for_c%cols(0))
        allocate(new_csr_matrix_for_c%vals(0))

        new_csr_matrix_for_c%rows = 0

        new_csr_matrix_for_c%n_rows = 0
        new_csr_matrix_for_c%n_cols = 0
    end function new_csr_matrix_for_c

    subroutine delete_csr_matrix_for_c(this)
        implicit none
        class(csr_matrix_for_c) :: this
        allocate(this%rows(1))
        allocate(this%cols(0))
        allocate(this%vals(0))

        this%rows = 0

        this%n_rows = 0
        this%n_cols = 0
    end subroutine delete_csr_matrix_for_c

    subroutine insert_csr_matrix_for_c(this, cols, vals)
        implicit none
        class(csr_matrix_for_c) :: this
        integer(kind=8), intent(in) :: cols(:)
        real(kind=8), intent(in)    :: vals(:)

        integer(kind=8) :: row_id, n_elms
        type(row_elems) :: elem

        n_elms = size(cols)

        this%rows = [this%rows, n_elms+maxval(this%rows)]
        this%cols = [this%cols, cols-1]
        this%vals = [this%vals, vals]

        this%n_cols = maxval([this%n_cols, maxval(cols)])
        this%n_rows = this%n_rows + 1
    end subroutine insert_csr_matrix_for_c


    function new_csr_matrix()
        implicit none
        type(csr_matrix) :: new_csr_matrix
        allocate(new_csr_matrix%rows(0))
        allocate(new_csr_matrix%cols(0))
        new_csr_matrix%n_rows = 0
        new_csr_matrix%n_cols = 0
    end function new_csr_matrix

    subroutine delete(this)
        implicit none
        class(csr_matrix) :: this
        deallocate(this%rows)
        deallocate(this%cols)
        allocate(this%rows(0))
        allocate(this%cols(0))
        this%n_rows = 0
        this%n_cols = 0
    end subroutine delete

    subroutine insert(this, cols, vals)
        implicit none
        class(csr_matrix) :: this
        integer(kind=8), intent(in) :: cols(:)
        real(kind=8), intent(in)    :: vals(:)

        integer(kind=8) :: row_id, n_cols
        type(row_elems) :: elem

        n_cols = size(cols)

        row_id = this%n_rows + 1
        this%n_cols = maxval([this%n_cols, maxval(cols)])

        this%rows = [this%rows, row_id]

        elem%n = n_cols
        elem%cols = cols
        elem%vals = vals

        this%cols = [this%cols, elem]
        this%n_rows = this%n_rows + 1
    end subroutine insert


end module mod_csr