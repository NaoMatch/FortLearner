module mod_csr
    use mod_timer
    use mod_random
    use mod_common_type
    use mod_sort
    implicit none

    type csr_matrix
        integer(kind=8) :: start_index = 1
        integer(kind=8), allocatable :: rows(:)
        integer(kind=8), allocatable :: cols(:)
        real(kind=8), allocatable :: vals(:)

        integer(kind=8) :: n_rows, n_cols
        integer(kind=8) :: offset
    contains
        procedure :: to_dense
        procedure :: n_elements_per_row
        procedure :: count_col_idx
        procedure :: delete
        procedure, pass :: insert_value, insert_csr
        generic :: insert => insert_value, insert_csr
    end type csr_matrix

    interface csr_matrix 
        procedure :: new_csr_matrix
    end interface csr_matrix 

    interface dense2csr_weighted_sampling
        procedure :: dense2csr_weighted_sampling_mat
        procedure :: dense2csr_weighted_sampling_vec
    end interface dense2csr_weighted_sampling

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

    function to_dense(this) result(matrix)
        implicit none
        class(csr_matrix) :: this
        real(kind=8), allocatable :: matrix(:,:)

        integer(kind=8) :: row, ini, fin, ii, col
        real(kind=8) :: val

        allocate(matrix(this%n_rows, this%n_cols))
        matrix(:,:) = 0d0

        do row=1, this%n_rows, 1
            ini = this%rows(row) - this%offset
            fin = this%rows(row+1) - this%start_index
            do ii=ini, fin, 1
                col = this%cols(ii) - this%offset
                val = this%vals(ii)
                matrix(row, col) = val
            end do
        end do
    end function to_dense

    function n_elements_per_row(this) result(n_elements)
        implicit none
        class(csr_matrix) :: this
        integer(kind=8), allocatable :: n_elements(:)

        integer(kind=8) :: row, ini, fin, ii, col
        real(kind=8) :: val

        allocate(n_elements(this%n_rows))
        n_elements(:) = 0d0

        do row=1, this%n_rows, 1
            ini = this%rows(row) - this%offset
            fin = this%rows(row+1) - this%start_index
            n_elements(row) = fin - ini + 1
        end do
    end function n_elements_per_row

    function count_col_idx(this) result(counter)
        implicit none
        class(csr_matrix) :: this
        integer(kind=8), allocatable :: counter(:)

        integer(kind=8) :: row, ini, fin, ii, col
        real(kind=8) :: val

        allocate(counter(this%n_cols))
        counter(:) = 0d0

        do row=1, this%n_rows, 1
            ini = this%rows(row) - this%offset
            fin = this%rows(row+1) - this%start_index
            do ii=ini, fin, 1
                col = this%cols(ii) - this%offset
                counter(col) = counter(col) + 1
            end do
        end do
    end function count_col_idx

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

    subroutine insert_value(this, cols, vals)
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
    end subroutine insert_value

    subroutine insert_csr(this, csr)
        implicit none
        class(csr_matrix) :: this
        type(csr_matrix) :: csr

        integer(kind=8) :: row_id, n_elms, row_offset

        if (csr%n_cols /= this%n_cols) stop "Number of Column Missmatch."
        if (csr%offset /= this%offset) stop "Offset Missmatch."

        n_elms = size(this%rows)
        if (n_elms == 1_8) then
            this%rows = csr%cols
        else
            row_offset = this%rows(n_elms)
            this%rows = [this%rows(1:n_elms-1), row_offset+csr%rows(1:)]
        end if
        this%cols = [this%cols, csr%cols]
        this%vals = [this%vals, csr%vals]
        
        this%n_rows = this%n_rows + csr%n_rows
    end subroutine insert_csr


    function dense2csr_weighted_sampling_mat(mat, n_top, dim, start_index, negative_weights) result(sp_mat)
        implicit none
        type(csr_matrix) :: sp_mat
        real(kind=8), intent(in) :: mat(:,:)
        integer(kind=8), intent(in) :: n_top
        integer(kind=8), optional :: dim
        integer(kind=8), optional :: start_index
        character(len=*), optional :: negative_weights

        integer(kind=8) :: dim_opt, start_index_opt
        integer(kind=8) :: n_columns, c, n_samples, s
        real(kind=8), allocatable :: tmp_vec(:), tmp_psum(:)
        integer(kind=8), allocatable :: indices(:)
        character(len=:), allocatable :: how
        integer(kind=8)        :: date_value1(8), date_value2(8)

        ! Argument Check --------------------------------------------------------------------------
        dim_opt = 2
        if (present(dim)) dim_opt = dim
        if (dim_opt>2) goto 990

        start_index_opt = 1
        if (present(start_index)) start_index_opt = start_index

        if (n_top<=0) goto 991

        how = "filter"
        if (present(negative_weights)) how = negative_weights


        ! Processing ------------------------------------------------------------------------------
        if (dim_opt==1_8) then
            n_columns = size(mat, dim=1_8)
            n_samples = size(mat, dim=2_8)
        else
            n_columns = size(mat, dim=2_8)
            n_samples = size(mat, dim=1_8)
        end if

        allocate(tmp_vec(n_columns))
        allocate(indices(n_top))
        sp_mat = csr_matrix(n_columns, start_index_opt)

        do s=1, n_samples, 1
            if (dim_opt==1_8) then
                tmp_vec = mat(:,s)
            else
                tmp_vec = mat(s,:)
            end if 

            call weighted_sampling(indices, n_top, tmp_vec, n_columns, replace=.false., negative_weights=how)

            ! call quick_sort(indices, size(indices, kind=kind(0_8)))
            call sp_mat%insert(indices, tmp_vec(indices))
        end do


        return
        990 continue 
        stop "argument 'dim' must be 1 or 2."

        991 continue 
        stop "argument 'n_top' must be greater equal 1."
    end function dense2csr_weighted_sampling_mat

    function dense2csr_weighted_sampling_vec(vec, n_top, start_index, negative_weights) result(indices)
        implicit none
        real(kind=8), intent(in) :: vec(:)
        integer(kind=8), intent(in) :: n_top
        integer(kind=8), optional :: start_index
        character(len=*), optional :: negative_weights

        integer(kind=8) :: start_index_opt
        integer(kind=8) :: n_columns, c, n_samples, s
        real(kind=8), allocatable :: tmp_vec(:), tmp_psum(:)
        integer(kind=8), allocatable :: indices(:)
        character(len=:), allocatable :: how

        ! Argument Check --------------------------------------------------------------------------
        start_index_opt = 1
        if (present(start_index)) start_index_opt = start_index
        
        if (n_top<=0) goto 991
        
        how = "filter"
        if (present(negative_weights)) how = negative_weights

        ! Processing ------------------------------------------------------------------------------
        n_columns = size(vec)
        
        allocate(indices(n_top))

        ! call quick_sort(indices, size(indices, kind=kind(0_8)))
        call weighted_sampling(indices, n_top, vec, n_columns, replace=.false.)
        
        ! print*, indices
        return
        991 continue 
        stop "argument 'n_top' must be greater equal 1."
    end function dense2csr_weighted_sampling_vec

end module mod_csr