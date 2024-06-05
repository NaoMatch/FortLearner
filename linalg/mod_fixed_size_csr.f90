module mod_fixed_size_csr
    !$ use omp_lib
    use mod_timer
    use mod_random
    use mod_sort
    implicit none

    type fixed_size_csr_matrix
        integer(kind=8) :: start_index = 1
        integer(kind=8), allocatable :: rows(:)
        integer(kind=8), allocatable :: cols(:)
        real(kind=8), allocatable :: vals(:)

        integer(kind=8) :: n_rows, n_cols
        integer(kind=8) :: offset
    contains
        procedure :: to_dense
    end type fixed_size_csr_matrix
    
    interface fixed_size_csr_matrix 
        procedure :: new_fixed_size_csr_matrix
    end interface fixed_size_csr_matrix 

contains

    function new_fixed_size_csr_matrix(n_rows, n_cols, n_non_zero_per_row, start_index) result(fcsr)
        implicit none
        integer(kind=8), intent(in) :: n_rows, n_cols, n_non_zero_per_row
        integer(kind=8), optional :: start_index
        type(fixed_size_csr_matrix) :: fcsr
        integer(kind=8) :: r

        if (present(start_index)) fcsr%start_index = start_index

        allocate(fcsr%rows(n_rows+1))
        allocate(fcsr%cols(n_non_zero_per_row*n_rows))
        allocate(fcsr%vals(n_non_zero_per_row*n_rows))

        fcsr%rows(1) = start_index
        do r=1, n_rows, 1
            fcsr%rows(r+1) = n_non_zero_per_row * r + start_index
        end do

        fcsr%cols = start_index
        fcsr%vals = 0d0

        fcsr%n_rows = n_rows
        fcsr%n_cols = n_cols

        fcsr%offset = fcsr%start_index - 1
    end function new_fixed_size_csr_matrix

    function to_dense(this) result(matrix)
        implicit none
        class(fixed_size_csr_matrix) :: this
        real(kind=8), allocatable :: matrix(:,:)

        integer(kind=8) :: row, ini, fin, ii, col
        real(kind=8) :: val

        allocate(matrix(this%n_rows, this%n_cols))
        matrix(:,:) = 0d0

        do row=1, this%n_rows, 1
            ini = this%rows(row) - this%offset!*0
            fin = this%rows(row+1) - this%start_index!*0
            do ii=ini, fin, 1
                col = this%cols(ii) - this%offset!*0
                val = this%vals(ii)
                matrix(row, col) = val
            end do
        end do
    end function to_dense    

    subroutine dense2fcsr_weighted_sampling_mat(sp_mat, mat, n_top, dim, start_index, negative_weights, n_jobs)
        implicit none
        type(fixed_size_csr_matrix), allocatable, intent(inout) :: sp_mat
        real(kind=8), intent(in) :: mat(:,:)
        integer(kind=8), intent(in) :: n_top
        integer(kind=8), optional :: dim
        integer(kind=8), optional :: start_index
        character(len=*), optional :: negative_weights
        integer(kind=8), optional :: n_jobs

        integer(kind=8) :: dim_opt, start_index_opt
        integer(kind=8) :: n_columns, c, n_samples, s
        real(kind=8), allocatable :: tmp_vec(:), tmp_psum(:)
        integer(kind=8), allocatable :: indices(:)
        character(len=:), allocatable :: how
        integer(kind=8)        :: ini
        integer(kind=8)        :: n_threads
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

        n_threads = 4
        if (present(n_jobs)) n_threads = n_jobs

        ! Processing ------------------------------------------------------------------------------
        if (dim_opt==1_8) then
            n_columns = size(mat, dim=1_8)
            n_samples = size(mat, dim=2_8)
        else
            n_columns = size(mat, dim=2_8)
            n_samples = size(mat, dim=1_8)
        end if
        allocate(tmp_vec(n_columns))
        sp_mat = fixed_size_csr_matrix(n_samples, n_columns, n_top, start_index_opt)

        ! print*, '*********************************************************************************************'
        CALL OMP_SET_NUM_THREADS(1)
        !!$omp parallel shared(sp_mat), private(tmp_vec, ini, indices)
        !!$omp do
        do s=1, n_samples, 1
            ini = sp_mat%rows(s) - sp_mat%offset
            if (dim_opt==1_8) then
                tmp_vec = mat(:,s)
            else
                tmp_vec = mat(s,:)
            end if 
            
            call weighted_sampling(indices, n_top, tmp_vec, n_columns, replace=.false.)
            
            ! call quick_sort(indices, size(indices, kind=kind(0_8)))
            sp_mat%cols(ini:ini+size(indices)-1) = indices + sp_mat%offset
            sp_mat%vals(ini:ini+size(indices)-1) = tmp_vec(indices)
        end do
        !!$omp end do
        !!$omp end parallel


        return
        990 continue 
        stop "argument 'dim' must be 1 or 2."

        991 continue 
        stop "argument 'n_top' must be greater equal 1."
    end subroutine dense2fcsr_weighted_sampling_mat


    function extract_corresponding_position_vals_(input_mat, ref_csr) result(vals)
        implicit none
        real(kind=8), intent(in) :: input_mat(:,:)
        type(fixed_size_csr_matrix), intent(in) :: ref_csr

        real(kind=8), allocatable :: vals(:)

        integer(kind=8) :: row, ini, fin, ii, col, nnz
        real(kind=8) :: val

        if (size(ref_csr%rows) == 1_8) goto 999

        allocate(vals(size(ref_csr%vals)))

        nnz = 1
        do row=1, ref_csr%n_rows, 1
            ini = ref_csr%rows(row) - ref_csr%offset
            fin = ref_csr%rows(row+1) - ref_csr%start_index
            do ii=ini, fin, 1
                col = ref_csr%cols(ii) - ref_csr%offset
                vals(nnz) = input_mat(row, col)
                nnz = nnz + 1
            end do
        end do

        return
        999 continue
        stop "input csr matrix is not assigned."
    end function extract_corresponding_position_vals_    


end module mod_fixed_size_csr