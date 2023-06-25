module mod_data_holder
    use mod_const
    use mod_common
    use mod_error
    use mod_random
    use mod_linalg
    use mod_discretizer
    implicit none
    
    !> A type for an additional work space used in 'data_holder'
    type work_space
        real(kind=4), allocatable :: x_r4(:)
        real(kind=8), allocatable :: x_r8(:)
        integer(kind=4), allocatable :: i_i4(:)
        integer(kind=8), allocatable :: i_i8(:)
    end type work_space

    !> A type for Explanatory variable pointer
    type x_pointer
        real(kind=4), pointer :: x_r4_ptr(:,:)
        real(kind=8), pointer :: x_r8_ptr(:,:)
        integer(kind=4), pointer :: x_i4_ptr(:,:)
        integer(kind=8), pointer :: x_i8_ptr(:,:)
    end type x_pointer

    !> A type for objective variable pointer
    type y_pointer
        real(kind=4), pointer    :: y_r4_ptr(:,:)
        real(kind=8), pointer    :: y_r8_ptr(:,:)
        integer(kind=4), pointer :: y_i4_ptr(:,:)
        integer(kind=8), pointer :: y_i8_ptr(:,:)
    end type y_pointer

    !> A type for Data holder in FortLearner
    type data_holder
        logical(kind=4) :: is_preprocessed = f_        !< already preprocessed or not. If true, skip proprocessing phase. If not, the appropriate preprocessing will be performed (i.e. binning).
        logical(kind=4) :: is_random_rotation = f_     !< to random rotation or not. If true, rotated matrix is stored to 'rr_mat_rX'.
        logical(kind=4) :: is_presort = f_             !< to presort or not. If true, presorted results column by colum is stored to 'works' (not implemented now).
        logical(kind=4) :: is_hist = f_                !< to binning or not. If true, binning array is stored row by row to 'x_hist_row'
        logical(kind=4) :: is_trans_x = f_             !< whether the entered explanatory variables have been transposed or not. Must be TRUE, if one want to use extra_tree_regressor%fit_faster().
        integer(kind=8) :: n_samples                   !< number of samples of input explanatory variable and objective variables.
        integer(kind=8) :: n_columns                   !< number of columns of input explanatory variable.
        integer(kind=8) :: n_outputs                   !< number of columns of input objective variable. In extra_tree_regressor%fit_faster(), this must be 1.

        integer(kind=8) :: x_shape(2)                  !< shape of input explanatory variable
        integer(kind=8) :: y_shape(2)                  !< shape of input objective variable

        type(x_pointer) :: x_ptr                       !< pointer to input explanatory variable
        type(y_pointer) :: y_ptr                       !< pointer to input objective variable
        type(x_pointer) :: x_t_ptr                     !< pointer to transposed input explanatory variable
        integer(kind=4), allocatable :: x_hist(:,:)    !< binned array of input explanatory variables
        integer(kind=4), pointer :: x_hist_ptr(:,:)    !< binned array of input explanatory variables

        type(work_space), allocatable :: works(:)      !< to be stored presorted results by column
        type(work_space), pointer :: works_ptr(:)      !< to be stored presorted results by column
        type(work_space), pointer :: rr_works_ptr(:)      !< to be stored presorted results by column
        real(kind=8), allocatable :: rr_mat_r8(:,:)    !< random rotated explanatory variable kind=4
        real(kind=8), pointer :: rr_mat_r8_ptr(:,:)    !< random rotated explanatory variable kind=8
        real(kind=4), allocatable :: rr_mat_r4(:,:)    !< random rotated explanatory variable kind=4
        real(kind=4), pointer :: rr_mat_r4_ptr(:,:)    !< random rotated explanatory variable kind=4

        type(discretizer) :: disc                      !< discretizer object
        real(kind=8), pointer :: cluster_centers_ptr(:,:)
        ! 
        type(work_space), allocatable :: x_hist_row(:) !< to be stored binned array of input explanatory variables by row to speed up by memory sequential access. @todo It could be achieved by storing the transposed and binned array in 'x_hist'. 
        type(work_space), pointer :: x_hist_row_ptr(:) !< to be stored binned array of input explanatory variables by row to speed up by memory sequential access. @todo It could be achieved by storing the transposed and binned array in 'x_hist'. 

        ! real(kind=8), pointer :: x_t_ptr
        real(kind=8), allocatable :: y_sq(:,:)
    contains
        procedure :: preprocess_store_colwise
        procedure :: preprocess_random_rotate
        procedure :: preprocess_random_rotate_new
        procedure :: preprocess_hist
        procedure :: preprocess_hist_new
        procedure :: preprocess_presort
        procedure :: preprocess_presort_new
        procedure :: preprocess_y_sq
    end type data_holder

    !> An interface to create new 'data_holder'.
    interface data_holder
        module procedure :: new_data_holder_i4_r4
        module procedure :: new_data_holder_r8_r8
        module procedure :: new_data_holder_r8_i8
        module procedure :: new_data_holder_r8
    end interface data_holder


contains
    !> A subroutine to create new 'data_holder' object.
    !> 'x' and 'y' are associated to 'x_ptr' and 'y_ptr'.
    !! \param x input features
    !! \param y input responses
    function new_data_holder_r8_r8(x, y, is_trans_x)
        implicit none
        type(data_holder)    :: new_data_holder_r8_r8
        real(kind=8), target :: x(:,:)
        real(kind=8), target :: y(:,:)
        logical(kind=4)      :: is_trans_x

        integer(kind=8) :: x_shape(2), y_shape(2)
        type(error)     :: err

        new_data_holder_r8_r8 % is_trans_x = is_trans_x
        x_shape = shape(x)
        y_shape = shape(y)
        if (is_trans_x) then
            x_shape = x_shape(2:1:-1)
            new_data_holder_r8_r8 % x_t_ptr % x_r8_ptr => x
        else
            new_data_holder_r8_r8 % x_ptr % x_r8_ptr   => x
        end if

        new_data_holder_r8_r8 % n_outputs          =  y_shape(2)
        new_data_holder_r8_r8 % y_shape            =  y_shape
        new_data_holder_r8_r8 % y_ptr % y_r8_ptr   => y
        call err % sample_size_mismatch(x_shape, "x", y_shape, "y", "data_holder")
        new_data_holder_r8_r8 % n_samples          =  x_shape(1)
        new_data_holder_r8_r8 % n_columns          =  x_shape(2)
        new_data_holder_r8_r8 % x_shape            =  x_shape

        new_data_holder_r8_r8 % is_preprocessed = f_
    end function new_data_holder_r8_r8

    function new_data_holder_r8(x)
        implicit none
        type(data_holder)    :: new_data_holder_r8
        real(kind=8), target :: x(:,:)
        logical(kind=4)      :: is_trans_x

        integer(kind=8) :: x_shape(2)
        type(error)     :: err

        x_shape = shape(x)
        new_data_holder_r8 % x_ptr % x_r8_ptr   => x

        new_data_holder_r8 % n_samples          =  x_shape(1)
        new_data_holder_r8 % n_columns          =  x_shape(2)
        new_data_holder_r8 % x_shape            =  x_shape

        new_data_holder_r8 % is_preprocessed = f_
    end function new_data_holder_r8
    include "./include/new_data_holder/inc_new_data_holder.f90"


    !> A subroutine to build histgram, its strategy (how to discretize continuous values) depends on 'preprocessing::discretizer'
    !! @todo replace ifdealloc
    !! \return returns binned input explanatory array and stored to 'x_hist' and 'x_hist_row'.
    !! \param max_bins maximum number of bins.
    !! \param strategy binning strategy.
    subroutine preprocess_hist(this, max_bins, strategy)
        implicit none
        class(data_holder)           :: this
        integer(kind=8), intent(in)  :: max_bins
        character(len=*), intent(in) :: strategy
        integer(kind=8) :: c, i, j
        integer(kind=8) :: n_samples, n_columns

        if (this%is_hist) return
        if (allocated(this%x_hist)) deallocate(this%x_hist)

        n_samples = this % n_samples
        n_columns = this % n_columns
        allocate(this%x_hist(n_samples, n_columns))

        this%disc = discretizer(max_bins, strategy)
        call this%disc%fit(this % x_ptr % x_r8_ptr)

        this%x_hist = this%disc%transform(this % x_ptr % x_r8_ptr)
        allocate(this%x_hist_row(this%n_samples))
        do i=1, this%n_samples, 1
            allocate(this%x_hist_row(i)%i_i4(this%n_columns))
            do j=1, this%n_columns, 1
                this%x_hist_row(i)%i_i4(j) = this%x_hist(i,j)
            end do
        end do
        ! print*, "finish"
        this%is_hist = t_
    end subroutine preprocess_hist

    subroutine preprocess_hist_new(this, x_hist, x_hist_row, max_bins, strategy)
        implicit none
        class(data_holder)           :: this
        integer(kind=4), allocatable, target, intent(inout) :: x_hist(:,:)
        type(work_space), allocatable, target, intent(inout) :: x_hist_row(:)
        integer(kind=8), intent(in)  :: max_bins
        character(len=*), intent(in) :: strategy
        integer(kind=8) :: c, i, j
        integer(kind=8) :: n_samples, n_columns

        if (this%is_hist) return
        if (allocated(x_hist)) deallocate(x_hist)
        if (allocated(x_hist_row)) deallocate(x_hist_row)

        n_samples = this % n_samples
        n_columns = this % n_columns
        allocate(x_hist(n_samples, n_columns))

        this%disc = discretizer(max_bins, strategy)
        call this%disc%fit(this % x_ptr % x_r8_ptr)

        x_hist = this%disc%transform(this % x_ptr % x_r8_ptr)
        allocate(x_hist_row(this%n_samples))
        do i=1, this%n_samples, 1
            allocate(x_hist_row(i)%i_i4(this%n_columns))
            do j=1, this%n_columns, 1
                x_hist_row(i)%i_i4(j) = x_hist(i,j)
            end do
        end do
        ! print*, "finish"
        this%is_hist = t_

        this%x_hist_ptr => x_hist
        this%x_hist_row_ptr => x_hist_row
    end subroutine preprocess_hist_new

    subroutine preprocess_y_sq(this)
        implicit none
        class(data_holder)           :: this
        integer(kind=8) :: c, i, j
        integer(kind=8) :: n_samples, n_outputs

        if (allocated(this%y_sq)) deallocate(this%y_sq)

        n_samples = this % n_samples
        n_outputs = this % n_outputs
        allocate(this%y_sq(n_samples, n_outputs))

        do j=1, n_outputs, 1
            do i=1, n_samples, 1
                this%y_sq(i,j) = this%y_ptr%y_r8_ptr(i,j)**2d0
            end do
        end do
    end subroutine preprocess_y_sq


    !> A subroutine to store input explanatory array with column wise
    subroutine preprocess_store_colwise(this)
        implicit none
        class(data_holder) :: this
        integer(kind=8)    :: i, n

        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_columns))
        do i=1, this%n_columns, 1
            if ( associated(this%x_ptr%x_r8_ptr) ) then
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                allocate(this%works(i)%x_r8(this%n_samples))
                do n=1, this%n_samples, 1
                    this%works(i)%x_r8(n) = this%x_ptr%x_r8_ptr(n,i)
                end do
            else
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                allocate(this%works(i)%x_r4(this%n_samples))
                do n=1, this%n_samples, 1
                    this%works(i)%x_r4(n) = this%x_ptr%x_r4_ptr(n,i)
                end do
            end if
        end do
    end subroutine preprocess_store_colwise


    !> A subroutine to sort input explanatory array by column and their original indices respect with input explanatory array.
    subroutine preprocess_presort(this)
        implicit none
        class(data_holder) :: this
        integer(kind=8)    :: i, n

        if (this%is_presort) return
        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_columns))
        do i=1, this%n_columns, 1
            if ( associated(this%x_ptr%x_r8_ptr) ) then
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                if (allocated(this%works(i)%i_i8)) deallocate(this%works(i)%i_i8)
                allocate(this%works(i)%x_r8(this%n_samples))
                allocate(this%works(i)%i_i8(this%n_samples))
                do n=1, this%n_samples, 1
                    this%works(i)%x_r8(n) = this%x_ptr%x_r8_ptr(n,i)
                    this%works(i)%i_i8(n) = n
                end do
                call pbucket_argsort(this%works(i)%x_r8, this%works(i)%i_i8, int(this%n_samples, kind=8))
            else
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                if (allocated(this%works(i)%i_i4)) deallocate(this%works(i)%i_i4)
                allocate(this%works(i)%x_r4(this%n_samples))
                allocate(this%works(i)%i_i4(this%n_samples))
                do n=1, this%n_samples, 1
                    this%works(i)%x_r4(n) = this%x_ptr%x_r4_ptr(n,i)
                    this%works(i)%i_i4(n) = n
                end do
                call pbucket_argsort(this%works(i)%x_r4, this%works(i)%i_i4, int(this%n_samples, kind=4))
            end if
        end do
        this%is_presort = t_
    end subroutine preprocess_presort


    subroutine preprocess_presort_new(this, works)
        implicit none
        class(data_holder) :: this
        type(work_space), allocatable, target :: works(:)
        integer(kind=8)    :: i, n

        print*, "preprocess_presort_new: ", loc(works)
        if (this%is_presort) return
        if (allocated(works)) deallocate(works)
        allocate(works(this%n_columns))
        do i=1, this%n_columns, 1
            if ( associated(this%x_ptr%x_r8_ptr) ) then
                if (allocated(works(i)%x_r8)) deallocate(works(i)%x_r8)
                if (allocated(works(i)%i_i8)) deallocate(works(i)%i_i8)
                allocate(works(i)%x_r8(this%n_samples))
                allocate(works(i)%i_i8(this%n_samples))
                do n=1, this%n_samples, 1
                    works(i)%x_r8(n) = this%x_ptr%x_r8_ptr(n,i)
                    works(i)%i_i8(n) = n
                end do
                call pbucket_argsort(works(i)%x_r8, works(i)%i_i8, int(this%n_samples, kind=8))
            else
                if (allocated(works(i)%x_r4)) deallocate(works(i)%x_r4)
                if (allocated(works(i)%i_i4)) deallocate(works(i)%i_i4)
                allocate(works(i)%x_r4(this%n_samples))
                allocate(works(i)%i_i4(this%n_samples))
                do n=1, this%n_samples, 1
                    works(i)%x_r4(n) = this%x_ptr%x_r4_ptr(n,i)
                    works(i)%i_i4(n) = n
                end do
                call pbucket_argsort(works(i)%x_r4, works(i)%i_i4, int(this%n_samples, kind=4))
            end if
        end do
        this%works_ptr => works
        this%is_presort = t_
    end subroutine preprocess_presort_new


    !> A subroutine to random rotate input explanatory array and store generated random rotation matrix to 'rr_mat_rX'.
    subroutine preprocess_random_rotate(this)
        implicit none
        class(data_holder) :: this
        real(kind=4), allocatable :: rr_mat_r4(:,:)
        real(kind=8), allocatable :: rr_mat_r8(:,:)
        integer(kind=8) :: i

        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_columns))
        if ( associated(this%x_ptr%x_r8_ptr) ) then
            allocate(rr_mat_r8(this%n_columns, this%n_columns))
            call random_rotation(rr_mat_r8, this%n_columns)

            do i=1, this%n_columns, 1
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                allocate(this%works(i)%x_r8(this%n_samples))
                call multi_mat_vec(this%x_ptr%x_r8_ptr, rr_mat_r8(:,i), this%works(i)%x_r8, this%n_samples, this%n_columns)
            end do
            this%rr_mat_r8 = rr_mat_r8
        else
            allocate(rr_mat_r4(this%n_columns, this%n_columns))
            call random_rotation(rr_mat_r4, int(this%n_columns, kind=4))

            do i=1, this%n_columns, 1
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                allocate(this%works(i)%x_r4(this%n_samples))
                call multi_mat_vec(this%x_ptr%x_r4_ptr, rr_mat_r4(:,i), this%works(i)%x_r4, & 
                        int(this%n_samples, kind=4), int(this%n_columns, kind=4))
            end do
            this%rr_mat_r4 = rr_mat_r4
        end if
    end subroutine preprocess_random_rotate

    subroutine preprocess_random_rotate_new(this, rr_works, rr_mat_r8)
        implicit none
        class(data_holder) :: this
        type(work_space), allocatable, target :: rr_works(:)
        real(kind=8), allocatable, target :: rr_mat_r8(:,:)
        integer(kind=8) :: i

        if (allocated(rr_works)) deallocate(rr_works)
        allocate(rr_works(this%n_columns))
        allocate(rr_mat_r8(this%n_columns, this%n_columns))
        call random_rotation(rr_mat_r8, this%n_columns)
        if ( associated(this%x_ptr%x_r8_ptr) ) then
            do i=1, this%n_columns, 1
                if (allocated(rr_works(i)%x_r8)) deallocate(rr_works(i)%x_r8)
                allocate(rr_works(i)%x_r8(this%n_samples))
                call multi_mat_vec(this%x_ptr%x_r8_ptr, rr_mat_r8(:,i), rr_works(i)%x_r8, this%n_samples, this%n_columns)
            end do
        end if
        this % rr_works_ptr => rr_works
        this % rr_mat_r8_ptr => rr_mat_r8
    end subroutine preprocess_random_rotate_new

end module mod_data_holder