module mod_data_holder
    use mod_const
    use mod_common
    use mod_random
    use mod_linalg
    use mod_discretizer
    implicit none
    
    !> A type for work space
    type work_space
        real(kind=4), allocatable :: x_r4(:)
        real(kind=8), allocatable :: x_r8(:)
        integer(kind=4), allocatable :: i_i4(:)
        integer(kind=8), allocatable :: i_i8(:)
    end type work_space

    !> Explanatory variable pointer
    type x_pointer
        real(kind=4), pointer :: x_r4_ptr(:,:)
        real(kind=8), pointer :: x_r8_ptr(:,:)
    end type x_pointer

    !> Objective variable pointer
    type y_pointer
        real(kind=4), pointer    :: y_r4_ptr(:,:)
        real(kind=8), pointer    :: y_r8_ptr(:,:)
        integer(kind=4), pointer :: y_i4_ptr(:,:)
        integer(kind=8), pointer :: y_i8_ptr(:,:)
    end type y_pointer

    !> A type of Data holder in FortLearner
    type data_holder
        logical(kind=4) :: is_classification
        logical(kind=4) :: is_preprocessed = f_
        logical(kind=4) :: is_random_rotation = f_
        logical(kind=4) :: is_presort = f_
        integer(kind=8) :: n_outputs
        integer(kind=8) :: n_rows
        integer(kind=8) :: n_cols
        integer(kind=8) :: n_outs
        type(x_pointer) :: x_ptr
        type(y_pointer) :: y_ptr
        integer(kind=4), allocatable :: x_hist(:,:)

        type(work_space), allocatable :: works(:)
        real(kind=4), allocatable :: rr_mat_r4(:,:)
        real(kind=8), allocatable :: rr_mat_r8(:,:)

        type(discretizer) :: disc
    contains
        procedure :: preprocess_store_colwise
        procedure :: preprocess_random_rotate
        procedure :: preprocess_hist
    end type data_holder

    !> Interface to data_holder.
    interface data_holder
        procedure :: new_data_holder_r8_r8
    end interface data_holder

contains

    !> A subroutine to override data_holder.
    !> 'x' and 'y' are associated to 'x_ptr' and 'y_ptr'.
    !! \param x input features
    !! \param y input responses
    function new_data_holder_r8_r8(x, y)
        implicit none
        type(data_holder)    :: new_data_holder_r8_r8
        real(kind=8), target :: x(:,:)
        real(kind=8), target :: y(:,:)

        integer(kind=8) :: x_shape(2), y_shape(2)

        x_shape = shape(x)
        y_shape = shape(y)

        new_data_holder_r8_r8 % is_classification =  f_
        new_data_holder_r8_r8 % n_outputs         =  y_shape(2)
        new_data_holder_r8_r8 % n_rows            =  x_shape(1)
        new_data_holder_r8_r8 % n_cols            =  x_shape(2)
        new_data_holder_r8_r8 % x_ptr % x_r8_ptr => x
        new_data_holder_r8_r8 % y_ptr % y_r8_ptr => y

        new_data_holder_r8_r8 % is_preprocessed = f_
    end function new_data_holder_r8_r8


    !> A subroutine to build histgram, its strategy (how to discretize continuous values) depends on 'preprocessing::discretizer'
    subroutine preprocess_hist(this, max_bins, strategy)
        implicit none
        class(data_holder)           :: this
        integer(kind=8), intent(in)  :: max_bins
        character(len=*), intent(in) :: strategy
        integer(kind=8) :: c, i

        real(kind=8), allocatable :: vector(:)

        if (allocated(this%disc%column_discretizers)) deallocate(this%disc%column_discretizers)
        if (allocated(vector)) deallocate(vector)
        if (allocated(this%x_hist)) deallocate(this%x_hist)

        allocate(this%disc%column_discretizers(this%n_cols))
        allocate(vector(this%n_rows))
        allocate(this%x_hist(this%n_rows, this%n_cols))

        do c=1, this%n_cols, 1
            this%disc%column_discretizers(c) = column_discretizer(max_bins=max_bins, strategy=strategy)
            
            do i=1, this%n_rows, 1
                vector(i) = this % x_ptr % x_r8_ptr(i, c)
            end do

            call this%disc%column_discretizers(c)%fit(vector)

            this%x_hist(:,c) = this%disc%column_discretizers(c)%transform(vector)
        end do
    end subroutine preprocess_hist


    !> A subroutine to store 'x' with column wise
    subroutine preprocess_store_colwise(this)
        implicit none
        class(data_holder) :: this
        integer(kind=8)    :: i, n

        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_cols))
        do i=1, this%n_cols, 1
            if ( associated(this%x_ptr%x_r8_ptr) ) then
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                allocate(this%works(i)%x_r8(this%n_rows))
                do n=1, this%n_rows, 1
                    this%works(i)%x_r8(n) = this%x_ptr%x_r8_ptr(n,i)
                end do
            else
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                allocate(this%works(i)%x_r4(this%n_rows))
                do n=1, this%n_rows, 1
                    this%works(i)%x_r4(n) = this%x_ptr%x_r4_ptr(n,i)
                end do
            end if
        end do
    end subroutine preprocess_store_colwise


    !> A subroutine to sort 'x' by column and their original indices respect with 'x'.
    subroutine preprocess_presort(this)
        implicit none
        class(data_holder) :: this
        integer(kind=8)    :: i, n

        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_cols))
        do i=1, this%n_cols, 1
            if ( associated(this%x_ptr%x_r8_ptr) ) then
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                if (allocated(this%works(i)%i_i8)) deallocate(this%works(i)%i_i8)
                allocate(this%works(i)%x_r8(this%n_rows))
                allocate(this%works(i)%i_i8(this%n_rows))
                do n=1, this%n_rows, 1
                    this%works(i)%x_r8(n) = this%x_ptr%x_r8_ptr(n,i)
                    this%works(i)%i_i8(n) = n
                end do
                call pbucket_argsort_r8(this%works(i)%x_r8, this%works(i)%i_i8, int(this%n_rows, kind=8))
            else
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                if (allocated(this%works(i)%i_i4)) deallocate(this%works(i)%i_i4)
                allocate(this%works(i)%x_r4(this%n_rows))
                allocate(this%works(i)%i_i4(this%n_rows))
                do n=1, this%n_rows, 1
                    this%works(i)%x_r4(n) = this%x_ptr%x_r4_ptr(n,i)
                    this%works(i)%i_i4(n) = n
                end do
                call pbucket_argsort_r4(this%works(i)%x_r4, this%works(i)%i_i4, int(this%n_rows, kind=4))
            end if
        end do
    end subroutine preprocess_presort


    !> A subroutine to random rotate input 'x' and store generated random rotation matrix to 'work'.
    subroutine preprocess_random_rotate(this)
        implicit none
        class(data_holder) :: this
        real(kind=4), allocatable :: rr_mat_r4(:,:)
        real(kind=8), allocatable :: rr_mat_r8(:,:)
        integer(kind=8) :: i

        if (allocated(this%works)) deallocate(this%works)
        allocate(this%works(this%n_cols))
        if ( associated(this%x_ptr%x_r8_ptr) ) then
            allocate(rr_mat_r8(this%n_cols, this%n_cols))
            call random_rotation(rr_mat_r8, this%n_cols)

            do i=1, this%n_cols, 1
                if (allocated(this%works(i)%x_r8)) deallocate(this%works(i)%x_r8)
                allocate(this%works(i)%x_r8(this%n_rows))
                call multi_mat_vec(this%x_ptr%x_r8_ptr, rr_mat_r8(:,i), this%works(i)%x_r8, this%n_rows, this%n_cols)
            end do
            this%rr_mat_r8 = rr_mat_r8
        else
            allocate(rr_mat_r4(this%n_cols, this%n_cols))
            call random_rotation(rr_mat_r4, int(this%n_cols, kind=4))

            do i=1, this%n_cols, 1
                if (allocated(this%works(i)%x_r4)) deallocate(this%works(i)%x_r4)
                allocate(this%works(i)%x_r4(this%n_rows))
                call multi_mat_vec(this%x_ptr%x_r4_ptr, rr_mat_r4(:,i), this%works(i)%x_r4, & 
                        int(this%n_rows, kind=4), int(this%n_cols, kind=4))
            end do
            this%rr_mat_r4 = rr_mat_r4
        end if
    end subroutine preprocess_random_rotate

end module mod_data_holder