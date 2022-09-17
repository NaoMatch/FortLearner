module mod_encoder
    use mod_sort
    implicit none
    
    type one_hot_encoder
        integer(kind=8) :: n_uniq_vals
        integer(kind=8), allocatable :: original_vals(:)
    contains
        procedure, pass :: transform_i8_vector_to_matrix
        procedure, pass :: transform_i8_matrix_to_matrix
        generic :: transform => transform_i8_vector_to_matrix, transform_i8_matrix_to_matrix

        procedure :: inverse_transform
    end type one_hot_encoder

    interface one_hot_encoder
        module procedure new_one_hot_encoder
    end interface

contains

    function new_one_hot_encoder() result(res)
        implicit none
        type(one_hot_encoder) :: res
        res%n_uniq_vals = -1
    end function new_one_hot_encoder

    function transform_i8_vector_to_matrix(this, x) result(x_ohe)
        implicit none
        class(one_hot_encoder) :: this
        integer(kind=8), intent(in) :: x(:)
        integer(kind=8), allocatable :: x_ohe(:,:)

        integer(kind=8) :: n_samples, n, x_n, position
        integer(kind=8), allocatable :: x_sorted(:)

        n_samples = size(x)

        allocate(x_sorted, source=x)
        x_sorted(:) = x(:)
        call pbucket_sort(x_sorted, n_samples)

        call collect_unique_values(this%original_vals, x_sorted, n_samples)
        this%n_uniq_vals = size(this%original_vals)

        allocate(x_ohe(n_samples, this%n_uniq_vals))
        x_ohe(:,:) = 0_8
        do n=1, n_samples, 1
            x_n = x(n)
            position = minloc(abs(this%original_vals-x_n), dim=1)
            x_ohe(n,position) = 1
        end do
    end function transform_i8_vector_to_matrix
    
    function transform_i8_matrix_to_matrix(this, x) result(x_ohe)
        implicit none
        class(one_hot_encoder) :: this
        integer(kind=8), intent(in) :: x(:,:)
        integer(kind=8), allocatable :: x_ohe(:,:)

        integer(kind=8) :: n_columns
        
        n_columns = size(x, dim=2)

        if (n_columns /= 1_8) then
            print*, "Number of columns must be 1 in " // __FILE__
            stop
        else
            x_ohe = this%transform_i8_vector_to_matrix(x(:,1))
        end if
    end function transform_i8_matrix_to_matrix

    function inverse_transform(this, x_ohe) result(v_inv)
        implicit none
        class(one_hot_encoder) :: this
        integer(kind=8), intent(in) :: x_ohe(:,:)
        integer(kind=8), allocatable :: v_inv(:)

        integer(kind=8) :: n_samples, n_columns, n, position

        n_columns = size(x_ohe, dim=2)
        if (n_columns /= this%n_uniq_vals) then
            print*, "Number of columns must be equal that of unique values of 'ohe_hot_encoder' in " &
                // __FILE__ // "."
            stop
        end if

        n_samples = size(x_ohe, dim=1)
        allocate(v_inv(n_samples))
        do n=1, n_samples, 1
            position = maxloc(x_ohe(n,:), dim=1)
            v_inv(n) = this%original_vals(position)
        end do
    end function inverse_transform
    
end module mod_encoder