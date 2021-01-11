program main_dicretizer
    use mod_discretizer
    implicit none

    integer(kind=8) :: n_samples, n_columns
    real(kind=8), ALLOCATABLE    :: vector(:)
    integer(kind=8), ALLOCATABLE :: vector_disc(:)
    real(kind=8), ALLOCATABLE    :: matrix(:,:)
    integer(kind=8), ALLOCATABLE :: matrix_disc(:,:)
    integer(kind=8) :: i

    type(column_discretizer) :: col_disc
    type(discretizer) :: disc
    
    n_samples = 10
    n_columns = 5

    print*, '============================================================='
    print*, "MATRIX SHAPE: ", n_samples, n_columns
    print*, "VECTOR SHAPE: ", n_samples
    allocate(vector(n_samples))
    allocate(matrix(n_samples, n_columns))
    allocate(vector_disc(n_samples))
    allocate(matrix_disc(n_samples, n_columns))

    print*, '============================================================='
    print*, "Randomization"
    call RANDOM_NUMBER(vector)
    call RANDOM_NUMBER(matrix)


    print*, '============================================================='
    print*, "VECTOR, STRATEGY: uniform"
    col_disc = column_discretizer(max_bins=5_8, strategy="uniform")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do

    print*, '============================================================='
    print*, "VECTOR, STRATEGY: quantile"
    col_disc = column_discretizer(max_bins=5_8, strategy="quantile")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do

    print*, '============================================================='
    print*, "VECTOR, STRATEGY: kmeans"
    col_disc = column_discretizer(max_bins=5_8, strategy="kmeans")
    call col_disc%fit(vector)
    vector_disc = col_disc%transform(vector)
    do i=1, n_samples, 1
        print*, vector(i), vector_disc(i)
    end do


    print*, '============================================================='
    print*, "MATRIX, STRATEGY: uniform"
    disc = discretizer(max_bins=5_8, strategy="uniform")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

    print*, '============================================================='
    print*, "MATRIX, STRATEGY: quantile"
    disc = discretizer(max_bins=5_8, strategy="quantile")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

    print*, '============================================================='
    print*, "MATRIX, STRATEGY: kmeans"
    disc = discretizer(max_bins=5_8, strategy="kmeans")
    call disc%fit(matrix)
    matrix_disc = disc%transform(matrix)
    do i=1, n_samples, 1
        print*, matrix(i,:), " : ",  matrix_disc(i,:)
    end do

end program main_dicretizer
