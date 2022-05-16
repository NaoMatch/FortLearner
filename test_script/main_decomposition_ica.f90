program main_decomposition_ica
    use mod_ica
    use mod_common
    implicit none
    
    type(ica) :: ica1
    CHARACTER(len=256) :: file_name_x_bin
    real(kind=8), ALLOCATABLE :: x(:,:)
    
    file_name_x_bin = "../sample_data/make_regression_X_0000100000x00100.bin"
    call read_bin_2d(file_name_x_bin, x)

    ica1 = ica()
    call ica1%fit(x)

end program main_decomposition_ica