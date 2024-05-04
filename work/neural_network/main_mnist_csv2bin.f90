program main_mnist_csv2bin
    use mod_common
    implicit none

    character(:), allocatable :: d_sep, n_rows_char, n_cols_char, file_csv, file_bin, dtype_in, dtype_out

    file_csv = "../../sample_data/mnist_train_images.csv"
    file_bin = "../../sample_data/mnist_train_images.bin"
    call read2bin_2d(file_csv, file_bin, 60000_8, 784_8, f_, "r", "r")

    file_csv = "../../sample_data/mnist_train_labels.csv"
    file_bin = "../../sample_data/mnist_train_labels.bin"
    call read2bin_2d(file_csv, file_bin, 60000_8, 10_8, f_, "r", "r")

    file_csv = "../../sample_data/mnist_test_images.csv"
    file_bin = "../../sample_data/mnist_test_images.bin"
    call read2bin_2d(file_csv, file_bin, 10000_8, 784_8, f_, "r", "r")

    file_csv = "../../sample_data/mnist_test_labels.csv"
    file_bin = "../../sample_data/mnist_test_labels.bin"
    call read2bin_2d(file_csv, file_bin, 10000_8, 10_8, f_, "r", "r")

end program main_mnist_csv2bin