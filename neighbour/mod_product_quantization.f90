module mod_product_quantization
    use mod_const
    use mod_common_type
    use mod_kmeans
    implicit none
    

    type product_quantization
        integer(kind=8) :: n_code_books = 4_8
        integer(kind=8) :: n_clusters = 256_8
        type(jagged_matrix_r8), ALLOCATABLE :: codebooks(:)
    contains
        ! procedure :: fit => fit_product_quantization
    end type product_quantization

    !> Constructor
    interface product_quantization
        module procedure :: product_quantization
    end interface product_quantization

contains

    !> Contructor.
    !! \param n_code_books number of code books (number of sub-vector)
    !! \param n_clusters number of cluster 
    function new_product_quantization(n_code_books, n_clusters)
        implicit none
        type(product_quantization) :: new_product_quantization
        integer(kind=8), OPTIONAL  :: n_code_books
        integer(kind=8), OPTIONAL  :: n_clusters

        if (present(n_code_books)) new_product_quantization%n_code_books = n_code_books
        if (present(n_clusters))   new_product_quantization%n_clusters   = n_clusters
    end function new_product_quantization


end module mod_product_quantization
