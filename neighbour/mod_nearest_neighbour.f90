module mod_nearest_neighbour
    implicit none

    type index
        integer(kind=8), allocatable :: idx(:) !< indices from query to N-th nearest points
    end type index

    type distance
        real(kind=8), allocatable :: dst(:) !< distances from query to N-th nearest points
    end type distance

    type neighbor_results
        type(index),    ALLOCATABLE :: indices(:)
        type(distance), ALLOCATABLE :: distances(:)
    end type neighbor_results

end module mod_nearest_neighbour
