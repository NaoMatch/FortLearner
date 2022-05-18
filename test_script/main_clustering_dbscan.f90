program main_clustering_dbscan
    use mod_timer
    use mod_common
    use mod_stats
    use mod_dbscan
    use mod_brute_force_search
    implicit none

    CHARACTER(len=256) :: file_name_x_bin
    CHARACTER(len=256) :: file_name_y_bin
    integer(kind=8)    :: len_y

    real(kind=8), ALLOCATABLE :: x(:,:)
    integer(kind=8), ALLOCATABLE :: y(:,:)

    integer(kind=8), allocatable :: uniq_y(:), count_y(:)

    integer(kind=8) :: i

    type(dbscan) :: db
    type(brute_force_search) :: brute
    type(neighbor_results) :: res

    file_name_x_bin = "../sample_data/make_brobs_X_750x2.bin"
    file_name_y_bin = "../sample_data/make_brobs_DBSCAN_750x2.bin"
    call read_bin_2d(file_name_x_bin, x)
    call read_bin_2d(file_name_y_bin, y)

    db = dbscan(n_neighbors=8_8, radius=0.25d0, neighbour_algo="brute_force_search")
    call db%fit(x)

    print*, " ----- Pred -----"
    len_y = size(y)
    call groupby_count_i8(uniq_y, count_y, db%labels_, len_y)
    print*, uniq_y
    print*, count_y

    print*, " ----- True -----"
    len_y = size(y)
    call groupby_count_i8(uniq_y, count_y, y(:,1), len_y)
    print*, uniq_y
    print*, count_y
    
end program main_clustering_dbscan