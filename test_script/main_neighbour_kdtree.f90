program main_neighbour_kdtree
    use mod_common
    use mod_nearest_neighbour
    use mod_brute_force_search
    use mod_kdtree
    implicit none

    type(brute_force_search) :: bfsearch
    type(kdtree) :: kdtsearch
    type(neighbor_results) :: res

    real(kind=8), allocatable :: x(:,:)
    real(kind=8), allocatable :: q(:,:)
    real(kind=8), allocatable :: d(:,:)
    integer(kind=8), allocatable :: i(:,:), i_pack_sk(:), i_pack_fl(:)
    integer(kind=8) :: n, counter
    integer(kind=8) :: n_x, n_max
    real(kind=8)    :: radius

    character(len=256) :: fn_x
    character(len=256) :: fn_q
    character(len=256) :: fn_d
    character(len=256) :: fn_i

    fn_x = "../sample_data/nnsearch_X_0000100000x00100.bin"
    fn_q = "../sample_data/nnsearch_Q_0000000100x00100.bin"
    fn_d = "../sample_data/nnsearch_D_0000000100x100000.bin"
    fn_i = "../sample_data/nnsearch_I_0000000100x100000.bin"

    call read_bin_2d(fn_x, x)
    call read_bin_2d(fn_q, q)
    call read_bin_2d(fn_d, d)
    call read_bin_2d(fn_i, i)

    n_x = size(x, dim=1)
    n_x = 1000
    radius = 20d0

    kdtsearch = kdtree(min_samples_in_leaf=64_8)
    call kdtsearch%build(x)
    res = kdtsearch%query(q, n_neighbors=n_x)
    do n=1, size(q, dim=1), 1
        ! print*, '*********************************************************************************************'
        ! print*, i(n,:n_x)
        ! print*, res%indices(n)%idx(:n_x)
        ! print*, res%distances(n)%dst(:n_x)
        counter = count(i(n,:n_x) == res%indices(n)%idx(:n_x))
        if (counter /= n_x) then
            stop "n_neighbors: Result Mismatch!"
        end if
    end do
    print*, "n_neighbors: Success!"

    kdtsearch = kdtree(min_samples_in_leaf=64_8)
    call kdtsearch%build(x)
    res = kdtsearch%query(q, radius=radius)
    do n=1, size(q, dim=1), 1
        i_pack_sk = pack(i(n,:), mask=d(n,:)<=radius)
        i_pack_fl = res%indices(n)%idx(:)
        ! n_max = minval([size(i_pack_sk)+0_8, 10_8])
        ! print*, '*********************************************************************************************'
        ! print*, size(i_pack_sk), size(i_pack_fl)
        ! print*, int(i_pack_sk(:n_max))
        ! print*, int(res%indices(n)%idx(:n_max))
        ! print*, real(d(n, :n_max))
        ! print*, real(res%distances(n)%dst(:n_max))
        counter = count(i_pack_sk == i_pack_fl)
        if (counter /= size(i_pack_sk)) then
            stop "radius: Result Mismatch!"
        end if
    end do
    print*, "radius: Success!"
end program main_neighbour_kdtree