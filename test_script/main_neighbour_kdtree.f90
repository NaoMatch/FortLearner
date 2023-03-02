program main_neighbour_kdtree
    use mod_common
    use mod_nearest_neighbour
    use mod_brute_force_search
    use mod_kdtree
    use mod_balltree
    implicit none
    integer(kind=8)        :: date_value1(8), date_value2(8)
    type(brute_force_search) :: bfsearch
    type(kdtree) :: kdtsearch
    type(balltree) :: btsearch
    type(neighbor_results) :: res, res_new

    real(kind=8), allocatable :: x(:,:)
    real(kind=8), allocatable :: q(:,:)
    real(kind=8), allocatable :: d(:,:)
    integer(kind=8), allocatable :: i(:,:), i_pack_sk(:), i_pack_fl(:)
    integer(kind=8) :: n, j, counter
    integer(kind=8) :: n_x, n_max
    real(kind=8)    :: radius

    character(len=256) :: fn_x
    character(len=256) :: fn_q
    character(len=256) :: fn_d
    character(len=256) :: fn_i

    real(kind=8), allocatable    :: x_train(:,:), x_test(:,:)
    integer(kind=8), allocatable :: y_train(:,:), y_test(:,:)

    integer(kind=8), allocatable :: pred_train_lsvc(:,:), pred_test_lsvc(:,:)
    integer(kind=8), allocatable :: pred_train_poly(:,:), pred_test_poly(:,:)
    integer(kind=8), allocatable :: pred_train_rbf(:,:), pred_test_rbf(:,:)
    integer(kind=8), allocatable :: pred_train_linear(:,:), pred_test_linear(:,:)
    integer(kind=8), allocatable :: pred_train_sigmoid(:,:), pred_test_sigmoid(:,:)

    CHARACTER(len=256), allocatable :: fns_x_train(:), fns_y_train(:)
    CHARACTER(len=256), allocatable :: fns_x_test(:), fns_y_test(:)

    CHARACTER(len=256), allocatable :: data_desc(:)

    character(len=1024) :: fmt, fmt1, fmt2
    character(len=1024) :: header, header1, header2, header3, header4, header5

    integer(kind=8) :: iter, n_iter, min_seconds, max_depth
    integer(kind=8) :: n_test

    fns_x_train = [ &
        "../sample_data/mnist_X_train.bin                           ", &
        "../sample_data/make_regression_X_train_0000000100x00005.bin",&
        "../sample_data/make_regression_X_train_0000000100x00010.bin",&
        "../sample_data/make_regression_X_train_0000000100x00050.bin",&
        "../sample_data/make_regression_X_train_0000000100x00100.bin",&
        "../sample_data/make_regression_X_train_0000000100x00200.bin",&
        "../sample_data/make_regression_X_train_0000000100x00400.bin",&
        "../sample_data/make_regression_X_train_0000000100x00800.bin",&
        "../sample_data/make_regression_X_train_0000001000x00005.bin",&
        "../sample_data/make_regression_X_train_0000001000x00010.bin",&
        "../sample_data/make_regression_X_train_0000001000x00050.bin",&
        "../sample_data/make_regression_X_train_0000001000x00100.bin",&
        "../sample_data/make_regression_X_train_0000001000x00200.bin",&
        "../sample_data/make_regression_X_train_0000001000x00400.bin",&
        "../sample_data/make_regression_X_train_0000001000x00800.bin",&
        "../sample_data/make_regression_X_train_0000010000x00005.bin",&
        "../sample_data/make_regression_X_train_0000010000x00010.bin",&
        "../sample_data/make_regression_X_train_0000010000x00050.bin",&
        "../sample_data/make_regression_X_train_0000010000x00100.bin",&
        "../sample_data/make_regression_X_train_0000010000x00200.bin",&
        "../sample_data/make_regression_X_train_0000010000x00400.bin",&
        "../sample_data/make_regression_X_train_0000010000x00800.bin",&
        "../sample_data/make_regression_X_train_0000100000x00005.bin",&
        "../sample_data/make_regression_X_train_0000100000x00010.bin",&
        "../sample_data/make_regression_X_train_0000100000x00050.bin",&
        "../sample_data/make_regression_X_train_0000100000x00100.bin",&
        "../sample_data/make_regression_X_train_0000100000x00200.bin",&
        "../sample_data/make_regression_X_train_0000100000x00400.bin",&
        "../sample_data/make_regression_X_train_0000100000x00800.bin"&
    ]

    fns_x_test = [ &
        "../sample_data/mnist_X_test.bin                           ", &
        "../sample_data/make_regression_X_test_0000000100x00005.bin",&
        "../sample_data/make_regression_X_test_0000000100x00010.bin",&
        "../sample_data/make_regression_X_test_0000000100x00050.bin",&
        "../sample_data/make_regression_X_test_0000000100x00100.bin",&
        "../sample_data/make_regression_X_test_0000000100x00200.bin",&
        "../sample_data/make_regression_X_test_0000000100x00400.bin",&
        "../sample_data/make_regression_X_test_0000000100x00800.bin",&
        "../sample_data/make_regression_X_test_0000001000x00005.bin",&
        "../sample_data/make_regression_X_test_0000001000x00010.bin",&
        "../sample_data/make_regression_X_test_0000001000x00050.bin",&
        "../sample_data/make_regression_X_test_0000001000x00100.bin",&
        "../sample_data/make_regression_X_test_0000001000x00200.bin",&
        "../sample_data/make_regression_X_test_0000001000x00400.bin",&
        "../sample_data/make_regression_X_test_0000001000x00800.bin",&
        "../sample_data/make_regression_X_test_0000010000x00005.bin",&
        "../sample_data/make_regression_X_test_0000010000x00010.bin",&
        "../sample_data/make_regression_X_test_0000010000x00050.bin",&
        "../sample_data/make_regression_X_test_0000010000x00100.bin",&
        "../sample_data/make_regression_X_test_0000010000x00200.bin",&
        "../sample_data/make_regression_X_test_0000010000x00400.bin",&
        "../sample_data/make_regression_X_test_0000010000x00800.bin",&
        "../sample_data/make_regression_X_test_0000100000x00005.bin",&
        "../sample_data/make_regression_X_test_0000100000x00010.bin",&
        "../sample_data/make_regression_X_test_0000100000x00050.bin",&
        "../sample_data/make_regression_X_test_0000100000x00100.bin",&
        "../sample_data/make_regression_X_test_0000100000x00200.bin",&
        "../sample_data/make_regression_X_test_0000100000x00400.bin",&
        "../sample_data/make_regression_X_test_0000100000x00800.bin"&
    ]

    n_iter = 100000
    ! n_iter = 1
    min_seconds = 60
    min_seconds = 1
    

    do j=10, size(fns_x_train), 1
        print*, ""
        print*, trim(fns_x_train(j))
        call read_bin_2d(fns_x_train(j), x_train, print_log=f_)
        call read_bin_2d(fns_x_test(j), x_test, print_log=f_)

        n_test = size(x_test, dim=1) * 0.1d0

        ! btsearch = balltree(min_samples_in_leaf=128_8)
        ! call btsearch%build(x_train)
        ! call date_and_time(values=date_value1)
        ! do iter=1, n_iter, 1
        !     res = btsearch%query(q=x_test, n_neighbors=10_8)
        !     call date_and_time(values=date_value2)
        !     if (time_diff(date_value1, date_value2) >= min_seconds*1000_8) exit
        ! end do
        ! call date_and_time(values=date_value2)
        ! print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))

        kdtsearch = kdtree(min_samples_in_leaf=16_8)
        call kdtsearch%build(x_train)
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            res = kdtsearch%query(q=x_test(1:n_test,:), n_neighbors=10_8)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= min_seconds*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        print*, res%indices(1)%idx(:)
        print*, res%distances(1)%dst(:)

        kdtsearch = kdtree(min_samples_in_leaf=16_8)
        call kdtsearch%build_new(x_train)
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            res_new = kdtsearch%query_new(q=x_test(1:n_test,:), n_neighbors=10_8)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= min_seconds*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        print*, res_new%indices(1)%idx(:)
        print*, res_new%distances(1)%dst(:)

        kdtsearch = kdtree(min_samples_in_leaf=16_8)
        call kdtsearch%build_new(x_train)
        call date_and_time(values=date_value1)
        do iter=1, n_iter, 1
            res_new = kdtsearch%query_new_02(q=x_test(1:n_test,:), n_neighbors=10_8)
            call date_and_time(values=date_value2)
            if (time_diff(date_value1, date_value2) >= min_seconds*1000_8) exit
        end do
        call date_and_time(values=date_value2)
        print   '("   Duration [sec] :        ", f12.6)', time_diff(date_value1, date_value2) / 1000d0 / dble(maxval([iter-1, 1_8]))
        print*, res_new%indices(1)%idx(:)
        print*, res_new%distances(1)%dst(:)
    end do























    stop

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