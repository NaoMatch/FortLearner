module mod_dgemm
    use iso_c_binding
    implicit none

    Interface
        subroutine my_dgemm_000(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_000')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_000

        subroutine my_dgemm_001(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_001')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_001

        subroutine my_dgemm_002(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_002')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_002

        subroutine my_dgemm_003(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_003')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_003

        subroutine my_dgemm_004(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_004')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_004

        subroutine my_dgemm_005(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_005')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_005

        subroutine my_dgemm_006(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_006')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_006

        subroutine my_dgemm_007(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_007')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_007

        subroutine my_dgemm_008(a, b, c, m, n, k) & 
            Bind(C,Name='my_dgemm_008')
            Import
            type(c_ptr), value        :: a, b, c
            integer(c_int64_t), value :: m, n, k
        end subroutine my_dgemm_008
    end Interface

contains

    function min(x,y)
        integer(kind=8) :: min
        integer(kind=8), intent(in) :: x,y
        min = minval((/x,y/))
    end function min

    function max(x,y)
        integer(kind=8) :: max
        integer(kind=8), intent(in) :: x,y
        max = maxval((/x,y/))
    end function max

    subroutine my_dgemm_naive00_00(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        integer(kind=8) :: i, j, k

        c(:,:) = 0d0
        do j=1, l, 1
            do i=1, m, 1
                do k=1, n, 1
                    c(i,j) = c(i,j) + a(i,k) * b(k,j)
                end do
            end do
        end do
    end subroutine my_dgemm_naive00_00

    subroutine my_dgemm_naive00_01(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        integer(kind=8) :: i, j, k
        real(kind=8)    :: tmp

        do j=1, l, 1
            do i=1, m, 1
                tmp = 0d0
                do k=1, n, 1
                    tmp = tmp + a(i,k) * b(k,j)
                end do
                c(i,j) = tmp
            end do
        end do
    end subroutine my_dgemm_naive00_01

    subroutine my_dgemm_naive00_02(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        real(kind=8)    :: tmp

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step, cny

        cny = 0
        j3_start=1;  do j3=j3_start, l, min(panel_l, l-j3_start+1)                          ! print*, "j3:             ", j3, l, min(panel_l, l-j3_start+1)
        j2_start=j3; do j2=j2_start, min(l, j3_start+panel_l-1), min(block_l, l-j2_start+1) ! print*, "    j2:         ", j2, min(l, j3_start+panel_l-1), min(block_l, l-j2_start+1)
        j1_start=j2; do j1=j1_start, min(l, j2_start+block_l-1), min(tile_l, l-j1_start+1)  ! print*, "        j1:     ", j1, min(l, j2_start+block_l-1), min(tile_l, l-j1_start+1)
        j0_start=j1; do j0=j0_start, min(l, j1_start+tile_l-1), 1                           ! print*, "            j0: ", j0_start, min(l, j1_start+tile_l-1), j0_max

            i3_start=1;  do i3=i3_start, m, min(panel_m, m-i3_start+1)                          ! print*, "i3:             ", i3, m, min(panel_m, m-i3_start+1)
            i2_start=i3; do i2=i2_start, min(m, i3_start+panel_m-1), min(block_m, m-i2_start+1) ! print*, "    i2:         ", i2, min(m, i3_start+panel_m-1), min(block_m, m-i2_start+1)
            i1_start=i2; do i1=i1_start, min(m, i2_start+block_m-1), min(tile_m, m-i1_start+1)  ! print*, "        i1:     ", i1, min(m, i2_start+block_m-1), min(tile_m, m-i1_start+1)
            i0_start=i1; do i0=i0_start, min(m, i1_start+tile_m-1), 1                           ! print*, "            i0: ", i0_start, min(m, i1_start+tile_l-1), i0_max

                tmp = 0d0
                k3_start=1;  do k3=k3_start, n, min(panel_n, n-k3_start+1)                          ! print*, "k3:             ", k3, n, min(panel_n, n-k3_start+1)
                k2_start=k3; do k2=k2_start, min(n, k3_start+panel_n-1), min(block_n, n-k2_start+1) ! print*, "    k2:         ", k2, min(n, k3_start+panel_n-1), min(block_n, n-k2_start+1)
                k1_start=k2; do k1=k1_start, min(n, k2_start+block_n-1), min(tile_n, n-k1_start+1)  ! print*, "        k1:     ", k1, min(n, k2_start+block_n-1), min(tile_n, n-k1_start+1)
                k0_start=k1; do k0=k0_start, min(n, k1_start+tile_n-1), 1                           ! print*, "            k0: ", k0_start, min(n, k1_start+tile_n-1), k0_max
                    tmp = tmp + a(i0,k0) * b(k0,j0)
                k0_start = k0_start + 1; end do
                k1_start = k1_start + min(tile_n, n-k1_start+1); end do
                k2_start = k2_start + min(block_n, n-k2_start+1); end do
                k3_start = k3_start + min(panel_n, n-k3_start+1); end do

                c(i0,j0) = tmp

            i0_start = i0_start + 1; end do
            i1_start = i1_start + min(tile_m, m-i1_start+1); end do
            i2_start = i2_start + min(block_m, m-i2_start+1); end do
            i3_start = i3_start + min(panel_m, m-i3_start+1); end do

        j0_start = j0_start + 1; end do
        j1_start = j1_start + min(tile_l, l-j1_start+1); end do
        j2_start = j2_start + min(block_l, l-j2_start+1); end do
        j3_start = j3_start + min(panel_l, l-j3_start+1); end do

    end subroutine my_dgemm_naive00_02

    subroutine my_dgemm_naive00_03(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        ! integer(kind=8) :: panel_m=8, panel_n=8, panel_l=8
        ! integer(kind=8) :: block_m=4, block_n=4, block_l=4
        ! integer(kind=8) :: tile_m=2,   tile_n=2,   tile_l=2
        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        real(kind=8)    :: tmp

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step, cny

        c(:,:) = 0d0
        cny = 0
        j3_start=1;  do j3=j3_start, l, min(panel_l, l-j3_start+1)                          ! j3_max = l; j3_step = min(panel_l, l-j3_start+1); print*, "j3:             ", j3, j3_max, j3_step
        j2_start=j3; do j2=j2_start, min(l, j3_start+panel_l-1), min(block_l, l-j2_start+1) ! j2_max = min(l, j3_start+panel_l-1); j2_step = min(block_l, l-j2_start+1); print*, "    j2:         ", j2, j2_max, j2_step
        j1_start=j2; do j1=j1_start, min(l, j2_start+block_l-1), min(tile_l, l-j1_start+1)  ! j1_max = min(l, j2_start+block_l-1); j1_step = min(tile_l, l-j1_start+1); print*, "        j1:     ", j1, j1_max, j1_step

            i3_start=1;  do i3=i3_start, m, min(panel_m, m-i3_start+1)                          ! i3_max = m; i3_step = min(panel_m, m-i3_start+1); print*, "i3:             ", i3, i3_max, i3_step
            i2_start=i3; do i2=i2_start, min(m, i3_start+panel_m-1), min(block_m, m-i2_start+1) ! i2_max = min(m, i3_start+panel_m-1); i2_step = min(block_m, m-i2_start+1); print*, "    i2:         ", i2, i2_max, i2_step
            i1_start=i2; do i1=i1_start, min(m, i2_start+block_m-1), min(tile_m, m-i1_start+1)  ! i1_max = min(m, i2_start+block_m-1); i1_step = min(tile_m, m-i1_start+1); print*, "        i1:     ", i1, i1_max, i1_step

                k3_start=1;  do k3=k3_start, n, min(panel_n, n-k3_start+1)                          ! k3_max = n; k3_step = min(panel_n, n-k3_start+1); print*, "k3:             ", k3, k3_max, k3_step
                k2_start=k3; do k2=k2_start, min(n, k3_start+panel_n-1), min(block_n, n-k2_start+1) ! k2_max = min(n, k3_start+panel_n-1); k2_step = min(block_n, n-k2_start+1); print*, "    k2:         ", k2, k2_max, k2_step
                k1_start=k2; do k1=k1_start, min(n, k2_start+block_n-1), min(tile_n, n-k1_start+1)  ! k1_max = min(n, k2_start+block_n-1); k1_step = min(tile_n, n-k1_start+1); print*, "        k1:     ", k1, k1_max, k1_step

                    j0_start=j1; j0_max = j0_start+min(tile_l-1, l-j0_start); do j0=j0_start, j0_max, 1                           ! print*, "            j0: ", j0, j0_max, 1_8
                    i0_start=i1; i0_max = i0_start+min(tile_m-1, m-i0_start); do i0=i0_start, i0_max, 1                           ! print*, "            i0: ", i0, i0_max, 1_8
                    tmp = 0d0
                    k0_start=k1; k0_max = k0_start+min(tile_n-1, n-k0_start); do k0=k0_start, k0_max, 1                           ! print*, "            k0: ", k0, k0_max, 1_8
                        tmp = tmp + a(i0,k0) * b(k0,j0)
                    end do
                    c(i0,j0) = c(i0,j0) + tmp
                    end do
                    end do

                k1_start = k1_start + min(tile_n,  n-k1_start+1); end do
                k2_start = k2_start + min(block_n, n-k2_start+1); end do
                k3_start = k3_start + min(panel_n, n-k3_start+1); end do

            i1_start = i1_start + min(tile_m,  m-i1_start+1); end do
            i2_start = i2_start + min(block_m, m-i2_start+1); end do
            i3_start = i3_start + min(panel_m, m-i3_start+1); end do

        j1_start = j1_start + min(tile_l,  l-j1_start+1); end do
        j2_start = j2_start + min(block_l, l-j2_start+1); end do
        j3_start = j3_start + min(panel_l, l-j3_start+1); end do
    end subroutine my_dgemm_naive00_03

    subroutine my_dgemm_naive00_04(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        ! integer(kind=8) :: panel_m=8, panel_n=8, panel_l=8
        ! integer(kind=8) :: block_m=4, block_n=4, block_l=4
        ! integer(kind=8) :: tile_m=2,   tile_n=2,   tile_l=2
        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        real(kind=8)    :: tmp

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step

        c(:,:) = 0d0
        j3_start=1;  do j3=j3_start, l, min(panel_l, l-j3_start+1)                          ! j3_max = l; j3_step = min(panel_l, l-j3_start+1); print*, "j3:             ", j3, j3_max, j3_step
        i3_start=1;  do i3=i3_start, m, min(panel_m, m-i3_start+1)                          ! i3_max = m; i3_step = min(panel_m, m-i3_start+1); print*, "i3:             ", i3, i3_max, i3_step
        k3_start=1;  do k3=k3_start, n, min(panel_n, n-k3_start+1)                          ! k3_max = n; k3_step = min(panel_n, n-k3_start+1); print*, "k3:             ", k3, k3_max, k3_step

            j2_start=j3; do j2=j2_start, min(l, j3_start+panel_l-1), min(block_l, l-j2_start+1) ! j2_max = min(l, j3_start+panel_l-1); j2_step = min(block_l, l-j2_start+1); print*, "    j2:         ", j2, j2_max, j2_step
            i2_start=i3; do i2=i2_start, min(m, i3_start+panel_m-1), min(block_m, m-i2_start+1) ! i2_max = min(m, i3_start+panel_m-1); i2_step = min(block_m, m-i2_start+1); print*, "    i2:         ", i2, i2_max, i2_step
            k2_start=k3; do k2=k2_start, min(n, k3_start+panel_n-1), min(block_n, n-k2_start+1) ! k2_max = min(n, k3_start+panel_n-1); k2_step = min(block_n, n-k2_start+1); print*, "    k2:         ", k2, k2_max, k2_step

                j1_start=j2; do j1=j1_start, min(l, j2_start+block_l-1), min(tile_l, l-j1_start+1)  ! j1_max = min(l, j2_start+block_l-1); j1_step = min(tile_l, l-j1_start+1); print*, "        j1:     ", j1, j1_max, j1_step
                i1_start=i2; do i1=i1_start, min(m, i2_start+block_m-1), min(tile_m, m-i1_start+1)  ! i1_max = min(m, i2_start+block_m-1); i1_step = min(tile_m, m-i1_start+1); print*, "        i1:     ", i1, i1_max, i1_step
                k1_start=k2; do k1=k1_start, min(n, k2_start+block_n-1), min(tile_n, n-k1_start+1)  ! k1_max = min(n, k2_start+block_n-1); k1_step = min(tile_n, n-k1_start+1); print*, "        k1:     ", k1, k1_max, k1_step

                    j0_start=j1; j0_max = j0_start+min(tile_l-1, l-j0_start); do j0=j0_start, j0_max, 1 ! print*, "            j0: ", j0, j0_max, 1_8
                    i0_start=i1; i0_max = i0_start+min(tile_m-1, m-i0_start); do i0=i0_start, i0_max, 1 ! print*, "            i0: ", i0, i0_max, 1_8
                    tmp = 0d0
                    k0_start=k1; k0_max = k0_start+min(tile_n-1, n-k0_start); do k0=k0_start, k0_max, 1 ! print*, "            k0: ", k0, k0_max, 1_8
                        tmp = tmp + a(i0,k0) * b(k0,j0)
                    end do
                    c(i0,j0) = c(i0,j0) + tmp
                    end do
                    end do

                k1_start = k1_start + min(tile_n,  n-k1_start+1); end do
                i1_start = i1_start + min(tile_m,  m-i1_start+1); end do
                j1_start = j1_start + min(tile_l,  l-j1_start+1); end do

            k2_start = k2_start + min(block_n, n-k2_start+1); end do
            i2_start = i2_start + min(block_m, m-i2_start+1); end do
            j2_start = j2_start + min(block_l, l-j2_start+1); end do

        k3_start = k3_start + min(panel_n, n-k3_start+1); end do
        i3_start = i3_start + min(panel_m, m-i3_start+1); end do
        j3_start = j3_start + min(panel_l, l-j3_start+1); end do
    end subroutine my_dgemm_naive00_04

    subroutine my_dgemm_naive00_05(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l


        real(kind=8), allocatable :: a_cache(:,:), b_cache(:,:)
        real(kind=8), allocatable :: a_v_cache(:), b_v_cache(:)
        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        ! integer(kind=8) :: panel_m=8, panel_n=8, panel_l=8
        ! integer(kind=8) :: block_m=4, block_n=4, block_l=4
        ! integer(kind=8) :: tile_m=2,   tile_n=2,   tile_l=2
        real(kind=8)    :: tmp, tmp2

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step, jj1, jj0
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step, j0_base

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step, ii1, ii0
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step, i0_base

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step, kk1, kk0
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step, k0_base

        integer(kind=8) :: cnt

        ! allocate(a_cache(block_m, block_n))
        allocate(a_v_cache(block_m * block_n))
        ! allocate(b_cache(block_n, block_l))

        c(:,:) = 0d0
        ! L3-Loop
        do j3=1, l, min(panel_l, l-1+1) !! j3_max = l; j3_step = min(panel_l, l-j3_start+1); print*, "j3:             ", j3, j3_max, j3_step
        do i3=1, m, min(panel_m, m-1+1) !! i3_max = m; i3_step = min(panel_m, m-i3_start+1); print*, "i3:             ", i3, i3_max, i3_step
        do k3=1, n, min(panel_n, n-1+1) !! k3_max = n; k3_step = min(panel_n, n-k3_start+1); print*, "k3:             ", k3, k3_max, k3_step

            j3_max = min(panel_l, l-j3)-1
            i3_max = min(panel_m, m-i3)-1
            k3_max = min(panel_n, n-k3)-1

            ! L2-Loop
            do j2=j3, j3+j3_max, min(block_l, l-j3+1) !! j2_max = min(l, j3_start+panel_l-1); j2_step = min(block_l, l-j2_start+1); print*, "    j2:         ", j2, j2_max, j2_step
            do i2=i3, i3+i3_max, min(block_m, m-i3+1) !! i2_max = min(m, i3_start+panel_m-1); i2_step = min(block_m, m-i2_start+1); print*, "    i2:         ", i2, i2_max, i2_step
            do k2=k3, k3+k3_max, min(block_n, n-k3+1) !! k2_max = min(n, k3_start+panel_n-1); k2_step = min(block_n, n-k2_start+1); print*, "    k2:         ", k2, k2_max, k2_step

                j2_max = min(block_l, l-j2)-1
                i2_max = min(block_m, m-i2)-1
                k2_max = min(block_n, n-k2)-1

                ! L1-Loop
                cnt = 1
                do j1=j2, j2+j2_max, min(tile_l, l-j2+1)  !! j1_max = min(l, j2_start+block_l-1); j1_step = min(tile_l, l-j1_start+1); print*, "        j1:     ", j1, j1_max, j1_step
                do i1=i2, i2+i2_max, min(tile_m, m-i2+1)  !! i1_max = min(m, i2_start+block_m-1); i1_step = min(tile_m, m-i1_start+1); print*, "        i1:     ", i1, i1_max, i1_step
                do k1=k2, k2+k2_max, min(tile_n, n-k2+1)  !! k1_max = min(n, k2_start+block_n-1); k1_step = min(tile_n, n-k1_start+1); print*, "        k1:     ", k1, k1_max, k1_step

                    j1_max = min(tile_l-1, l-j1)
                    i1_max = min(tile_m-1, m-i1)
                    k1_max = min(tile_n-1, n-k1)

                    do k0=k1, k1+k1_max, 1 !! print*, "            k0: ", k0, k0_max, 1_8
                    do j0=j1, j1+j1_max, 1 !! print*, "            j0: ", j0, j0_max, 1_8
                    tmp = b(k0,j0)
                    do i0=i1, i1+i1_max, 1 !! print*, "            i0: ", i0, i0_max, 1_8
                        c(i0,j0) = c(i0,j0) + a(i0,k0) * tmp
                    end do
                    end do
                    end do

                end do
                end do
                end do

            end do
            end do
            end do

        end do
        end do
        end do


        ! stop
    end subroutine my_dgemm_naive00_05

    subroutine my_dgemm_naive00_06(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l


        real(kind=8), allocatable :: a_cache(:,:), b_cache(:,:)
        real(kind=8), allocatable :: a_v_cache(:), b_v_cache(:)
        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        ! integer(kind=8) :: panel_m=8, panel_n=8, panel_l=8
        ! integer(kind=8) :: block_m=4, block_n=4, block_l=4
        ! integer(kind=8) :: tile_m=2,   tile_n=2,   tile_l=2
        real(kind=8)    :: tmp, tmp2

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step, jj1, jj0
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step, j0_base

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step, ii1, ii0
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step, i0_base

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step, kk1, kk0
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step, k0_base

        integer(kind=8) :: l3, l3_start, l3_stop, l3_max, l3_step
        integer(kind=8) :: l2, l2_start, l2_stop, l2_max, l2_step
        integer(kind=8) :: l1, l1_start, l1_stop, l1_max, l1_step, ll1, ll0
        integer(kind=8) :: l0, l0_start, l0_stop, l0_max, l0_step, l0_base

        integer(kind=8) :: cnt

        ! allocate(a_cache(block_m, block_n))
        allocate(a_v_cache(block_m))
        ! allocate(b_cache(block_n, block_l))

        c(:,:) = 0d0
        ! L3-Loop
        do j3=1, l, min(panel_l, l-1+1) !! j3_max = l; j3_step = min(panel_l, l-j3_start+1); print*, "j3:             ", j3, j3_max, j3_step
        j3_max = min(panel_l, l-j3)-1
        do i3=1, m, min(panel_m, m-1+1) !! i3_max = m; i3_step = min(panel_m, m-i3_start+1); print*, "i3:             ", i3, i3_max, i3_step
        i3_max = min(panel_m, m-i3)-1
        do k3=1, n, min(panel_n, n-1+1) !! k3_max = n; k3_step = min(panel_n, n-k3_start+1); print*, "k3:             ", k3, k3_max, k3_step
        k3_max = min(panel_n, n-k3)-1


            ! L2-Loop
            do j2=j3, j3+j3_max, min(block_l, l-j3+1) !! j2_max = min(l, j3_start+panel_l-1); j2_step = min(block_l, l-j2_start+1); print*, "    j2:         ", j2, j2_max, j2_step
            j2_max = min(block_l, l-j2)-1
            do i2=i3, i3+i3_max, min(block_m, m-i3+1) !! i2_max = min(m, i3_start+panel_m-1); i2_step = min(block_m, m-i2_start+1); print*, "    i2:         ", i2, i2_max, i2_step
            i2_max = min(block_m, m-i2)-1
            do k2=k3, k3+k3_max, min(block_n, n-k3+1) !! k2_max = min(n, k3_start+panel_n-1); k2_step = min(block_n, n-k2_start+1); print*, "    k2:         ", k2, k2_max, k2_step
            k2_max = min(block_n, n-k2)-1

                
                ! L1-Loop
                cnt = 1
                do j1=j2, j2+j2_max, min(tile_l, l-j2+1)  !! j1_max = min(l, j2_start+block_l-1); j1_step = min(tile_l, l-j1_start+1); print*, "        j1:     ", j1, j1_max, j1_step
                j1_max = min(tile_l-1, l-j1)
                do i1=i2, i2+i2_max, min(tile_m, m-i2+1)  !! i1_max = min(m, i2_start+block_m-1); i1_step = min(tile_m, m-i1_start+1); print*, "        i1:     ", i1, i1_max, i1_step
                i1_max = min(tile_m-1, m-i1)
                do k1=k2, k2+k2_max, min(tile_n, n-k2+1)  !! k1_max = min(n, k2_start+block_n-1); k1_step = min(tile_n, n-k1_start+1); print*, "        k1:     ", k1, k1_max, k1_step
                k1_max = min(tile_n-1, n-k1)


                    do k0=k1, k1+k1_max, 1 !! print*, "            k0: ", k0, k0_max, 1_8
                    do j0=j1, j1+j1_max, 1 !! print*, "            j0: ", j0, j0_max, 1_8
                    tmp = b(k0,j0)
                    do i0=i1, i1+i1_max, 1 !! print*, "            i0: ", i0, i0_max, 1_8
                        c(i0,j0) = c(i0,j0) + a(i0,k0) * tmp
                    end do
                    end do
                    end do

                end do
                end do
                end do

            end do
            end do
            end do

        end do
        end do
        end do
    end subroutine my_dgemm_naive00_06

    subroutine my_dgemm_naive00_07(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l


        real(kind=8), allocatable :: a_cache(:,:), b_cache(:,:)
        real(kind=8), allocatable :: a_v_cache(:), b_v_cache(:)
        integer(kind=8) :: panel_m=256, panel_n=256, panel_l=256
        integer(kind=8) :: block_m=128, block_n=128, block_l=64
        integer(kind=8) :: tile_m=32,   tile_n=32,   tile_l=32
        ! integer(kind=8) :: panel_m=8, panel_n=8, panel_l=8
        ! integer(kind=8) :: block_m=4, block_n=4, block_l=4
        ! integer(kind=8) :: tile_m=2,   tile_n=2,   tile_l=2
        real(kind=8)    :: tmp, tmp2

        integer(kind=8) :: j3, j3_start, j3_stop, j3_max, j3_step, l_panel, l_block, l_tile
        integer(kind=8) :: j2, j2_start, j2_stop, j2_max, j2_step
        integer(kind=8) :: j1, j1_start, j1_stop, j1_max, j1_step, jj1, jj0, j00, j1_4
        integer(kind=8) :: j0, j0_start, j0_stop, j0_max, j0_step, j0_base

        integer(kind=8) :: i3, i3_start, i3_stop, i3_max, i3_step, m_panel, m_block, m_tile
        integer(kind=8) :: i2, i2_start, i2_stop, i2_max, i2_step
        integer(kind=8) :: i1, i1_start, i1_stop, i1_max, i1_step, ii1, ii0, i00, i1_4
        integer(kind=8) :: i0, i0_start, i0_stop, i0_max, i0_step, i0_base

        integer(kind=8) :: k3, k3_start, k3_stop, k3_max, k3_step, n_panel, n_block, n_tile
        integer(kind=8) :: k2, k2_start, k2_stop, k2_max, k2_step
        integer(kind=8) :: k1, k1_start, k1_stop, k1_max, k1_step, kk1, kk0, k00, k1_4
        integer(kind=8) :: k0, k0_start, k0_stop, k0_max, k0_step, k0_base

        integer(kind=8) :: l3, l3_start, l3_stop, l3_max, l3_step
        integer(kind=8) :: l2, l2_start, l2_stop, l2_max, l2_step
        integer(kind=8) :: l1, l1_start, l1_stop, l1_max, l1_step, ll1, ll0
        integer(kind=8) :: l0, l0_start, l0_stop, l0_max, l0_step, l0_base

        integer(kind=8) :: cnt

        ! allocate(a_cache(block_m, block_n))
        allocate(a_v_cache(block_m))
        ! allocate(b_cache(block_n, block_l))

        c(:,:) = 0d0
        ! L3-Loop
        do j3=1, l, min(panel_l, l-1+1) !! j3_max = l; j3_step = min(panel_l, l-j3_start+1); print*, "j3:             ", j3, j3_max, j3_step
        j3_max = min(panel_l, l-j3)-1
        do i3=1, m, min(panel_m, m-1+1) !! i3_max = m; i3_step = min(panel_m, m-i3_start+1); print*, "i3:             ", i3, i3_max, i3_step
        i3_max = min(panel_m, m-i3)-1
        do k3=1, n, min(panel_n, n-1+1) !! k3_max = n; k3_step = min(panel_n, n-k3_start+1); print*, "k3:             ", k3, k3_max, k3_step
        k3_max = min(panel_n, n-k3)-1


            ! L2-Loop
            do j2=j3, j3+j3_max, min(block_l, l-j3+1) !! j2_max = min(l, j3_start+panel_l-1); j2_step = min(block_l, l-j2_start+1); print*, "    j2:         ", j2, j2_max, j2_step
            j2_max = min(block_l, l-j2)-1
            do i2=i3, i3+i3_max, min(block_m, m-i3+1) !! i2_max = min(m, i3_start+panel_m-1); i2_step = min(block_m, m-i2_start+1); print*, "    i2:         ", i2, i2_max, i2_step
            i2_max = min(block_m, m-i2)-1
            do k2=k3, k3+k3_max, min(block_n, n-k3+1) !! k2_max = min(n, k3_start+panel_n-1); k2_step = min(block_n, n-k2_start+1); print*, "    k2:         ", k2, k2_max, k2_step
            k2_max = min(block_n, n-k2)-1

                
                ! L1-Loop
                cnt = 1
                do j1=j2, j2+j2_max, min(tile_l, l-j2+1)  !! j1_max = min(l, j2_start+block_l-1); j1_step = min(tile_l, l-j1_start+1); print*, "        j1:     ", j1, j1_max, j1_step
                j1_max = min(tile_l-1, l-j1)
                do i1=i2, i2+i2_max, min(tile_m, m-i2+1)  !! i1_max = min(m, i2_start+block_m-1); i1_step = min(tile_m, m-i1_start+1); print*, "        i1:     ", i1, i1_max, i1_step
                i1_max = min(tile_m-1, m-i1)
                do k1=k2, k2+k2_max, min(tile_n, n-k2+1)  !! k1_max = min(n, k2_start+block_n-1); k1_step = min(tile_n, n-k1_start+1); print*, "        k1:     ", k1, k1_max, k1_step
                k1_max = min(tile_n-1, n-k1)


                    do k0=k1, k1+k1_max, 8 !! print*, "            k0: ", k0, k0_max, 1_8
                    do k00=0, 8-1, 1
                    do j0=j1, j1+j1_max, 8 !! print*, "            j0: ", j0, j0_max, 1_8
                    do j00=0, 8-1, 1
                        tmp = b(k0+k00,j0+j00)
                        do i0=i1, i1+i1_max, 4 !! print*, "            i0: ", i0, i0_max, 1_4
                            do i00=0, 4-1, 1
                                c(i0+i00,j0+j00) = c(i0+i00,j0+j00) + a(i0+i00,k0+k00) * tmp
                            end do
                        end do
                    end do
                    end do
                    end do
                    end do

                end do
                end do
                end do

            end do
            end do
            end do

        end do
        end do
        end do
    end subroutine my_dgemm_naive00_07



    subroutine my_dgemm_naive01_00(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        integer(kind=8) :: i, j, k

        c(:,:) = 0d0
        do i=1, m, 1
            do j=1, l, 1
                do k=1, n, 1
                    c(i,j) = c(i,j) + a(i,k) * b(k,j)
                end do
            end do
        end do
    end subroutine my_dgemm_naive01_00

    subroutine my_dgemm_naive01_01(a, b, c, m, n, l)
        implicit none
        real(kind=8), intent(in)    :: a(m,n)
        real(kind=8), intent(in)    :: b(n,l)
        real(kind=8), intent(inout) :: c(m,l)
        integer(kind=8), intent(in) :: m, n, l

        integer(kind=8) :: i, j, k
        real(kind=8)    :: tmp

        do i=1, m, 1
            do j=1, l, 1
                tmp = 0d0
                do k=1, n, 1
                    tmp = tmp + a(i,k) * b(k,j)
                end do
                c(i,j) = tmp
            end do
        end do
    end subroutine my_dgemm_naive01_01

end module mod_dgemm
