program main_dll
    use mod_timer
    use :: iso_c_binding
    implicit none
    
    interface
        function dlopen(filename,mode) bind(c,name="dlopen")
            ! void *dlopen(const char *filename, int mode);
            use iso_c_binding
            implicit none
            type(c_ptr) :: dlopen
            character(c_char), intent(in) :: filename(*)
            integer(c_int), value :: mode
        end function

        function dlsym(handle,name) bind(c,name="dlsym")
            ! void *dlsym(void *handle, const char *name);
            use iso_c_binding
            implicit none
            type(c_funptr) :: dlsym
            type(c_ptr), value :: handle
            character(c_char), intent(in) :: name(*)
        end function

        function dlclose(handle) bind(c,name="dlclose")
            ! int dlclose(void *handle);
            use iso_c_binding
            implicit none
            integer(c_int) :: dlclose
            type(c_ptr), value :: handle
        end function
    end interface

    abstract interface
        subroutine called_mydgemv(a_t, x, y, lda, ldx, ldy) bind(c)
            use, intrinsic :: iso_c_binding
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine called_mydgemv
    end interface

    integer(kind=8)        :: date_value1(8), date_value2(8)
    integer(c_int), parameter :: rtld_lazy=1 ! value extracte from the C header file
    integer(c_int), parameter :: rtld_now=2 ! value extracte from the C header file
    type(c_funptr) :: proc_addr
    type(c_ptr) :: handle
    procedure(called_mydgemv), bind(c), pointer :: mydgemv


    real(kind=8), allocatable :: a_t(:,:), x(:), y(:), y_(:)
    integer(kind=8) :: lda, ldx, ldy
    integer(kind=8) :: i

    call system("python create_dyn_mydgemv.py 123 128")
    call system("gcc-8 -mavx512f -mavx512dq -unroll -O3 -shared -o mydgemv_123.so mydgemv_123.c")

    handle=dlopen("./mydgemv_123.so"//c_null_char, RTLD_LAZY)
    if (.not. c_associated(handle))then
        print*, 'Unable to load DLL ./mydgemv_123.so'
        stop
    end if

    proc_addr=dlsym(handle, "mydgemv_123"//c_null_char)
    if (.not. c_associated(proc_addr))then
        write(*,*) 'Unable to load the procedure t_times2'
        stop
    end if
    call c_f_procpointer( proc_addr, mydgemv )


    lda = 100
    ldx = 128
    ldy = lda
    allocate(a_t(ldx, lda))
    allocate(x(ldx))
    allocate(y(ldy))
    allocate(y_(ldy))

    call random_number(a_t)
    call random_number(x)

    call date_and_time(values=date_value1)
    do i=1, 1000000, 1
        call mydgemv(a_t, x, y, lda, ldx, ldy)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sum(y)
    
    call openblas_set_num_threads(1)
    call date_and_time(values=date_value1)
    do i=1, 1000000, 1
        call dgemv("T", ldx, lda, 1d0, a_t, ldx, x, 1_8, 0d0, y_, 1_8)
    end do
    call date_and_time(values=date_value2)
    print*, time_diff(date_value1, date_value2), sum(y_)

    print*, sum(y - y_)




end program main_dll