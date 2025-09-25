module mod_openblas_threads
    use iso_c_binding
    implicit none
    private
    public :: openblas_get_num_threads, openblas_set_num_threads, openblas_set_num_threads_local

    interface
        ! 現在のスレッド数を取得
        integer(c_int) function openblas_get_num_threads() bind(C, name="openblas_get_num_threads")
            use iso_c_binding
        end function
        ! スレッド数を設定（古くからあるAPI）
        subroutine openblas_set_num_threads(n) bind(C, name="openblas_set_num_threads")
            use iso_c_binding
            integer(c_int), value :: n
        end subroutine
        ! 0.3.27+ : 以前の値を返す"local"版（あれば使う）
        integer(c_int) function openblas_set_num_threads_local(n) bind(C, name="openblas_set_num_threads_local")
            use iso_c_binding
            integer(c_int), value :: n
        end function
    end interface
end module
