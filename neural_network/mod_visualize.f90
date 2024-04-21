module mod_visualize
    use mod_variable
    implicit none
    
contains
    function num2char_i8(num)
        character(:), allocatable   :: num2char_i8
        integer(kind=8), intent(in) :: num
        integer(kind=8) :: num_digit

        num_digit = 4
        allocate(character(num_digit)::num2char_i8)
        write (num2char_i8, '(i0)') num
    end function num2char_i8

    subroutine plot_dot_graph(var, file, verbose)
        implicit none
        type(variable)   :: var
        character(len=*) :: file
        logical(kind=4), optional :: verbose
        character(len=:), allocatable :: txt
        integer(kind=8) :: newunit
        character(len=:), allocatable :: cmd
        logical(kind=4) :: verbose_opt

        verbose_opt = .true.
        if (present(verbose)) verbose_opt = verbose

        txt = get_dot_graph(var, verbose_opt)

        txt = "digraph g{" // new_line("a") // txt // new_line("a") // "}"

        open(newunit=newunit, file="tmp_graph.dot")
        write(newunit, *) txt
        close(newunit)

        cmd = "dot tmp_graph.dot -o " // file // " -T png"
        call system(cmd)
    end subroutine plot_dot_graph

    function get_dot_graph(var, verbose) result(txt)
        implicit none
        type(variable) :: var
        character(len=:), allocatable :: txt
        logical(kind=4) :: verbose

        integer(kind=8) :: vid, fid
        integer(kind=8), allocatable :: func_ids(:)
        integer(kind=8), allocatable :: func_gs(:)
        integer(kind=8), allocatable :: func_seen(:)

        integer(kind=8) :: id_in_1, id_in_2
        integer(kind=8) :: id_out_1, id_out_2

        vid = var%id
        allocate(func_ids(0))
        allocate(func_gs(0))
        allocate(func_seen(0))

        func_ids = [func_ids, vstack(vid)%fid]
        func_gs = [func_gs, 1_8]
        txt = dot_from_var(vid, verbose)

        do while (size(func_ids)>0)
            fid = func_ids(1)
            ! print*, fid
            select type (creator => fstack(fid)%func)
                class is (base_function)
                    id_in_1  = creator%id_in_1
                    id_in_2  = creator%id_in_2

                    txt = txt // dot_from_func(fid) // new_line("a")

                    if (id_in_1>0) then
                        txt = txt // dot_from_var(id_in_1, verbose) // new_line("a")
                    end if
                    if (id_in_2>0) then
                        txt = txt // dot_from_var(id_in_2, verbose) // new_line("a")
                    end if

                    if (id_in_1>0) then
                        if (vstack(id_in_1)%fid>0) then
                            if (.not. any(vstack(id_in_1)%fid == func_seen)) then
                                func_ids = [func_ids, vstack(id_in_1)%fid]
                                func_gs = [func_gs, 1_8]
                            end if
                        end if
                    end if

                    if (id_in_2>0) then
                        if (vstack(id_in_2)%fid>0) then
                            if (.not. any(vstack(id_in_2)%fid == func_seen)) then
                                func_ids = [func_ids, vstack(id_in_2)%fid]
                                func_gs = [func_gs, 1_8]
                            end if
                        end if
                    end if
                    func_seen = [func_seen, fid]
                end select 
            call pop_by_index(func_ids, func_gs, 1_8)
        end do
    end function get_dot_graph

    function dot_from_func(fid) result(txt)
        implicit none
        integer(kind=8) :: fid
        character(len=:), allocatable :: txt
        integer(kind=8) :: fid_
        character(len=:), allocatable :: fid_chr
        character(len=:), allocatable :: id_in_1_chr, id_in_2_chr
        character(len=:), allocatable :: id_out_1_chr, id_out_2_chr

        fid_ = fid + size(vstack)
        fid_chr = num2char_i8(fid_)
        select type (creator => fstack(fid)%func)
            class is (base_function)
                txt = fid_chr // ' [label="' // trim(creator%fname) // '", color=lightblue,stype=filled, shape=box]' &
                    // new_line('a')
                
                if (creator%id_in_1>0)  id_in_1_chr  = num2char_i8(creator%id_in_1)
                if (creator%id_in_2>0)  id_in_2_chr  = num2char_i8(creator%id_in_2)
                if (creator%id_out_1>0) id_out_1_chr = num2char_i8(creator%id_out_1)
                if (creator%id_out_2>0) id_out_2_chr = num2char_i8(creator%id_out_2)

                if (creator%id_in_1>0) then
                    txt = txt // id_in_1_chr // " -> " // fid_chr // new_line('a')
                end if
                if (creator%id_in_2>0) then
                    txt = txt // id_in_2_chr // " -> " // fid_chr // new_line('a')
                end if
                if (creator%id_out_1>0) then
                    txt = txt // fid_chr // " -> " // id_out_1_chr // new_line('a')
                end if
                if (creator%id_out_2>0) then
                    txt = txt // fid_chr // " -> " // id_out_2_chr // new_line('a')
                end if
        end select 
    end function dot_from_func

    function dot_from_var(vid, verbose) result(txt)
        implicit none
        integer(kind=8) :: vid
        logical(kind=4) :: verbose
        character(len=:), allocatable :: txt
        character(len=:), allocatable :: vid_chr
        character(len=:), allocatable :: name

        integer(kind=8) :: n_rows, n_cols
        character(len=:), allocatable :: n_rows_chr, n_cols_chr

        vid_chr = num2char_i8(vid)
        name = ""
        if (allocated(vstack(vid)%name)) then
            name = trim(vstack(vid)%name)
        end if

        if (verbose) then
            n_rows = size(vstack(vid)%v, dim=1)
            n_cols = size(vstack(vid)%v, dim=2)
            n_rows_chr = num2char_i8(n_rows)
            n_cols_chr = num2char_i8(n_cols)
            name = name // " [" // n_rows_chr // ", " // n_cols_chr // "]"
        end if

        txt = vid_chr // ' [label="' // name // '", color=orange, style=filled]' // new_line('a')
    end function dot_from_var

end module mod_visualize