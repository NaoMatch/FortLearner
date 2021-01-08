    subroutine sort_r8(this)
        implicit none
        class(work) :: this
        call quick_sort(this % tmp_r8(1:this % idx_i8-1), this % idx_i8-1)
    end subroutine sort_r8
