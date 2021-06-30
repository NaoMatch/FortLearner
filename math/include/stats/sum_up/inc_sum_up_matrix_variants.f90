subroutine sum_up_matrix_naive_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i, j
    real(kind=8)    :: tmp
    include "./include/stats/sum_up/inc_sum_up_matrix_f.f90"
end subroutine sum_up_matrix_naive_r8

subroutine sum_up_matrix_naive_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i, j
    integer(kind=8) :: tmp
    include "./include/stats/sum_up/inc_sum_up_matrix_f.f90"
end subroutine sum_up_matrix_naive_i8


! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_f.f90"
end subroutine sum_up_matrix_02_r8

subroutine sum_up_matrix_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_f.f90"
end subroutine sum_up_matrix_02_i8

subroutine sum_up_matrix_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_f.f90"
end subroutine sum_up_matrix_04_r8

subroutine sum_up_matrix_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_f.f90"
end subroutine sum_up_matrix_04_i8

subroutine sum_up_matrix_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_f.f90"
end subroutine sum_up_matrix_08_r8

subroutine sum_up_matrix_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_f.f90"
end subroutine sum_up_matrix_08_i8

subroutine sum_up_matrix_16_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: tmp

    num_unroll = n - mod(n, 16)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_f.f90"
end subroutine sum_up_matrix_16_r8

subroutine sum_up_matrix_16_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: tmp

    num_unroll = n - mod(n, 16)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_f.f90"
end subroutine sum_up_matrix_16_i8

subroutine sum_up_matrix_32_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: tmp

    num_unroll = n - mod(n, 32)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_32_f.f90"
end subroutine sum_up_matrix_32_r8

subroutine sum_up_matrix_32_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: tmp

    num_unroll = n - mod(n, 32)
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_32_f.f90"
end subroutine sum_up_matrix_32_i8


! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_02_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_02_f.f90"
end subroutine sum_up_matrix_02_02_r8

subroutine sum_up_matrix_02_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_02_f.f90"
end subroutine sum_up_matrix_02_02_i8

subroutine sum_up_matrix_02_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_04_f.f90"
end subroutine sum_up_matrix_02_04_r8

subroutine sum_up_matrix_02_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_04_f.f90"
end subroutine sum_up_matrix_02_04_i8

subroutine sum_up_matrix_02_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_08_f.f90"
end subroutine sum_up_matrix_02_08_r8

subroutine sum_up_matrix_02_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_08_f.f90"
end subroutine sum_up_matrix_02_08_i8

subroutine sum_up_matrix_02_16_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31

    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_16_f.f90"
end subroutine sum_up_matrix_02_16_r8

subroutine sum_up_matrix_02_16_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31    
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_02_16_f.f90"
end subroutine sum_up_matrix_02_16_i8

subroutine sum_up_matrix_04_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_02_f.f90"
end subroutine sum_up_matrix_04_02_r8

subroutine sum_up_matrix_04_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_02_f.f90"
end subroutine sum_up_matrix_04_02_i8

subroutine sum_up_matrix_04_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_04_f.f90"
end subroutine sum_up_matrix_04_04_r8

subroutine sum_up_matrix_04_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_04_f.f90"
end subroutine sum_up_matrix_04_04_i8

subroutine sum_up_matrix_04_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_08_f.f90"
end subroutine sum_up_matrix_04_08_r8

subroutine sum_up_matrix_04_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_04_08_f.f90"
end subroutine sum_up_matrix_04_08_i8

subroutine sum_up_matrix_08_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_02_f.f90"
end subroutine sum_up_matrix_08_02_r8

subroutine sum_up_matrix_08_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_02_f.f90"
end subroutine sum_up_matrix_08_02_i8

subroutine sum_up_matrix_08_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_04_f.f90"
end subroutine sum_up_matrix_08_04_r8

subroutine sum_up_matrix_08_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_08_04_f.f90"
end subroutine sum_up_matrix_08_04_i8

subroutine sum_up_matrix_16_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_02_f.f90"
end subroutine sum_up_matrix_16_02_r8

subroutine sum_up_matrix_16_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_02_f.f90"
end subroutine sum_up_matrix_16_02_i8

subroutine sum_up_matrix_16_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_04_f.f90"
end subroutine sum_up_matrix_16_04_r8

subroutine sum_up_matrix_16_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_16_04_f.f90"
end subroutine sum_up_matrix_16_04_i8

subroutine sum_up_matrix_32_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_32_02_f.f90"
end subroutine sum_up_matrix_32_02_r8

subroutine sum_up_matrix_32_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_unroll_32_02_f.f90"
end subroutine sum_up_matrix_32_02_i8


! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_02_f.f90"
end subroutine sum_up_matrix_parallel_02_r8

subroutine sum_up_matrix_parallel_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 2)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_02_f.f90"
end subroutine sum_up_matrix_parallel_02_i8

subroutine sum_up_matrix_parallel_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_04_f.f90"
end subroutine sum_up_matrix_parallel_04_r8

subroutine sum_up_matrix_parallel_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 4)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_04_f.f90"
end subroutine sum_up_matrix_parallel_04_i8

subroutine sum_up_matrix_parallel_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_08_f.f90"
end subroutine sum_up_matrix_parallel_08_r8

subroutine sum_up_matrix_parallel_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15

    num_unroll = n - mod(n, 8)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_08_f.f90"
end subroutine sum_up_matrix_parallel_08_i8

subroutine sum_up_matrix_parallel_16_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: tmp

    num_unroll = n - mod(n, 16)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_16_f.f90"
end subroutine sum_up_matrix_parallel_16_r8

subroutine sum_up_matrix_parallel_16_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: tmp

    num_unroll = n - mod(n, 16)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_16_f.f90"
end subroutine sum_up_matrix_parallel_16_i8

subroutine sum_up_matrix_parallel_32_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: num_unroll, i, j
    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: tmp

    num_unroll = n - mod(n, 32)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_32_f.f90"
end subroutine sum_up_matrix_parallel_32_r8

subroutine sum_up_matrix_parallel_32_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: num_unroll, i, j
    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: tmp

    num_unroll = n - mod(n, 32)
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_unroll_32_f.f90"
end subroutine sum_up_matrix_parallel_32_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_02_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_02_r8

subroutine sum_up_matrix_parallel_ver01_02_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_02_i8

subroutine sum_up_matrix_parallel_ver02_02_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_02_r8

subroutine sum_up_matrix_parallel_ver02_02_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_02_i8

subroutine sum_up_matrix_parallel_ver01_02_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_04_r8

subroutine sum_up_matrix_parallel_ver01_02_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_04_i8

subroutine sum_up_matrix_parallel_ver02_02_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_04_r8

subroutine sum_up_matrix_parallel_ver02_02_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_04_i8


subroutine sum_up_matrix_parallel_ver01_02_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_08_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_08_r8

subroutine sum_up_matrix_parallel_ver01_02_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_02_08_f.f90"
end subroutine sum_up_matrix_parallel_ver01_02_08_i8

subroutine sum_up_matrix_parallel_ver02_02_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_08_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_08_r8

subroutine sum_up_matrix_parallel_ver02_02_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_02_08_f.f90"
end subroutine sum_up_matrix_parallel_ver02_02_08_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_04_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_02_r8

subroutine sum_up_matrix_parallel_ver01_04_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_02_i8

! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver02_04_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_02_r8

subroutine sum_up_matrix_parallel_ver02_04_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_02_i8

! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_04_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_04_r8

subroutine sum_up_matrix_parallel_ver01_04_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_04_i8

! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver02_04_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_04_r8

subroutine sum_up_matrix_parallel_ver02_04_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_04_i8

! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_04_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_08_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_08_r8

subroutine sum_up_matrix_parallel_ver01_04_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_04_08_f.f90"
end subroutine sum_up_matrix_parallel_ver01_04_08_i8

! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver02_04_08_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_08_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_08_r8

subroutine sum_up_matrix_parallel_ver02_04_08_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_04_08_f.f90"
end subroutine sum_up_matrix_parallel_ver02_04_08_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_08_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_08_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_08_02_r8

subroutine sum_up_matrix_parallel_ver01_08_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_08_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_08_02_i8

subroutine sum_up_matrix_parallel_ver02_08_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_08_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_08_02_r8

subroutine sum_up_matrix_parallel_ver02_08_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_08_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_08_02_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_08_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_08_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_08_04_r8

subroutine sum_up_matrix_parallel_ver01_08_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_08_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_08_04_i8

subroutine sum_up_matrix_parallel_ver02_08_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_08_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_08_04_r8

subroutine sum_up_matrix_parallel_ver02_08_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_08_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_08_04_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_16_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_16_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_16_02_r8

subroutine sum_up_matrix_parallel_ver01_16_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_16_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_16_02_i8

subroutine sum_up_matrix_parallel_ver02_16_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_16_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_16_02_r8

subroutine sum_up_matrix_parallel_ver02_16_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_16_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_16_02_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_16_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_16_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_16_04_r8

subroutine sum_up_matrix_parallel_ver01_16_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_16_04_f.f90"
end subroutine sum_up_matrix_parallel_ver01_16_04_i8

subroutine sum_up_matrix_parallel_ver02_16_04_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_16_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_16_04_r8

subroutine sum_up_matrix_parallel_ver02_16_04_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2, tmp3, tmp4
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_16_04_f.f90"
end subroutine sum_up_matrix_parallel_ver02_16_04_i8

! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
! --------------------------------------------------------------------------
subroutine sum_up_matrix_parallel_ver01_32_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_32_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_32_02_r8

subroutine sum_up_matrix_parallel_ver01_32_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver01_unroll_32_02_f.f90"
end subroutine sum_up_matrix_parallel_ver01_32_02_i8

subroutine sum_up_matrix_parallel_ver02_32_02_r8(r,x,n,c)
    implicit none
    real(kind=8), intent(inout) :: r(c)
    real(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in) :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    real(kind=8) :: r00, r01, r02, r03
    real(kind=8) :: r04, r05, r06, r07
    real(kind=8) :: r08, r09, r10, r11
    real(kind=8) :: r12, r13, r14, r15
    real(kind=8) :: r16, r17, r18, r19
    real(kind=8) :: r20, r21, r22, r23
    real(kind=8) :: r24, r25, r26, r27
    real(kind=8) :: r28, r29, r30, r31
    real(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_32_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_32_02_r8

subroutine sum_up_matrix_parallel_ver02_32_02_i8(r,x,n,c)
    implicit none
    integer(kind=8), intent(inout) :: r(c)
    integer(kind=8), intent(in)    :: x(n,c)
    integer(kind=8), intent(in)    :: n,c

    integer(kind=8) :: i_unroll, j_unroll, i, j

    integer(kind=8) :: r00, r01, r02, r03
    integer(kind=8) :: r04, r05, r06, r07
    integer(kind=8) :: r08, r09, r10, r11
    integer(kind=8) :: r12, r13, r14, r15
    integer(kind=8) :: r16, r17, r18, r19
    integer(kind=8) :: r20, r21, r22, r23
    integer(kind=8) :: r24, r25, r26, r27
    integer(kind=8) :: r28, r29, r30, r31
    integer(kind=8) :: tmp1, tmp2
    include "./include/stats/sum_up/inc_sum_up_matrix_parallel_ver02_unroll_32_02_f.f90"
end subroutine sum_up_matrix_parallel_ver02_32_02_i8

