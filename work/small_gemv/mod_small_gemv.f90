module mod_small_gemv
    use iso_c_binding
    implicit none

    interface
        subroutine mydgemv(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv

        subroutine mydgemv_X(a_t, tmp, x, y, lda, ldx, ldy) bind(C, name='mydgemv_X')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), tmp(ldx), x(ldx), y(ldy)
        end subroutine mydgemv_X
    end interface

    Interface
        subroutine mydgemv_n_verX(a, tmp, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_verX')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), tmp(lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_verX

        subroutine mydgemv_t_verX(a_t, tmp, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_verX')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), tmp(ldx), x(ldx), y(ldy)
        end subroutine mydgemv_t_verX

        subroutine mydgemv_n_ver0_0(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_0

        subroutine mydgemv_t_ver0_0(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_0

        subroutine mydgemv_n_ver0_1_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_1_unroll1
    
        subroutine mydgemv_t_ver0_1_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_1_unroll1
        
    
        subroutine mydgemv_n_ver0_1_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_1_unroll2
    
        subroutine mydgemv_t_ver0_1_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_1_unroll2
        
        subroutine mydgemv_n_ver0_1_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_1_unroll4
    
        subroutine mydgemv_t_ver0_1_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_1_unroll4
        
    
        subroutine mydgemv_n_ver0_1_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_1_unroll8
    
        subroutine mydgemv_t_ver0_1_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_1_unroll8




        subroutine mydgemv_n_ver0_2_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_2_unroll1
    
        subroutine mydgemv_n_ver0_2_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_2_unroll2
    
        subroutine mydgemv_n_ver0_2_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_2_unroll4
    
        subroutine mydgemv_n_ver0_2_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver0_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver0_2_unroll8

        subroutine mydgemv_t_ver0_2_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_2_unroll1
    
        subroutine mydgemv_t_ver0_2_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_2_unroll2
    
        subroutine mydgemv_t_ver0_2_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_2_unroll4
    
        subroutine mydgemv_t_ver0_2_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver0_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver0_2_unroll8


        subroutine mydgemv_ver0_4_c_2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_2

        subroutine mydgemv_ver0_4_c_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_4

        subroutine mydgemv_ver0_4_c_6(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_6')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_6

        subroutine mydgemv_ver0_4_c_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_8

        subroutine mydgemv_ver0_4_c_10(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_10')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_10

        subroutine mydgemv_ver0_4_c_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_12

        subroutine mydgemv_ver0_4_c_14(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_14')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_14

        subroutine mydgemv_ver0_4_c_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_16

        subroutine mydgemv_ver0_4_c_18(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_18')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_18

        subroutine mydgemv_ver0_4_c_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_20

        subroutine mydgemv_ver0_4_c_22(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_22')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_22

        subroutine mydgemv_ver0_4_c_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_24

        subroutine mydgemv_ver0_4_c_26(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_26')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_26

        subroutine mydgemv_ver0_4_c_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_28

        subroutine mydgemv_ver0_4_c_30(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_30')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_30

        subroutine mydgemv_ver0_4_c_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_32

        subroutine mydgemv_ver0_4_c_34(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_34')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_34

        subroutine mydgemv_ver0_4_c_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_36

        subroutine mydgemv_ver0_4_c_38(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_38')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_38

        subroutine mydgemv_ver0_4_c_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_40

        subroutine mydgemv_ver0_4_c_42(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_42')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_42

        subroutine mydgemv_ver0_4_c_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_44

        subroutine mydgemv_ver0_4_c_46(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_46')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_46

        subroutine mydgemv_ver0_4_c_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_48

        subroutine mydgemv_ver0_4_c_50(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_50')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_50

        subroutine mydgemv_ver0_4_c_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_52

        subroutine mydgemv_ver0_4_c_54(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_54')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_54

        subroutine mydgemv_ver0_4_c_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_56

        subroutine mydgemv_ver0_4_c_58(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_58')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_58

        subroutine mydgemv_ver0_4_c_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_60

        subroutine mydgemv_ver0_4_c_62(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_62')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_62

        subroutine mydgemv_ver0_4_c_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_64

        subroutine mydgemv_ver0_4_c_66(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_66')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_66

        subroutine mydgemv_ver0_4_c_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_68

        subroutine mydgemv_ver0_4_c_70(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_70')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_70

        subroutine mydgemv_ver0_4_c_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_72

        subroutine mydgemv_ver0_4_c_74(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_74')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_74

        subroutine mydgemv_ver0_4_c_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_76

        subroutine mydgemv_ver0_4_c_78(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_78')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_78

        subroutine mydgemv_ver0_4_c_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_80

        subroutine mydgemv_ver0_4_c_82(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_82')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_82

        subroutine mydgemv_ver0_4_c_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_84

        subroutine mydgemv_ver0_4_c_86(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_86')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_86

        subroutine mydgemv_ver0_4_c_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_88

        subroutine mydgemv_ver0_4_c_90(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_90')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_90

        subroutine mydgemv_ver0_4_c_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_92

        subroutine mydgemv_ver0_4_c_94(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_94')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_94

        subroutine mydgemv_ver0_4_c_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_96

        subroutine mydgemv_ver0_4_c_98(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_98')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_98

        subroutine mydgemv_ver0_4_c_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_100

        subroutine mydgemv_ver0_4_c_102(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_102')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_102

        subroutine mydgemv_ver0_4_c_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_104

        subroutine mydgemv_ver0_4_c_106(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_106')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_106

        subroutine mydgemv_ver0_4_c_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_108

        subroutine mydgemv_ver0_4_c_110(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_110')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_110

        subroutine mydgemv_ver0_4_c_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_112

        subroutine mydgemv_ver0_4_c_114(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_114')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_114

        subroutine mydgemv_ver0_4_c_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_116

        subroutine mydgemv_ver0_4_c_118(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_118')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_118

        subroutine mydgemv_ver0_4_c_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_120

        subroutine mydgemv_ver0_4_c_122(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_122')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_122

        subroutine mydgemv_ver0_4_c_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_124

        subroutine mydgemv_ver0_4_c_126(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_126')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_126

        subroutine mydgemv_ver0_4_c_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_128

        subroutine mydgemv_ver0_4_c_130(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_130')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_130

        subroutine mydgemv_ver0_4_c_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_132

        subroutine mydgemv_ver0_4_c_134(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_134')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_134

        subroutine mydgemv_ver0_4_c_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_136

        subroutine mydgemv_ver0_4_c_138(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_138')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_138

        subroutine mydgemv_ver0_4_c_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_140

        subroutine mydgemv_ver0_4_c_142(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_142')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_142

        subroutine mydgemv_ver0_4_c_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_144

        subroutine mydgemv_ver0_4_c_146(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_146')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_146

        subroutine mydgemv_ver0_4_c_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_148

        subroutine mydgemv_ver0_4_c_150(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_150')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_150

        subroutine mydgemv_ver0_4_c_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_152

        subroutine mydgemv_ver0_4_c_154(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_154')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_154

        subroutine mydgemv_ver0_4_c_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_156

        subroutine mydgemv_ver0_4_c_158(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_158')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_158

        subroutine mydgemv_ver0_4_c_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_160

        subroutine mydgemv_ver0_4_c_162(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_162')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_162

        subroutine mydgemv_ver0_4_c_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_164

        subroutine mydgemv_ver0_4_c_166(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_166')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_166

        subroutine mydgemv_ver0_4_c_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_168

        subroutine mydgemv_ver0_4_c_170(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_170')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_170

        subroutine mydgemv_ver0_4_c_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_172

        subroutine mydgemv_ver0_4_c_174(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_174')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_174

        subroutine mydgemv_ver0_4_c_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_176

        subroutine mydgemv_ver0_4_c_178(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_178')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_178

        subroutine mydgemv_ver0_4_c_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_180

        subroutine mydgemv_ver0_4_c_182(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_182')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_182

        subroutine mydgemv_ver0_4_c_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_184

        subroutine mydgemv_ver0_4_c_186(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_186')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_186

        subroutine mydgemv_ver0_4_c_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_188

        subroutine mydgemv_ver0_4_c_190(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_190')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_190

        subroutine mydgemv_ver0_4_c_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_192

        subroutine mydgemv_ver0_4_c_194(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_194')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_194

        subroutine mydgemv_ver0_4_c_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_196

        subroutine mydgemv_ver0_4_c_198(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_198')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_198

        subroutine mydgemv_ver0_4_c_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_200

        subroutine mydgemv_ver0_4_c_202(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_202')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_202

        subroutine mydgemv_ver0_4_c_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_204

        subroutine mydgemv_ver0_4_c_206(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_206')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_206

        subroutine mydgemv_ver0_4_c_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_208

        subroutine mydgemv_ver0_4_c_210(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_210')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_210

        subroutine mydgemv_ver0_4_c_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_212

        subroutine mydgemv_ver0_4_c_214(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_214')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_214

        subroutine mydgemv_ver0_4_c_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_216

        subroutine mydgemv_ver0_4_c_218(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_218')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_218

        subroutine mydgemv_ver0_4_c_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_220

        subroutine mydgemv_ver0_4_c_222(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_222')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_222

        subroutine mydgemv_ver0_4_c_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_224

        subroutine mydgemv_ver0_4_c_226(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_226')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_226

        subroutine mydgemv_ver0_4_c_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_228

        subroutine mydgemv_ver0_4_c_230(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_230')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_230

        subroutine mydgemv_ver0_4_c_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_232

        subroutine mydgemv_ver0_4_c_234(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_234')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_234

        subroutine mydgemv_ver0_4_c_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_236

        subroutine mydgemv_ver0_4_c_238(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_238')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_238

        subroutine mydgemv_ver0_4_c_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_240

        subroutine mydgemv_ver0_4_c_242(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_242')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_242

        subroutine mydgemv_ver0_4_c_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_244

        subroutine mydgemv_ver0_4_c_246(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_246')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_246

        subroutine mydgemv_ver0_4_c_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_248

        subroutine mydgemv_ver0_4_c_250(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_250')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_250

        subroutine mydgemv_ver0_4_c_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_252

        subroutine mydgemv_ver0_4_c_254(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_254')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_254

        subroutine mydgemv_ver0_4_c_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_256

        subroutine mydgemv_ver0_4_c_258(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_258')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_258

        subroutine mydgemv_ver0_4_c_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_260

        subroutine mydgemv_ver0_4_c_262(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_262')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_262

        subroutine mydgemv_ver0_4_c_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_264

        subroutine mydgemv_ver0_4_c_266(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_266')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_266

        subroutine mydgemv_ver0_4_c_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_268

        subroutine mydgemv_ver0_4_c_270(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_270')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_270

        subroutine mydgemv_ver0_4_c_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_272

        subroutine mydgemv_ver0_4_c_274(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_274')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_274

        subroutine mydgemv_ver0_4_c_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_276

        subroutine mydgemv_ver0_4_c_278(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_278')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_278

        subroutine mydgemv_ver0_4_c_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_280

        subroutine mydgemv_ver0_4_c_282(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_282')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_282

        subroutine mydgemv_ver0_4_c_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_284

        subroutine mydgemv_ver0_4_c_286(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_286')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_286

        subroutine mydgemv_ver0_4_c_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_288

        subroutine mydgemv_ver0_4_c_290(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_290')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_290

        subroutine mydgemv_ver0_4_c_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_292

        subroutine mydgemv_ver0_4_c_294(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_294')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_294

        subroutine mydgemv_ver0_4_c_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_296

        subroutine mydgemv_ver0_4_c_298(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_298')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_298

        subroutine mydgemv_ver0_4_c_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_300

        subroutine mydgemv_ver0_4_c_302(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_302')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_302

        subroutine mydgemv_ver0_4_c_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_304

        subroutine mydgemv_ver0_4_c_306(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_306')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_306

        subroutine mydgemv_ver0_4_c_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_308

        subroutine mydgemv_ver0_4_c_310(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_310')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_310

        subroutine mydgemv_ver0_4_c_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_312

        subroutine mydgemv_ver0_4_c_314(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_314')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_314

        subroutine mydgemv_ver0_4_c_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_316

        subroutine mydgemv_ver0_4_c_318(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_318')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_318

        subroutine mydgemv_ver0_4_c_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_320

        subroutine mydgemv_ver0_4_c_322(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_322')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_322

        subroutine mydgemv_ver0_4_c_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_324

        subroutine mydgemv_ver0_4_c_326(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_326')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_326

        subroutine mydgemv_ver0_4_c_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_328

        subroutine mydgemv_ver0_4_c_330(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_330')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_330

        subroutine mydgemv_ver0_4_c_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_332

        subroutine mydgemv_ver0_4_c_334(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_334')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_334

        subroutine mydgemv_ver0_4_c_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_336

        subroutine mydgemv_ver0_4_c_338(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_338')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_338

        subroutine mydgemv_ver0_4_c_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_340

        subroutine mydgemv_ver0_4_c_342(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_342')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_342

        subroutine mydgemv_ver0_4_c_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_344

        subroutine mydgemv_ver0_4_c_346(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_346')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_346

        subroutine mydgemv_ver0_4_c_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_348

        subroutine mydgemv_ver0_4_c_350(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_350')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_350

        subroutine mydgemv_ver0_4_c_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_352

        subroutine mydgemv_ver0_4_c_354(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_354')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_354

        subroutine mydgemv_ver0_4_c_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_356

        subroutine mydgemv_ver0_4_c_358(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_358')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_358

        subroutine mydgemv_ver0_4_c_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_360

        subroutine mydgemv_ver0_4_c_362(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_362')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_362

        subroutine mydgemv_ver0_4_c_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_364

        subroutine mydgemv_ver0_4_c_366(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_366')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_366

        subroutine mydgemv_ver0_4_c_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_368

        subroutine mydgemv_ver0_4_c_370(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_370')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_370

        subroutine mydgemv_ver0_4_c_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_372

        subroutine mydgemv_ver0_4_c_374(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_374')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_374

        subroutine mydgemv_ver0_4_c_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_376

        subroutine mydgemv_ver0_4_c_378(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_378')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_378

        subroutine mydgemv_ver0_4_c_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_380

        subroutine mydgemv_ver0_4_c_382(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_382')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_382

        subroutine mydgemv_ver0_4_c_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_384

        subroutine mydgemv_ver0_4_c_386(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_386')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_386

        subroutine mydgemv_ver0_4_c_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_388

        subroutine mydgemv_ver0_4_c_390(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_390')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_390

        subroutine mydgemv_ver0_4_c_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_392

        subroutine mydgemv_ver0_4_c_394(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_394')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_394

        subroutine mydgemv_ver0_4_c_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_396

        subroutine mydgemv_ver0_4_c_398(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_398')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_398

        subroutine mydgemv_ver0_4_c_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_400

        subroutine mydgemv_ver0_4_c_402(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_402')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_402

        subroutine mydgemv_ver0_4_c_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_404

        subroutine mydgemv_ver0_4_c_406(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_406')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_406

        subroutine mydgemv_ver0_4_c_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_408

        subroutine mydgemv_ver0_4_c_410(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_410')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_410

        subroutine mydgemv_ver0_4_c_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_412

        subroutine mydgemv_ver0_4_c_414(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_414')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_414

        subroutine mydgemv_ver0_4_c_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_416

        subroutine mydgemv_ver0_4_c_418(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_418')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_418

        subroutine mydgemv_ver0_4_c_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_420

        subroutine mydgemv_ver0_4_c_422(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_422')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_422

        subroutine mydgemv_ver0_4_c_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_424

        subroutine mydgemv_ver0_4_c_426(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_426')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_426

        subroutine mydgemv_ver0_4_c_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_428

        subroutine mydgemv_ver0_4_c_430(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_430')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_430

        subroutine mydgemv_ver0_4_c_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_432

        subroutine mydgemv_ver0_4_c_434(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_434')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_434

        subroutine mydgemv_ver0_4_c_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_436

        subroutine mydgemv_ver0_4_c_438(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_438')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_438

        subroutine mydgemv_ver0_4_c_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_440

        subroutine mydgemv_ver0_4_c_442(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_442')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_442

        subroutine mydgemv_ver0_4_c_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_444

        subroutine mydgemv_ver0_4_c_446(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_446')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_446

        subroutine mydgemv_ver0_4_c_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_448

        subroutine mydgemv_ver0_4_c_450(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_450')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_450

        subroutine mydgemv_ver0_4_c_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_452

        subroutine mydgemv_ver0_4_c_454(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_454')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_454

        subroutine mydgemv_ver0_4_c_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_456

        subroutine mydgemv_ver0_4_c_458(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_458')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_458

        subroutine mydgemv_ver0_4_c_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_460

        subroutine mydgemv_ver0_4_c_462(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_462')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_462

        subroutine mydgemv_ver0_4_c_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_464

        subroutine mydgemv_ver0_4_c_466(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_466')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_466

        subroutine mydgemv_ver0_4_c_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_468

        subroutine mydgemv_ver0_4_c_470(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_470')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_470

        subroutine mydgemv_ver0_4_c_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_472

        subroutine mydgemv_ver0_4_c_474(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_474')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_474

        subroutine mydgemv_ver0_4_c_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_476

        subroutine mydgemv_ver0_4_c_478(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_478')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_478

        subroutine mydgemv_ver0_4_c_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_480

        subroutine mydgemv_ver0_4_c_482(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_482')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_482

        subroutine mydgemv_ver0_4_c_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_484

        subroutine mydgemv_ver0_4_c_486(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_486')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_486

        subroutine mydgemv_ver0_4_c_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_488

        subroutine mydgemv_ver0_4_c_490(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_490')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_490

        subroutine mydgemv_ver0_4_c_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_492

        subroutine mydgemv_ver0_4_c_494(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_494')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_494

        subroutine mydgemv_ver0_4_c_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_496

        subroutine mydgemv_ver0_4_c_498(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_498')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_498

        subroutine mydgemv_ver0_4_c_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_500

        subroutine mydgemv_ver0_4_c_502(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_502')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_502

        subroutine mydgemv_ver0_4_c_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_504

        subroutine mydgemv_ver0_4_c_506(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_506')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_506

        subroutine mydgemv_ver0_4_c_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_508

        subroutine mydgemv_ver0_4_c_510(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_510')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_510

        subroutine mydgemv_ver0_4_c_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_4_c_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_4_c_512


        subroutine mydgemv_ver0_5_c_2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_2

        subroutine mydgemv_ver0_5_c_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_4

        subroutine mydgemv_ver0_5_c_6(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_6')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_6

        subroutine mydgemv_ver0_5_c_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_8

        subroutine mydgemv_ver0_5_c_10(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_10')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_10

        subroutine mydgemv_ver0_5_c_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_12

        subroutine mydgemv_ver0_5_c_14(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_14')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_14

        subroutine mydgemv_ver0_5_c_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_16

        subroutine mydgemv_ver0_5_c_18(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_18')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_18

        subroutine mydgemv_ver0_5_c_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_20

        subroutine mydgemv_ver0_5_c_22(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_22')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_22

        subroutine mydgemv_ver0_5_c_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_24

        subroutine mydgemv_ver0_5_c_26(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_26')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_26

        subroutine mydgemv_ver0_5_c_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_28

        subroutine mydgemv_ver0_5_c_30(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_30')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_30

        subroutine mydgemv_ver0_5_c_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_32

        subroutine mydgemv_ver0_5_c_34(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_34')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_34

        subroutine mydgemv_ver0_5_c_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_36

        subroutine mydgemv_ver0_5_c_38(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_38')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_38

        subroutine mydgemv_ver0_5_c_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_40

        subroutine mydgemv_ver0_5_c_42(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_42')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_42

        subroutine mydgemv_ver0_5_c_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_44

        subroutine mydgemv_ver0_5_c_46(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_46')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_46

        subroutine mydgemv_ver0_5_c_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_48

        subroutine mydgemv_ver0_5_c_50(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_50')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_50

        subroutine mydgemv_ver0_5_c_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_52

        subroutine mydgemv_ver0_5_c_54(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_54')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_54

        subroutine mydgemv_ver0_5_c_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_56

        subroutine mydgemv_ver0_5_c_58(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_58')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_58

        subroutine mydgemv_ver0_5_c_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_60

        subroutine mydgemv_ver0_5_c_62(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_62')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_62

        subroutine mydgemv_ver0_5_c_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_64

        subroutine mydgemv_ver0_5_c_66(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_66')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_66

        subroutine mydgemv_ver0_5_c_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_68

        subroutine mydgemv_ver0_5_c_70(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_70')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_70

        subroutine mydgemv_ver0_5_c_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_72

        subroutine mydgemv_ver0_5_c_74(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_74')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_74

        subroutine mydgemv_ver0_5_c_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_76

        subroutine mydgemv_ver0_5_c_78(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_78')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_78

        subroutine mydgemv_ver0_5_c_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_80

        subroutine mydgemv_ver0_5_c_82(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_82')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_82

        subroutine mydgemv_ver0_5_c_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_84

        subroutine mydgemv_ver0_5_c_86(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_86')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_86

        subroutine mydgemv_ver0_5_c_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_88

        subroutine mydgemv_ver0_5_c_90(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_90')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_90

        subroutine mydgemv_ver0_5_c_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_92

        subroutine mydgemv_ver0_5_c_94(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_94')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_94

        subroutine mydgemv_ver0_5_c_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_96

        subroutine mydgemv_ver0_5_c_98(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_98')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_98

        subroutine mydgemv_ver0_5_c_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_100

        subroutine mydgemv_ver0_5_c_102(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_102')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_102

        subroutine mydgemv_ver0_5_c_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_104

        subroutine mydgemv_ver0_5_c_106(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_106')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_106

        subroutine mydgemv_ver0_5_c_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_108

        subroutine mydgemv_ver0_5_c_110(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_110')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_110

        subroutine mydgemv_ver0_5_c_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_112

        subroutine mydgemv_ver0_5_c_114(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_114')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_114

        subroutine mydgemv_ver0_5_c_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_116

        subroutine mydgemv_ver0_5_c_118(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_118')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_118

        subroutine mydgemv_ver0_5_c_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_120

        subroutine mydgemv_ver0_5_c_122(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_122')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_122

        subroutine mydgemv_ver0_5_c_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_124

        subroutine mydgemv_ver0_5_c_126(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_126')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_126

        subroutine mydgemv_ver0_5_c_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_128

        subroutine mydgemv_ver0_5_c_130(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_130')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_130

        subroutine mydgemv_ver0_5_c_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_132

        subroutine mydgemv_ver0_5_c_134(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_134')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_134

        subroutine mydgemv_ver0_5_c_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_136

        subroutine mydgemv_ver0_5_c_138(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_138')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_138

        subroutine mydgemv_ver0_5_c_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_140

        subroutine mydgemv_ver0_5_c_142(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_142')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_142

        subroutine mydgemv_ver0_5_c_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_144

        subroutine mydgemv_ver0_5_c_146(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_146')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_146

        subroutine mydgemv_ver0_5_c_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_148

        subroutine mydgemv_ver0_5_c_150(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_150')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_150

        subroutine mydgemv_ver0_5_c_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_152

        subroutine mydgemv_ver0_5_c_154(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_154')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_154

        subroutine mydgemv_ver0_5_c_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_156

        subroutine mydgemv_ver0_5_c_158(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_158')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_158

        subroutine mydgemv_ver0_5_c_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_160

        subroutine mydgemv_ver0_5_c_162(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_162')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_162

        subroutine mydgemv_ver0_5_c_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_164

        subroutine mydgemv_ver0_5_c_166(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_166')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_166

        subroutine mydgemv_ver0_5_c_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_168

        subroutine mydgemv_ver0_5_c_170(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_170')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_170

        subroutine mydgemv_ver0_5_c_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_172

        subroutine mydgemv_ver0_5_c_174(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_174')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_174

        subroutine mydgemv_ver0_5_c_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_176

        subroutine mydgemv_ver0_5_c_178(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_178')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_178

        subroutine mydgemv_ver0_5_c_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_180

        subroutine mydgemv_ver0_5_c_182(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_182')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_182

        subroutine mydgemv_ver0_5_c_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_184

        subroutine mydgemv_ver0_5_c_186(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_186')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_186

        subroutine mydgemv_ver0_5_c_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_188

        subroutine mydgemv_ver0_5_c_190(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_190')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_190

        subroutine mydgemv_ver0_5_c_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_192

        subroutine mydgemv_ver0_5_c_194(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_194')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_194

        subroutine mydgemv_ver0_5_c_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_196

        subroutine mydgemv_ver0_5_c_198(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_198')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_198

        subroutine mydgemv_ver0_5_c_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_200

        subroutine mydgemv_ver0_5_c_202(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_202')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_202

        subroutine mydgemv_ver0_5_c_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_204

        subroutine mydgemv_ver0_5_c_206(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_206')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_206

        subroutine mydgemv_ver0_5_c_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_208

        subroutine mydgemv_ver0_5_c_210(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_210')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_210

        subroutine mydgemv_ver0_5_c_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_212

        subroutine mydgemv_ver0_5_c_214(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_214')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_214

        subroutine mydgemv_ver0_5_c_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_216

        subroutine mydgemv_ver0_5_c_218(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_218')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_218

        subroutine mydgemv_ver0_5_c_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_220

        subroutine mydgemv_ver0_5_c_222(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_222')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_222

        subroutine mydgemv_ver0_5_c_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_224

        subroutine mydgemv_ver0_5_c_226(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_226')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_226

        subroutine mydgemv_ver0_5_c_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_228

        subroutine mydgemv_ver0_5_c_230(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_230')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_230

        subroutine mydgemv_ver0_5_c_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_232

        subroutine mydgemv_ver0_5_c_234(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_234')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_234

        subroutine mydgemv_ver0_5_c_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_236

        subroutine mydgemv_ver0_5_c_238(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_238')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_238

        subroutine mydgemv_ver0_5_c_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_240

        subroutine mydgemv_ver0_5_c_242(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_242')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_242

        subroutine mydgemv_ver0_5_c_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_244

        subroutine mydgemv_ver0_5_c_246(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_246')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_246

        subroutine mydgemv_ver0_5_c_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_248

        subroutine mydgemv_ver0_5_c_250(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_250')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_250

        subroutine mydgemv_ver0_5_c_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_252

        subroutine mydgemv_ver0_5_c_254(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_254')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_254

        subroutine mydgemv_ver0_5_c_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_256

        subroutine mydgemv_ver0_5_c_258(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_258')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_258

        subroutine mydgemv_ver0_5_c_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_260

        subroutine mydgemv_ver0_5_c_262(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_262')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_262

        subroutine mydgemv_ver0_5_c_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_264

        subroutine mydgemv_ver0_5_c_266(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_266')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_266

        subroutine mydgemv_ver0_5_c_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_268

        subroutine mydgemv_ver0_5_c_270(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_270')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_270

        subroutine mydgemv_ver0_5_c_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_272

        subroutine mydgemv_ver0_5_c_274(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_274')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_274

        subroutine mydgemv_ver0_5_c_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_276

        subroutine mydgemv_ver0_5_c_278(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_278')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_278

        subroutine mydgemv_ver0_5_c_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_280

        subroutine mydgemv_ver0_5_c_282(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_282')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_282

        subroutine mydgemv_ver0_5_c_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_284

        subroutine mydgemv_ver0_5_c_286(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_286')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_286

        subroutine mydgemv_ver0_5_c_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_288

        subroutine mydgemv_ver0_5_c_290(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_290')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_290

        subroutine mydgemv_ver0_5_c_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_292

        subroutine mydgemv_ver0_5_c_294(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_294')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_294

        subroutine mydgemv_ver0_5_c_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_296

        subroutine mydgemv_ver0_5_c_298(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_298')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_298

        subroutine mydgemv_ver0_5_c_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_300

        subroutine mydgemv_ver0_5_c_302(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_302')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_302

        subroutine mydgemv_ver0_5_c_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_304

        subroutine mydgemv_ver0_5_c_306(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_306')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_306

        subroutine mydgemv_ver0_5_c_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_308

        subroutine mydgemv_ver0_5_c_310(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_310')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_310

        subroutine mydgemv_ver0_5_c_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_312

        subroutine mydgemv_ver0_5_c_314(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_314')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_314

        subroutine mydgemv_ver0_5_c_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_316

        subroutine mydgemv_ver0_5_c_318(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_318')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_318

        subroutine mydgemv_ver0_5_c_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_320

        subroutine mydgemv_ver0_5_c_322(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_322')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_322

        subroutine mydgemv_ver0_5_c_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_324

        subroutine mydgemv_ver0_5_c_326(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_326')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_326

        subroutine mydgemv_ver0_5_c_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_328

        subroutine mydgemv_ver0_5_c_330(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_330')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_330

        subroutine mydgemv_ver0_5_c_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_332

        subroutine mydgemv_ver0_5_c_334(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_334')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_334

        subroutine mydgemv_ver0_5_c_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_336

        subroutine mydgemv_ver0_5_c_338(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_338')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_338

        subroutine mydgemv_ver0_5_c_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_340

        subroutine mydgemv_ver0_5_c_342(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_342')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_342

        subroutine mydgemv_ver0_5_c_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_344

        subroutine mydgemv_ver0_5_c_346(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_346')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_346

        subroutine mydgemv_ver0_5_c_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_348

        subroutine mydgemv_ver0_5_c_350(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_350')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_350

        subroutine mydgemv_ver0_5_c_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_352

        subroutine mydgemv_ver0_5_c_354(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_354')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_354

        subroutine mydgemv_ver0_5_c_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_356

        subroutine mydgemv_ver0_5_c_358(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_358')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_358

        subroutine mydgemv_ver0_5_c_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_360

        subroutine mydgemv_ver0_5_c_362(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_362')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_362

        subroutine mydgemv_ver0_5_c_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_364

        subroutine mydgemv_ver0_5_c_366(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_366')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_366

        subroutine mydgemv_ver0_5_c_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_368

        subroutine mydgemv_ver0_5_c_370(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_370')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_370

        subroutine mydgemv_ver0_5_c_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_372

        subroutine mydgemv_ver0_5_c_374(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_374')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_374

        subroutine mydgemv_ver0_5_c_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_376

        subroutine mydgemv_ver0_5_c_378(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_378')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_378

        subroutine mydgemv_ver0_5_c_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_380

        subroutine mydgemv_ver0_5_c_382(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_382')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_382

        subroutine mydgemv_ver0_5_c_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_384

        subroutine mydgemv_ver0_5_c_386(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_386')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_386

        subroutine mydgemv_ver0_5_c_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_388

        subroutine mydgemv_ver0_5_c_390(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_390')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_390

        subroutine mydgemv_ver0_5_c_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_392

        subroutine mydgemv_ver0_5_c_394(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_394')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_394

        subroutine mydgemv_ver0_5_c_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_396

        subroutine mydgemv_ver0_5_c_398(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_398')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_398

        subroutine mydgemv_ver0_5_c_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_400

        subroutine mydgemv_ver0_5_c_402(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_402')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_402

        subroutine mydgemv_ver0_5_c_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_404

        subroutine mydgemv_ver0_5_c_406(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_406')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_406

        subroutine mydgemv_ver0_5_c_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_408

        subroutine mydgemv_ver0_5_c_410(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_410')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_410

        subroutine mydgemv_ver0_5_c_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_412

        subroutine mydgemv_ver0_5_c_414(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_414')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_414

        subroutine mydgemv_ver0_5_c_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_416

        subroutine mydgemv_ver0_5_c_418(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_418')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_418

        subroutine mydgemv_ver0_5_c_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_420

        subroutine mydgemv_ver0_5_c_422(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_422')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_422

        subroutine mydgemv_ver0_5_c_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_424

        subroutine mydgemv_ver0_5_c_426(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_426')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_426

        subroutine mydgemv_ver0_5_c_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_428

        subroutine mydgemv_ver0_5_c_430(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_430')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_430

        subroutine mydgemv_ver0_5_c_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_432

        subroutine mydgemv_ver0_5_c_434(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_434')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_434

        subroutine mydgemv_ver0_5_c_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_436

        subroutine mydgemv_ver0_5_c_438(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_438')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_438

        subroutine mydgemv_ver0_5_c_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_440

        subroutine mydgemv_ver0_5_c_442(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_442')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_442

        subroutine mydgemv_ver0_5_c_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_444

        subroutine mydgemv_ver0_5_c_446(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_446')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_446

        subroutine mydgemv_ver0_5_c_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_448

        subroutine mydgemv_ver0_5_c_450(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_450')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_450

        subroutine mydgemv_ver0_5_c_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_452

        subroutine mydgemv_ver0_5_c_454(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_454')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_454

        subroutine mydgemv_ver0_5_c_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_456

        subroutine mydgemv_ver0_5_c_458(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_458')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_458

        subroutine mydgemv_ver0_5_c_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_460

        subroutine mydgemv_ver0_5_c_462(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_462')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_462

        subroutine mydgemv_ver0_5_c_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_464

        subroutine mydgemv_ver0_5_c_466(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_466')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_466

        subroutine mydgemv_ver0_5_c_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_468

        subroutine mydgemv_ver0_5_c_470(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_470')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_470

        subroutine mydgemv_ver0_5_c_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_472

        subroutine mydgemv_ver0_5_c_474(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_474')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_474

        subroutine mydgemv_ver0_5_c_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_476

        subroutine mydgemv_ver0_5_c_478(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_478')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_478

        subroutine mydgemv_ver0_5_c_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_480

        subroutine mydgemv_ver0_5_c_482(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_482')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_482

        subroutine mydgemv_ver0_5_c_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_484

        subroutine mydgemv_ver0_5_c_486(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_486')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_486

        subroutine mydgemv_ver0_5_c_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_488

        subroutine mydgemv_ver0_5_c_490(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_490')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_490

        subroutine mydgemv_ver0_5_c_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_492

        subroutine mydgemv_ver0_5_c_494(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_494')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_494

        subroutine mydgemv_ver0_5_c_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_496

        subroutine mydgemv_ver0_5_c_498(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_498')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_498

        subroutine mydgemv_ver0_5_c_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_500

        subroutine mydgemv_ver0_5_c_502(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_502')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_502

        subroutine mydgemv_ver0_5_c_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_504

        subroutine mydgemv_ver0_5_c_506(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_506')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_506

        subroutine mydgemv_ver0_5_c_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_508

        subroutine mydgemv_ver0_5_c_510(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_510')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_510

        subroutine mydgemv_ver0_5_c_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_ver0_5_c_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_ver0_5_c_512        



    
        subroutine mydgemv_n_ver1_0(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_0
    
        subroutine mydgemv_t_ver1_0(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_0
    
        subroutine mydgemv_n_ver1_1_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_1_unroll1
    
        subroutine mydgemv_t_ver1_1_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_1_unroll1
    
        subroutine mydgemv_n_ver1_1_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_1_unroll2
    
        subroutine mydgemv_t_ver1_1_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_1_unroll2
    
        subroutine mydgemv_n_ver1_1_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_1_unroll4
    
        subroutine mydgemv_t_ver1_1_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_1_unroll4
    
        subroutine mydgemv_n_ver1_1_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_1_unroll8
    
        subroutine mydgemv_t_ver1_1_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_1_unroll8

        
        subroutine mydgemv_n_ver1_2_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_2_unroll1
    
        subroutine mydgemv_n_ver1_2_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_2_unroll2

        subroutine mydgemv_n_ver1_2_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_2_unroll4
    
        subroutine mydgemv_n_ver1_2_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_2_unroll8

        
        subroutine mydgemv_t_ver1_2_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_2_unroll1
    
        subroutine mydgemv_t_ver1_2_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_2_unroll2

        subroutine mydgemv_t_ver1_2_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_2_unroll4
    
        subroutine mydgemv_t_ver1_2_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_2_unroll8







        subroutine mydgemv_t_ver1_4_2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_2
          

        subroutine mydgemv_t_ver1_4_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_4
          

        subroutine mydgemv_t_ver1_4_6(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_6')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_6
          

        subroutine mydgemv_t_ver1_4_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_8
          

        subroutine mydgemv_t_ver1_4_10(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_10')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_10
          

        subroutine mydgemv_t_ver1_4_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_12
          

        subroutine mydgemv_t_ver1_4_14(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_14')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_14
          

        subroutine mydgemv_t_ver1_4_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_16
          

        subroutine mydgemv_t_ver1_4_18(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_18')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_18
          

        subroutine mydgemv_t_ver1_4_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_20
          

        subroutine mydgemv_t_ver1_4_22(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_22')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_22
          

        subroutine mydgemv_t_ver1_4_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_24
          

        subroutine mydgemv_t_ver1_4_26(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_26')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_26
          

        subroutine mydgemv_t_ver1_4_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_28
          

        subroutine mydgemv_t_ver1_4_30(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_30')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_30
          

        subroutine mydgemv_t_ver1_4_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_32
          

        subroutine mydgemv_t_ver1_4_34(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_34')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_34
          

        subroutine mydgemv_t_ver1_4_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_36
          

        subroutine mydgemv_t_ver1_4_38(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_38')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_38
          

        subroutine mydgemv_t_ver1_4_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_40
          

        subroutine mydgemv_t_ver1_4_42(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_42')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_42
          

        subroutine mydgemv_t_ver1_4_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_44
          

        subroutine mydgemv_t_ver1_4_46(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_46')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_46
          

        subroutine mydgemv_t_ver1_4_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_48
          

        subroutine mydgemv_t_ver1_4_50(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_50')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_50
          

        subroutine mydgemv_t_ver1_4_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_52
          

        subroutine mydgemv_t_ver1_4_54(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_54')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_54
          

        subroutine mydgemv_t_ver1_4_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_56
          

        subroutine mydgemv_t_ver1_4_58(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_58')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_58
          

        subroutine mydgemv_t_ver1_4_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_60
          

        subroutine mydgemv_t_ver1_4_62(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_62')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_62
          

        subroutine mydgemv_t_ver1_4_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_64
          

        subroutine mydgemv_t_ver1_4_66(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_66')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_66
          

        subroutine mydgemv_t_ver1_4_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_68
          

        subroutine mydgemv_t_ver1_4_70(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_70')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_70
          

        subroutine mydgemv_t_ver1_4_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_72
          

        subroutine mydgemv_t_ver1_4_74(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_74')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_74
          

        subroutine mydgemv_t_ver1_4_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_76
          

        subroutine mydgemv_t_ver1_4_78(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_78')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_78
          

        subroutine mydgemv_t_ver1_4_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_80
          

        subroutine mydgemv_t_ver1_4_82(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_82')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_82
          

        subroutine mydgemv_t_ver1_4_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_84
          

        subroutine mydgemv_t_ver1_4_86(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_86')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_86
          

        subroutine mydgemv_t_ver1_4_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_88
          

        subroutine mydgemv_t_ver1_4_90(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_90')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_90
          

        subroutine mydgemv_t_ver1_4_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_92
          

        subroutine mydgemv_t_ver1_4_94(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_94')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_94
          

        subroutine mydgemv_t_ver1_4_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_96
          

        subroutine mydgemv_t_ver1_4_98(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_98')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_98
          

        subroutine mydgemv_t_ver1_4_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_100
          

        subroutine mydgemv_t_ver1_4_102(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_102')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_102
          

        subroutine mydgemv_t_ver1_4_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_104
          

        subroutine mydgemv_t_ver1_4_106(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_106')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_106
          

        subroutine mydgemv_t_ver1_4_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_108
          

        subroutine mydgemv_t_ver1_4_110(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_110')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_110
          

        subroutine mydgemv_t_ver1_4_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_112
          

        subroutine mydgemv_t_ver1_4_114(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_114')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_114
          

        subroutine mydgemv_t_ver1_4_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_116
          

        subroutine mydgemv_t_ver1_4_118(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_118')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_118
          

        subroutine mydgemv_t_ver1_4_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_120
          

        subroutine mydgemv_t_ver1_4_122(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_122')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_122
          

        subroutine mydgemv_t_ver1_4_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_124
          

        subroutine mydgemv_t_ver1_4_126(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_126')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_126
          

        subroutine mydgemv_t_ver1_4_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_128
          

        subroutine mydgemv_t_ver1_4_130(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_130')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_130
          

        subroutine mydgemv_t_ver1_4_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_132
          

        subroutine mydgemv_t_ver1_4_134(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_134')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_134
          

        subroutine mydgemv_t_ver1_4_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_136
          

        subroutine mydgemv_t_ver1_4_138(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_138')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_138
          

        subroutine mydgemv_t_ver1_4_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_140
          

        subroutine mydgemv_t_ver1_4_142(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_142')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_142
          

        subroutine mydgemv_t_ver1_4_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_144
          

        subroutine mydgemv_t_ver1_4_146(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_146')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_146
          

        subroutine mydgemv_t_ver1_4_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_148
          

        subroutine mydgemv_t_ver1_4_150(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_150')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_150
          

        subroutine mydgemv_t_ver1_4_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_152
          

        subroutine mydgemv_t_ver1_4_154(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_154')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_154
          

        subroutine mydgemv_t_ver1_4_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_156
          

        subroutine mydgemv_t_ver1_4_158(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_158')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_158
          

        subroutine mydgemv_t_ver1_4_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_160
          

        subroutine mydgemv_t_ver1_4_162(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_162')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_162
          

        subroutine mydgemv_t_ver1_4_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_164
          

        subroutine mydgemv_t_ver1_4_166(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_166')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_166
          

        subroutine mydgemv_t_ver1_4_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_168
          

        subroutine mydgemv_t_ver1_4_170(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_170')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_170
          

        subroutine mydgemv_t_ver1_4_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_172
          

        subroutine mydgemv_t_ver1_4_174(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_174')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_174
          

        subroutine mydgemv_t_ver1_4_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_176
          

        subroutine mydgemv_t_ver1_4_178(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_178')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_178
          

        subroutine mydgemv_t_ver1_4_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_180
          

        subroutine mydgemv_t_ver1_4_182(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_182')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_182
          

        subroutine mydgemv_t_ver1_4_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_184
          

        subroutine mydgemv_t_ver1_4_186(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_186')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_186
          

        subroutine mydgemv_t_ver1_4_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_188
          

        subroutine mydgemv_t_ver1_4_190(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_190')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_190
          

        subroutine mydgemv_t_ver1_4_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_192
          

        subroutine mydgemv_t_ver1_4_194(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_194')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_194
          

        subroutine mydgemv_t_ver1_4_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_196
          

        subroutine mydgemv_t_ver1_4_198(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_198')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_198
          

        subroutine mydgemv_t_ver1_4_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_200
          

        subroutine mydgemv_t_ver1_4_202(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_202')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_202
          

        subroutine mydgemv_t_ver1_4_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_204
          

        subroutine mydgemv_t_ver1_4_206(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_206')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_206
          

        subroutine mydgemv_t_ver1_4_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_208
          

        subroutine mydgemv_t_ver1_4_210(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_210')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_210
          

        subroutine mydgemv_t_ver1_4_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_212
          

        subroutine mydgemv_t_ver1_4_214(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_214')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_214
          

        subroutine mydgemv_t_ver1_4_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_216
          

        subroutine mydgemv_t_ver1_4_218(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_218')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_218
          

        subroutine mydgemv_t_ver1_4_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_220
          

        subroutine mydgemv_t_ver1_4_222(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_222')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_222
          

        subroutine mydgemv_t_ver1_4_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_224
          

        subroutine mydgemv_t_ver1_4_226(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_226')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_226
          

        subroutine mydgemv_t_ver1_4_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_228
          

        subroutine mydgemv_t_ver1_4_230(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_230')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_230
          

        subroutine mydgemv_t_ver1_4_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_232
          

        subroutine mydgemv_t_ver1_4_234(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_234')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_234
          

        subroutine mydgemv_t_ver1_4_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_236
          

        subroutine mydgemv_t_ver1_4_238(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_238')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_238
          

        subroutine mydgemv_t_ver1_4_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_240
          

        subroutine mydgemv_t_ver1_4_242(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_242')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_242
          

        subroutine mydgemv_t_ver1_4_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_244
          

        subroutine mydgemv_t_ver1_4_246(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_246')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_246
          

        subroutine mydgemv_t_ver1_4_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_248
          

        subroutine mydgemv_t_ver1_4_250(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_250')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_250
          

        subroutine mydgemv_t_ver1_4_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_252
          

        subroutine mydgemv_t_ver1_4_254(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_254')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_254
          

        subroutine mydgemv_t_ver1_4_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_256
          

        subroutine mydgemv_t_ver1_4_258(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_258')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_258
          

        subroutine mydgemv_t_ver1_4_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_260
          

        subroutine mydgemv_t_ver1_4_262(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_262')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_262
          

        subroutine mydgemv_t_ver1_4_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_264
          

        subroutine mydgemv_t_ver1_4_266(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_266')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_266
          

        subroutine mydgemv_t_ver1_4_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_268
          

        subroutine mydgemv_t_ver1_4_270(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_270')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_270
          

        subroutine mydgemv_t_ver1_4_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_272
          

        subroutine mydgemv_t_ver1_4_274(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_274')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_274
          

        subroutine mydgemv_t_ver1_4_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_276
          

        subroutine mydgemv_t_ver1_4_278(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_278')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_278
          

        subroutine mydgemv_t_ver1_4_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_280
          

        subroutine mydgemv_t_ver1_4_282(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_282')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_282
          

        subroutine mydgemv_t_ver1_4_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_284
          

        subroutine mydgemv_t_ver1_4_286(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_286')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_286
          

        subroutine mydgemv_t_ver1_4_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_288
          

        subroutine mydgemv_t_ver1_4_290(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_290')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_290
          

        subroutine mydgemv_t_ver1_4_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_292
          

        subroutine mydgemv_t_ver1_4_294(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_294')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_294
          

        subroutine mydgemv_t_ver1_4_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_296
          

        subroutine mydgemv_t_ver1_4_298(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_298')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_298
          

        subroutine mydgemv_t_ver1_4_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_300
          

        subroutine mydgemv_t_ver1_4_302(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_302')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_302
          

        subroutine mydgemv_t_ver1_4_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_304
          

        subroutine mydgemv_t_ver1_4_306(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_306')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_306
          

        subroutine mydgemv_t_ver1_4_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_308
          

        subroutine mydgemv_t_ver1_4_310(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_310')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_310
          

        subroutine mydgemv_t_ver1_4_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_312
          

        subroutine mydgemv_t_ver1_4_314(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_314')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_314
          

        subroutine mydgemv_t_ver1_4_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_316
          

        subroutine mydgemv_t_ver1_4_318(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_318')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_318
          

        subroutine mydgemv_t_ver1_4_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_320
          

        subroutine mydgemv_t_ver1_4_322(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_322')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_322
          

        subroutine mydgemv_t_ver1_4_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_324
          

        subroutine mydgemv_t_ver1_4_326(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_326')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_326
          

        subroutine mydgemv_t_ver1_4_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_328
          

        subroutine mydgemv_t_ver1_4_330(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_330')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_330
          

        subroutine mydgemv_t_ver1_4_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_332
          

        subroutine mydgemv_t_ver1_4_334(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_334')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_334
          

        subroutine mydgemv_t_ver1_4_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_336
          

        subroutine mydgemv_t_ver1_4_338(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_338')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_338
          

        subroutine mydgemv_t_ver1_4_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_340
          

        subroutine mydgemv_t_ver1_4_342(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_342')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_342
          

        subroutine mydgemv_t_ver1_4_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_344
          

        subroutine mydgemv_t_ver1_4_346(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_346')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_346
          

        subroutine mydgemv_t_ver1_4_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_348
          

        subroutine mydgemv_t_ver1_4_350(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_350')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_350
          

        subroutine mydgemv_t_ver1_4_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_352
          

        subroutine mydgemv_t_ver1_4_354(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_354')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_354
          

        subroutine mydgemv_t_ver1_4_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_356
          

        subroutine mydgemv_t_ver1_4_358(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_358')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_358
          

        subroutine mydgemv_t_ver1_4_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_360
          

        subroutine mydgemv_t_ver1_4_362(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_362')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_362
          

        subroutine mydgemv_t_ver1_4_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_364
          

        subroutine mydgemv_t_ver1_4_366(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_366')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_366
          

        subroutine mydgemv_t_ver1_4_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_368
          

        subroutine mydgemv_t_ver1_4_370(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_370')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_370
          

        subroutine mydgemv_t_ver1_4_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_372
          

        subroutine mydgemv_t_ver1_4_374(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_374')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_374
          

        subroutine mydgemv_t_ver1_4_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_376
          

        subroutine mydgemv_t_ver1_4_378(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_378')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_378
          

        subroutine mydgemv_t_ver1_4_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_380
          

        subroutine mydgemv_t_ver1_4_382(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_382')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_382
          

        subroutine mydgemv_t_ver1_4_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_384
          

        subroutine mydgemv_t_ver1_4_386(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_386')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_386
          

        subroutine mydgemv_t_ver1_4_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_388
          

        subroutine mydgemv_t_ver1_4_390(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_390')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_390
          

        subroutine mydgemv_t_ver1_4_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_392
          

        subroutine mydgemv_t_ver1_4_394(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_394')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_394
          

        subroutine mydgemv_t_ver1_4_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_396
          

        subroutine mydgemv_t_ver1_4_398(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_398')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_398
          

        subroutine mydgemv_t_ver1_4_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_400
          

        subroutine mydgemv_t_ver1_4_402(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_402')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_402
          

        subroutine mydgemv_t_ver1_4_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_404
          

        subroutine mydgemv_t_ver1_4_406(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_406')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_406
          

        subroutine mydgemv_t_ver1_4_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_408
          

        subroutine mydgemv_t_ver1_4_410(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_410')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_410
          

        subroutine mydgemv_t_ver1_4_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_412
          

        subroutine mydgemv_t_ver1_4_414(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_414')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_414
          

        subroutine mydgemv_t_ver1_4_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_416
          

        subroutine mydgemv_t_ver1_4_418(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_418')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_418
          

        subroutine mydgemv_t_ver1_4_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_420
          

        subroutine mydgemv_t_ver1_4_422(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_422')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_422
          

        subroutine mydgemv_t_ver1_4_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_424
          

        subroutine mydgemv_t_ver1_4_426(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_426')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_426
          

        subroutine mydgemv_t_ver1_4_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_428
          

        subroutine mydgemv_t_ver1_4_430(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_430')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_430
          

        subroutine mydgemv_t_ver1_4_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_432
          

        subroutine mydgemv_t_ver1_4_434(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_434')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_434
          

        subroutine mydgemv_t_ver1_4_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_436
          

        subroutine mydgemv_t_ver1_4_438(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_438')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_438
          

        subroutine mydgemv_t_ver1_4_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_440
          

        subroutine mydgemv_t_ver1_4_442(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_442')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_442
          

        subroutine mydgemv_t_ver1_4_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_444
          

        subroutine mydgemv_t_ver1_4_446(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_446')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_446
          

        subroutine mydgemv_t_ver1_4_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_448
          

        subroutine mydgemv_t_ver1_4_450(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_450')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_450
          

        subroutine mydgemv_t_ver1_4_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_452
          

        subroutine mydgemv_t_ver1_4_454(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_454')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_454
          

        subroutine mydgemv_t_ver1_4_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_456
          

        subroutine mydgemv_t_ver1_4_458(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_458')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_458
          

        subroutine mydgemv_t_ver1_4_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_460
          

        subroutine mydgemv_t_ver1_4_462(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_462')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_462
          

        subroutine mydgemv_t_ver1_4_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_464
          

        subroutine mydgemv_t_ver1_4_466(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_466')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_466
          

        subroutine mydgemv_t_ver1_4_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_468
          

        subroutine mydgemv_t_ver1_4_470(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_470')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_470
          

        subroutine mydgemv_t_ver1_4_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_472
          

        subroutine mydgemv_t_ver1_4_474(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_474')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_474
          

        subroutine mydgemv_t_ver1_4_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_476
          

        subroutine mydgemv_t_ver1_4_478(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_478')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_478
          

        subroutine mydgemv_t_ver1_4_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_480
          

        subroutine mydgemv_t_ver1_4_482(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_482')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_482
          

        subroutine mydgemv_t_ver1_4_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_484
          

        subroutine mydgemv_t_ver1_4_486(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_486')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_486
          

        subroutine mydgemv_t_ver1_4_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_488
          

        subroutine mydgemv_t_ver1_4_490(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_490')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_490
          

        subroutine mydgemv_t_ver1_4_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_492
          

        subroutine mydgemv_t_ver1_4_494(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_494')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_494
          

        subroutine mydgemv_t_ver1_4_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_496
          

        subroutine mydgemv_t_ver1_4_498(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_498')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_498
          

        subroutine mydgemv_t_ver1_4_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_500
          

        subroutine mydgemv_t_ver1_4_502(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_502')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_502
          

        subroutine mydgemv_t_ver1_4_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_504
          

        subroutine mydgemv_t_ver1_4_506(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_506')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_506
          

        subroutine mydgemv_t_ver1_4_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_508
          

        subroutine mydgemv_t_ver1_4_510(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_510')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_510
          

        subroutine mydgemv_t_ver1_4_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_4_512
        
        



        subroutine mydgemv_t_ver1_5_2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_2
          

        subroutine mydgemv_t_ver1_5_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_4
          

        subroutine mydgemv_t_ver1_5_6(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_6')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_6
          

        subroutine mydgemv_t_ver1_5_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_8
          

        subroutine mydgemv_t_ver1_5_10(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_10')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_10
          

        subroutine mydgemv_t_ver1_5_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_12
          

        subroutine mydgemv_t_ver1_5_14(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_14')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_14
          

        subroutine mydgemv_t_ver1_5_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_16
          

        subroutine mydgemv_t_ver1_5_18(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_18')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_18
          

        subroutine mydgemv_t_ver1_5_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_20
          

        subroutine mydgemv_t_ver1_5_22(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_22')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_22
          

        subroutine mydgemv_t_ver1_5_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_24
          

        subroutine mydgemv_t_ver1_5_26(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_26')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_26
          

        subroutine mydgemv_t_ver1_5_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_28
          

        subroutine mydgemv_t_ver1_5_30(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_30')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_30
          

        subroutine mydgemv_t_ver1_5_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_32
          

        subroutine mydgemv_t_ver1_5_34(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_34')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_34
          

        subroutine mydgemv_t_ver1_5_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_36
          

        subroutine mydgemv_t_ver1_5_38(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_38')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_38
          

        subroutine mydgemv_t_ver1_5_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_40
          

        subroutine mydgemv_t_ver1_5_42(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_42')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_42
          

        subroutine mydgemv_t_ver1_5_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_44
          

        subroutine mydgemv_t_ver1_5_46(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_46')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_46
          

        subroutine mydgemv_t_ver1_5_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_48
          

        subroutine mydgemv_t_ver1_5_50(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_50')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_50
          

        subroutine mydgemv_t_ver1_5_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_52
          

        subroutine mydgemv_t_ver1_5_54(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_54')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_54
          

        subroutine mydgemv_t_ver1_5_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_56
          

        subroutine mydgemv_t_ver1_5_58(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_58')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_58
          

        subroutine mydgemv_t_ver1_5_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_60
          

        subroutine mydgemv_t_ver1_5_62(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_62')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_62
          

        subroutine mydgemv_t_ver1_5_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_64
          

        subroutine mydgemv_t_ver1_5_66(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_66')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_66
          

        subroutine mydgemv_t_ver1_5_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_68
          

        subroutine mydgemv_t_ver1_5_70(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_70')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_70
          

        subroutine mydgemv_t_ver1_5_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_72
          

        subroutine mydgemv_t_ver1_5_74(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_74')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_74
          

        subroutine mydgemv_t_ver1_5_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_76
          

        subroutine mydgemv_t_ver1_5_78(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_78')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_78
          

        subroutine mydgemv_t_ver1_5_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_80
          

        subroutine mydgemv_t_ver1_5_82(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_82')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_82
          

        subroutine mydgemv_t_ver1_5_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_84
          

        subroutine mydgemv_t_ver1_5_86(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_86')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_86
          

        subroutine mydgemv_t_ver1_5_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_88
          

        subroutine mydgemv_t_ver1_5_90(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_90')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_90
          

        subroutine mydgemv_t_ver1_5_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_92
          

        subroutine mydgemv_t_ver1_5_94(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_94')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_94
          

        subroutine mydgemv_t_ver1_5_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_96
          

        subroutine mydgemv_t_ver1_5_98(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_98')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_98
          

        subroutine mydgemv_t_ver1_5_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_100
          

        subroutine mydgemv_t_ver1_5_102(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_102')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_102
          

        subroutine mydgemv_t_ver1_5_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_104
          

        subroutine mydgemv_t_ver1_5_106(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_106')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_106
          

        subroutine mydgemv_t_ver1_5_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_108
          

        subroutine mydgemv_t_ver1_5_110(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_110')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_110
          

        subroutine mydgemv_t_ver1_5_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_112
          

        subroutine mydgemv_t_ver1_5_114(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_114')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_114
          

        subroutine mydgemv_t_ver1_5_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_116
          

        subroutine mydgemv_t_ver1_5_118(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_118')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_118
          

        subroutine mydgemv_t_ver1_5_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_120
          

        subroutine mydgemv_t_ver1_5_122(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_122')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_122
          

        subroutine mydgemv_t_ver1_5_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_124
          

        subroutine mydgemv_t_ver1_5_126(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_126')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_126
          

        subroutine mydgemv_t_ver1_5_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_128
          

        subroutine mydgemv_t_ver1_5_130(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_130')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_130
          

        subroutine mydgemv_t_ver1_5_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_132
          

        subroutine mydgemv_t_ver1_5_134(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_134')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_134
          

        subroutine mydgemv_t_ver1_5_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_136
          

        subroutine mydgemv_t_ver1_5_138(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_138')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_138
          

        subroutine mydgemv_t_ver1_5_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_140
          

        subroutine mydgemv_t_ver1_5_142(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_142')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_142
          

        subroutine mydgemv_t_ver1_5_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_144
          

        subroutine mydgemv_t_ver1_5_146(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_146')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_146
          

        subroutine mydgemv_t_ver1_5_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_148
          

        subroutine mydgemv_t_ver1_5_150(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_150')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_150
          

        subroutine mydgemv_t_ver1_5_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_152
          

        subroutine mydgemv_t_ver1_5_154(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_154')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_154
          

        subroutine mydgemv_t_ver1_5_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_156
          

        subroutine mydgemv_t_ver1_5_158(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_158')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_158
          

        subroutine mydgemv_t_ver1_5_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_160
          

        subroutine mydgemv_t_ver1_5_162(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_162')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_162
          

        subroutine mydgemv_t_ver1_5_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_164
          

        subroutine mydgemv_t_ver1_5_166(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_166')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_166
          

        subroutine mydgemv_t_ver1_5_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_168
          

        subroutine mydgemv_t_ver1_5_170(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_170')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_170
          

        subroutine mydgemv_t_ver1_5_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_172
          

        subroutine mydgemv_t_ver1_5_174(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_174')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_174
          

        subroutine mydgemv_t_ver1_5_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_176
          

        subroutine mydgemv_t_ver1_5_178(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_178')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_178
          

        subroutine mydgemv_t_ver1_5_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_180
          

        subroutine mydgemv_t_ver1_5_182(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_182')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_182
          

        subroutine mydgemv_t_ver1_5_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_184
          

        subroutine mydgemv_t_ver1_5_186(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_186')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_186
          

        subroutine mydgemv_t_ver1_5_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_188
          

        subroutine mydgemv_t_ver1_5_190(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_190')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_190
          

        subroutine mydgemv_t_ver1_5_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_192
          

        subroutine mydgemv_t_ver1_5_194(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_194')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_194
          

        subroutine mydgemv_t_ver1_5_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_196
          

        subroutine mydgemv_t_ver1_5_198(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_198')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_198
          

        subroutine mydgemv_t_ver1_5_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_200
          

        subroutine mydgemv_t_ver1_5_202(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_202')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_202
          

        subroutine mydgemv_t_ver1_5_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_204
          

        subroutine mydgemv_t_ver1_5_206(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_206')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_206
          

        subroutine mydgemv_t_ver1_5_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_208
          

        subroutine mydgemv_t_ver1_5_210(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_210')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_210
          

        subroutine mydgemv_t_ver1_5_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_212
          

        subroutine mydgemv_t_ver1_5_214(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_214')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_214
          

        subroutine mydgemv_t_ver1_5_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_216
          

        subroutine mydgemv_t_ver1_5_218(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_218')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_218
          

        subroutine mydgemv_t_ver1_5_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_220
          

        subroutine mydgemv_t_ver1_5_222(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_222')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_222
          

        subroutine mydgemv_t_ver1_5_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_224
          

        subroutine mydgemv_t_ver1_5_226(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_226')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_226
          

        subroutine mydgemv_t_ver1_5_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_228
          

        subroutine mydgemv_t_ver1_5_230(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_230')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_230
          

        subroutine mydgemv_t_ver1_5_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_232
          

        subroutine mydgemv_t_ver1_5_234(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_234')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_234
          

        subroutine mydgemv_t_ver1_5_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_236
          

        subroutine mydgemv_t_ver1_5_238(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_238')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_238
          

        subroutine mydgemv_t_ver1_5_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_240
          

        subroutine mydgemv_t_ver1_5_242(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_242')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_242
          

        subroutine mydgemv_t_ver1_5_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_244
          

        subroutine mydgemv_t_ver1_5_246(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_246')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_246
          

        subroutine mydgemv_t_ver1_5_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_248
          

        subroutine mydgemv_t_ver1_5_250(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_250')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_250
          

        subroutine mydgemv_t_ver1_5_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_252
          

        subroutine mydgemv_t_ver1_5_254(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_254')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_254
          

        subroutine mydgemv_t_ver1_5_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_256
          

        subroutine mydgemv_t_ver1_5_258(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_258')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_258
          

        subroutine mydgemv_t_ver1_5_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_260
          

        subroutine mydgemv_t_ver1_5_262(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_262')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_262
          

        subroutine mydgemv_t_ver1_5_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_264
          

        subroutine mydgemv_t_ver1_5_266(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_266')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_266
          

        subroutine mydgemv_t_ver1_5_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_268
          

        subroutine mydgemv_t_ver1_5_270(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_270')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_270
          

        subroutine mydgemv_t_ver1_5_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_272
          

        subroutine mydgemv_t_ver1_5_274(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_274')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_274
          

        subroutine mydgemv_t_ver1_5_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_276
          

        subroutine mydgemv_t_ver1_5_278(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_278')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_278
          

        subroutine mydgemv_t_ver1_5_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_280
          

        subroutine mydgemv_t_ver1_5_282(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_282')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_282
          

        subroutine mydgemv_t_ver1_5_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_284
          

        subroutine mydgemv_t_ver1_5_286(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_286')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_286
          

        subroutine mydgemv_t_ver1_5_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_288
          

        subroutine mydgemv_t_ver1_5_290(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_290')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_290
          

        subroutine mydgemv_t_ver1_5_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_292
          

        subroutine mydgemv_t_ver1_5_294(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_294')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_294
          

        subroutine mydgemv_t_ver1_5_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_296
          

        subroutine mydgemv_t_ver1_5_298(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_298')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_298
          

        subroutine mydgemv_t_ver1_5_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_300
          

        subroutine mydgemv_t_ver1_5_302(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_302')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_302
          

        subroutine mydgemv_t_ver1_5_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_304
          

        subroutine mydgemv_t_ver1_5_306(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_306')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_306
          

        subroutine mydgemv_t_ver1_5_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_308
          

        subroutine mydgemv_t_ver1_5_310(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_310')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_310
          

        subroutine mydgemv_t_ver1_5_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_312
          

        subroutine mydgemv_t_ver1_5_314(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_314')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_314
          

        subroutine mydgemv_t_ver1_5_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_316
          

        subroutine mydgemv_t_ver1_5_318(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_318')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_318
          

        subroutine mydgemv_t_ver1_5_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_320
          

        subroutine mydgemv_t_ver1_5_322(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_322')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_322
          

        subroutine mydgemv_t_ver1_5_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_324
          

        subroutine mydgemv_t_ver1_5_326(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_326')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_326
          

        subroutine mydgemv_t_ver1_5_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_328
          

        subroutine mydgemv_t_ver1_5_330(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_330')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_330
          

        subroutine mydgemv_t_ver1_5_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_332
          

        subroutine mydgemv_t_ver1_5_334(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_334')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_334
          

        subroutine mydgemv_t_ver1_5_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_336
          

        subroutine mydgemv_t_ver1_5_338(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_338')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_338
          

        subroutine mydgemv_t_ver1_5_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_340
          

        subroutine mydgemv_t_ver1_5_342(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_342')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_342
          

        subroutine mydgemv_t_ver1_5_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_344
          

        subroutine mydgemv_t_ver1_5_346(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_346')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_346
          

        subroutine mydgemv_t_ver1_5_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_348
          

        subroutine mydgemv_t_ver1_5_350(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_350')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_350
          

        subroutine mydgemv_t_ver1_5_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_352
          

        subroutine mydgemv_t_ver1_5_354(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_354')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_354
          

        subroutine mydgemv_t_ver1_5_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_356
          

        subroutine mydgemv_t_ver1_5_358(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_358')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_358
          

        subroutine mydgemv_t_ver1_5_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_360
          

        subroutine mydgemv_t_ver1_5_362(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_362')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_362
          

        subroutine mydgemv_t_ver1_5_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_364
          

        subroutine mydgemv_t_ver1_5_366(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_366')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_366
          

        subroutine mydgemv_t_ver1_5_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_368
          

        subroutine mydgemv_t_ver1_5_370(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_370')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_370
          

        subroutine mydgemv_t_ver1_5_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_372
          

        subroutine mydgemv_t_ver1_5_374(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_374')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_374
          

        subroutine mydgemv_t_ver1_5_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_376
          

        subroutine mydgemv_t_ver1_5_378(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_378')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_378
          

        subroutine mydgemv_t_ver1_5_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_380
          

        subroutine mydgemv_t_ver1_5_382(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_382')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_382
          

        subroutine mydgemv_t_ver1_5_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_384
          

        subroutine mydgemv_t_ver1_5_386(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_386')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_386
          

        subroutine mydgemv_t_ver1_5_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_388
          

        subroutine mydgemv_t_ver1_5_390(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_390')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_390
          

        subroutine mydgemv_t_ver1_5_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_392
          

        subroutine mydgemv_t_ver1_5_394(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_394')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_394
          

        subroutine mydgemv_t_ver1_5_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_396
          

        subroutine mydgemv_t_ver1_5_398(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_398')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_398
          

        subroutine mydgemv_t_ver1_5_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_400
          

        subroutine mydgemv_t_ver1_5_402(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_402')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_402
          

        subroutine mydgemv_t_ver1_5_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_404
          

        subroutine mydgemv_t_ver1_5_406(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_406')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_406
          

        subroutine mydgemv_t_ver1_5_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_408
          

        subroutine mydgemv_t_ver1_5_410(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_410')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_410
          

        subroutine mydgemv_t_ver1_5_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_412
          

        subroutine mydgemv_t_ver1_5_414(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_414')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_414
          

        subroutine mydgemv_t_ver1_5_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_416
          

        subroutine mydgemv_t_ver1_5_418(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_418')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_418
          

        subroutine mydgemv_t_ver1_5_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_420
          

        subroutine mydgemv_t_ver1_5_422(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_422')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_422
          

        subroutine mydgemv_t_ver1_5_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_424
          

        subroutine mydgemv_t_ver1_5_426(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_426')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_426
          

        subroutine mydgemv_t_ver1_5_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_428
          

        subroutine mydgemv_t_ver1_5_430(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_430')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_430
          

        subroutine mydgemv_t_ver1_5_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_432
          

        subroutine mydgemv_t_ver1_5_434(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_434')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_434
          

        subroutine mydgemv_t_ver1_5_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_436
          

        subroutine mydgemv_t_ver1_5_438(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_438')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_438
          

        subroutine mydgemv_t_ver1_5_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_440
          

        subroutine mydgemv_t_ver1_5_442(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_442')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_442
          

        subroutine mydgemv_t_ver1_5_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_444
          

        subroutine mydgemv_t_ver1_5_446(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_446')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_446
          

        subroutine mydgemv_t_ver1_5_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_448
          

        subroutine mydgemv_t_ver1_5_450(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_450')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_450
          

        subroutine mydgemv_t_ver1_5_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_452
          

        subroutine mydgemv_t_ver1_5_454(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_454')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_454
          

        subroutine mydgemv_t_ver1_5_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_456
          

        subroutine mydgemv_t_ver1_5_458(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_458')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_458
          

        subroutine mydgemv_t_ver1_5_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_460
          

        subroutine mydgemv_t_ver1_5_462(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_462')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_462
          

        subroutine mydgemv_t_ver1_5_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_464
          

        subroutine mydgemv_t_ver1_5_466(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_466')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_466
          

        subroutine mydgemv_t_ver1_5_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_468
          

        subroutine mydgemv_t_ver1_5_470(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_470')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_470
          

        subroutine mydgemv_t_ver1_5_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_472
          

        subroutine mydgemv_t_ver1_5_474(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_474')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_474
          

        subroutine mydgemv_t_ver1_5_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_476
          

        subroutine mydgemv_t_ver1_5_478(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_478')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_478
          

        subroutine mydgemv_t_ver1_5_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_480
          

        subroutine mydgemv_t_ver1_5_482(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_482')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_482
          

        subroutine mydgemv_t_ver1_5_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_484
          

        subroutine mydgemv_t_ver1_5_486(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_486')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_486
          

        subroutine mydgemv_t_ver1_5_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_488
          

        subroutine mydgemv_t_ver1_5_490(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_490')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_490
          

        subroutine mydgemv_t_ver1_5_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_492
          

        subroutine mydgemv_t_ver1_5_494(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_494')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_494
          

        subroutine mydgemv_t_ver1_5_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_496
          

        subroutine mydgemv_t_ver1_5_498(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_498')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_498
          

        subroutine mydgemv_t_ver1_5_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_500
          

        subroutine mydgemv_t_ver1_5_502(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_502')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_502
          

        subroutine mydgemv_t_ver1_5_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_504
          

        subroutine mydgemv_t_ver1_5_506(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_506')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_506
          

        subroutine mydgemv_t_ver1_5_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_508
          

        subroutine mydgemv_t_ver1_5_510(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_510')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_510
          

        subroutine mydgemv_t_ver1_5_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver1_5_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver1_5_512        


    
        subroutine mydgemv_n_ver2_0(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_0
    
        subroutine mydgemv_t_ver2_0(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_0



    
        subroutine mydgemv_n_ver2_1_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_1_unroll1
    
        subroutine mydgemv_t_ver2_1_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_1_unroll1
    
        subroutine mydgemv_n_ver2_1_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_1_unroll2
    
        subroutine mydgemv_t_ver2_1_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_1_unroll2
    
        subroutine mydgemv_n_ver2_1_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_1_unroll4
    
        subroutine mydgemv_t_ver2_1_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_1_unroll4
    
        subroutine mydgemv_n_ver2_1_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_1_unroll8
    
        subroutine mydgemv_t_ver2_1_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_1_unroll8

    
        subroutine mydgemv_n_ver2_2_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_2_unroll1
    
        subroutine mydgemv_t_ver2_2_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_2_unroll1
    
        subroutine mydgemv_n_ver2_2_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_2_unroll2
    
        subroutine mydgemv_t_ver2_2_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_2_unroll2
    
        subroutine mydgemv_n_ver2_2_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_2_unroll4
    
        subroutine mydgemv_t_ver2_2_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_2_unroll4
    
        subroutine mydgemv_n_ver2_2_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_2_unroll8
    
        subroutine mydgemv_t_ver2_2_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_2_unroll8





        subroutine mydgemv_t_ver2_4_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_4
          

        subroutine mydgemv_t_ver2_4_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_8
          

        subroutine mydgemv_t_ver2_4_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_12
          

        subroutine mydgemv_t_ver2_4_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_16
          

        subroutine mydgemv_t_ver2_4_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_20
          

        subroutine mydgemv_t_ver2_4_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_24
          

        subroutine mydgemv_t_ver2_4_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_28
          

        subroutine mydgemv_t_ver2_4_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_32
          

        subroutine mydgemv_t_ver2_4_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_36
          

        subroutine mydgemv_t_ver2_4_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_40
          

        subroutine mydgemv_t_ver2_4_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_44
          

        subroutine mydgemv_t_ver2_4_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_48
          

        subroutine mydgemv_t_ver2_4_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_52
          

        subroutine mydgemv_t_ver2_4_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_56
          

        subroutine mydgemv_t_ver2_4_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_60
          

        subroutine mydgemv_t_ver2_4_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_64
          

        subroutine mydgemv_t_ver2_4_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_68
          

        subroutine mydgemv_t_ver2_4_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_72
          

        subroutine mydgemv_t_ver2_4_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_76
          

        subroutine mydgemv_t_ver2_4_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_80
          

        subroutine mydgemv_t_ver2_4_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_84
          

        subroutine mydgemv_t_ver2_4_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_88
          

        subroutine mydgemv_t_ver2_4_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_92
          

        subroutine mydgemv_t_ver2_4_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_96
          

        subroutine mydgemv_t_ver2_4_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_100
          

        subroutine mydgemv_t_ver2_4_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_104
          

        subroutine mydgemv_t_ver2_4_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_108
          

        subroutine mydgemv_t_ver2_4_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_112
          

        subroutine mydgemv_t_ver2_4_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_116
          

        subroutine mydgemv_t_ver2_4_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_120
          

        subroutine mydgemv_t_ver2_4_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_124
          

        subroutine mydgemv_t_ver2_4_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_128
          

        subroutine mydgemv_t_ver2_4_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_132
          

        subroutine mydgemv_t_ver2_4_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_136
          

        subroutine mydgemv_t_ver2_4_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_140
          

        subroutine mydgemv_t_ver2_4_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_144
          

        subroutine mydgemv_t_ver2_4_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_148
          

        subroutine mydgemv_t_ver2_4_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_152
          

        subroutine mydgemv_t_ver2_4_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_156
          

        subroutine mydgemv_t_ver2_4_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_160
          

        subroutine mydgemv_t_ver2_4_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_164
          

        subroutine mydgemv_t_ver2_4_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_168
          

        subroutine mydgemv_t_ver2_4_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_172
          

        subroutine mydgemv_t_ver2_4_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_176
          

        subroutine mydgemv_t_ver2_4_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_180
          

        subroutine mydgemv_t_ver2_4_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_184
          

        subroutine mydgemv_t_ver2_4_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_188
          

        subroutine mydgemv_t_ver2_4_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_192
          

        subroutine mydgemv_t_ver2_4_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_196
          

        subroutine mydgemv_t_ver2_4_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_200
          

        subroutine mydgemv_t_ver2_4_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_204
          

        subroutine mydgemv_t_ver2_4_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_208
          

        subroutine mydgemv_t_ver2_4_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_212
          

        subroutine mydgemv_t_ver2_4_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_216
          

        subroutine mydgemv_t_ver2_4_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_220
          

        subroutine mydgemv_t_ver2_4_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_224
          

        subroutine mydgemv_t_ver2_4_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_228
          

        subroutine mydgemv_t_ver2_4_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_232
          

        subroutine mydgemv_t_ver2_4_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_236
          

        subroutine mydgemv_t_ver2_4_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_240
          

        subroutine mydgemv_t_ver2_4_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_244
          

        subroutine mydgemv_t_ver2_4_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_248
          

        subroutine mydgemv_t_ver2_4_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_252
          

        subroutine mydgemv_t_ver2_4_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_256
          

        subroutine mydgemv_t_ver2_4_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_260
          

        subroutine mydgemv_t_ver2_4_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_264
          

        subroutine mydgemv_t_ver2_4_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_268
          

        subroutine mydgemv_t_ver2_4_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_272
          

        subroutine mydgemv_t_ver2_4_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_276
          

        subroutine mydgemv_t_ver2_4_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_280
          

        subroutine mydgemv_t_ver2_4_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_284
          

        subroutine mydgemv_t_ver2_4_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_288
          

        subroutine mydgemv_t_ver2_4_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_292
          

        subroutine mydgemv_t_ver2_4_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_296
          

        subroutine mydgemv_t_ver2_4_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_300
          

        subroutine mydgemv_t_ver2_4_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_304
          

        subroutine mydgemv_t_ver2_4_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_308
          

        subroutine mydgemv_t_ver2_4_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_312
          

        subroutine mydgemv_t_ver2_4_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_316
          

        subroutine mydgemv_t_ver2_4_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_320
          

        subroutine mydgemv_t_ver2_4_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_324
          

        subroutine mydgemv_t_ver2_4_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_328
          

        subroutine mydgemv_t_ver2_4_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_332
          

        subroutine mydgemv_t_ver2_4_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_336
          

        subroutine mydgemv_t_ver2_4_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_340
          

        subroutine mydgemv_t_ver2_4_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_344
          

        subroutine mydgemv_t_ver2_4_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_348
          

        subroutine mydgemv_t_ver2_4_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_352
          

        subroutine mydgemv_t_ver2_4_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_356
          

        subroutine mydgemv_t_ver2_4_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_360
          

        subroutine mydgemv_t_ver2_4_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_364
          

        subroutine mydgemv_t_ver2_4_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_368
          

        subroutine mydgemv_t_ver2_4_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_372
          

        subroutine mydgemv_t_ver2_4_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_376
          

        subroutine mydgemv_t_ver2_4_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_380
          

        subroutine mydgemv_t_ver2_4_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_384
          

        subroutine mydgemv_t_ver2_4_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_388
          

        subroutine mydgemv_t_ver2_4_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_392
          

        subroutine mydgemv_t_ver2_4_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_396
          

        subroutine mydgemv_t_ver2_4_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_400
          

        subroutine mydgemv_t_ver2_4_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_404
          

        subroutine mydgemv_t_ver2_4_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_408
          

        subroutine mydgemv_t_ver2_4_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_412
          

        subroutine mydgemv_t_ver2_4_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_416
          

        subroutine mydgemv_t_ver2_4_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_420
          

        subroutine mydgemv_t_ver2_4_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_424
          

        subroutine mydgemv_t_ver2_4_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_428
          

        subroutine mydgemv_t_ver2_4_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_432
          

        subroutine mydgemv_t_ver2_4_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_436
          

        subroutine mydgemv_t_ver2_4_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_440
          

        subroutine mydgemv_t_ver2_4_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_444
          

        subroutine mydgemv_t_ver2_4_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_448
          

        subroutine mydgemv_t_ver2_4_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_452
          

        subroutine mydgemv_t_ver2_4_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_456
          

        subroutine mydgemv_t_ver2_4_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_460
          

        subroutine mydgemv_t_ver2_4_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_464
          

        subroutine mydgemv_t_ver2_4_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_468
          

        subroutine mydgemv_t_ver2_4_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_472
          

        subroutine mydgemv_t_ver2_4_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_476
          

        subroutine mydgemv_t_ver2_4_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_480
          

        subroutine mydgemv_t_ver2_4_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_484
          

        subroutine mydgemv_t_ver2_4_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_488
          

        subroutine mydgemv_t_ver2_4_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_492
          

        subroutine mydgemv_t_ver2_4_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_496
          

        subroutine mydgemv_t_ver2_4_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_500
          

        subroutine mydgemv_t_ver2_4_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_504
          

        subroutine mydgemv_t_ver2_4_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_508
          

        subroutine mydgemv_t_ver2_4_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_4_512

        subroutine mydgemv_t_ver2_5_4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_4
          

        subroutine mydgemv_t_ver2_5_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_8
          

        subroutine mydgemv_t_ver2_5_12(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_12
          

        subroutine mydgemv_t_ver2_5_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_16
          

        subroutine mydgemv_t_ver2_5_20(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_20
          

        subroutine mydgemv_t_ver2_5_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_24
          

        subroutine mydgemv_t_ver2_5_28(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_28
          

        subroutine mydgemv_t_ver2_5_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_32
          

        subroutine mydgemv_t_ver2_5_36(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_36
          

        subroutine mydgemv_t_ver2_5_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_40
          

        subroutine mydgemv_t_ver2_5_44(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_44
          

        subroutine mydgemv_t_ver2_5_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_48
          

        subroutine mydgemv_t_ver2_5_52(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_52
          

        subroutine mydgemv_t_ver2_5_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_56
          

        subroutine mydgemv_t_ver2_5_60(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_60
          

        subroutine mydgemv_t_ver2_5_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_64
          

        subroutine mydgemv_t_ver2_5_68(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_68
          

        subroutine mydgemv_t_ver2_5_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_72
          

        subroutine mydgemv_t_ver2_5_76(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_76
          

        subroutine mydgemv_t_ver2_5_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_80
          

        subroutine mydgemv_t_ver2_5_84(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_84
          

        subroutine mydgemv_t_ver2_5_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_88
          

        subroutine mydgemv_t_ver2_5_92(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_92
          

        subroutine mydgemv_t_ver2_5_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_96
          

        subroutine mydgemv_t_ver2_5_100(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_100
          

        subroutine mydgemv_t_ver2_5_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_104
          

        subroutine mydgemv_t_ver2_5_108(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_108
          

        subroutine mydgemv_t_ver2_5_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_112
          

        subroutine mydgemv_t_ver2_5_116(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_116
          

        subroutine mydgemv_t_ver2_5_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_120
          

        subroutine mydgemv_t_ver2_5_124(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_124
          

        subroutine mydgemv_t_ver2_5_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_128
          

        subroutine mydgemv_t_ver2_5_132(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_132
          

        subroutine mydgemv_t_ver2_5_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_136
          

        subroutine mydgemv_t_ver2_5_140(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_140
          

        subroutine mydgemv_t_ver2_5_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_144
          

        subroutine mydgemv_t_ver2_5_148(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_148
          

        subroutine mydgemv_t_ver2_5_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_152
          

        subroutine mydgemv_t_ver2_5_156(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_156
          

        subroutine mydgemv_t_ver2_5_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_160
          

        subroutine mydgemv_t_ver2_5_164(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_164
          

        subroutine mydgemv_t_ver2_5_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_168
          

        subroutine mydgemv_t_ver2_5_172(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_172
          

        subroutine mydgemv_t_ver2_5_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_176
          

        subroutine mydgemv_t_ver2_5_180(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_180
          

        subroutine mydgemv_t_ver2_5_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_184
          

        subroutine mydgemv_t_ver2_5_188(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_188
          

        subroutine mydgemv_t_ver2_5_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_192
          

        subroutine mydgemv_t_ver2_5_196(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_196
          

        subroutine mydgemv_t_ver2_5_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_200
          

        subroutine mydgemv_t_ver2_5_204(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_204
          

        subroutine mydgemv_t_ver2_5_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_208
          

        subroutine mydgemv_t_ver2_5_212(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_212
          

        subroutine mydgemv_t_ver2_5_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_216
          

        subroutine mydgemv_t_ver2_5_220(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_220
          

        subroutine mydgemv_t_ver2_5_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_224
          

        subroutine mydgemv_t_ver2_5_228(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_228
          

        subroutine mydgemv_t_ver2_5_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_232
          

        subroutine mydgemv_t_ver2_5_236(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_236
          

        subroutine mydgemv_t_ver2_5_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_240
          

        subroutine mydgemv_t_ver2_5_244(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_244
          

        subroutine mydgemv_t_ver2_5_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_248
          

        subroutine mydgemv_t_ver2_5_252(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_252
          

        subroutine mydgemv_t_ver2_5_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_256
          

        subroutine mydgemv_t_ver2_5_260(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_260
          

        subroutine mydgemv_t_ver2_5_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_264
          

        subroutine mydgemv_t_ver2_5_268(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_268
          

        subroutine mydgemv_t_ver2_5_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_272
          

        subroutine mydgemv_t_ver2_5_276(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_276
          

        subroutine mydgemv_t_ver2_5_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_280
          

        subroutine mydgemv_t_ver2_5_284(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_284
          

        subroutine mydgemv_t_ver2_5_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_288
          

        subroutine mydgemv_t_ver2_5_292(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_292
          

        subroutine mydgemv_t_ver2_5_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_296
          

        subroutine mydgemv_t_ver2_5_300(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_300
          

        subroutine mydgemv_t_ver2_5_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_304
          

        subroutine mydgemv_t_ver2_5_308(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_308
          

        subroutine mydgemv_t_ver2_5_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_312
          

        subroutine mydgemv_t_ver2_5_316(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_316
          

        subroutine mydgemv_t_ver2_5_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_320
          

        subroutine mydgemv_t_ver2_5_324(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_324
          

        subroutine mydgemv_t_ver2_5_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_328
          

        subroutine mydgemv_t_ver2_5_332(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_332
          

        subroutine mydgemv_t_ver2_5_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_336
          

        subroutine mydgemv_t_ver2_5_340(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_340
          

        subroutine mydgemv_t_ver2_5_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_344
          

        subroutine mydgemv_t_ver2_5_348(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_348
          

        subroutine mydgemv_t_ver2_5_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_352
          

        subroutine mydgemv_t_ver2_5_356(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_356
          

        subroutine mydgemv_t_ver2_5_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_360
          

        subroutine mydgemv_t_ver2_5_364(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_364
          

        subroutine mydgemv_t_ver2_5_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_368
          

        subroutine mydgemv_t_ver2_5_372(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_372
          

        subroutine mydgemv_t_ver2_5_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_376
          

        subroutine mydgemv_t_ver2_5_380(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_380
          

        subroutine mydgemv_t_ver2_5_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_384
          

        subroutine mydgemv_t_ver2_5_388(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_388
          

        subroutine mydgemv_t_ver2_5_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_392
          

        subroutine mydgemv_t_ver2_5_396(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_396
          

        subroutine mydgemv_t_ver2_5_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_400
          

        subroutine mydgemv_t_ver2_5_404(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_404
          

        subroutine mydgemv_t_ver2_5_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_408
          

        subroutine mydgemv_t_ver2_5_412(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_412
          

        subroutine mydgemv_t_ver2_5_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_416
          

        subroutine mydgemv_t_ver2_5_420(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_420
          

        subroutine mydgemv_t_ver2_5_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_424
          

        subroutine mydgemv_t_ver2_5_428(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_428
          

        subroutine mydgemv_t_ver2_5_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_432
          

        subroutine mydgemv_t_ver2_5_436(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_436
          

        subroutine mydgemv_t_ver2_5_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_440
          

        subroutine mydgemv_t_ver2_5_444(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_444
          

        subroutine mydgemv_t_ver2_5_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_448
          

        subroutine mydgemv_t_ver2_5_452(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_452
          

        subroutine mydgemv_t_ver2_5_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_456
          

        subroutine mydgemv_t_ver2_5_460(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_460
          

        subroutine mydgemv_t_ver2_5_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_464
          

        subroutine mydgemv_t_ver2_5_468(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_468
          

        subroutine mydgemv_t_ver2_5_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_472
          

        subroutine mydgemv_t_ver2_5_476(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_476
          

        subroutine mydgemv_t_ver2_5_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_480
          

        subroutine mydgemv_t_ver2_5_484(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_484
          

        subroutine mydgemv_t_ver2_5_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_488
          

        subroutine mydgemv_t_ver2_5_492(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_492
          

        subroutine mydgemv_t_ver2_5_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_496
          

        subroutine mydgemv_t_ver2_5_500(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_500
          

        subroutine mydgemv_t_ver2_5_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_504
          

        subroutine mydgemv_t_ver2_5_508(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_508
          

        subroutine mydgemv_t_ver2_5_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver2_5_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver2_5_512



        subroutine mydgemv_n_ver3_0(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_0
    
        subroutine mydgemv_t_ver3_0(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_0')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_0

    
        subroutine mydgemv_n_ver3_1_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_1_unroll1
    
        subroutine mydgemv_t_ver3_1_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_1_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_1_unroll1
    
        subroutine mydgemv_n_ver3_1_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_1_unroll2
    
        subroutine mydgemv_t_ver3_1_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_1_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_1_unroll2
    
        subroutine mydgemv_n_ver3_1_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_1_unroll4
    
        subroutine mydgemv_t_ver3_1_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_1_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_1_unroll4
    
        subroutine mydgemv_n_ver3_1_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_1_unroll8
    
        subroutine mydgemv_t_ver3_1_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_1_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_1_unroll8

    
        subroutine mydgemv_n_ver3_2_unroll1(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_2_unroll1
    
        subroutine mydgemv_t_ver3_2_unroll1(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_2_unroll1')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_2_unroll1
    
        subroutine mydgemv_n_ver3_2_unroll2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_2_unroll2
    
        subroutine mydgemv_t_ver3_2_unroll2(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_2_unroll2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_2_unroll2
    
        subroutine mydgemv_n_ver3_2_unroll4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_2_unroll4
    
        subroutine mydgemv_t_ver3_2_unroll4(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_2_unroll4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_2_unroll4
    
        subroutine mydgemv_n_ver3_2_unroll8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_2_unroll8
    
        subroutine mydgemv_t_ver3_2_unroll8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_2_unroll8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_2_unroll8


        subroutine mydgemv_t_ver3_4_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_8
          

        subroutine mydgemv_t_ver3_4_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_16
          

        subroutine mydgemv_t_ver3_4_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_24
          

        subroutine mydgemv_t_ver3_4_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_32
          

        subroutine mydgemv_t_ver3_4_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_40
          

        subroutine mydgemv_t_ver3_4_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_48
          

        subroutine mydgemv_t_ver3_4_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_56
          

        subroutine mydgemv_t_ver3_4_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_64
          

        subroutine mydgemv_t_ver3_4_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_72
          

        subroutine mydgemv_t_ver3_4_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_80
          

        subroutine mydgemv_t_ver3_4_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_88
          

        subroutine mydgemv_t_ver3_4_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_96
          

        subroutine mydgemv_t_ver3_4_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_104
          

        subroutine mydgemv_t_ver3_4_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_112
          

        subroutine mydgemv_t_ver3_4_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_120
          

        subroutine mydgemv_t_ver3_4_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_128
          

        subroutine mydgemv_t_ver3_4_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_136
          

        subroutine mydgemv_t_ver3_4_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_144
          

        subroutine mydgemv_t_ver3_4_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_152
          

        subroutine mydgemv_t_ver3_4_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_160
          

        subroutine mydgemv_t_ver3_4_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_168
          

        subroutine mydgemv_t_ver3_4_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_176
          

        subroutine mydgemv_t_ver3_4_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_184
          

        subroutine mydgemv_t_ver3_4_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_192
          

        subroutine mydgemv_t_ver3_4_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_200
          

        subroutine mydgemv_t_ver3_4_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_208
          

        subroutine mydgemv_t_ver3_4_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_216
          

        subroutine mydgemv_t_ver3_4_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_224
          

        subroutine mydgemv_t_ver3_4_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_232
          

        subroutine mydgemv_t_ver3_4_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_240
          

        subroutine mydgemv_t_ver3_4_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_248
          

        subroutine mydgemv_t_ver3_4_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_256
          

        subroutine mydgemv_t_ver3_4_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_264
          

        subroutine mydgemv_t_ver3_4_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_272
          

        subroutine mydgemv_t_ver3_4_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_280
          

        subroutine mydgemv_t_ver3_4_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_288
          

        subroutine mydgemv_t_ver3_4_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_296
          

        subroutine mydgemv_t_ver3_4_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_304
          

        subroutine mydgemv_t_ver3_4_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_312
          

        subroutine mydgemv_t_ver3_4_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_320
          

        subroutine mydgemv_t_ver3_4_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_328
          

        subroutine mydgemv_t_ver3_4_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_336
          

        subroutine mydgemv_t_ver3_4_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_344
          

        subroutine mydgemv_t_ver3_4_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_352
          

        subroutine mydgemv_t_ver3_4_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_360
          

        subroutine mydgemv_t_ver3_4_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_368
          

        subroutine mydgemv_t_ver3_4_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_376
          

        subroutine mydgemv_t_ver3_4_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_384
          

        subroutine mydgemv_t_ver3_4_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_392
          

        subroutine mydgemv_t_ver3_4_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_400
          

        subroutine mydgemv_t_ver3_4_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_408
          

        subroutine mydgemv_t_ver3_4_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_416
          

        subroutine mydgemv_t_ver3_4_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_424
          

        subroutine mydgemv_t_ver3_4_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_432
          

        subroutine mydgemv_t_ver3_4_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_440
          

        subroutine mydgemv_t_ver3_4_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_448
          

        subroutine mydgemv_t_ver3_4_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_456
          

        subroutine mydgemv_t_ver3_4_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_464
          

        subroutine mydgemv_t_ver3_4_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_472
          

        subroutine mydgemv_t_ver3_4_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_480
          

        subroutine mydgemv_t_ver3_4_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_488
          

        subroutine mydgemv_t_ver3_4_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_496
          

        subroutine mydgemv_t_ver3_4_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_504
          

        subroutine mydgemv_t_ver3_4_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_4_512


        subroutine mydgemv_n_ver3_4_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_8
          

        subroutine mydgemv_n_ver3_4_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_16
          

        subroutine mydgemv_n_ver3_4_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_24
          

        subroutine mydgemv_n_ver3_4_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_32
          

        subroutine mydgemv_n_ver3_4_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_40
          

        subroutine mydgemv_n_ver3_4_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_48
          

        subroutine mydgemv_n_ver3_4_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_56
          

        subroutine mydgemv_n_ver3_4_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_64
          

        subroutine mydgemv_n_ver3_4_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_72
          

        subroutine mydgemv_n_ver3_4_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_80
          

        subroutine mydgemv_n_ver3_4_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_88
          

        subroutine mydgemv_n_ver3_4_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_96
          

        subroutine mydgemv_n_ver3_4_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_104
          

        subroutine mydgemv_n_ver3_4_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_112
          

        subroutine mydgemv_n_ver3_4_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_120
          

        subroutine mydgemv_n_ver3_4_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_128
          

        subroutine mydgemv_n_ver3_4_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_136
          

        subroutine mydgemv_n_ver3_4_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_144
          

        subroutine mydgemv_n_ver3_4_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_152
          

        subroutine mydgemv_n_ver3_4_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_160
          

        subroutine mydgemv_n_ver3_4_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_168
          

        subroutine mydgemv_n_ver3_4_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_176
          

        subroutine mydgemv_n_ver3_4_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_184
          

        subroutine mydgemv_n_ver3_4_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_192
          

        subroutine mydgemv_n_ver3_4_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_200
          

        subroutine mydgemv_n_ver3_4_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_208
          

        subroutine mydgemv_n_ver3_4_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_216
          

        subroutine mydgemv_n_ver3_4_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_224
          

        subroutine mydgemv_n_ver3_4_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_232
          

        subroutine mydgemv_n_ver3_4_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_240
          

        subroutine mydgemv_n_ver3_4_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_248
          

        subroutine mydgemv_n_ver3_4_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_256
          

        subroutine mydgemv_n_ver3_4_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_264
          

        subroutine mydgemv_n_ver3_4_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_272
          

        subroutine mydgemv_n_ver3_4_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_280
          

        subroutine mydgemv_n_ver3_4_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_288
          

        subroutine mydgemv_n_ver3_4_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_296
          

        subroutine mydgemv_n_ver3_4_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_304
          

        subroutine mydgemv_n_ver3_4_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_312
          

        subroutine mydgemv_n_ver3_4_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_320
          

        subroutine mydgemv_n_ver3_4_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_328
          

        subroutine mydgemv_n_ver3_4_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_336
          

        subroutine mydgemv_n_ver3_4_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_344
          

        subroutine mydgemv_n_ver3_4_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_352
          

        subroutine mydgemv_n_ver3_4_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_360
          

        subroutine mydgemv_n_ver3_4_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_368
          

        subroutine mydgemv_n_ver3_4_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_376
          

        subroutine mydgemv_n_ver3_4_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_384
          

        subroutine mydgemv_n_ver3_4_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_392
          

        subroutine mydgemv_n_ver3_4_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_400
          

        subroutine mydgemv_n_ver3_4_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_408
          

        subroutine mydgemv_n_ver3_4_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_416
          

        subroutine mydgemv_n_ver3_4_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_424
          

        subroutine mydgemv_n_ver3_4_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_432
          

        subroutine mydgemv_n_ver3_4_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_440
          

        subroutine mydgemv_n_ver3_4_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_448
          

        subroutine mydgemv_n_ver3_4_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_456
          

        subroutine mydgemv_n_ver3_4_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_464
          

        subroutine mydgemv_n_ver3_4_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_472
          

        subroutine mydgemv_n_ver3_4_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_480
          

        subroutine mydgemv_n_ver3_4_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_488
          

        subroutine mydgemv_n_ver3_4_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_496
          

        subroutine mydgemv_n_ver3_4_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_504
          

        subroutine mydgemv_n_ver3_4_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver3_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver3_4_512
        
        


        subroutine mydgemv_t_ver3_5_8(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_8
          

        subroutine mydgemv_t_ver3_5_16(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_16
          

        subroutine mydgemv_t_ver3_5_24(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_24
          

        subroutine mydgemv_t_ver3_5_32(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_32
          

        subroutine mydgemv_t_ver3_5_40(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_40
          

        subroutine mydgemv_t_ver3_5_48(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_48
          

        subroutine mydgemv_t_ver3_5_56(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_56
          

        subroutine mydgemv_t_ver3_5_64(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_64
          

        subroutine mydgemv_t_ver3_5_72(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_72
          

        subroutine mydgemv_t_ver3_5_80(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_80
          

        subroutine mydgemv_t_ver3_5_88(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_88
          

        subroutine mydgemv_t_ver3_5_96(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_96
          

        subroutine mydgemv_t_ver3_5_104(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_104
          

        subroutine mydgemv_t_ver3_5_112(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_112
          

        subroutine mydgemv_t_ver3_5_120(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_120
          

        subroutine mydgemv_t_ver3_5_128(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_128
          

        subroutine mydgemv_t_ver3_5_136(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_136
          

        subroutine mydgemv_t_ver3_5_144(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_144
          

        subroutine mydgemv_t_ver3_5_152(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_152
          

        subroutine mydgemv_t_ver3_5_160(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_160
          

        subroutine mydgemv_t_ver3_5_168(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_168
          

        subroutine mydgemv_t_ver3_5_176(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_176
          

        subroutine mydgemv_t_ver3_5_184(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_184
          

        subroutine mydgemv_t_ver3_5_192(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_192
          

        subroutine mydgemv_t_ver3_5_200(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_200
          

        subroutine mydgemv_t_ver3_5_208(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_208
          

        subroutine mydgemv_t_ver3_5_216(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_216
          

        subroutine mydgemv_t_ver3_5_224(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_224
          

        subroutine mydgemv_t_ver3_5_232(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_232
          

        subroutine mydgemv_t_ver3_5_240(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_240
          

        subroutine mydgemv_t_ver3_5_248(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_248
          

        subroutine mydgemv_t_ver3_5_256(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_256
          

        subroutine mydgemv_t_ver3_5_264(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_264
          

        subroutine mydgemv_t_ver3_5_272(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_272
          

        subroutine mydgemv_t_ver3_5_280(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_280
          

        subroutine mydgemv_t_ver3_5_288(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_288
          

        subroutine mydgemv_t_ver3_5_296(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_296
          

        subroutine mydgemv_t_ver3_5_304(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_304
          

        subroutine mydgemv_t_ver3_5_312(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_312
          

        subroutine mydgemv_t_ver3_5_320(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_320
          

        subroutine mydgemv_t_ver3_5_328(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_328
          

        subroutine mydgemv_t_ver3_5_336(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_336
          

        subroutine mydgemv_t_ver3_5_344(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_344
          

        subroutine mydgemv_t_ver3_5_352(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_352
          

        subroutine mydgemv_t_ver3_5_360(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_360
          

        subroutine mydgemv_t_ver3_5_368(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_368
          

        subroutine mydgemv_t_ver3_5_376(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_376
          

        subroutine mydgemv_t_ver3_5_384(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_384
          

        subroutine mydgemv_t_ver3_5_392(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_392
          

        subroutine mydgemv_t_ver3_5_400(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_400
          

        subroutine mydgemv_t_ver3_5_408(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_408
          

        subroutine mydgemv_t_ver3_5_416(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_416
          

        subroutine mydgemv_t_ver3_5_424(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_424
          

        subroutine mydgemv_t_ver3_5_432(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_432
          

        subroutine mydgemv_t_ver3_5_440(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_440
          

        subroutine mydgemv_t_ver3_5_448(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_448
          

        subroutine mydgemv_t_ver3_5_456(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_456
          

        subroutine mydgemv_t_ver3_5_464(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_464
          

        subroutine mydgemv_t_ver3_5_472(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_472
          

        subroutine mydgemv_t_ver3_5_480(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_480
          

        subroutine mydgemv_t_ver3_5_488(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_488
          

        subroutine mydgemv_t_ver3_5_496(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_496
          

        subroutine mydgemv_t_ver3_5_504(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_504
          

        subroutine mydgemv_t_ver3_5_512(a_t, x, y, lda, ldx, ldy) bind(C, name='mydgemv_t_ver3_5_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a_t(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_t_ver3_5_512        





        subroutine mydgemv_n_ver1_4_2(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_2')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_2
          

        subroutine mydgemv_n_ver1_4_4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_4
          

        subroutine mydgemv_n_ver1_4_6(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_6')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_6
          

        subroutine mydgemv_n_ver1_4_8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(ldx, lda), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_8
          

        subroutine mydgemv_n_ver1_4_10(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_10')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_10
          

        subroutine mydgemv_n_ver1_4_12(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_12
          

        subroutine mydgemv_n_ver1_4_14(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_14')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_14
          

        subroutine mydgemv_n_ver1_4_16(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_16
          

        subroutine mydgemv_n_ver1_4_18(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_18')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_18
          

        subroutine mydgemv_n_ver1_4_20(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_20
          

        subroutine mydgemv_n_ver1_4_22(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_22')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_22
          

        subroutine mydgemv_n_ver1_4_24(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_24
          

        subroutine mydgemv_n_ver1_4_26(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_26')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_26
          

        subroutine mydgemv_n_ver1_4_28(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_28
          

        subroutine mydgemv_n_ver1_4_30(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_30')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_30
          

        subroutine mydgemv_n_ver1_4_32(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_32
          

        subroutine mydgemv_n_ver1_4_34(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_34')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_34
          

        subroutine mydgemv_n_ver1_4_36(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_36
          

        subroutine mydgemv_n_ver1_4_38(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_38')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_38
          

        subroutine mydgemv_n_ver1_4_40(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_40
          

        subroutine mydgemv_n_ver1_4_42(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_42')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_42
          

        subroutine mydgemv_n_ver1_4_44(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_44
          

        subroutine mydgemv_n_ver1_4_46(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_46')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_46
          

        subroutine mydgemv_n_ver1_4_48(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_48
          

        subroutine mydgemv_n_ver1_4_50(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_50')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_50
          

        subroutine mydgemv_n_ver1_4_52(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_52
          

        subroutine mydgemv_n_ver1_4_54(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_54')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_54
          

        subroutine mydgemv_n_ver1_4_56(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_56
          

        subroutine mydgemv_n_ver1_4_58(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_58')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_58
          

        subroutine mydgemv_n_ver1_4_60(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_60
          

        subroutine mydgemv_n_ver1_4_62(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_62')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_62
          

        subroutine mydgemv_n_ver1_4_64(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_64
          

        subroutine mydgemv_n_ver1_4_66(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_66')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_66
          

        subroutine mydgemv_n_ver1_4_68(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_68
          

        subroutine mydgemv_n_ver1_4_70(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_70')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_70
          

        subroutine mydgemv_n_ver1_4_72(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_72
          

        subroutine mydgemv_n_ver1_4_74(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_74')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_74
          

        subroutine mydgemv_n_ver1_4_76(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_76
          

        subroutine mydgemv_n_ver1_4_78(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_78')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_78
          

        subroutine mydgemv_n_ver1_4_80(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_80
          

        subroutine mydgemv_n_ver1_4_82(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_82')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_82
          

        subroutine mydgemv_n_ver1_4_84(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_84
          

        subroutine mydgemv_n_ver1_4_86(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_86')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_86
          

        subroutine mydgemv_n_ver1_4_88(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_88
          

        subroutine mydgemv_n_ver1_4_90(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_90')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_90
          

        subroutine mydgemv_n_ver1_4_92(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_92
          

        subroutine mydgemv_n_ver1_4_94(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_94')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_94
          

        subroutine mydgemv_n_ver1_4_96(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_96
          

        subroutine mydgemv_n_ver1_4_98(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_98')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_98
          

        subroutine mydgemv_n_ver1_4_100(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_100
          

        subroutine mydgemv_n_ver1_4_102(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_102')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_102
          

        subroutine mydgemv_n_ver1_4_104(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_104
          

        subroutine mydgemv_n_ver1_4_106(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_106')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_106
          

        subroutine mydgemv_n_ver1_4_108(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_108
          

        subroutine mydgemv_n_ver1_4_110(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_110')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_110
          

        subroutine mydgemv_n_ver1_4_112(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_112
          

        subroutine mydgemv_n_ver1_4_114(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_114')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_114
          

        subroutine mydgemv_n_ver1_4_116(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_116
          

        subroutine mydgemv_n_ver1_4_118(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_118')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_118
          

        subroutine mydgemv_n_ver1_4_120(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_120
          

        subroutine mydgemv_n_ver1_4_122(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_122')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_122
          

        subroutine mydgemv_n_ver1_4_124(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_124
          

        subroutine mydgemv_n_ver1_4_126(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_126')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_126
          

        subroutine mydgemv_n_ver1_4_128(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_128
          

        subroutine mydgemv_n_ver1_4_130(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_130')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_130
          

        subroutine mydgemv_n_ver1_4_132(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_132
          

        subroutine mydgemv_n_ver1_4_134(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_134')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_134
          

        subroutine mydgemv_n_ver1_4_136(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_136
          

        subroutine mydgemv_n_ver1_4_138(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_138')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_138
          

        subroutine mydgemv_n_ver1_4_140(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_140
          

        subroutine mydgemv_n_ver1_4_142(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_142')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_142
          

        subroutine mydgemv_n_ver1_4_144(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_144
          

        subroutine mydgemv_n_ver1_4_146(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_146')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_146
          

        subroutine mydgemv_n_ver1_4_148(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_148
          

        subroutine mydgemv_n_ver1_4_150(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_150')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_150
          

        subroutine mydgemv_n_ver1_4_152(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_152
          

        subroutine mydgemv_n_ver1_4_154(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_154')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_154
          

        subroutine mydgemv_n_ver1_4_156(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_156
          

        subroutine mydgemv_n_ver1_4_158(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_158')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_158
          

        subroutine mydgemv_n_ver1_4_160(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_160
          

        subroutine mydgemv_n_ver1_4_162(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_162')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_162
          

        subroutine mydgemv_n_ver1_4_164(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_164
          

        subroutine mydgemv_n_ver1_4_166(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_166')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_166
          

        subroutine mydgemv_n_ver1_4_168(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_168
          

        subroutine mydgemv_n_ver1_4_170(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_170')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_170
          

        subroutine mydgemv_n_ver1_4_172(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_172
          

        subroutine mydgemv_n_ver1_4_174(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_174')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_174
          

        subroutine mydgemv_n_ver1_4_176(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_176
          

        subroutine mydgemv_n_ver1_4_178(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_178')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_178
          

        subroutine mydgemv_n_ver1_4_180(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_180
          

        subroutine mydgemv_n_ver1_4_182(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_182')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_182
          

        subroutine mydgemv_n_ver1_4_184(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_184
          

        subroutine mydgemv_n_ver1_4_186(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_186')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_186
          

        subroutine mydgemv_n_ver1_4_188(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_188
          

        subroutine mydgemv_n_ver1_4_190(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_190')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_190
          

        subroutine mydgemv_n_ver1_4_192(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_192
          

        subroutine mydgemv_n_ver1_4_194(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_194')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_194
          

        subroutine mydgemv_n_ver1_4_196(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_196
          

        subroutine mydgemv_n_ver1_4_198(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_198')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_198
          

        subroutine mydgemv_n_ver1_4_200(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_200
          

        subroutine mydgemv_n_ver1_4_202(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_202')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_202
          

        subroutine mydgemv_n_ver1_4_204(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_204
          

        subroutine mydgemv_n_ver1_4_206(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_206')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_206
          

        subroutine mydgemv_n_ver1_4_208(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_208
          

        subroutine mydgemv_n_ver1_4_210(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_210')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_210
          

        subroutine mydgemv_n_ver1_4_212(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_212
          

        subroutine mydgemv_n_ver1_4_214(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_214')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_214
          

        subroutine mydgemv_n_ver1_4_216(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_216
          

        subroutine mydgemv_n_ver1_4_218(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_218')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_218
          

        subroutine mydgemv_n_ver1_4_220(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_220
          

        subroutine mydgemv_n_ver1_4_222(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_222')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_222
          

        subroutine mydgemv_n_ver1_4_224(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_224
          

        subroutine mydgemv_n_ver1_4_226(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_226')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_226
          

        subroutine mydgemv_n_ver1_4_228(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_228
          

        subroutine mydgemv_n_ver1_4_230(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_230')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_230
          

        subroutine mydgemv_n_ver1_4_232(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_232
          

        subroutine mydgemv_n_ver1_4_234(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_234')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_234
          

        subroutine mydgemv_n_ver1_4_236(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_236
          

        subroutine mydgemv_n_ver1_4_238(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_238')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_238
          

        subroutine mydgemv_n_ver1_4_240(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_240
          

        subroutine mydgemv_n_ver1_4_242(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_242')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_242
          

        subroutine mydgemv_n_ver1_4_244(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_244
          

        subroutine mydgemv_n_ver1_4_246(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_246')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_246
          

        subroutine mydgemv_n_ver1_4_248(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_248
          

        subroutine mydgemv_n_ver1_4_250(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_250')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_250
          

        subroutine mydgemv_n_ver1_4_252(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_252
          

        subroutine mydgemv_n_ver1_4_254(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_254')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_254
          

        subroutine mydgemv_n_ver1_4_256(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_256
          

        subroutine mydgemv_n_ver1_4_258(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_258')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_258
          

        subroutine mydgemv_n_ver1_4_260(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_260
          

        subroutine mydgemv_n_ver1_4_262(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_262')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_262
          

        subroutine mydgemv_n_ver1_4_264(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_264
          

        subroutine mydgemv_n_ver1_4_266(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_266')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_266
          

        subroutine mydgemv_n_ver1_4_268(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_268
          

        subroutine mydgemv_n_ver1_4_270(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_270')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_270
          

        subroutine mydgemv_n_ver1_4_272(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_272
          

        subroutine mydgemv_n_ver1_4_274(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_274')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_274
          

        subroutine mydgemv_n_ver1_4_276(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_276
          

        subroutine mydgemv_n_ver1_4_278(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_278')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_278
          

        subroutine mydgemv_n_ver1_4_280(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_280
          

        subroutine mydgemv_n_ver1_4_282(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_282')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_282
          

        subroutine mydgemv_n_ver1_4_284(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_284
          

        subroutine mydgemv_n_ver1_4_286(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_286')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_286
          

        subroutine mydgemv_n_ver1_4_288(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_288
          

        subroutine mydgemv_n_ver1_4_290(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_290')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_290
          

        subroutine mydgemv_n_ver1_4_292(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_292
          

        subroutine mydgemv_n_ver1_4_294(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_294')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_294
          

        subroutine mydgemv_n_ver1_4_296(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_296
          

        subroutine mydgemv_n_ver1_4_298(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_298')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_298
          

        subroutine mydgemv_n_ver1_4_300(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_300
          

        subroutine mydgemv_n_ver1_4_302(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_302')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_302
          

        subroutine mydgemv_n_ver1_4_304(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_304
          

        subroutine mydgemv_n_ver1_4_306(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_306')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_306
          

        subroutine mydgemv_n_ver1_4_308(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_308
          

        subroutine mydgemv_n_ver1_4_310(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_310')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_310
          

        subroutine mydgemv_n_ver1_4_312(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_312
          

        subroutine mydgemv_n_ver1_4_314(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_314')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_314
          

        subroutine mydgemv_n_ver1_4_316(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_316
          

        subroutine mydgemv_n_ver1_4_318(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_318')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_318
          

        subroutine mydgemv_n_ver1_4_320(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_320
          

        subroutine mydgemv_n_ver1_4_322(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_322')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_322
          

        subroutine mydgemv_n_ver1_4_324(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_324
          

        subroutine mydgemv_n_ver1_4_326(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_326')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_326
          

        subroutine mydgemv_n_ver1_4_328(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_328
          

        subroutine mydgemv_n_ver1_4_330(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_330')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_330
          

        subroutine mydgemv_n_ver1_4_332(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_332
          

        subroutine mydgemv_n_ver1_4_334(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_334')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_334
          

        subroutine mydgemv_n_ver1_4_336(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_336
          

        subroutine mydgemv_n_ver1_4_338(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_338')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_338
          

        subroutine mydgemv_n_ver1_4_340(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_340
          

        subroutine mydgemv_n_ver1_4_342(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_342')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_342
          

        subroutine mydgemv_n_ver1_4_344(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_344
          

        subroutine mydgemv_n_ver1_4_346(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_346')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_346
          

        subroutine mydgemv_n_ver1_4_348(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_348
          

        subroutine mydgemv_n_ver1_4_350(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_350')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_350
          

        subroutine mydgemv_n_ver1_4_352(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_352
          

        subroutine mydgemv_n_ver1_4_354(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_354')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_354
          

        subroutine mydgemv_n_ver1_4_356(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_356
          

        subroutine mydgemv_n_ver1_4_358(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_358')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_358
          

        subroutine mydgemv_n_ver1_4_360(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_360
          

        subroutine mydgemv_n_ver1_4_362(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_362')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_362
          

        subroutine mydgemv_n_ver1_4_364(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_364
          

        subroutine mydgemv_n_ver1_4_366(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_366')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_366
          

        subroutine mydgemv_n_ver1_4_368(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_368
          

        subroutine mydgemv_n_ver1_4_370(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_370')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_370
          

        subroutine mydgemv_n_ver1_4_372(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_372
          

        subroutine mydgemv_n_ver1_4_374(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_374')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_374
          

        subroutine mydgemv_n_ver1_4_376(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_376
          

        subroutine mydgemv_n_ver1_4_378(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_378')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_378
          

        subroutine mydgemv_n_ver1_4_380(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_380
          

        subroutine mydgemv_n_ver1_4_382(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_382')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_382
          

        subroutine mydgemv_n_ver1_4_384(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_384
          

        subroutine mydgemv_n_ver1_4_386(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_386')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_386
          

        subroutine mydgemv_n_ver1_4_388(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_388
          

        subroutine mydgemv_n_ver1_4_390(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_390')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_390
          

        subroutine mydgemv_n_ver1_4_392(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_392
          

        subroutine mydgemv_n_ver1_4_394(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_394')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_394
          

        subroutine mydgemv_n_ver1_4_396(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_396
          

        subroutine mydgemv_n_ver1_4_398(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_398')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_398
          

        subroutine mydgemv_n_ver1_4_400(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_400
          

        subroutine mydgemv_n_ver1_4_402(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_402')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_402
          

        subroutine mydgemv_n_ver1_4_404(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_404
          

        subroutine mydgemv_n_ver1_4_406(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_406')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_406
          

        subroutine mydgemv_n_ver1_4_408(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_408
          

        subroutine mydgemv_n_ver1_4_410(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_410')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_410
          

        subroutine mydgemv_n_ver1_4_412(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_412
          

        subroutine mydgemv_n_ver1_4_414(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_414')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_414
          

        subroutine mydgemv_n_ver1_4_416(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_416
          

        subroutine mydgemv_n_ver1_4_418(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_418')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_418
          

        subroutine mydgemv_n_ver1_4_420(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_420
          

        subroutine mydgemv_n_ver1_4_422(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_422')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_422
          

        subroutine mydgemv_n_ver1_4_424(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_424
          

        subroutine mydgemv_n_ver1_4_426(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_426')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_426
          

        subroutine mydgemv_n_ver1_4_428(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_428
          

        subroutine mydgemv_n_ver1_4_430(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_430')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_430
          

        subroutine mydgemv_n_ver1_4_432(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_432
          

        subroutine mydgemv_n_ver1_4_434(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_434')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_434
          

        subroutine mydgemv_n_ver1_4_436(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_436
          

        subroutine mydgemv_n_ver1_4_438(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_438')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_438
          

        subroutine mydgemv_n_ver1_4_440(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_440
          

        subroutine mydgemv_n_ver1_4_442(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_442')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_442
          

        subroutine mydgemv_n_ver1_4_444(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_444
          

        subroutine mydgemv_n_ver1_4_446(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_446')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_446
          

        subroutine mydgemv_n_ver1_4_448(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_448
          

        subroutine mydgemv_n_ver1_4_450(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_450')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_450
          

        subroutine mydgemv_n_ver1_4_452(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_452
          

        subroutine mydgemv_n_ver1_4_454(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_454')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_454
          

        subroutine mydgemv_n_ver1_4_456(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_456
          

        subroutine mydgemv_n_ver1_4_458(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_458')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_458
          

        subroutine mydgemv_n_ver1_4_460(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_460
          

        subroutine mydgemv_n_ver1_4_462(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_462')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_462
          

        subroutine mydgemv_n_ver1_4_464(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_464
          

        subroutine mydgemv_n_ver1_4_466(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_466')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_466
          

        subroutine mydgemv_n_ver1_4_468(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_468
          

        subroutine mydgemv_n_ver1_4_470(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_470')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_470
          

        subroutine mydgemv_n_ver1_4_472(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_472
          

        subroutine mydgemv_n_ver1_4_474(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_474')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_474
          

        subroutine mydgemv_n_ver1_4_476(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_476
          

        subroutine mydgemv_n_ver1_4_478(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_478')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_478
          

        subroutine mydgemv_n_ver1_4_480(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_480
          

        subroutine mydgemv_n_ver1_4_482(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_482')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_482
          

        subroutine mydgemv_n_ver1_4_484(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_484
          

        subroutine mydgemv_n_ver1_4_486(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_486')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_486
          

        subroutine mydgemv_n_ver1_4_488(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_488
          

        subroutine mydgemv_n_ver1_4_490(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_490')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_490
          

        subroutine mydgemv_n_ver1_4_492(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_492
          

        subroutine mydgemv_n_ver1_4_494(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_494')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_494
          

        subroutine mydgemv_n_ver1_4_496(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_496
          

        subroutine mydgemv_n_ver1_4_498(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_498')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_498
          

        subroutine mydgemv_n_ver1_4_500(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_500
          

        subroutine mydgemv_n_ver1_4_502(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_502')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_502
          

        subroutine mydgemv_n_ver1_4_504(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_504
          

        subroutine mydgemv_n_ver1_4_506(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_506')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_506
          

        subroutine mydgemv_n_ver1_4_508(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_508
          

        subroutine mydgemv_n_ver1_4_510(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_510')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_510
          

        subroutine mydgemv_n_ver1_4_512(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver1_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver1_4_512     
        
        


        subroutine mydgemv_n_ver2_4_4(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_4')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_4
          

        subroutine mydgemv_n_ver2_4_8(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_8')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_8
          

        subroutine mydgemv_n_ver2_4_12(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_12')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_12
          

        subroutine mydgemv_n_ver2_4_16(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_16')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_16
          

        subroutine mydgemv_n_ver2_4_20(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_20')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_20
          

        subroutine mydgemv_n_ver2_4_24(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_24')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_24
          

        subroutine mydgemv_n_ver2_4_28(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_28')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_28
          

        subroutine mydgemv_n_ver2_4_32(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_32')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_32
          

        subroutine mydgemv_n_ver2_4_36(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_36')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_36
          

        subroutine mydgemv_n_ver2_4_40(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_40')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_40
          

        subroutine mydgemv_n_ver2_4_44(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_44')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_44
          

        subroutine mydgemv_n_ver2_4_48(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_48')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_48
          

        subroutine mydgemv_n_ver2_4_52(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_52')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_52
          

        subroutine mydgemv_n_ver2_4_56(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_56')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_56
          

        subroutine mydgemv_n_ver2_4_60(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_60')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_60
          

        subroutine mydgemv_n_ver2_4_64(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_64')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_64
          

        subroutine mydgemv_n_ver2_4_68(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_68')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_68
          

        subroutine mydgemv_n_ver2_4_72(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_72')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_72
          

        subroutine mydgemv_n_ver2_4_76(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_76')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_76
          

        subroutine mydgemv_n_ver2_4_80(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_80')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_80
          

        subroutine mydgemv_n_ver2_4_84(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_84')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_84
          

        subroutine mydgemv_n_ver2_4_88(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_88')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_88
          

        subroutine mydgemv_n_ver2_4_92(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_92')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_92
          

        subroutine mydgemv_n_ver2_4_96(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_96')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_96
          

        subroutine mydgemv_n_ver2_4_100(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_100')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_100
          

        subroutine mydgemv_n_ver2_4_104(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_104')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_104
          

        subroutine mydgemv_n_ver2_4_108(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_108')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_108
          

        subroutine mydgemv_n_ver2_4_112(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_112')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_112
          

        subroutine mydgemv_n_ver2_4_116(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_116')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_116
          

        subroutine mydgemv_n_ver2_4_120(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_120')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_120
          

        subroutine mydgemv_n_ver2_4_124(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_124')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_124
          

        subroutine mydgemv_n_ver2_4_128(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_128')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_128
          

        subroutine mydgemv_n_ver2_4_132(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_132')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_132
          

        subroutine mydgemv_n_ver2_4_136(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_136')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_136
          

        subroutine mydgemv_n_ver2_4_140(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_140')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_140
          

        subroutine mydgemv_n_ver2_4_144(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_144')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_144
          

        subroutine mydgemv_n_ver2_4_148(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_148')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_148
          

        subroutine mydgemv_n_ver2_4_152(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_152')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_152
          

        subroutine mydgemv_n_ver2_4_156(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_156')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_156
          

        subroutine mydgemv_n_ver2_4_160(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_160')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_160
          

        subroutine mydgemv_n_ver2_4_164(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_164')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_164
          

        subroutine mydgemv_n_ver2_4_168(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_168')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_168
          

        subroutine mydgemv_n_ver2_4_172(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_172')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_172
          

        subroutine mydgemv_n_ver2_4_176(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_176')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_176
          

        subroutine mydgemv_n_ver2_4_180(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_180')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_180
          

        subroutine mydgemv_n_ver2_4_184(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_184')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_184
          

        subroutine mydgemv_n_ver2_4_188(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_188')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_188
          

        subroutine mydgemv_n_ver2_4_192(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_192')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_192
          

        subroutine mydgemv_n_ver2_4_196(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_196')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_196
          

        subroutine mydgemv_n_ver2_4_200(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_200')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_200
          

        subroutine mydgemv_n_ver2_4_204(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_204')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_204
          

        subroutine mydgemv_n_ver2_4_208(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_208')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_208
          

        subroutine mydgemv_n_ver2_4_212(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_212')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_212
          

        subroutine mydgemv_n_ver2_4_216(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_216')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_216
          

        subroutine mydgemv_n_ver2_4_220(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_220')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_220
          

        subroutine mydgemv_n_ver2_4_224(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_224')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_224
          

        subroutine mydgemv_n_ver2_4_228(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_228')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_228
          

        subroutine mydgemv_n_ver2_4_232(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_232')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_232
          

        subroutine mydgemv_n_ver2_4_236(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_236')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_236
          

        subroutine mydgemv_n_ver2_4_240(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_240')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_240
          

        subroutine mydgemv_n_ver2_4_244(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_244')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_244
          

        subroutine mydgemv_n_ver2_4_248(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_248')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_248
          

        subroutine mydgemv_n_ver2_4_252(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_252')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_252
          

        subroutine mydgemv_n_ver2_4_256(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_256')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_256
          

        subroutine mydgemv_n_ver2_4_260(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_260')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_260
          

        subroutine mydgemv_n_ver2_4_264(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_264')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_264
          

        subroutine mydgemv_n_ver2_4_268(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_268')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_268
          

        subroutine mydgemv_n_ver2_4_272(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_272')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_272
          

        subroutine mydgemv_n_ver2_4_276(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_276')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_276
          

        subroutine mydgemv_n_ver2_4_280(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_280')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_280
          

        subroutine mydgemv_n_ver2_4_284(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_284')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_284
          

        subroutine mydgemv_n_ver2_4_288(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_288')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_288
          

        subroutine mydgemv_n_ver2_4_292(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_292')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_292
          

        subroutine mydgemv_n_ver2_4_296(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_296')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_296
          

        subroutine mydgemv_n_ver2_4_300(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_300')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_300
          

        subroutine mydgemv_n_ver2_4_304(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_304')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_304
          

        subroutine mydgemv_n_ver2_4_308(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_308')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_308
          

        subroutine mydgemv_n_ver2_4_312(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_312')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_312
          

        subroutine mydgemv_n_ver2_4_316(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_316')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_316
          

        subroutine mydgemv_n_ver2_4_320(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_320')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_320
          

        subroutine mydgemv_n_ver2_4_324(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_324')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_324
          

        subroutine mydgemv_n_ver2_4_328(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_328')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_328
          

        subroutine mydgemv_n_ver2_4_332(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_332')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_332
          

        subroutine mydgemv_n_ver2_4_336(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_336')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_336
          

        subroutine mydgemv_n_ver2_4_340(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_340')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_340
          

        subroutine mydgemv_n_ver2_4_344(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_344')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_344
          

        subroutine mydgemv_n_ver2_4_348(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_348')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_348
          

        subroutine mydgemv_n_ver2_4_352(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_352')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_352
          

        subroutine mydgemv_n_ver2_4_356(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_356')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_356
          

        subroutine mydgemv_n_ver2_4_360(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_360')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_360
          

        subroutine mydgemv_n_ver2_4_364(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_364')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_364
          

        subroutine mydgemv_n_ver2_4_368(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_368')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_368
          

        subroutine mydgemv_n_ver2_4_372(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_372')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_372
          

        subroutine mydgemv_n_ver2_4_376(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_376')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_376
          

        subroutine mydgemv_n_ver2_4_380(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_380')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_380
          

        subroutine mydgemv_n_ver2_4_384(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_384')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_384
          

        subroutine mydgemv_n_ver2_4_388(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_388')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_388
          

        subroutine mydgemv_n_ver2_4_392(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_392')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_392
          

        subroutine mydgemv_n_ver2_4_396(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_396')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_396
          

        subroutine mydgemv_n_ver2_4_400(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_400')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_400
          

        subroutine mydgemv_n_ver2_4_404(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_404')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_404
          

        subroutine mydgemv_n_ver2_4_408(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_408')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_408
          

        subroutine mydgemv_n_ver2_4_412(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_412')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_412
          

        subroutine mydgemv_n_ver2_4_416(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_416')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_416
          

        subroutine mydgemv_n_ver2_4_420(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_420')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_420
          

        subroutine mydgemv_n_ver2_4_424(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_424')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_424
          

        subroutine mydgemv_n_ver2_4_428(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_428')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_428
          

        subroutine mydgemv_n_ver2_4_432(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_432')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_432
          

        subroutine mydgemv_n_ver2_4_436(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_436')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_436
          

        subroutine mydgemv_n_ver2_4_440(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_440')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_440
          

        subroutine mydgemv_n_ver2_4_444(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_444')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_444
          

        subroutine mydgemv_n_ver2_4_448(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_448')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_448
          

        subroutine mydgemv_n_ver2_4_452(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_452')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_452
          

        subroutine mydgemv_n_ver2_4_456(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_456')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_456
          

        subroutine mydgemv_n_ver2_4_460(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_460')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_460
          

        subroutine mydgemv_n_ver2_4_464(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_464')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_464
          

        subroutine mydgemv_n_ver2_4_468(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_468')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_468
          

        subroutine mydgemv_n_ver2_4_472(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_472')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_472
          

        subroutine mydgemv_n_ver2_4_476(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_476')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_476
          

        subroutine mydgemv_n_ver2_4_480(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_480')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_480
          

        subroutine mydgemv_n_ver2_4_484(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_484')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_484
          

        subroutine mydgemv_n_ver2_4_488(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_488')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_488
          

        subroutine mydgemv_n_ver2_4_492(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_492')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_492
          

        subroutine mydgemv_n_ver2_4_496(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_496')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_496
          

        subroutine mydgemv_n_ver2_4_500(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_500')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_500
          

        subroutine mydgemv_n_ver2_4_504(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_504')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_504
          

        subroutine mydgemv_n_ver2_4_508(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_508')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_508
          

        subroutine mydgemv_n_ver2_4_512(a, x, y, lda, ldx, ldy) bind(C, name='mydgemv_n_ver2_4_512')
            import
            integer(c_int64_t), value :: lda, ldx, ldy
            real(c_double), intent(in) :: a(lda, ldx), x(ldx), y(ldy)
        end subroutine mydgemv_n_ver2_4_512        
    end interface 

    
contains
    
end module mod_small_gemv