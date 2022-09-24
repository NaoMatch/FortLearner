module mod_softmax
    use mod_variable_in_variable
    use mod_wengert_list
    use mod_activation_function
    use mod_exponential
    use mod_summation
    use mod_spread
    use mod_division
    implicit none

    type softmax_base
    contains
        procedure :: act  => act_softmax_base
    end type softmax_base
    type(softmax_base) :: softmax

contains

    function act_softmax_base(this, input_var) result(output_var)
        implicit none
        class(softmax_base) :: this
        type(variable) :: input_var
        type(variable) :: output_var

        integer(kind=8) :: ncopies
        type(variable) :: h_exp, h_sum, h_spread, h_inv

        ncopies = input_var%ncolumns()

        h_exp = exp(input_var)
        h_sum = sum(h_exp, dim=2_8)
        h_spread = spread(h_sum, dim=2_8, ncopies=10_8)
        output_var = h_exp / h_spread
    end function act_softmax_base

end module mod_softmax