module mod_datetime
    use :: mod_kinds
    implicit none
    
contains

    function get_datetime() result(datetime)
        implicit none
        integer(i64) :: datetime
        integer(i64) :: date_value(8)
        call date_and_time(values=date_value)
        datetime = date_value(1) * 10_8**13 & ! year
                + date_value(2) * 10_8**11  & ! month
                + date_value(3) * 10_8**9   & ! day
                + date_value(5) * 10_8**7   & ! hour 
                + date_value(6) * 10_8**5   & ! minute
                + date_value(7) * 10_8**3   & ! second
                + date_value(8) * 10_8        ! milisecond
    end function get_datetime

end module mod_datetime
