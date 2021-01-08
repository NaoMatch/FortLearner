!> A module for timer.
module mod_timer
    implicit none

    ! Sample ------------------------------------------
    ! integer(kind=8) :: date_value1(8), date_value2(8)
    ! call date_and_time(values=date_value1)
    ! .....process.....
    ! call date_and_time(values=date_value2)
    ! print*, time_diff(date_value1, date_value2)
    ! -------------------------------------------------

contains

    !> A function to get datetime values
    !! \return returns datetime values
    !! \param date_value datetime values
    function datetime2second(date_value)
        implicit none
        integer(kind=8) :: date_value(8)
        real(kind=8)    :: datetime2second
        datetime2second = ( date_value(5) )*60.         ! Hours to minutes
        datetime2second = ( datetime2second + date_value(6) )*60. ! Minutes to seconds
        datetime2second = ( datetime2second + date_value(7) )*1e3 ! Seconds to milliseconds
        datetime2second = datetime2second + date_value(8)         ! Add milliseconds
    end function datetime2second

    !> A function to compute time difference
    !! \return returns time difference
    !! \param date_value1 datetime value at time 1
    !! \param date_value1 datetime value at time 2
    function time_diff(date_value1, date_value2)
        implicit none
        integer(kind=8) :: date_value1(8), date_value2(8)
        integer(kind=8) :: time_diff
        time_diff = abs(datetime2second(date_value1) - datetime2second(date_value2))
    end function time_diff

end module mod_timer
