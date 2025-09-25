module mod_config
    use :: mod_kinds
    implicit none
    
    logical, parameter :: ENABLE_WARNING = .true.
    
    integer(i64), parameter :: FORMAT_VER = 1

    integer(i64), parameter :: INITIAL_MODEL_POOL_CAPACITY = 16
    integer(i64), parameter :: GROWTH_MODEL_POOL_CAPACITY  = 16

end module mod_config
