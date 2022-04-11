module mod_hash_table
    !$ use omp_lib
    use mod_common
    use mod_sort
    use mod_common_type
    use mod_hash
    implicit none

    type index_sets
        type(jagged_vector_i8), ALLOCATABLE :: indices(:)
    end type index_sets

    !> Hash Table. Creation Only.
    !> NO Insertion, NO Deletion, NO Collision Check.
    type hash_table
        integer(kind=8) :: n_samples, n_columns !< input data shape
        integer(kind=8) :: n_hash_key !< number of hash keys
        integer(kind=8), allocatable :: hashes(:)
        integer(kind=8), allocatable :: indices(:)
        integer(kind=8), allocatable :: unique_hashes(:)
        type(jagged_vector_i8), ALLOCATABLE :: hash2indices(:)
    contains
        procedure :: create_table
        procedure :: search
        procedure :: search_all
        procedure :: dump => dump_hash_table
        procedure :: load => load_hash_table
    end type hash_table
    
    interface hash_table
        module procedure :: new_hash_table
    end interface hash_table

contains

    function new_hash_table()
        implicit none
        type(hash_table) :: new_hash_table
    end function new_hash_table

    subroutine create_table(this, x)
        implicit none
        class(hash_table) :: this
        integer(kind=8), intent(in) :: x(:,:)

        integer(kind=8) :: n_samples, n_columns, x_shape(2)
        integer(kind=8) :: i, l, ini, fin

        x_shape        = shape(x)
        this%n_samples = x_shape(1)
        this%n_columns = x_shape(2)

        allocate( this%hashes(this%n_samples) )
        allocate( this%indices(this%n_samples) )
        do i=1, this%n_samples, 1
            this%indices(i) = i
        end do

        ! Input to Hash Value
        this%hashes = one_at_a_time_hash(x, this%n_samples, this%n_columns)
        call quick_argsort(this%hashes, this%indices, this%n_samples)

        ! Collect Unique Values
        this%n_hash_key = count_unique(this%hashes, this%n_samples)
        call collect_unique_values(this%unique_hashes, this%hashes, this%n_samples)

        allocate(this%hash2indices(this%n_hash_key))
        if ( this%n_hash_key .eq. n_samples ) then
            do l=1, this%n_hash_key, 1
                this%hash2indices(l)%vector = (/l/)
            end do
        else
            ini = 1
            do l=1, this%n_hash_key-1, 1
                fin = ini
                do while (t_)
                    if (this%hashes(fin) .ne. this%hashes(fin+1)) exit
                    fin = fin+1
                end do
                this%hash2indices(l)%vector = this%indices(ini:fin)
                ini = fin + 1
                call quick_sort(this%hash2indices(l)%vector, size(this%hash2indices(l)%vector)+0_8)
            end do
            this%hash2indices(this%n_hash_key)%vector = this%indices(ini:this%n_samples)
            call quick_sort(this%hash2indices(this%n_hash_key)%vector, size(this%hash2indices(this%n_hash_key)%vector)+0_8)
        end if
    end subroutine create_table

    function search(this, q)
        implicit none
        class(hash_table)           :: this
        integer(kind=8), intent(in) :: q(:)
        integer(kind=8), allocatable :: search(:)
        integer(kind=8) :: hash, idx
        hash = one_at_a_time_hash(q, this%n_columns)
        idx = binary_search_left(this%unique_hashes, this%n_hash_key, hash)
        search = this%hash2indices(idx)%vector(:)
    end function search

    function search_all(this, q)
        implicit none
        class(hash_table)           :: this
        integer(kind=8), intent(in) :: q(:,:)
        type(index_sets) :: search_all
        integer(kind=8) :: hash, idx, n_samples, n_columns, i
        integer(kind=8), allocatable :: hashes(:), table_indices(:)
        n_samples = size(q, dim=1)
        hashes = one_at_a_time_hash(q(:,:), n_samples, this%n_columns)
        allocate( search_all%indices(n_samples) )

        !opm parallel num_threads(8)
        !opm do private(i, idx)
        do i=1, n_samples, 1
            ! idx = linear_search(this%unique_hashes, this%n_hash_key, hashes(i))
            idx = binary_search_left(this%unique_hashes, this%n_hash_key, hashes(i))
            search_all%indices(i)%vector = this%hash2indices(idx)%vector(:)
        end do
        !opm end do
        !opm end parallel
    end function search_all

    subroutine dump_hash_table(this, file_name)
        implicit none
        class(hash_table) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit, n
        open(newunit=newunit, file=file_name, form="unformatted", status="replace")
        write(newunit) this%n_columns
        write(newunit) this%n_hash_key
        write(newunit) this%unique_hashes
        do n=1, this%n_hash_key, 1
            write(newunit) size(this%hash2indices(n)%vector(:))+0_8
            write(newunit) this%hash2indices(n)%vector(:)
        end do
        close(newunit)
    end subroutine dump_hash_table

    subroutine load_hash_table(this, file_name)
        implicit none
        class(hash_table) :: this
        character(len=*), intent(in) :: file_name
        integer(kind=8) :: newunit, n, n_vec
        open(newunit=newunit, file=file_name, form="unformatted")
        read(newunit) this%n_columns
        read(newunit) this%n_hash_key
        allocate(this%unique_hashes(this%n_hash_key))
        read(newunit) this%unique_hashes
        allocate(this%hash2indices(this%n_hash_key))
        do n=1, this%n_hash_key, 1
            read(newunit) n_vec
            allocate(this%hash2indices(n)%vector(n_vec))
            read(newunit) this%hash2indices(n)%vector(:)
        end do
        close(newunit)
    end subroutine load_hash_table

end module mod_hash_table
