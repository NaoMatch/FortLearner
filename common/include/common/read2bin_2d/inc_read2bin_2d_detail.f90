if ( output_dtype .eq. "r" ) then
    if (allocated(matrix_r)) deallocate(matrix_r)
    allocate(matrix_r(n_samples, n_columns))
    print*, "Reading File : ", input_file_name
    open(10, file=input_file_name)
    if (skip_header) then
        print*, "Skip Header"
        read(10, '()')
    end if
    if ( input_dtype .eq. "r" ) then
        print*, "Read 'Real'"
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/100)
            read(10, *) matrix_r(i, :)
        end do
    else
        print*, "Read 'Integer'"
        allocate(tmp_i(n_columns))
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/100)
            read(10, *) tmp_i(:)
            matrix_r(i, :) = tmp_i(:)
        end do
    end if

    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_samples, n_columns
    write(10) bit_size
    write(10) matrix_r
    close(10)

elseif ( output_dtype .eq. "i" ) then
    if (allocated(matrix_i)) deallocate(matrix_i)
    allocate(matrix_i(n_samples, n_columns))
    print*, "Reading File : ", input_file_name
    open(10, file=input_file_name)
    if (skip_header) then
        print*, "Skip Header"
        read(10, '()')
    end if
    if ( input_dtype .eq. "i" ) then
        print*, "Read 'Integer'"
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/100)
            read(10, *) matrix_i(i, :)
        end do
    else
        print*, "Read 'Real'"
        allocate(tmp_r(n_columns))
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/100)
            read(10, *) tmp_r(:)
            matrix_i(i, :) = tmp_r(:)
        end do
    end if
    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_samples, n_columns
    write(10) bit_size
    write(10) matrix_i
    close(10)
end if
