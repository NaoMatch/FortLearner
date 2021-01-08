if ( output_dtype .eq. "r" ) then
    allocate(vector_r(n_rows))
    print*, "Reading File : ", input_file_name
    open(10, file=input_file_name)
    if (skip_header) read(10, '()')
    if ( input_dtype .eq. "r" ) then
        do i=1, n_rows
            call progress_bar(i, n_rows, n_rows/1000)
            read(10, *) vector_r(i)
        end do
    else
        do i=1, n_rows
            call progress_bar(i, n_rows, n_rows/1000)
            read(10, *) tmp_i
            vector_r(i) = tmp_i
        end do
    end if
    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_rows
    write(10) bit_size
    write(10) vector_r
    close(10)

elseif ( output_dtype .eq. "i" ) then
    allocate(vector_i(n_rows))
    print*, "Reading File : ", input_file_name
    open(10, file=input_file_name)
    if (skip_header) read(10, '()')
    if ( input_dtype .eq. "i" ) then
        do i=1, n_rows
            call progress_bar(i, n_rows, n_rows/1000)
            read(10, *) vector_i(i)
        end do
    else
        do i=1, n_rows
            call progress_bar(i, n_rows, n_rows/1000)
            read(10, *) tmp_r
            vector_i(i) = tmp_r
        end do
    end if
    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_rows
    write(10) bit_size
    write(10) vector_i
    close(10)
end if
