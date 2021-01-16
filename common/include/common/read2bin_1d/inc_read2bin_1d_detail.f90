dtype_r = "r"
dtype_i = "i"
if ( output_dtype .eq. dtype_r ) then
    if (allocated(vector_r)) deallocate(vector_r)
    allocate(vector_r(n_samples))
    print*, "Reading File : ", trim(input_file_name)
    open(10, file=input_file_name)
    if (skip_header) read(10, '()')
    if ( input_dtype .eq. dtype_r ) then
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/1000)
            read(10, *) vector_r(i)
        end do
    else
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/1000)
            read(10, *) tmp_i
            vector_r(i) = tmp_i
        end do
    end if
    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_samples
    write(10) bit_size
    write(10) vector_r
    close(10)

elseif ( output_dtype .eq. dtype_i ) then
    if (allocated(vector_i)) deallocate(vector_i)
    allocate(vector_i(n_samples))
    print*, "Reading File : ", trim(input_file_name)
    open(10, file=input_file_name)
    if (skip_header) read(10, '()')
    if ( input_dtype .eq. dtype_i ) then
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/1000)
            read(10, *) vector_i(i)
        end do
    else
        do i=1, n_samples
            call progress_bar(i, n_samples, n_samples/1000)
            read(10, *) tmp_r
            vector_i(i) = tmp_r
        end do
    end if
    close(10)

    print*, "Saving Binary : ", output_file_name
    open(10, file=output_file_name, form='unformatted', status='replace')
    write(10) nd
    write(10) output_dtype
    write(10) n_samples
    write(10) bit_size
    write(10) vector_i
    close(10)
end if
