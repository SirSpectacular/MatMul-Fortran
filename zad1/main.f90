program mainp
    use 
    use utilities
    implicit none
    integer (kind = 8) :: clck_counts_beg, clck_counts_end, clck_rate
    real (kind = 8), allocatable, dimension(:,:) :: first
    real (kind = 8), allocatable, dimension(:,:) :: second
    real (kind = 8), allocatable, dimension(:,:) :: multiply
    real (kind = 8) :: result
    integer (kind = 4) :: i, args_count, status, mode, s(4)
    character (len = 32), dimension(:), allocatable :: args
    character (len = 32) :: filename

    args_count = command_argument_count()
    allocate(args(args_count))

    do i = 1, args_count
        call get_command_argument(i, args(i))
    end do

    do i = 1, args_count - 2
        read(args(i), '(i5)') s(i)
    end do

    read(args(args_count - 1), '(i5)') mode
    filename = args(args_count)
    
    allocate(first(s(1),s(2)))
    allocate(second(s(3),s(4)))
    allocate(multiply(s(2),s(3)))

    multiply = 0.d0

    call random_number(first)
    call random_number(second)

    call system_clock(clck_counts_beg, clck_rate)

    select case(mode)
        case(0)
            call mm(first, second, multiply, status)
        case(1)
            call mm_dot(first, second, multiply, status)
        case(2)
            call mm_mul(first, second, multiply, status)
        case(3)
            call mm_cache(first, second, multiply, status)
        case(4)
            call mm_dotcache(first, second, multiply, status)
        case default
            call mm(first, second, multiply, status)
    end select

    call system_clock(clck_counts_end, clck_rate)
    result = (clck_counts_end - clck_counts_beg) / real (clck_rate)

    if(status == 0) then
        !write(*, *) "FIRST ARRAY"
        !call print_array(transpose(first), s(2), s(1))
        !write(*, *) "SECOND ARRAY"
        !call print_array(transpose(second), s(4), s(3))
        !write(*, *) "RESULT"
        !call print_array(transpose(multiply), s(3), s(2))

        call save_results(result, filename)
    end if

    deallocate(first)
    deallocate(second)
    deallocate(multiply)
    deallocate(args)
end program