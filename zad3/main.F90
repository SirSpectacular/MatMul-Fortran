program main
    use matrix_multiplication1
    use matrix_multiplication2
    use matrix_multiplication3
    implicit none
    real (kind = 8), allocatable                        :: first(:,:)
    real (kind = 8), allocatable                        :: second(:,:) 
    real (kind = 8), allocatable                        :: multiply(:,:)
    integer (kind = 4)                                  :: status 

    character (len = 32), dimension(:), allocatable     :: argv
    integer (kind = 4)                                  :: argc
    integer (kind = 4)                                  :: N, mode, i
    real (kind = 8)                                     :: start, stop

    argc = command_argument_count()

    if(argc .EQ. 2) then

        allocate(argv(argc))

        do i = 1, argc
            call get_command_argument(i, argv(i))
        end do

        read(argv(1), '(i5)') N
        read(argv(2), '(i5)') mode
    
        allocate(first(N,N))
        allocate(second(N,N))
        allocate(multiply(N,N))

        first = 1
        second = 1

        call cpu_time(start)

            select case(mode)
            case(0)
                call mm(first, second, multiply, status)
            case(1)
                call mm_dot(first, second, multiply, status)
            case(2)
                call mm_cache(first, second, multiply, status)
            case(3)
                call mm_dot_cache(first, second, multiply, status)
            case(4)
                call mm_matmul(first, second, multiply, status)
            case default
                call mm(first, second, multiply, status)
        end select

        call cpu_time(stop)

        write(*,*) stop-start

        deallocate(argv)
        deallocate(first)
        deallocate(second)
        deallocate(multiply)	
    end if
end program main 