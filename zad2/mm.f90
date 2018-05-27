module matrix_multiplication2
    implicit none	
contains	
    subroutine mm_dot(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)      :: first(:,:)      ! pierwsza macierz
        real (kind = 8), intent(in)      :: second(: ,:)    ! druga macierz
        real (kind = 8), intent(out)     :: multiply(:,:)   ! macierz wynikowa
        integer (kind = 4), intent(out)  :: status          ! kod błędu, 0 gdy OK

        !local variables
        integer (kind = 4)               :: first_shape(2), second_shape(2), multiply_shape(2), i,j

        first_shape = shape(first)
        second_shape = shape(second)
        multiply_shape = shape(multiply)

        if((first_shape(1) .NE. second_shape(2)) .OR. (multiply_shape(1) .NE. second_shape(1)) .OR. (multiply_shape(2) .NE. first_shape(2))  ) then
            status = -1
            return
        end if
        
        do i=1, first_shape(2)
            do j=1, second_shape(1)
                multiply(i,j) = dot_product(first(:, i),second(j, :))
            end do
        end do

        status = 0
    end subroutine

    subroutine mm_cache(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)      :: first(:,:)      ! pierwsza macierz
        real (kind = 8), intent(in)      :: second(: ,:)    ! druga macierz
        real (kind = 8), intent(out)     :: multiply(:,:)   ! macierz wynikowa
        integer (kind = 4), intent(out)  :: status          ! kod błędu, 0 gdy OK

        !zmienne lokalne
        integer (kind = 4)               :: first_shape(2), second_shape(2), multiply_shape(2), a, b, i, j, k, ichunk
        real (kind = 8)                  :: accumulator

        first_shape = shape(first)
        second_shape = shape(second)
        multiply_shape = shape(multiply)
        ichunk = 1024   ! wielkość mojego L2

        if((first_shape(1) .NE. second_shape(2)) .OR. (multiply_shape(1) .NE. second_shape(1)) .OR. (multiply_shape(2) .NE. first_shape(2))  ) then
            status = -1
            return
        end if

        do a=1, first_shape(2), ichunk
            do b=1, second_shape(1), ichunk
                do i=a, min(a + ichunk - 1, first_shape(2))
                    do j = b, min(b + ichunk - 1, second_shape(1))
                        accumulator = 0
                        do k = 1, first_shape(1)
                            accumulator = accumulator + first(k, i)*second(j, k)
                        end do
                        multiply(i, j) = accumulator
                    end do
                end do
            end do
        end do

        status = 0
    end subroutine

    subroutine mm_dot_cache(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)      :: first(:,:)      ! pierwsza macierz
        real (kind = 8), intent(in)      :: second(: ,:)    ! druga macierz
        real (kind = 8), intent(out)     :: multiply(:,:)   ! macierz wynikowa
        integer (kind = 4), intent(out)  :: status          ! kod błędu, 0 gdy OK

        !zmienne lokalne
        integer (kind = 4)               :: first_shape(2), second_shape(2), multiply_shape(2), a, b, i, j, ichunk

        first_shape = shape(first)
        second_shape = shape(second)
        multiply_shape = shape(multiply)
        ichunk = 1024

        if((first_shape(1) .NE. second_shape(2)) .OR. (multiply_shape(1) .NE. second_shape(1)) .OR. (multiply_shape(2) .NE. first_shape(2))  ) then
            status = -1
            return
        end if

        do a=1, first_shape(2), ichunk
            do b=1, second_shape(1), ichunk
                do i=a, min(a + ichunk - 1, first_shape(2))
                    do j = b, min(b + ichunk - 1, second_shape(1))
                        multiply(i,j) = dot_product(first(:, i),second(j, :))
                    end do
                end do
            end do
        end do

        status = 0
    end subroutine
end module matrix_multiplication2