
module matrix_multiplication1	
    implicit none	
contains	
    subroutine mm(first, second, multiply, status)
        implicit none
        real (kind = 8), intent(in)      :: first(:,:)      ! pierwsza macierz
        real (kind = 8), intent(in)      :: second(: ,:)    ! druga macierz
        real (kind = 8), intent(out)     :: multiply(:,:)   ! macierz wynikowa
        integer (kind = 4), intent(out)  :: status          ! kod błędu, 0 gdy OK

        !zmienne lokalne
        integer (kind = 4)               :: first_shape(2), second_shape(2), multiply_shape(2), i, j, k
        real (kind = 8)                  :: accumulator

        first_shape = shape(first)
        second_shape = shape(second)
        multiply_shape = shape(multiply)

        if((first_shape(1) .NE. second_shape(2)) .OR. (multiply_shape(1) .NE. second_shape(1)) .OR. (multiply_shape(2) .NE. first_shape(2))  ) then
            status = -1
            return
        end if
        
        do i = 1,first_shape(2) 
            do j = 1,second_shape(1) 
                accumulator = 0	
                do k = 1,first_shape(1)	
                    accumulator = accumulator + first(k,i) * second(j,k)	
                end do	
                multiply(i,j) = accumulator	
            end do	
        end do	

        status = 0
    end subroutine
end module matrix_multiplication1