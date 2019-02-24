subroutine in_co_proj1
    use vars
    implicit none
    integer(4)      :: i,j
    
    ! initial conditions
    do j = 1, jmax
        do i = 1, imax 
            phi(i,j) = u_inf*meshx(i,j)
        end do 
    end do

end subroutine in_co_proj1