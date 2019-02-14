subroutine boco_proj1
    use vars
    implicit none
    integer(4)              :: i,j
    
    !* we can rewrite this subroutine
    !* excluding all the boundary conditions
    !* and let only the airfoil conditions
    
    ! upper boundary
    j = jmax
    do i = 1, imax
        phi(i,j) = u_inf*meshx(i,j) 
    end do 

    ! inlet boundary 
    i = 1
    do j = 1, jmax
        phi(i,j) = u_inf*meshx(i,j) 
    end do

    ! outlet boundary
    i = imax
    do j = 1, jmax
        phi(i,j) = u_inf*meshx(i,j) 
    end do

    ! bottom boundary
    j = 1
    do i = 1, imax
        phi(i,j) = 0.0d0 
    end do
    ! airfoil boundary
    do i = ILE, ITE
        phi(i,j) = u_inf*(2*t - 4*t*meshx(i,j))
    end do

end subroutine boco_proj1