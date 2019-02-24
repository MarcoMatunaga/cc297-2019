subroutine jacobi(d_x,d_y,Nj)
    use vars
    implicit none
    real(8), intent(in)         :: d_x, d_y
    real(8), intent(out)        :: Nj

    Nj = - 2.0d0/d_x**2.0d0 - 2.0d0/d_y**2.0d0 

end subroutine jacobi

! subroutine Gauss_Seidel(args)
!     implicit none
!     real :: args
    
! end subroutine Gauss_Seidel

! subroutine SOR(args)
!     implicit none
!     real :: args
    
! end subroutine SOR

! subroutine Line_Gauss_Seidel(args)
!     implicit none
!     real :: args
    
! end subroutine Line_Gauss_Seidel

! subroutine SLOR(args)
!     implicit none
!     real :: args
    
! end subroutine SLOR