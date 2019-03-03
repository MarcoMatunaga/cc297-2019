subroutine jacobi(d_x,d_y,residue_cp,c_cp)
    implicit none
    real(8), intent(in)     :: d_x, d_y, residue_cp
    real(8), intent(out)    :: c_cp
    real(8)                 :: Nj

    Nj = -2.0d0/(d_x**2.0d0) -2.0d0/(d_y**2.0d0)
    c_cp = -residue_cp/Nj

end subroutine jacobi

subroutine Gauss_Seidel(d_x,d_y,px,py,residue_cp,c_cp)
    implicit none
    real(8), intent(in)         :: d_x, d_y, residue_cp, px, py
    real(8), intent(out)        :: c_cp
    real(8)                     :: Nj, Ngs

    Nj   = -2.0d0/d_x**2.0d0 -2.0d0/d_y**2.0d0
    Ngs  = -residue_cp -px/d_x**2.0d0 -py/d_y**2.0d0
    c_cp = Ngs/Nj

end subroutine Gauss_Seidel

subroutine SOR(d_x,d_y,px,py,residue_cp,c_cp)
    use vars
    implicit none
    real(8), intent(in)         :: d_x, d_y, residue_cp, px, py
    real(8), intent(out)        :: c_cp
    real(8)                     :: Nj, Nsor

    Nj   = -2.0d0/d_x**2.0d0 -2.0d0/d_y**2.0d0
    Nsor = -residue_cp -px/d_x**2.0d0 -py/d_y**2.0d0
    c_cp = r*Nsor/Nj
    
end subroutine SOR

! subroutine Line_Gauss_Seidel(d_x)
!      implicit none
!     real(8), intent(in)         :: d_x, d_y, residue_cp, px, py
!     real(8), intent(out)        :: c_cp
!     real(8)                     :: Nlgs

!     ! thomas(a,b,c,d,x,n)
!     call thomas()

! end subroutine Line_Gauss_Seidel

! subroutine SLOR(args)
!     implicit none
!     real :: args
    
! end subroutine SLOR