module vars
    implicit none
integer(4)                              :: ITE, ILE
real(8)                                 :: XSF, YSF

!
integer(4)                              :: imax, jmax
real(8),dimension(:,:),allocatable      :: meshx, meshy

! Flow variables
real(8)                                 :: u_inf
real(8),dimension(:,:),allocatable      :: phi

! geometry variables
real(8)                                 :: t

!
real(8)                                 :: r

!
integer(4)                              :: max_iter, which_method
real(8)                                 :: conv, N, residue, max_residue

contains

    subroutine allocate_vars
        implicit none
        ! mesh variables
        allocate(meshx(imax,jmax),meshy(imax,jmax))
        allocate(phi(imax,jmax))

    end subroutine allocate_vars
    
    subroutine deallocate_vars
        implicit none
        deallocate(meshx,meshy)
        deallocate(phi)

    end subroutine deallocate_vars

end module vars
