module vars_mesh
implicit none
integer(4)                              :: ITE, ILE
real(8)                                 :: XSF, YSF

!
integer(4)                              :: imax, jmax
real(8),dimension(:,:),allocatable      :: meshx, meshy
real(8)                                 :: Deltax

contains

    subroutine allocate_vars
        implicit none
        ! mesh variables
        allocate(meshx(imax,jmax),meshy(imax,jmax))
        
    end subroutine allocate_vars
    
    subroutine deallocate_vars
        implicit none
        deallocate(meshx,meshy)
    end subroutine deallocate_vars

end module vars_mesh

module vars_flow
implicit none
! flow properties
!real(8)                                      :: gama, c_v, R, a_crcontains
    
end module vars_flow

