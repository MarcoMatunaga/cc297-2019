subroutine mesh_project1
    use vars
    implicit none
    integer(4)                              :: i,j
    real(8)                                 :: Deltax

    Deltax = 1.0d0/(ITE-ILE)
    
    do j = 1, jmax

        do i = ILE, ITE
            ! mesh from airfoil leading edge to airfoil trailing edge
            meshx(i,j) = (i - ILE)*Deltax
        end do

        do i = ITE + 1, imax 
            ! mesh from the trailing edge to the outlet boundary
            meshx(i,j) = meshx(i-1,j) + ( meshx(i-1,j) - meshx(i-2,j) )*XSF
        end do 

        do i = ILE - 1, 1, -1
            ! mesh from the inlet boundary to trailing edge  
            meshx(i,j) = meshx(i+1,j) + ( meshx(i+1,j) - meshx(i+2,j) )*XSF
        end do

    end do 

    do i = 1, imax
        
        meshy(i,1) = -Deltax/2.0d0
        meshy(i,2) =  Deltax/2.0d0

        do j = 3, jmax
            meshy(i,j) = meshy(i,j-1) + ( meshy(i,j-1) - meshy(i,j-2) )*YSF
        end do

    end do 

end subroutine mesh_project1

! subroutine cartesian_mesh(Length,Height)
!     use vars
!     implicit none
!     integer(4)          :: i,j
!     real(8)             :: Deltax, Deltay
!     real(8), intent(in) :: Length, Height 
    
!     Deltax = Length/DBLE(imax)
!     Deltay = Height/DBLE(jmax)

!     do i = 1, imax
!         aux_x(i) = i*Deltax
!     end do

!     do j = 1, jmax
!         aux_y(j) = j*Deltay
!     end do

!     do j = 1, jmax
!         do i = 1, imax
!             meshx(i,j) = aux_x(i)
!             meshy(i,j) = aux_y(j)
!         end do
!     end do

!     ! create a mesh tecplot file

!     open(2,file='mesh.dat')
!     write(2,*) 'TITLE = "Projeto1" '
!     write(2,*) 'VARIABLES = "X" "Y" '
!     write(2,*) 'ZONE I = ', imax, ' J =', jmax, ' DATAPACKING = POINT' 
!     do j = 1, jmax
!         do i = 1, imax
!             write(2,*) meshx(i,j), meshy(i,j)
!         end do
!     end do

! end subroutine cartesian_mesh

! subroutine read_mesh
!     use vars
!     implicit none

!     open(1,file='mesh') 
!     read(1,*) imax,jmax
!     close(1)

! end subroutine read_mesh