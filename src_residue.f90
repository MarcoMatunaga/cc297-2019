subroutine residue_calc
    use vars
    implicit none
    integer(4)                                   :: i, j 
    real(8)                                      :: delta_x, delta_y
    real(8),dimension(imax,jmax)                 :: c 

max_residue = -1.0d0

    do j = 2, jmax - 1
        do i = 2, imax - 1

            delta_x = (meshx(i+1,j) - meshx(i-1,j))/2.0d0
            delta_y = (meshy(i,j+1) - meshy(i,j-1))/2.0d0

            residue = (2.0d0/(meshx(i+1,j) - meshx(i-1,j) ) )*( (phi(i+1,j) - &
                            phi(i,j))/(meshx(i+1,j) - meshx(i,j)) - &
                      (phi(i,j) - phi(i-1,j))/(meshx(i,j) - meshx(i-1,j)) ) + &
                      (2.0d0/(meshy(i,j+1) - meshy(i,j-1) ) )*( (phi(i,j+1) - &
                            phi(i,j))/(meshy(i,j+1) - meshy(i,j)) - &
                      (phi(i,j) - phi(i,j-1))/(meshy(i,j) - meshy(i,j-1)) )

            ! call the relaxation method
            if (which_method == 1) call jacobi(delta_x,delta_y,residue,c(i,j))
            if (which_method == 2) call gauss_seidel(delta_x,delta_y,c(i-1,j),c(i,j-1),residue,c(i,j)) 
            if (which_method == 3) call SOR(delta_x,delta_y,c(i-1,j),c(i,j-1),residue,c(i,j)) 

            ! update the solution 

            if(dabs(residue) > max_residue) max_residue = dabs(residue)

        end do 
    end do

    do j = 2, jmax - 1
        do i = 2, imax - 1
            phi(i,j) = phi(i,j) + c(i,j)
        end do 
    end do

end subroutine residue_calc
