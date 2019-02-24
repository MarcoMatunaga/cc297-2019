subroutine residue_calc
    use vars
    implicit none
    integer(4)              :: i, j 
    real(8)                 :: delta_x, delta_y

    do j = 2, jmax - 1
        do i = 2, imax - 1

            delta_x = meshx(i+1,j) - meshx(i,j)
            delta_y = meshy(i,j+1) - meshy(i,j)

            residue = (2.0d0/(meshx(i+1,j) - meshx(i-1,j) ) )*( (phi(i+1,j) - phi(i,j))/delta_x - &
                      (phi(i,j) - phi(i-1,j))/(meshx(i,j) - meshx(i-1,j)) ) + &
                      (2.0d0/(meshy(i,j+1) - meshy(i,j-1) ) )*( (phi(i,j+1) - phi(i,j))/delta_y - &
                      (phi(i,j) - phi(i,j-1))/(meshy(i,j) - meshy(i,j-1)) )

            ! call the relaxation method
            if (which_method == 1) call jacobi(delta_x,delta_y,N)

            ! update the solution 
            phi(i,j) = phi(i,j) - residue/N

            if(residue > max_residue) max_residue = log10(residue)

        end do 
    end do

end subroutine residue_calc