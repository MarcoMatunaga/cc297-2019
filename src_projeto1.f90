program projeto1
    use vars
    use output_routines_proj1
    implicit none
    integer(4)           :: iter

    iter = 0

    namelist /PAR_mesh/ imax, jmax, ITE, ILE, XSF, YSF
    namelist /PAR_Method/ which_method, conv, max_iter 
    namelist /PAR_Flow/ u_inf
    namelist /PAR_Geometry/ t

    max_residue = -100.0d0

    open(2,file='inputs_proj1')
    read(2,PAR_mesh)
    read(2,PAR_Method)
    read(2,PAR_Flow)
    read(2,PAR_Geometry)
    close(2)

    call allocate_vars
    call mesh_project1

    do while(max_residue <= conv .or. iter > max_iter)
        
        call residue_calc
        iter = iter + 1

    end do

    call output_proj1
    call deallocate_vars
end program projeto1