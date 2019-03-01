program projeto1
    use vars
    use output_routines_proj1
    implicit none
    integer(4)           :: iter

    namelist /PAR_mesh/ imax, jmax, ITE, ILE, XSF, YSF
    namelist /PAR_Method/ which_method, r, conv, max_iter 
    namelist /PAR_Flow/ u_inf
    namelist /PAR_Geometry/ t

    max_residue = 1.0d0
    iter = 0

    open(2,file='inputs_proj1')
    read(2,PAR_mesh)
    read(2,PAR_Method)
    read(2,PAR_Flow)
    read(2,PAR_Geometry)
    close(2)

    call allocate_vars
    call mesh_project1
    call inco_proj1
    call boco_proj1

    do while(log10(max_residue) >= conv .and. iter < max_iter)
        
        call residue_calc
        call boco_proj1
        iter = iter + 1
        print *, iter, log10(max_residue)

    end do

    call output_proj1
    call deallocate_vars
end program projeto1