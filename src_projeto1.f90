program projeto1
    use vars
    use output_routines_proj1
    implicit none

    namelist /PAR_mesh/ imax, jmax, ITE, ILE, XSF, YSF
    namelist /PAR_Flow/ u_inf
    namelist /PAR_Geometry/ t

    open(2,file='inputs_proj1')
    read(2,PAR_mesh)
    close(2)

    call allocate_vars
    call mesh_project1
    call output_proj1
    call deallocate_vars
end program projeto1