program projeto1
    use vars_mesh
    use output_routines_proj1
    implicit none
    namelist /PAR_mesh/ imax, jmax, ITE, ILE, XSF, YSF
    open(2,file='inputs_proj1')
    read(2,PAR_mesh)
    close(2)

    call allocate_vars
    call mesh_project1
    call output_proj1
    call deallocate_vars
end program projeto1