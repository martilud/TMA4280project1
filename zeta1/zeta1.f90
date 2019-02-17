PROGRAM zeta1
    include "mpif.h" 
    INTEGER :: size, rank, error
    INTEGER :: argc
    INTEGER :: n, i
    CHARACTER(32) :: argv
    REAL, dimension(:), allocatable :: vector, localvector

    call MPI_Init(error)
    call MPI_Comm_size(MPI_COMM_WORLD, size, error)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc == 1) then
        call GET_COMMAND_ARGUMENT(1,argv)
        READ(argv,*) n
    endif
    allocate(localvector(n))
    if (rank == 0) then
        allocate(vector(n))
        do i = 1,n
            vector(i) = 1.0/(i*i)
        enddo
        ! FIX SIZE OF LOCALVECTOR. HOW?
        call MPI_Scatter(vector, n, MPI_REAL, localvector, n, MPI_REAL, 0, MPI_COMM_WORLD, error) 
        deallocate(vector)
    endif
    deallocate(localvector)
    call MPI_Finalize(error) 
END PROGRAM zeta1 
