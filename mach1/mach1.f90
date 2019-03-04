PROGRAM mach1
    IMPLICIT NONE
    include "mpif.h" 
    INTEGER :: size, rank, error
    INTEGER :: argc
    INTEGER :: n, localn
    INTEGER*8 :: i
    CHARACTER(32) :: argv
    REAL*8, dimension(:), allocatable :: vector, localvector, globalvector
    REAL*8 :: frac1, frac2, acfrac1, acfrac2, tiny
    LOGICAL :: localequal, equal 

    tiny = 1e-16

    ! Initialize MPI
    call MPI_Init(error)
    call MPI_Comm_size(MPI_COMM_WORLD, size, error)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc == 1) then
        call GET_COMMAND_ARGUMENT(1,argv)
        if (argv == "utest") then
            n = size * 4
        else
            READ(argv,*) n
        endif
    endif
    
    ! All MPI stuff (mostly) has to be done in the main program
    localn =  n/size
    allocate(localvector(localn))

    ! Process 0 makes the vector
    if (rank == 0) then
        allocate(vector(n))
        frac1 = 1.0/5.0
        frac2 = 1.0/239.0
        acfrac1 = frac1
        acfrac2 = frac2
        do i = 1,n
            if (modulo(i,2) == 1) then
                vector(i) = (4*acfrac1 - acfrac2)/(2*i - 1)
            else                     
                vector(i) = (-4*acfrac1 + acfrac2)/(2*i - 1)
            end if
            ! Instead of calculating (frac)^(2*i - 1), this is much faster
            acfrac1 = acfrac1*frac1*frac1
            acfrac2 = acfrac2*frac2*frac2
        enddo
    endif

    ! Divide the work
    call MPI_Scatter(vector, localn, MPI_DOUBLE_PRECISION, localvector, localn, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, error) 

    ! =============================
    !
    ! DO WORK HERE
    !
    ! =============================

    ! Unit Testing (also some extra juciy MPI stuff)
    if (argv == "utest") then

        ! Allocate for copy
        allocate(globalvector(n))

        if (rank == 0) then
            PRINT*, "=== Commencing Unit Test of mach1 ==="
            globalvector = vector
        endif

        ! Broadcast a copy of the original vector to all processes
        call MPI_Bcast(globalvector, n, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, error)

        localequal = .TRUE.
        ! Check if the local vector matches the corresponding elements of the original vector
        do i = 1, localn
            if (abs(localvector(i) - globalvector(rank*localn + i)) > tiny) then
                localequal = .FALSE.
            endif
        enddo

        ! Reduction to check if any process had the wrong elements
        call MPI_reduce(localequal,equal, size, MPI_LOGICAL, MPI_LAND, 0, MPI_COMM_WORLD, error)

        ! process 0 prints out if the unit thest was sucessful or not
        if (rank == 0) then
            if (equal) then
                PRINT*, "Unit Test Successful!"
            else
                PRINT*, "Unit Test Failed!"
            endif
            PRINT*, "========================================="
        endif
        deallocate(globalvector)
    endif

    ! Deallocate allocated arrays
    deallocate(localvector)
    if (rank == 0) then
        deallocate(vector)
    endif

    call MPI_Finalize(error) 
END PROGRAM mach1 

