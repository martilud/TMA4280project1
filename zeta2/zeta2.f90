PROGRAM zeta2
    IMPLICIT NONE
    include "mpif.h" 
    INTEGER :: size, rank, error
    INTEGER :: argc
    INTEGER :: n, localn, i
    CHARACTER(32) :: argv
    REAL, dimension(:), allocatable :: vector, localvector
    REAL :: localsum, pi, pi_real, test, diff

    ! Initialize MPI
    call MPI_Init(error)
    call MPI_Comm_size(MPI_COMM_WORLD, size, error)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
    
    ! Check if number of MPI processes is a multiple of 2 
    if (modulo(size,2) == 1) then
        if(rank == 0) then
            PRINT*, "ABORT. Number of MPI processes has to be a multiple of 2"
        endif
        STOP
    endif

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc /= 1) then
        if (rank == 0) then
            PRINT*, "zeta2 needs an input value n=integer or a string 'utest'"
        endif
        STOP
    else
        call GET_COMMAND_ARGUMENT(1,argv)
        if (argv == "utest") then
            n = size * 4
        else
            READ(argv,*) n
        endif
    endif
    
    ! Check if n is divisible by size
    if (modulo(n,size) /= 0 .or. n<size) then
        if(rank == 0) then
            PRINT*, "ABORT. n has to be divisible by number of MPI processes"
        endif
        STOP
    endif

    localn =  n/size
    allocate(localvector(localn))

    ! Process 0 makes the vector
    if (rank == 0) then
        allocate(vector(n))
        do i = 1,n
            vector(i) = 1.0/(i*i)
        enddo
    endif

    ! Divide the work
    call MPI_Scatter(vector, localn, MPI_DOUBLE_PRECISION, localvector, localn, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, error) 

    ! =============================
    ! Calculate local sum
    localsum = 0
    do i = 1, localn
       localsum = localsum + localvector(i) 
    enddo
    ! Add partial sums together to process 0
    call MPI_Reduce(localsum, pi, size, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, error) 
    ! =============================

    ! Deallocate allocated arrays. For some reason, localvector refuses to be deallocated
    !deallocate(localvector)
    if (rank == 0) then
        deallocate(vector)

        ! Finish calculating pi
        pi = SQRT(pi*6)
        
        ! Execute unit test
        if (argv == "utest") then
            PRINT*, "=== Commencing Unit Test of zeta2 ==="
            pi_real = 4*atan(1.0)
            test = pi_real
            diff = abs(pi - pi_real)
            PRINT*, "Calculated Pi using", size, "MPI processes and n =", n, ": ", pi
            PRINT*, "Difference from actual Pi : ", diff
            if (diff < test) then
                PRINT*, "Unit Test Successful!"
            else
                PRINT*, "Unit Test Failed!"
            endif
            PRINT*, "====================================="
        else
            PRINT*, "Pi = ", pi
        endif
    endif

    call MPI_Finalize(error) 
END PROGRAM zeta2 

