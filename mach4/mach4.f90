PROGRAM mach4
    IMPLICIT NONE
    include "mpif.h" 
    INTEGER :: size, rank, error
    INTEGER :: argc
    INTEGER :: n, localn
    INTEGER*8 :: i
    CHARACTER(32) :: argv1, argv2
    REAL, dimension(:), allocatable :: vector, localvector
    REAL :: frac1, frac2, acfrac1, acfrac2
    REAL :: localsum, pi, pi_real, test, diff, time1, time2

    ! Initialize MPI
    call MPI_Init(error)
    call MPI_Comm_size(MPI_COMM_WORLD, size, error)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
    
    ! Check if number of MPI processes is a multiple of 2. Allows to run with just one process
    if (size /=1 .and. modulo(size,2) == 1) then
        if (rank == 0) then
            PRINT*, "ABORT. Number of MPI processes has to be a multiple of 2"
        endif
        STOP
    endif

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc < 1 .or. argc > 2) then
        if (rank == 0)  then
            PRINT*, "mach4 needs an input value n=integer or a string 'utest' or a string 'vtest' and an input value n=integer"
        endif
        STOP
    else
        call GET_COMMAND_ARGUMENT(1,argv1)
        if (argv1 == "utest") then
            n = size * 4
        else if (argv1 == "vtest") then
            call GET_COMMAND_ARGUMENT(2,argv2)
            READ(argv2,*) n
        else
            READ(argv1,*) n
        endif
    endif
    
    ! Check if n is divisible by size
    if (modulo(n,size) /= 0 .or. n < size) then
        if (rank == 0) then
            PRINT*, "ABORT. n has to be divisible by number of MPI processes"
        endif
        STOP
    endif
        
    localn =  n/size
    allocate(localvector(localn))
    
    time1 = MPI_Wtime()

    ! Process 0 makes the vector
    if (rank == 0) then
        allocate(vector(n))
        frac1 = 1.0/5.0
        frac2 = 1.0/239.0
        !$OMP PARALLEL DO 
        do i = 1,n
            if (modulo(i,2) == 1) then
                vector(i) = 4*frac1**(2*i-1)/(2*i-1) - frac2**(2*i-1)/(2*i - 1)
            else                     
                vector(i) = -4*frac1**(2*i-1)/(2*i-1) + frac2**(2*i-1)/(2*i - 1)
            end if
        enddo
        !$OMP END PARALLEL DO
    endif

    ! Divide the work
    call MPI_Scatter(vector, localn, MPI_DOUBLE_PRECISION, localvector, localn, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, error) 
    
    ! =============================
    ! Calculate local sum
    localsum = 0
    !$OMP PARALLEL DO REDUCTION(+:localsum)
    do i = 1, localn
       localsum = localsum + localvector(i) 
    enddo
    !$OMP END PARALLEL DO

    ! Add partial sums together to process 0
    call MPI_Reduce(localsum, pi, size, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, error) 
    ! =============================

    time2 = MPI_Wtime()

    ! Deallocate allocated arrays, for some reason, localvector refuses to be deallocated
    !deallocate(localvector)
    if (rank == 0) then
        deallocate(vector)

        ! Finish calculating pi
        pi = 4*pi

        ! Execute unit test
        if (argv1 == "utest") then
            PRINT*, "=== Commencing Unit Test of zeta2 ==="
            pi_real = 4*atan(1.0)
            test = 4*(4*1.0/5.0 - 1.0/239.0)  
            diff = abs(pi - pi_real)
            PRINT*, "Calculated Pi using", size, "MPI processes and n =", n, ": ", pi
            PRINT*, "Difference from actual Pi : ", diff
            if (diff < test) then
                PRINT*, "Unit Test Successful!"
            else
                PRINT*, "Unit Test Failed!"
            endif
            PRINT*, "====================================="
        else if (argv1 == "vtest") then
            pi_real = 4*atan(1.0)
            diff = abs(pi-pi_real)
            PRINT*, diff, time2-time1 
        else
            PRINT*, "Pi = ", pi
        endif
    endif
    call MPI_Finalize(error) 
END PROGRAM mach4 

