PROGRAM zeta4
    use omp_lib
    IMPLICIT NONE
    include "mpif.h" 
    INTEGER :: size, rank, error
    INTEGER :: argc
    INTEGER :: n, localn, p
    INTEGER*8 :: i
    CHARACTER(32) :: argv1, argv2
    REAL*8, dimension(:), allocatable :: vector, localvector
    REAL*8 :: localsum, pi, pi_real, test, diff, time1, time2

    ! Initialize MPI
    call MPI_Init(error)
    call MPI_Comm_size(MPI_COMM_WORLD, size, error)
    call MPI_Comm_rank(MPI_COMM_WORLD, rank, error)
     
    ! Check if number of MPI processes is a multiple of 2. Allows to run with just one process 
    if (size /=1 .and. modulo(size,2) == 1) then
        if(rank == 0) then
            PRINT*, "ABORT. Number of MPI processes has to be a multiple of 2"
        endif
        STOP
    endif

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc < 1 .or. argc > 2) then
        if (rank == 0) then
            PRINT*, "zeta4 needs an input value n=integer or a string 'utest' or a string 'vtest' and an input value n=integer"
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
    if (modulo(n,size) /= 0 .or. n<size) then
        if(rank == 0) then
            PRINT*, "ABORT. n has to be divisible by number of MPI processes"
        endif
        STOP
    endif

    localn =  n/size
    allocate(localvector(localn))

    !time1 = MPI_Wtime()
    !call CPU_TIME(time1)
    time1 = omp_get_wtime()

    ! Process 0 makes the vector
    if (rank == 0) then
        allocate(vector(n))
        !$OMP PARALLEL DO 
        do i = 1,n
            vector(i) = 1.0/(i*i)
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

    ! Deallocate allocated arrays. For some reason, localvector refuses to be deallocated
    !deallocate(localvector)
    if (argv1 == "utest") then
        if (rank == 0) then
            PRINT*, "=== Commencing Unit Test of zeta4 ==="
            pi_real = 4*atan(1.0)
            test = pi_real
            pi = sqrt(pi*6)
            diff = abs(pi - pi_real)
            PRINT*, "Calculated Pi using", size, "MPI processes, OpenMP and n =", n, ": ", pi
            PRINT*, "Difference from actual Pi : ", diff
        end if

        !call MPI_Barrier(MPI_COMM_WORLD)

        !$OMP PARALLEL  
        p = omp_get_num_threads()
        PRINT*, "Hello from process rank", rank, ", thread number", omp_get_thread_num(), "!" 
        !$OMP BARRIER
        !$OMP END PARALLEL 
        !call MPI_Barrier(MPI_COMM_WORLD)
        if (rank == 0) then
            if (diff < test) then
                PRINT*, "Unit Test Successful!"
            else
                PRINT*, "Unit Test Failed!"
            endif
        endif
    endif
    if (rank == 0) then
        ! Finish calculating pi
        pi = sqrt(pi*6)

        !call CPU_TIME(time2)
        !time2 = MPI_Wtime()
        time2 = omp_get_wtime()        
        deallocate(vector)
        if (argv1 == "utest") then
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
END PROGRAM zeta4 

