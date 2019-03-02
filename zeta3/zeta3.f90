PROGRAM zeta3
    INTEGER :: n, p, argc, stat
    REAL :: pi
    CHARACTER(32) :: argv

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc /= 1) then
        ! Invalid input
        PRINT*, "zeta3 needs an input value n=integer or a string 'utest' or 'vtest' to indicate tests "
        STOP
    else 
        call GET_COMMAND_ARGUMENT(1,argv)
        ! Check for utest or vtest. If not, calculate pi and print to screen
        if (argv == "utest") then
            call zeta3utest(stat)
        else if (argv == "vtest") then
            call zeta3vtest()
        else
            READ(argv,*) n
            p = omp_get_max_threads()
            call zeta3Calc(n, pi, p)
            PRINT*, "Pi = ", pi
        endif
    endif
END PROGRAM zeta3

SUBROUTINE zeta3utest(stat)
    INTEGER, INTENT(out) :: stat
    REAL :: pi, test, pi_real, diff
    INTEGER :: n, p

    PRINT*, "=== Commencing Unit Test of zeta3 ==="

    n = 3
    p = 2

    ! Calculate Pi
    call zeta3calc(n,pi, p)
    pi_real = 4*atan(1.0)
    test = pi_real - SQRT(6.0)
    diff = abs(pi - pi_real)
    PRINT*, "Calculated Pi using n = 3 and 2 threads : ", pi
    PRINT*, "Difference from actual Pi : ", diff

    ! Simple test. Set stat to 1 if successful, 0 if not.
    if (diff < test) then
        PRINT*, "Unit Test Successful!"
        stat = 1
    else
        PRINT*, "Unit Test Failed!"
        stat = 0
    endif
    PRINT*, "====================================="
END SUBROUTINE zeta3utest

SUBROUTINE zeta3vtest()
    INTEGER :: n, k, p
    REAL :: pi, pi_real, start, finish

    PRINT*, "=== Commencing Verification Test of zeta3 ==="

    ! Create file.
    open(1, file = "zeta3.txt")
    ! Create header for file. CHAR(9) is tab
    write(1,*) "processes", CHAR(9), "n", CHAR(9), "difference", CHAR(9), "time"

    pi_real = 4*atan(1.0)

    ! Calculate Pi for different n. Save n, result and time used in file
    do p = 1,4
        n = 2
        do k = 1, 24
            call CPU_TIME(start)
            call zeta3calc(n, pi, p)
            call CPU_TIME(finish)
            write(1,*) p, n, abs(pi - pi_real), finish-start
            PRINT*, "p = ", p, "n = ", n, ". Time = ", finish-start
            n = n * 2
        enddo
    enddo

    PRINT*, "Verfication Test Completed! Results saved in 'zeta3.txt'"
    PRINT*, "============================================="
END SUBROUTINE zeta3vtest

SUBROUTINE zeta3Calc(n, pi, p)
    INTEGER, INTENT(in) :: n, p
    REAL, INTENT(out) :: pi
    INTEGER*8 :: i
    ! Calculate Pi by the Riemann Zeta method
    pi = 0

    !$OMP PARALLEL DO REDUCTION(+:pi) NUM_THREADS(p)
    do i = 1,n
        pi = pi + 1.0/(i*i)
    end do
    !$OMP END PARALLEL DO

    ! pi^2 / 6 = sum
    pi = SQRT(pi*6)
END SUBROUTINE zeta3Calc
