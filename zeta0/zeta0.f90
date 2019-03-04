PROGRAM zeta0
    IMPLICIT NONE
    INTEGER :: n, argc, stat
    DOUBLE PRECISION :: pi
    CHARACTER(32) :: argv

    ! Note that this file is almost identical to mach0
    ! I've just swapped mach0 with zeta0

    ! Get input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc /= 1) then
        ! Invalid input
        PRINT*, "zeta0 needs an input value n=integer or a string 'utest' or 'vtest' to indicate tests "
        STOP
    else 
        call GET_COMMAND_ARGUMENT(1,argv)
        ! Check for utest or vtest. If not, calculate pi and print to screen
        if (argv == "utest") then
            call zeta0utest(stat)
        else if (argv == "vtest") then
            call zeta0vtest()
        else
            READ(argv,*) n
            CALL zeta0Calc(n, pi)
            PRINT*, "Pi = ", pi
        endif
    endif
END PROGRAM zeta0

SUBROUTINE zeta0utest(stat)
    INTEGER, INTENT(out) :: stat
    DOUBLE PRECISION :: pi, test, pi_real, diff
    INTEGER :: n

    PRINT*, "=== Commencing Unit Test of zeta0 ==="

    n = 3

    ! Calculate Pi
    call zeta0calc(n,pi)
    pi_real = 4*atan(1.0)
    test = pi_real - SQRT(6.0)
    diff = abs(pi - pi_real)
    PRINT*, "Calculated Pi using n = 3 : ", pi
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
END SUBROUTINE zeta0utest

SUBROUTINE zeta0vtest()
    INTEGER :: n, k
    DOUBLE PRECISION :: pi, pi_real, start, finish

    PRINT*, "=== Commencing Verification Test of zeta0 ==="

    ! Create file.
    open(1, file = "zeta0.txt")
    ! Create header for file. CHAR(9) is tab
    write(1,*) "n", CHAR(9), "difference", CHAR(9), "time"

    n = 2
    pi_real = 4*atan(1.0)

    ! Calculate Pi for different n. Save n, result and time used in file
    do k = 1, 24
        call CPU_TIME(start)
        call zeta0calc(n,pi)
        call CPU_TIME(finish)
        write(1,*) n, abs(pi - pi_real), finish-start
        PRINT*, "n = ", n, ". Time = ", finish-start
        n = n * 2
    enddo

    PRINT*, "Verfication Test Completed! Results saved in 'zeta0.txt'"
    PRINT*, "============================================="
END SUBROUTINE zeta0vtest

SUBROUTINE zeta0Calc(n, pi)
    INTEGER, INTENT(in) :: n
    DOUBLE PRECISION, INTENT(out) :: pi
    INTEGER*8 :: i
    ! Calculate Pi by the Riemann Zeta method
    pi = 0
    do i = 1,n
        pi = pi + 1.0/(i*i)
    end do
    ! pi^2 / 6 = sum
    pi = SQRT(pi*6)
END SUBROUTINE zeta0Calc
