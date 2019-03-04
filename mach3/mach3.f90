PROGRAM mach3
    IMPLICIT NONE
    INTEGER :: n, p, argc, stat
    REAL*8 :: pi
    CHARACTER(32) :: argv

    ! Get Input
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc /= 1) then
        ! Invalid input
        PRINT*, "mach3 needs an input value n=integer or a string 'utest' or 'vtest' to indicate tests"
        STOP
    else 
        call GET_COMMAND_ARGUMENT(1,argv)
        ! Check for utest or vtest. If not, calculate pi and print to screen
        if (argv == "utest") then
            call mach3utest(stat)
        else if (argv == "vtest") then
            call mach3vtest()
        else
            READ(argv,*) n
            p = 2
            CALL mach3Calc2(n, pi, p)
            PRINT*, "Pi = ", pi
        endif
    endif
END PROGRAM mach3

SUBROUTINE mach3utest(stat)
    INTEGER, INTENT(out) :: stat
    REAL*8 :: pi, pi_real, test, diff
    INTEGER :: n, p

    PRINT*, "=== Commencing Unit Test of mach3 ==="

    n = 3
    p = 2

    ! Calculate Pi
    call mach3calc2(n,pi, p)
    pi_real = 4*atan(1.0)
    diff = abs(pi - pi_real)
    PRINT*, "Calculated Pi using n = 3 and 2 threads : ", pi
    PRINT*, "Difference from actual Pi : ", diff


    ! Testing criterion. See report
    test = 4*(4*1.0/5.0 - 1.0/239.0) - pi_real 
    
    ! Simple test. Set stat to 1 if successful, 0 if not.
    if (diff < test) then
        PRINT*, "Unit Test Successful!"
        stat = 1
    else
        PRINT*, "Unit Test Failed!"
        stat = 0
    endif
    PRINT*, "====================================="
END SUBROUTINE mach3utest

SUBROUTINE mach3vtest()
    use omp_lib
    INTEGER :: n, k, p
    REAL*8 :: pi, pi_real, start, finish

    PRINT*, "=== Commencing Verification Test of mach3 ==="

    ! Create file
    open(1, file = "mach3.txt")
    ! Create header for file. Char(9) is tab
    write(1,*) "processes", CHAR(9), "n", CHAR(9), "difference", CHAR(9), "time"
    pi_real = 4*atan(1.0)

    ! Calculate Pi for different n. Save n, result and time used in file
    do p = 1,4
        n = 2
        do k = 1, 24
            !call CPU_TIME(start)
            start = omp_get_wtime()
            call mach3calc2(n,pi, p)
            !call CPU_TIME(finish)
            finish = omp_get_wtime()
            write(1,*) p, n, abs(pi - pi_real), finish-start
            PRINT*, "p =", p, "n = ", n, ". Time = ", finish-start
            n = n * 2
        enddo
    enddo
    PRINT*, "Verfication Test Completed! Results saved in 'mach3.txt'"
    PRINT*, "============================================="
END SUBROUTINE mach3vtest

SUBROUTINE mach3Calc1(n, pi)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n
    REAL*8, INTENT(out) :: pi    
    REAL*8 :: frac1, frac2, acfrac1, acfrac2, sum1, sum2
    INTEGER*8 :: i

    ! Calculate Pi by the Machin formula.
    ! This version is much faster than mach3Calc2

    frac1 = 1.0/5.0
    frac2 = 1.0/239.0

    ! "Actual" fraction
    acfrac1 = frac1
    acfrac2 = frac2
    sum1 = 0.0
    sum2 = 0.0

    do i = 1,n
        ! Instead of calculating (-1)^(i-1), the modulo is faster for large i
        if (modulo(i,2) == 1) then
            sum1 = sum1 + acfrac1/(2*i - 1)
            sum2 = sum2 + acfrac2/(2*i - 1)
        else                     
            sum1 = sum1 - acfrac1/(2*i - 1)
            sum2 = sum2 - acfrac2/(2*i - 1)
        end if
        ! Instead of calculating (frac)^(2*i - 1), this is much faster
        acfrac1 = acfrac1*frac1*frac1
        acfrac2 = acfrac2*frac2*frac2
    end do

    ! pi/4 = 4*sum1 - sum2
    pi = 4 * (4*sum1 - sum2)
END SUBROUTINE mach3Calc1

SUBROUTINE mach3Calc2(n, pi, p)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n, p
    REAL*8, INTENT(out) :: pi    
    REAL*8 :: frac1, frac2,  sum1, sum2
    INTEGER*8 :: i

    ! Calculate Pi by the Machiin formula.

    frac1 = 1.0/5.0
    frac2 = 1.0/239.0
    sum1 = 0.0
    sum2 = 0.0
    !$OMP PARALLEL DO REDUCTION(+:sum1,sum2) NUM_THREADS(p)
    do i = 1,n
        if (modulo(i,2) == 1) then
            sum1 = sum1 + frac1**(2*i-1)/(2*i - 1)
            sum2 = sum2 + frac2**(2*i-1)/(2*i - 1)
        else                     
            sum1 = sum1 - frac1**(2*i-1)/(2*i - 1)
            sum2 = sum2 - frac2**(2*i-1)/(2*i - 1)
        end if
    end do
    !$OMP END PARALLEL DO
    pi = 4 * (4*sum1 - sum2)
END SUBROUTINE mach3Calc2
