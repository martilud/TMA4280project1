PROGRAM mach0
    IMPLICIT NONE
    INTEGER :: n, argc, stat
    REAL :: pi
    CHARACTER(32) :: argv
    argc = COMMAND_ARGUMENT_COUNT()
    if (argc /= 1) then
        ! Invalid input
        PRINT*, "mach0 needs an input value n=integer or a string 'utest' or 'vtest' to indicate tests "
        STOP
    else 
        call GET_COMMAND_ARGUMENT(1,argv)
        ! Check for utest or vtest. If not, calculate pi and print to screen
        if (argv == "utest") then
            call mach0utest(stat)
        else if (argv == "vtest") then
            call mach0vtest()
        else
            READ(argv,*) n
            CALL mach0Calc1(n, pi)
            PRINT*, "Pi = ", pi
        endif
    endif

END PROGRAM mach0

SUBROUTINE mach0utest(stat)
    INTEGER, INTENT(out) :: stat
    REAL :: pi, pi_real, test, diff
    INTEGER :: n
    PRINT*, "=== Commencing Unit Test of mach0 ==="
    n = 3
    call mach0calc1(n,pi)
    pi_real = 4*atan(1.0)
    test = 4*(4*1.0/5.0 - 1.0/239.0)  
    diff = abs(pi - pi_real)
    PRINT*, "Calculated Pi using n = 3 : ", pi
    PRINT*, "Difference from actual Pi : ", diff
    if (diff < test) then
        PRINT*, "Unit Test Successful!"
        stat = 1
    else
        PRINT*, "Unit Test Failed!"
        stat = 0
    endif
    PRINT*, "====================================="
END SUBROUTINE mach0utest

SUBROUTINE mach0vtest()
    INTEGER :: n, k
    REAL :: pi, pi_real, start, finish
    PRINT*, "=== Commencing Verification Test of mach0 ==="
    pi_real = 4*atan(1.0)
    open(1, file = "mach0.txt")
    write(1,*) "n", CHAR(9), "difference", CHAR(9), "time"
    n = 2
    do k = 1, 24
        call CPU_TIME(start)
        call mach0calc1(n,pi)
        call CPU_TIME(finish)
        write(1,*) n, abs(pi - pi_real), finish-start
        PRINT*, "n = ", n, ". Time = ", finish-start
        n = n * 2
    enddo
    PRINT*, "Verfication Test Completed! Results saved in 'mach0.txt'"
    PRINT*, "============================================="
END SUBROUTINE mach0vtest



SUBROUTINE mach0Calc1(n, pi)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n
    REAL, INTENT(out) :: pi    
    REAL :: frac1, frac2, acfrac1, acfrac2, den, sum1, sum2
    INTEGER :: i
    frac1 = 1.0/5.0
    frac2 = 1.0/239.0
    acfrac1 = frac1
    acfrac2 = frac2
    sum1 = 0.0
    sum2 = 0.0
    do i = 1,n
        if (modulo(i,2) == 1) then
            sum1 = sum1 + acfrac1/(2*i - 1)
            sum2 = sum2 + acfrac2/(2*i - 1)
        else                     
            sum1 = sum1 - acfrac1/(2*i - 1)
            sum2 = sum2 - acfrac2/(2*i - 1)
        end if
        acfrac1 = acfrac1*frac1*frac1
        acfrac2 = acfrac2*frac2*frac2
    end do
    pi = 4 * (4*sum1 - sum2)
END SUBROUTINE mach0Calc1

SUBROUTINE mach0Calc2(n, pi)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n
    REAL, INTENT(out) :: pi    
    REAL :: frac1, frac2, den, sum1, sum2
    INTEGER :: i
    frac1 = 1.0/5.0
    frac2 = 1.0/239.0
    sum1 = 0.0
    sum2 = 0.0
    do i = 1,n
        if (modulo(i,2) == 1) then
            sum1 = sum1 + frac1**(2*i-1)/(2*i - 1)
            sum2 = sum2 + frac2**(2*i-1)/(2*i - 1)
        else                     
            sum1 = sum1 - frac1**(2*i-1)/(2*i - 1)
            sum2 = sum2 - frac2**(2*i-1)/(2*i - 1)
        end if
    end do
    pi = 4 * (4*sum1 - sum2)
END SUBROUTINE mach0Calc2
