PROGRAM mach0
    IMPLICIT NONE
    INTEGER :: n, argc
    REAL :: pi
    CHARACTER(32) :: argv

    argc = COMMAND_ARGUMENT_COUNT()
    if (argc < 1) then
        PRINT*, "mach0 needs an input value n=integer"
        STOP
    end if

    call GET_COMMAND_ARGUMENT(1,argv)
    READ(argv,*) n

    call mach0Calc2(n,pi)

     PRINT*, "Pi = ", pi
END PROGRAM mach0


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
