SUBROUTINE zeta0Calc(n, pi)
    IMPLICIT NONE
    INTEGER*8, INTENT(in) :: n
    REAL*8, INTENT(out) :: pi    
    INTEGER :: i
    pi = 0
    do i = 1,n
        pi = pi + 1.0/(i*i)
    end do
    pi = DSQRT(pi*6)
END SUBROUTINE zeta0Calc

PROGRAM zeta0
    IMPLICIT NONE
    INTEGER*8 :: n
    REAL*8 :: pi
    n = 65000
    CALL zeta0Calc(n, pi)
    PRINT*, "Pi = ", pi
END PROGRAM zeta0
