PROGRAM zeta0utest
    IMPLICIT NONE
    INTEGER :: n
    REAL :: pi, pi_real, diff
    INTEGER :: i
    n = 3
    pi_real = 4*atan(1.0)
    pi = 0
    do i = 1,n
        pi = pi + 1.0/(i*i)
    end do
    pi = SQRT(pi*6)
    diff = abs(pi - pi_real)
    if(diff < pi_real) then
        PRINT*, "Calculated pi using method 1 is: ", pi
        PRINT*, "Difference from real pi is: ", diff
        PRINT*, "Unit test of zeta0 successful"
    else
        PRINT*, "Unit test of zeta0 failed"
    endif
END PROGRAM zeta0utest
