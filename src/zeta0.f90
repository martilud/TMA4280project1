PROGRAM zeta0
    IMPLICIT NONE
    INTEGER :: n, argc
    REAL :: pi
    CHARACTER(32) :: argv

    argc = COMMAND_ARGUMENT_COUNT()
    if (argc < 1) then
        PRINT*, "zeta0 needs an input value n=integer"
        STOP
    end if

    call GET_COMMAND_ARGUMENT(1,argv)
    READ(argv,*) n

    CALL zeta0Calc(n, pi)
    PRINT*, "Pi = ", pi
END PROGRAM zeta0

SUBROUTINE zeta0Calc(n, pi)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n
    REAL, INTENT(out) :: pi
    INTEGER :: i
    pi = 0
    do i = 1,n
        pi = pi + 1.0/(i*i)
    end do
    pi = SQRT(pi*6)
END SUBROUTINE zeta0Calc

