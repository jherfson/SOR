SUBROUTINE TIME_IN_S(time_in_sec)

USE PORTLIB

IMPLICIT NONE

REAL(8), INTENT(OUT) :: time_in_sec

time_in_sec = TIMEF()

RETURN

END