SUBROUTINE FORMAT_TIME(time_in_seconds,d,h,m,s)

IMPLICIT NONE

INTEGER :: d,h,m,s
REAL(8) :: t2,t3,time_in_seconds

d  = INT(time_in_seconds/REAL(86400,KIND=8))
t2 = MOD(time_in_seconds,REAL(86400,KIND=8))
h  = INT(t2/REAL(3600,KIND=8))
t3 = MOD(t2,REAL(3600,KIND=8))
m  = INT(t3/REAL(60,KIND=8))
s  = INT(MOD(t3,REAL(60,KIND=8)))

RETURN

END