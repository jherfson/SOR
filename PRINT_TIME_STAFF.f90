SUBROUTINE PRINT_TIME_STAFF(n,done,n_of_files_left)

IMPLICIT NONE

!passed variables

INTEGER, INTENT(IN) :: n, n_of_files_left
LOGICAL, INTENT(IN) :: done

!internal variables

CHARACTER(24)       :: ffdate
INTEGER             :: days, hours, minutes, seconds
REAL(8)             :: time2, mean_time, time_left, total_time


CALL TIME_IN_S(time2)                                           
                                                                          
mean_time =	time2 / REAL(n,KIND=8)
time_left = REAL(n_of_files_left,KIND=8) * mean_time
                                                                          
CALL FORMAT_TIME(time_left,days,hours,minutes,seconds)                    

IF(done)THEN
	CALL FDATE(ffdate)
	WRITE(*,FMT='(80("-"),2/,12X,57("*"),/,11X,A,/,12X,57("*"),/)') ' *** Calculations finished at '//TRIM(ffdate)//' ***'
	total_time = time2
	CALL FORMAT_TIME(total_time,days,hours,minutes,seconds)
	WRITE(UNIT=*,FMT='(X,A,X,I3.1,3(X,A,X,I2.1),A)') '    --> Total time:',days,'day(s),',hours,'hour(s),',minutes,'minute(s) and',seconds,' second(s) <--'
ELSE
	WRITE(UNIT=*,FMT='(X,A,X,I3.1,3(X,A,X,I2.1),A)') '    ...About',days,'day(s),',hours,'hour(s),',minutes,'minute(s) and',seconds,' second(s) left.'
END IF

WRITE(*,FMT='(A)',ADVANCE = 'NO') ''

END