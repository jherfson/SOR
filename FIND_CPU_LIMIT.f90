!Subroutine required: 
!SEARCH_IN_VECT

SUBROUTINE FIND_CPU_LIM(vec,CPU_lim)

USE SEARCH_IN_VECT__MODULE 

IMPLICIT NONE

!Passed variables

CHARACTER(*), DIMENSION(:), INTENT(IN)  :: vec
CHARACTER(1),               INTENT(OUT) :: CPU_lim

!Internal variable

INTEGER                                 :: rec

	

!Find the line where the wanted message is
CALL SEARCH_IN_VECT(vec,'CPU limit has been exceeded - restart optimisation',rec)

!If the message was found, assign '1'. Else, assign '0'.

IF(rec.EQ.0)THEN
	CPU_lim = '0'
ELSE
	CPU_lim = '1'
END IF


RETURN

END




MODULE FIND_CPU_LIM__MODULE

	
INTERFACE

	SUBROUTINE FIND_CPU_LIM(vec,CPU_lim)
		CHARACTER(*),  DIMENSION(:), INTENT(IN)  :: vec
		CHARACTER(1),                INTENT(OUT) :: CPU_lim
	END SUBROUTINE
	
END INTERFACE

END MODULE