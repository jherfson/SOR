SUBROUTINE FIND_FREQ(vec,freq,conf)

USE CONF_LIM__MODULE

USE SEARCH_IN_VECT__MODULE

IMPLICIT NONE

!Passed variables

CHARACTER(*),  DIMENSION(:),          INTENT(IN)  :: vec
CHARACTER(10),                        INTENT(OUT) :: freq
INTEGER,                    OPTIONAL, INTENT(IN)  :: conf

!Internal variables

INTEGER                                           :: rec, c_start, c_end

!External variables

CHARACTER(8), EXTERNAL                            :: FIELD



!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                     
	CALL CONF_LIM(vec,c_start,c_end,conf) 
ELSE                                      
	CALL CONF_LIM(vec,c_start,c_end)      
END IF

!Find the line where the expression in brackets is.

CALL SEARCH_IN_VECT(vec,'Frequencies (cm-1)',rec,start_line=c_start)


!If the above expression was not found from the line 'c_start' on, or if it was found,
!but in a line that stands outside the end of the desired configuration, assign '---'
!to 'freq'. Else, assign to 'freq' the corresponding found value

IF((rec.EQ.0).OR.((rec+2).GT.c_end))THEN
	freq = 'not_found'
ELSE
	freq = FIELD(vec(rec+2),1,' ')
	IF(TRIM(freq).EQ.'Frequency') freq = FIELD(vec(rec+2),2,' ')
END IF


END




MODULE FIND_FREQ__MODULE

	
INTERFACE

	SUBROUTINE FIND_FREQ(vec,freq,conf)
		CHARACTER(*),  DIMENSION(:),          INTENT(IN)  :: vec
		CHARACTER(10),                        INTENT(OUT) :: freq
		INTEGER,                    OPTIONAL, INTENT(IN)  :: conf
	END SUBROUTINE
	
END INTERFACE

END MODULE