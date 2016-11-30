SUBROUTINE FIND_DEF_FREQ(vec,def_freq,conf)

USE CONF_LIM__MODULE

USE SEARCH_IN_VECT__MODULE

IMPLICIT NONE

!Passed variables

CHARACTER(*),  DIMENSION(:), INTENT(IN)  :: vec
CHARACTER(10), DIMENSION(:), INTENT(OUT) :: def_freq
INTEGER,       OPTIONAL,     INTENT(IN)  :: conf

!Internal variables

INTEGER                                  :: rec, rec_aux, c_start, c_end, n, i
INTEGER                                  :: number_of_lines, number_of_freq
LOGICAL                                  :: six_in_final_line


!External variables

CHARACTER(10), EXTERNAL                  :: FIELD



!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                     
	CALL CONF_LIM(vec,c_start,c_end,conf) 
ELSE                                      
	CALL CONF_LIM(vec,c_start,c_end)      
END IF

!Find the line where the expression in brackets is.

CALL SEARCH_IN_VECT(vec,'Vibrational Frequency Calculation',rec,start_line=c_start)
CALL SEARCH_IN_VECT(vec,'Frequency',rec_aux,start_line=rec+1)

!In general, there are six frequencies in each line started by 'Frequency'

six_in_final_line = .TRUE.
number_of_freq    = SIZE(def_freq)
number_of_lines   = INT((number_of_freq+3)/6)


!Verify if there are indeed six frequencies in the last line. 

IF (MOD(number_of_freq,6).EQ.3) six_in_final_line = .FALSE.

DO  n = 1,number_of_lines
	DO i = 1,6
		IF((n.EQ.number_of_lines).AND.(.NOT.six_in_final_line).AND.(i.GE.4)) EXIT
		def_freq( 6*(n-1) + i ) = FIELD(vec( rec_aux + 6*(n-1) ),i+1,' ')
	END DO
END DO

END




MODULE FIND_DEF_FREQ__MODULE

	
INTERFACE

	SUBROUTINE FIND_DEF_FREQ(vec,def_freq,conf)
		CHARACTER(*),  DIMENSION(:), INTENT(IN)  :: vec
		CHARACTER(10), DIMENSION(:), INTENT(OUT) :: def_freq
		INTEGER,       OPTIONAL,     INTENT(IN)  :: conf
	END SUBROUTINE
	
END INTERFACE

END MODULE