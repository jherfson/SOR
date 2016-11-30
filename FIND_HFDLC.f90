!This subroutine finds the high freq. dielectric constant matrix of configuration 
!number 'conf' in a vector named 'vec' and assigns its value to the square matrix 
!of order 3 'hfdlc'. The argument 'conf' is optional. Whan ommited, it is assumed
!to be 1. 

!Subroutines required: 
!NUM_REC
!READ_FILE
!CONF_LIM
!SEARCH_IN_VECT

!Function required: 
!FIELD


SUBROUTINE FIND_HFDLC(vec,hfdlc,conf)

USE CONF_LIM__MODULE
USE SEARCH_IN_VECT__MODULE 

IMPLICIT NONE

!Passed variables

CHARACTER(80), DIMENSION(:),             INTENT(IN)  :: vec
CHARACTER(10), DIMENSION(3,3),           INTENT(OUT) :: hfdlc
INTEGER,                       OPTIONAL, INTENT(IN)  :: conf

!Internal variables

INTEGER                                              :: c_start, c_end, rec, i, j
                                                    
!External function                                  
                                                    
CHARACTER(10), EXTERNAL                              :: FIELD


!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                        
	CALL CONF_LIM(vec,c_start,c_end,conf)    
ELSE                                         
	CALL CONF_LIM(vec,c_start,c_end)         
END IF                                       


!Find the line where the expression in brackets is.

CALL SEARCH_IN_VECT(vec,'High frequency dielectric constant tensor',rec,start_line=c_start)


!If the above expression was not found from the line 'c_start' on, or if it was found,
!but in a line that stands outside the end of the desired configuration, assign the
!value '---' to the static dielectric constant matrix. Else, take the matrix elements
!that lay in the 5th, 6th and 7th lines after the matching line, from the columns 11-40.

IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
	hfdlc = 'not_found'
ELSE
	DO i = 1,3
		hfdlc(i,:) = (/(FIELD(vec(rec+i+4)(j*10+1:j*10+10),1,' '),j=1,3)/)
	END DO
END IF


RETURN

END



MODULE FIND_HFDLC__MODULE

INTERFACE

	SUBROUTINE FIND_HFDLC(vec,hfdlc,conf)
		CHARACTER(80), DIMENSION(:),             INTENT(IN)  :: vec  
		CHARACTER(10), DIMENSION(3,3),           INTENT(OUT) :: hfdlc
		INTEGER,                       OPTIONAL, INTENT(IN)  :: conf 
	END SUBROUTINE

END INTERFACE

END MODULE