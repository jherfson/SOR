!Subroutines required: 
!CONF_LIM
!SEARCH_IN_VECT

!Function required: 
!FIELD


SUBROUTINE FIND_DEF_ENTROPY(vec,def_entropy,conf)

USE CONF_LIM__MODULE
USE SEARCH_IN_VECT__MODULE 

IMPLICIT NONE

!Passed variables

CHARACTER(*),            DIMENSION(:), INTENT(IN)    :: vec
INTEGER,       OPTIONAL,               INTENT(IN)    :: conf
CHARACTER(10), 	                       INTENT(OUT)   :: def_entropy

!Internal variables

CHARACTER(80)                                        :: temp
INTEGER                                              :: c_start, c_end, rec

!External function

CHARACTER(20), EXTERNAL                              :: FIELD


	
!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                    
	CALL CONF_LIM(vec,c_start,c_end,conf)
ELSE                                     
	CALL CONF_LIM(vec,c_start,c_end)     
END IF

!Find the line where the wanted value(s) is(are)

CALL SEARCH_IN_VECT(vec,'Vibrational properties (for region 1)',rec,start_line=c_start)

!If the above expression was not found from the line 'c_start' on, or if it was found,
!but in a line that stands outside the end of the desired configuration, assign '---'
!to the wanted variable(s). Else, assign to each variable the corresponding found value

IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
	def_entropy = 'not_found'
ELSE
	temp = vec(rec+3)
	def_entropy = FIELD(temp,3,' ')
	IF(def_entropy(2:2).EQ.'-') def_entropy = 'not_found'
END IF	


RETURN

END



!Interface needed due to the optional arguments

MODULE FIND_DEF_ENTROPY__MODULE

INTERFACE

	SUBROUTINE FIND_DEF_ENTROPY(vec,def_entropy,conf)
		CHARACTER(*),            DIMENSION(:), INTENT(IN)    :: vec
		INTEGER,       OPTIONAL,               INTENT(IN)    :: conf
		CHARACTER(10), 	                       INTENT(OUT)   :: def_entropy
	END SUBROUTINE
END INTERFACE

END MODULE