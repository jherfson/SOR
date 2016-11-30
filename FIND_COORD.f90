SUBROUTINE FIND_COORD(vec,axis,atom,coord_err,conf,coord_val)

USE CONF_LIM__MODULE 
USE SEARCH_IN_VECT__MODULE


IMPLICIT NONE

!Passed variables

CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec       
CHARACTER(10),               OPTIONAL, INTENT(OUT) :: coord_val 
CHARACTER(10),               OPTIONAL, INTENT(OUT) :: coord_err 
CHARACTER(1),                          INTENT(IN)  :: axis      
INTEGER,                     OPTIONAL, INTENT(IN)  :: conf      
INTEGER,                               INTENT(IN)  :: atom      

!Internal variables

CHARACTER(80)                                      :: line
CHARACTER(4)                                       :: atom_c
INTEGER                                            :: rec, c_start, c_end
                                                  
!External function                                
                                                  
CHARACTER(10), EXTERNAL                            :: FIELD


!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                    
	CALL CONF_LIM(vec,c_start,c_end,conf)
ELSE                                     
	CALL CONF_LIM(vec,c_start,c_end)     
END IF                                   

!Convert the number of the atom from interger to character

WRITE(atom_c, FMT='(I4.1)') atom

!Trick to clean the leadind blanks

atom_c = FIELD(atom_c,1,' ')

!Find the line where the wanted value(s) is(are)

CALL SEARCH_IN_VECT(vec,TRIM(atom_c)//' '//axis,rec,line,c_start)

!If the above expression was not found from the line 'c_start' on, or if it was found,
!but in a line that stands outside the end of the desired configuration, assign '---'
!to the wanted variable(s). Else, assign to each variable the corresponding found value

IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
	IF(PRESENT(coord_val)) coord_val = 'not_found'
	IF(PRESENT(coord_err)) coord_err = 'not_found'
ELSE
	IF(PRESENT(coord_val)) coord_val = FIELD(line,4,' ')
	IF(PRESENT(coord_err)) coord_err = FIELD(line,7,' ')
END IF


RETURN


END SUBROUTINE






MODULE FIND_COORD__MODULE

INTERFACE

	SUBROUTINE FIND_COORD(vec,axis,atom,coord_err,conf,coord_val)
		CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec      
		CHARACTER(10),               OPTIONAL, INTENT(OUT) :: coord_val
		CHARACTER(10),               OPTIONAL, INTENT(OUT) :: coord_err
		CHARACTER(1),                          INTENT(IN)  :: axis     
		INTEGER,                     OPTIONAL, INTENT(IN)  :: conf     
		INTEGER,                               INTENT(IN)  :: atom     
	END SUBROUTINE

END INTERFACE

END MODULE