!This subroutine finds the cell parameters and the cell volume of configuration 
!number 'conf' in a vector named 'vec' and assigns their values to the vectors
!'cell_full' (corresponding to full cell) and 'cell_prim' (primitive cell). Besides,
!the percent errors are assigned to the vector 'cell_err'. The first six components
!are corresponding to the cell parameters in themselves (a,b,c,alpha,beta and gamma,
!respectively), while the seventh component is regarding the volume. The argument 
!'conf' is optional. Whan ommited, it is assumed to be 1. 

!Subroutines required: 
!CONF_LIM
!SEARCH_IN_VECT

!Function required: 
!FIELD


SUBROUTINE FIND_CELL(vec,cell_err,conf,cell_full,cell_prim)

USE CONF_LIM__MODULE
USE SEARCH_IN_VECT__MODULE 

IMPLICIT NONE

!Passed variables

CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec
CHARACTER(10), DIMENSION(7), OPTIONAL, INTENT(OUT) :: cell_full,cell_prim,cell_err
INTEGER,                     OPTIONAL, INTENT(IN)  :: conf

!Internal variables

CHARACTER(14), DIMENSION(7)                        :: temp_cell_full,temp_cell_prim
INTEGER                                            :: c_start, c_end, rec, i

!External function

CHARACTER(14), EXTERNAL                            :: FIELD


	
!Find the configuration limits. If the configuration is not suplied, assume conf = 1. 

IF(PRESENT(conf))THEN                     
	CALL CONF_LIM(vec,c_start,c_end,conf) 
ELSE                                      
	CALL CONF_LIM(vec,c_start,c_end)      
END IF                                    

!If the primitive cell parameters are required, search them

IF(PRESENT(cell_prim))THEN

	!Find the line where the expression in brackets is.
	
	CALL SEARCH_IN_VECT(vec,'Final cell parameters and derivatives',rec,start_line=c_start)

	!If the above expression was not found from the line 'c_start' on, or if it was found,
	!but in a line that stands outside the end of the desired configuration, assign the
	!value '---' to the primitive cell vector. Else, take the cell parameters that lay 
	!in the 5th-8th lines after the matching line, in the second field. The volume is in
	!the fifth field in the 11th line after the matching one.
	
	IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
		cell_prim = 'not_found'
	ELSE
		temp_cell_prim = (/(FIELD(vec(rec+2+i),2,' '),i=1,6),FIELD(vec(rec+11),5,' ')/)
		cell_prim = (/(temp_cell_prim(i)(:10),i=1,7)/)
	END IF
END IF


IF(PRESENT(cell_full).OR.PRESENT(cell_err))THEN
	!Find the line where the expression in brackets is.
	CALL SEARCH_IN_VECT(vec,'Comparison of initial and final structures',rec,start_line=c_start)
END IF

!If the above expression was not found from the line 'c_start' on, or if it was found,
!but in a line that stands outside the end of the desired configuration, assign the 
!value '---' to the full cell and to the percent error vectors. Else, take the cell 
!parameters that lay in the 6th-11th lines after the matching line, in the 28th-41th
!columns. The volume is in the same columns, but in the 5th line after the matching 
!one.

!If the full cell parameters are required, assign their values

If(PRESENT(cell_full))THEN

	IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
		cell_full = 'not_found'
	ELSE
		temp_cell_full = (/(vec(rec+5+i)(28:41),i=1,6),vec(rec+5)(28:41)/)
		temp_cell_full = (/(FIELD(temp_cell_full(i),1,' '),i=1,7)/)
		cell_full = (/(temp_cell_full(i)(:10),i=1,7)/)
	END IF

END IF

!If the full cell parameters errors are required, assign their values

If(PRESENT(cell_err))THEN

	IF((rec.EQ.0).OR.(rec.GT.c_end))THEN
		cell_err = 'not_found'
	ELSE
		cell_err = (/(vec(rec+5+i)(68:75),i=1,6),vec(rec+5)(68:75)/)
		cell_err = (/(FIELD(cell_err(i),1,' '),i=1,7)/)
	END IF	

END IF

RETURN

END



!Interface needed due to the optional arguments

MODULE FIND_CELL__MODULE

INTERFACE

	SUBROUTINE FIND_CELL(vec,cell_err,conf,cell_full,cell_prim)
		CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec                
		CHARACTER(10), DIMENSION(7), OPTIONAL, INTENT(OUT) :: cell_full,cell_prim,cell_err
		INTEGER,                     OPTIONAL, INTENT(IN)  :: conf               
	END SUBROUTINE

END INTERFACE

END MODULE