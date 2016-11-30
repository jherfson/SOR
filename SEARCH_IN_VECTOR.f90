!This subroutine finds the expression 'expr' in the vector 'vec'. The number of the
!first component (of 'vec') where the expression is matched is assigned to the 
!integer variable 'rec', while the corresponding string is assigned to the variable 
!'line'.

!The search will start from the component 'start_line'. The arguments 'line', 'rec' 
!and 'start_line' are optionals. If 'start_line' is ommited, the search will start 
!from the first component. Note that if both 'line' and 'rec' are ommited, there is
!no point in calling this subroutine! If the expression is not matched at all, the 
!variable 'rec' returns 0, while 'line' returns a blank string.

SUBROUTINE SEARCH_IN_VECT(vec,expr,rec,line,start_line)

IMPLICIT NONE

!Passed variables

CHARACTER(*),  DIMENSION(:),          INTENT(IN)  :: vec       
CHARACTER(*),                         INTENT(IN)  :: expr      
CHARACTER(*),               OPTIONAL, INTENT(OUT) :: line      
INTEGER,                    OPTIONAL, INTENT(OUT) :: rec       
INTEGER,                    OPTIONAL, INTENT(IN)  :: start_line
                                         
!Internal variables                      
                                         
INTEGER                                           :: on_off, i



!If the optional argument 'start_line' is present, the search will begin from it.
!If it does not, the search will begin from the first component.

i = 1
IF(PRESENT(start_line)) i = start_line


on_off = 0

!If there is not a match yet, keep searching the expression until the end of the 
!vector be reached.

DO WHILE((i.LE.SIZE(vec)).AND.(on_off.EQ.0))
	on_off=INDEX(vec(i),expr)
	i = i + 1
END DO

!If the number of the matching line (vector component) is asked, assign its value

IF(PRESENT(rec))THEN
	IF(on_off.EQ.0)THEN
		!If there is no match, set rec = 0
		rec = 0
	ELSE
		rec = i - 1
	END IF
END IF

!If the content of the matching line (vector component) is asked, assign its value

IF(PRESENT(line))THEN
	IF(on_off.EQ.0)THEN
		!If there is no match, set line = ''
		line = ''
	ELSE
		line = vec(i-1)
	END IF
END IF


RETURN

END


MODULE SEARCH_IN_VECT__MODULE
	
INTERFACE

	SUBROUTINE SEARCH_IN_VECT(vec,expr,rec,line,start_line)
		CHARACTER(*),  DIMENSION(:),          INTENT(IN)  :: vec       
		CHARACTER(*),                         INTENT(IN)  :: expr      
		CHARACTER(*),               OPTIONAL, INTENT(OUT) :: line      
		INTEGER,                    OPTIONAL, INTENT(OUT) :: rec       
		INTEGER,                    OPTIONAL, INTENT(IN)  :: start_line
	END SUBROUTINE
	
END INTERFACE

END MODULE