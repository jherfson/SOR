!This function returns the content (as character) of the field 'nfield' of the
!string 'line'.	The separator between each two fields is 'delim'. More than one
!separators in a row are counted as one. The separator must be one character
!long. 

FUNCTION FIELD(line,nfield,delim)

IMPLICIT NONE

!Final value of the function

CHARACTER(*)             :: field

!Passed variables

CHARACTER(*), INTENT(IN) :: line, delim 
INTEGER,      INTENT(IN) :: nfield

!Internal variables

INTEGER                  :: pos, counter, f_start, f_end
LOGICAL                  :: not_found



!Identify fields one by one, until get to the desired field

pos = 1
DO counter = 1,nfield
	f_start = VERIFY(line(pos:),delim(1:1)) + pos - 1
	IF(f_start.EQ.pos - 1)THEN
		not_found = .TRUE.
		EXIT
	END IF
	f_end   = SCAN(line(f_start:),delim(1:1)) + f_start - 2
	pos     = f_end + 1	
END DO


IF(not_found)THEN
	field = ''
ELSE
	field = line(f_start:f_end)
	IF(f_end.EQ.(f_start - 2)) field = line(f_start:)
END IF


RETURN

END
