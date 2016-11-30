!This subroutine counts the number of lines in the file named 'filename' and assign 
!its value to the integer variable 'num_of_rec'. If the file 'filename' cannot be 
!found, an error message is displayed and the program that called this subroutine 
!is closed. 

SUBROUTINE NUM_REC(filename,num_of_rec)

USE PORTLIB

IMPLICIT NONE

!Passed variables

CHARACTER(*), INTENT(IN)  :: filename
INTEGER,      INTENT(OUT) :: num_of_rec

!Internal variables

CHARACTER(80)             :: temp_line
CHARACTER(70), PARAMETER  :: blank_line = REPEAT(' ',70)
INTEGER                   :: rec


! If the file 'filename' cannot be found, display an error message and stop

OPEN(UNIT=500,FILE=filename,STATUS='old',ERR=10)

rec=0

!read file 'inp.dat', line by line until end of file (EOF) be reached

DO WHILE(.NOT.EOF(500))

	rec = rec + 1

	READ(UNIT=500,FMT='(A)') temp_line

END DO

CLOSE(UNIT=500)

!number of records in file
 
num_of_rec = rec


RETURN

10  WRITE(*,'(/,X,80("!"))',ADVANCE='NO')
	WRITE(*,'("!",78X,"!")',ADVANCE='NO')
	WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The following file cannot be found:                                   "
	IF(LEN_TRIM(filename).LE.67) THEN
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO') '"'//TRIM(filename(1:LEN_TRIM(filename)))//'"'//blank_line(1:68-LEN_TRIM(filename))
	ELSE
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO') '"'//filename(1:64)//'..." '
	END IF 
	WRITE(*,'("!",78X,"!")',ADVANCE='NO')
	WRITE(*,'("!",X,A,58X,"!")',ADVANCE='NO') "STOPPING PROGRAM..."
	WRITE(*,'("!",78X,"!")',ADVANCE='NO') 
	WRITE(*,'(80("!"))')

CALL SLEEP(5)

STOP


END


