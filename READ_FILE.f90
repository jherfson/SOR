!This subroutine reads a file named 'filename' and assigns each ith line to the ith
!component of the vector 'vec'. If the file 'filename' cannot be found, an error
!message is displayed and the program that called this subroutine is closed. 

SUBROUTINE READ_FILE(filename,vec)

USE PORTLIB

IMPLICIT NONE

!Passed variables

CHARACTER(*),               INTENT(IN)  :: filename
CHARACTER(*), DIMENSION(*), INTENT(OUT) :: vec

!Internal variables

CHARACTER(70),              PARAMETER   :: blank_line = REPEAT(' ',70)
INTEGER                                 :: rec


! If the file 'filename' cannot be found, display an error message and stop

OPEN(UNIT=501,FILE=filename,STATUS='old',ERR=10)

rec = 0

!Read file line by line until end of file (EOF) be reached

DO WHILE(.NOT. EOF(501))

	rec = rec + 1

	READ(UNIT=501,FMT='(A)') vec(rec)
	
END DO

CLOSE(UNIT=501)

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
