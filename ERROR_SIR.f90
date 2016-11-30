SUBROUTINE ERROR_SIR(description)

IMPLICIT NONE

!Passed variable

CHARACTER(*), INTENT(IN) :: description



WRITE(*,'(/,X,80("!"))',ADVANCE='NO')
WRITE(*,'("!",78X,"!")',ADVANCE='NO')

SELECT CASE (description)
	CASE("abc")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The program 'gulp.exe' can't be found. Copy it to the same directory  "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "where this program is running and try again.                          " 
	CASE("no_file_name")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The line containing the file name pattern (#FILE_NAME) cannot be      "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "found. Check for a possible misspell.                                 " 
	CASE("name_not_before_#END")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The line with the file name pattern (#FILE_NAME) must occur before    "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "the line #END.                                                        "
	CASE("var_not_before_#END")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The first occurence of each variable label $i must be before the line "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "#END.                                                                 "
END SELECT

WRITE(*,'("!",78X,"!")',ADVANCE='NO')
WRITE(*,'("!",X,A,58X,"!")',ADVANCE='NO') "STOPPING PROGRAM..."
WRITE(*,'("!",78X,"!")',ADVANCE='NO') 
WRITE(*,'(80("!"))')

CALL SLEEP(5)

STOP

END