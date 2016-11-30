

!  05/11/2012 - Created.
!  15/11/2012 - Error warnings for absent number of messages and unmatching number of messages added.
!  15/11/2012 - Now an error warning is displayed when nothing is asked to be read.


SUBROUTINE ERROR_SOR(description)

IMPLICIT NONE

!Passed variable

CHARACTER(*), INTENT(IN) :: description



WRITE(*,'(/,X,80("!"))',ADVANCE='NO')
WRITE(*,'("!",78X,"!")',ADVANCE='NO')

SELECT CASE (description)
	CASE("no_file_name")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The line containing the file name pattern (#FILE_NAME) cannot be      "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "found. Check for a possible misspell.                                 " 
	CASE("name_not_before_#END")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The line with the file name pattern (#FILE_NAME) must occur before    "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "the line #END.                                                        "
	CASE("var_not_before_#END")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The first occurence of each variable label $i must be before the line "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "#END.                                                                 "
	CASE("num_of_obser_absent")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of observables to be read is absent.                       "
	CASE("number_of_obser_dont_match")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of observables found is not the same it was meant to be.   "
	CASE("num_of_err_absent")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of types of observable percentual errors to be read is     "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "absent.                                                               "
	CASE("number_of_err_dont_match")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of types of observable percentual errors found is not the  "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "same it was meant to be.                                              "
	CASE("num_of_mes_absent")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of types of messages to be read is absent.                 "
	CASE("number_of_mes_dont_match")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: The number of types of messages found is not the same it was meant to "
		WRITE(*,'("!",8X,A,"!")',ADVANCE='NO')       "be.                                                                   "
	CASE("nothing_to_read")
		WRITE(*,'("!",X,A,"!")',ADVANCE='NO') "ERROR: Nothing was asked to be read.                                         "

END SELECT

WRITE(*,'("!",78X,"!")',ADVANCE='NO')
WRITE(*,'("!",X,A,58X,"!")',ADVANCE='NO') "STOPPING PROGRAM..."
WRITE(*,'("!",78X,"!")',ADVANCE='NO') 
WRITE(*,'(80("!"))')

CALL SLEEP(5)

STOP

END