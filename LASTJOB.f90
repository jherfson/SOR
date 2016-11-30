SUBROUTINE LASTJOB(last_job)

IMPLICIT NONE

!Passed variable

INTEGER, INTENT(OUT) :: last_job

!Internal variable

LOGICAL              :: temp_file_exists, res_file_exists



last_job = 0

INQUIRE(FILE="continue_interrupted_job.temp",EXIST=temp_file_exists)

IF(temp_file_exists)THEN
	CALL SYSTEM("copy continue_interrupted_job.temp continue_interrupted_job.res > temporary.temp")
	CALL SYSTEM("del continue_interrupted_job.temp temporary.temp")
END IF

INQUIRE(FILE="continue_interrupted_job.res",EXIST=res_file_exists)

IF(res_file_exists)THEN
	OPEN(300,FILE="continue_interrupted_job.res",STATUS="OLD")
	READ(300,*) last_job
	CLOSE(300)
END IF


END