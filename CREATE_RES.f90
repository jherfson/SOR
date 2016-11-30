SUBROUTINE CREATE_RES(algar_cha,counter)

IMPLICIT NONE

!Passed variables

CHARACTER, INTENT(IN) :: algar_cha
INTEGER,   INTENT(IN) :: counter



OPEN(300,FILE="continue_interrupted_job.temp",STATUS="REPLACE")
WRITE(300,'(I'//TRIM(algar_cha)//'.'//TRIM(algar_cha)//')') counter
CLOSE(300)

CALL SYSTEM("copy continue_interrupted_job.temp continue_interrupted_job.res > temporary.temp")
CALL SYSTEM("del continue_interrupted_job.temp temporary.temp")




END