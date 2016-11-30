SUBROUTINE ASK_MODEL_NAME_SOR(model)

USE PORTLIB

IMPLICIT NONE

!Passed variable

CHARACTER(80), INTENT(OUT) :: model



WRITE(*,'(2/,X,A)') ' -> Enter the name of the file with the information about what to read'
WRITE(*,'(X,A)',ADVANCE = 'NO') ' -> Information file name: '
READ(*,*) model
CALL SLEEP(2)

END