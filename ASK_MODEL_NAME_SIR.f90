SUBROUTINE ASK_MODEL_NAME_SIR(model)

USE PORTLIB

IMPLICIT NONE

!Passed variable

CHARACTER(80), INTENT(OUT) :: model



WRITE(*,'(2/,X,A)') ' -> Enter the name of the file to be used as model to generate the input files'
WRITE(*,'(X,A)',ADVANCE = 'NO') ' -> Model file name: '
READ(*,*) model
CALL SLEEP(2)

END