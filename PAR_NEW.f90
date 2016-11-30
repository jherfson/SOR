SUBROUTINE PAR_NEW(absent,par_i,par_f,par_g,form,AS_term,par_len,par_new_cha)

IMPLICIT NONE

!Passed variables

CHARACTER(80), INTENT(IN)               :: form
CHARACTER(20),              PARAMETER   :: blank_str = REPEAT('0',20)                               
CHARACTER(20), INTENT(OUT)              :: par_new_cha
LOGICAL,       INTENT(IN)               :: absent
INTEGER,       INTENT(IN)               :: par_g, AS_term, par_len
REAL,          INTENT(IN)               :: par_i, par_f

!Internal variables

REAL                                    :: par_new_rea
INTEGER                                 :: par_new_int, par_len_temp

!External function

CHARACTER(80), EXTERNAL                 :: FIELD



IF(absent)THEN
	par_new_cha = ''
ELSE
	par_new_rea = par_i + REAL(AS_term - 1)*(par_f - par_i)/REAL(par_g - 1)
	IF(form(1:1).EQ.'I')THEN
		par_new_int = INT(par_new_rea)
		WRITE(par_new_cha,TRIM(form)) par_new_int
	ELSE
		WRITE(par_new_cha,TRIM(form)) par_new_rea
		par_new_cha  = FIELD(par_new_cha,1,' ')
		par_len_temp = LEN_TRIM(par_new_cha)
		par_new_cha  = blank_str(:par_len-par_len_temp)//TRIM(par_new_cha)
	END IF
END IF


END