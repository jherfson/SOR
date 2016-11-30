

! 05/11/2012 - Created.
! 08/11/2012 - Now the Helmholtz energy and the defect Helmholtz energy can be searched too.
! 18/12/2012 - Now the gradient's norm, defect gradient's norm and free energy can be searched too.
! 18/12/2012 - Now messages can be searched too.
! 18/12/2012 - Now it is possible to search for the message that says that the CPU time limit has been reached.
! 18/12/2012 - Problems	regarding spacing in the results file were fixed.
! 23/12/2012 - Now, when an output file is not found, the program goes on instead of stopping.
! 23/12/2012 - The subroutine WARNING_SOR has been included.
! 30/12/2012 - The info file format has been changed. Now it is more uniform and intuitive.
! 30/12/2012 - Errors in the sections of the info file are now searched by the subroutines CHECK_VAR_SECTION,
!              CHECK_MES_SECTION, CHECK_PERC_SECTION, CHECK_OBSER_SECTION and CHECK_NAME_PATT_SECTION.
! 30/12/2012 - In each section, the data are now got by its respective subroutine: GET_MESSAGES, GET_VARIABLES,
!              GET_OBSERVABLES, GET_NAME_PATTERN or GET_PERCENT_ERRORS.
! 30/12/2012 - Errors regarding wrong formats in variables section have been trapped.   


PROGRAM SOR  

USE ERROR_SOR__MODULE
USE WARNING_SOR__MODULE
USE FIRST_LINE__MODULE
USE OUTPUT_READER__MODULE
USE SEARCH_IN_VECT__MODULE
USE GET_MESSAGES__MODULE
USE GET_VARIABLES__MODULE
USE GET_OBSERVABLES__MODULE
USE GET_NAME_PATTERN__MODULE
USE GET_PERCENT_ERRORS__MODULE
USE CHECK_VAR_SECTION__MODULE
USE CHECK_MES_SECTION__MODULE
USE CHECK_PERC_SECTION__MODULE
USE CHECK_OBSER_SECTION__MODULE
USE CHECK_NAME_PATT_SECTION__MODULE
USE CHECK_RESUL_FILE_SECTION__MODULE

IMPLICIT NONE

!Internal variables

CHARACTER(80),  DIMENSION(:),   ALLOCATABLE :: line, obser_cha, perc_cha, mes_cha
CHARACTER(20),  DIMENSION(3)                :: par_new_cha
CHARACTER(80),  DIMENSION(:),   ALLOCATABLE :: form
CHARACTER(80)                               :: name, name_temp, algar_cha, model, results_file
CHARACTER(24)                               :: ffdate
CHARACTER(20),                  PARAMETER   :: blank_str = REPEAT('0',20)                               
CHARACTER	                                :: perc_switch, obser_switch, mes_switch
REAL,           DIMENSION(:),   ALLOCATABLE :: par_i, par_f
INTEGER,        DIMENSION(:),   ALLOCATABLE :: perc_int, perc_conf, obser_int, obser_conf, mes_int, mes_conf, par_g, pos_i_name, pos_f_name, par_len
INTEGER,        DIMENSION(4)                :: grid
INTEGER                                     :: S, i, j, k, nrec, n_of_files, algar, time1, div, counter, error 
INTEGER                                     :: num_of_obser, num_of_perc, num_of_mes, num_of_var

!External function

CHARACTER(80), EXTERNAL                     :: FIELD


!!!!!!!!!!!!!!!!!!!!!!
!Begin of the program!
!!!!!!!!!!!!!!!!!!!!!!


!Set a black background and green letters
!Print headline, ask the model file name, clear the display and print the headline again

CALL SYSTEM('color 0A')
CALL HEADLINE_SOR
CALL ASK_MODEL_NAME_SOR(model)
CALL SYSTEM('cls')
CALL HEADLINE_SOR

!Calculate the number of lines in the model file and assign each line to a component of 
!the vector "line"

CALL NUM_REC(TRIM(model),nrec,error)
IF(error.EQ.1) CALL ERROR_SOR("file_not_found",model)
ALLOCATE(line(nrec))
CALL READ_FILE(TRIM(model),line,error)
IF(error.EQ.1) CALL ERROR_SOR("file_not_found",model)



!Check if everything is alright in variables section. If so, return the number of variables.

CALL CHECK_VAR_SECTION(line,num_of_var)
ALLOCATE(par_i(num_of_var), par_f(num_of_var), par_g(num_of_var), form(num_of_var), par_len(num_of_var))
ALLOCATE(pos_i_name(num_of_var), pos_f_name(num_of_var))

!Check if the everything is alright in the name_pattern section. If so, pick the pattern.

CALL CHECK_NAME_PATT_SECTION(line,name)

!Check if the everything is alright in the results_file section. If so, pick the name.

CALL CHECK_RESUL_FILE_SECTION(line,results_file)

!Check if everything is alright in observables section. If so, return the number of observables.

CALL CHECK_OBSER_SECTION(line,num_of_obser)
IF(num_of_obser.NE.0) ALLOCATE(obser_cha(num_of_obser),obser_int(num_of_obser),obser_conf(num_of_obser))

!Check if everything is alright in percent_errors section. If so, return the number of types of percent errors.

CALL CHECK_PERC_SECTION(line,num_of_perc)
IF(num_of_perc.NE.0) ALLOCATE(perc_cha(num_of_perc),perc_int(num_of_perc),perc_conf(num_of_perc))

!Check if everything is alright in messages section. If so, return the number of types of messages.

CALL CHECK_MES_SECTION(line,num_of_mes)
IF(num_of_mes.NE.0) ALLOCATE(mes_cha(num_of_mes),mes_int(num_of_mes),mes_conf(num_of_mes))


!Get the following parameters for each variable: initial value, final value, grid, format and width

CALL GET_VARIABLES(line,par_i,par_f,par_g,form,par_len)

!Mark the beginning and the end of each variable label in the name pattern

CALL GET_NAME_PATTERN(name,pos_i_name,pos_f_name,par_len)

!Get information about what to read from each output file

IF(num_of_obser.NE.0) CALL GET_OBSERVABLES(line,obser_cha,obser_int,obser_conf)
IF(num_of_perc.NE.0)  CALL GET_PERCENT_ERRORS(line,perc_cha,perc_int,perc_conf)
IF(num_of_mes.NE.0)   CALL GET_MESSAGES(line,mes_cha,mes_int,mes_conf)


obser_switch = '1'
perc_switch  = '1'
mes_switch   = '1'
IF(num_of_obser.EQ.0) obser_switch = '0'
IF(num_of_perc.EQ.0)  perc_switch  = '0'
IF(num_of_mes.EQ.0)   mes_switch   = '0'


!Print the first line of the data file

SELECT CASE(perc_switch//obser_switch//mes_switch)
	CASE('111')
		CALL FIRST_LINE(TRIM(results_file),par_len,perc_cha,perc_int,perc_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)
	CASE('110')
		CALL FIRST_LINE(TRIM(results_file),par_len,perc_cha,perc_int,perc_conf,obser_cha,obser_int,obser_conf)
	CASE('101')
		CALL FIRST_LINE(TRIM(results_file),par_len,perc_cha,perc_int,perc_conf,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
	CASE('011')
		CALL FIRST_LINE(TRIM(results_file),par_len,obser_cha=obser_cha,obser_int=obser_int,obser_conf=obser_conf,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
	CASE('100')
		CALL FIRST_LINE(TRIM(results_file),par_len,perc_cha,perc_int,perc_conf)
	CASE('001')
		CALL FIRST_LINE(TRIM(results_file),par_len,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
	CASE('010')
		CALL FIRST_LINE(TRIM(results_file),par_len,obser_cha=obser_cha,obser_int=obser_int,obser_conf=obser_conf)
	CASE('000')
		CALL ERROR_SOR("nothing_to_read")
END SELECT

grid(1:num_of_var) = par_g
grid(num_of_var+1:4) = 1
n_of_files = grid(1)*grid(2)*grid(3)
algar = 0
div = n_of_files

DO WHILE(div.GE.1)
	div = INT(div/10)
	algar = algar + 1
END DO

WRITE(algar_cha,*) algar
algar_cha = FIELD(algar_cha,1,' ')

CALL FDATE(ffdate)

WRITE(*,FMT='(2/,12X,56("*"),/,11X,A,/,12X,56("*"),/)') ' ***   Reading started at  '//TRIM(ffdate)//'   ***'
WRITE(*,FMT='(A)',ADVANCE = 'NO') ''

CALL SYSTEM_CLOCK(time1)

counter = 0

DO i=1,grid(1)

	IF(num_of_var.GE.1) CALL PAR_NEW(par_i(1),par_f(1),par_g(1),form(1),i,par_len(1),par_new_cha(1))
	
	DO j=1,grid(2)
		
		IF(num_of_var.GE.2) CALL PAR_NEW(par_i(2),par_f(2),par_g(2),form(2),j,par_len(2),par_new_cha(2))
		
		DO k=1,grid(3)

			IF(num_of_var.EQ.3) CALL PAR_NEW(par_i(3),par_f(3),par_g(3),form(3),k,par_len(3),par_new_cha(3))
												
			
			counter = counter + 1
			name_temp = name
		
			DO S = 1,num_of_var
				IF(pos_i_name(S).EQ.1) THEN
					name_temp = TRIM(par_new_cha(S))//TRIM(name_temp(pos_f_name(S)+1:))
				ELSE
					name_temp = name_temp(:pos_i_name(S)-1)//TRIM(par_new_cha(S))//TRIM(name_temp(pos_f_name(S)+1:))
				END IF
			END DO

			OPEN(UNIT=501,FILE=TRIM(name_temp)//"",STATUS='old',IOSTAT=error)


			IF(error.EQ.0) THEN
				CLOSE(501)
				WRITE(*,'(80("-"),A)') '--> reading "'//TRIM(name_temp)//'"...'
				WRITE(*,FMT='(A)',ADVANCE="NO") ' '
			ELSE
				WRITE(*,'(80("-"),A)',ADVANCE="NO") ' '
			END IF

			!Read only the asked data from current output file

			SELECT CASE(perc_switch//obser_switch//mes_switch)
				CASE('111')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,perc_cha,perc_int,perc_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)
				CASE('110')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,perc_cha,perc_int,perc_conf,obser_cha,obser_int,obser_conf)
				CASE('101')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,perc_cha,perc_int,perc_conf,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
				CASE('011')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,obser_cha=obser_cha,obser_int=obser_int,obser_conf=obser_conf,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
				CASE('100')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,perc_cha,perc_int,perc_conf)
				CASE('001')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,mes_cha=mes_cha,mes_int=mes_int,mes_conf=mes_conf)
				CASE('010')
					CALL OUTPUT_READER(TRIM(name_temp),TRIM(results_file),par_new_cha,obser_cha=obser_cha,obser_int=obser_int,obser_conf=obser_conf)
			END SELECT

			IF(error.EQ.0) THEN
				WRITE(*,'(A,2(I'//TRIM(algar_cha)//'.'//TRIM(algar_cha)//',A))')   &
				& '    ...Done!           [',counter,' of ',n_of_files,']'
			ELSE
				WRITE(*,'(A,2(I'//TRIM(algar_cha)//'.'//TRIM(algar_cha)//',A))')   &
				& '     ...File not found! [',counter,' of ',n_of_files,']'
			END IF
			WRITE(*,'(A)',ADVANCE="NO") ' '


					 
		END DO

	END DO	

END DO

DEALLOCATE(line)

READ(*,*) S	

END