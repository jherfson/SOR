!obs: fix bug concerning tabs in FIELD

!
!  27/10/2012 - Created.
!  28/10/2012 - Now real time progress of the calculations is displayed at the end of each run.
!  29/10/2012 - Now a prediction of the time left is displayed.
!               Now the calculation and displaying of the time left are done by the subroutine PRINT_TIME_STAFF.
!               Now the program's headline is displayed by the subroutine HEADLINE. 
!  30/10/2012 - Now interrupted jobs can be automaticly restarted.
!  31/10/2012 - Now the number of the last job is verified by the subroutine LASTJOB.
!             - Now the restart file is created by the subroutine CREATE_RES.
!             - Now the model file name is asked by the subroutine ASK_MODEL_NAME.
!             - Now the value of each component of the variable par_new_cha is given by the subroutine PAR_NEW. 
!             - Now errors are dealt by the subroutire ERROR.
!  03/11/2012 - Now multiple occurence of the same variable label along the model file is allowed.






PROGRAM SIR

USE SEARCH_IN_VECT__MODULE
 

IMPLICIT NONE

!Internal variables

CHARACTER(80), DIMENSION(:),   ALLOCATABLE :: line, line_temp
CHARACTER(80), DIMENSION(3,3)              :: par
CHARACTER(20), DIMENSION(3)                :: par_new_cha
CHARACTER(80), DIMENSION(3)                :: form
CHARACTER(80)                              :: name, name_temp, algar_cha, model
CHARACTER(24)                              :: ffdate
CHARACTER(20),                 PARAMETER   :: blank_str = REPEAT('0',20)                               
CHARACTER(2),  DIMENSION(3)                :: S_cha
LOGICAL,       DIMENSION(3)                :: absent
LOGICAL                                    :: done
REAL,          DIMENSION(3)                :: par_i, par_f
REAL                                       :: time1
INTEGER,       DIMENSION(:,:), ALLOCATABLE :: rec2, pos_i, pos_f
INTEGER,       DIMENSION(3)                :: par_g, rec1, pos_i_name, pos_f_name, par_len, subs
INTEGER                                    :: S, p, l, t, v, k, nrec, rec_name, rec_end, n_of_files, algar, ind, posi 
INTEGER                                    :: n_of_files_left, n, div, last_job, counter, rec, subst, dim

!External function

CHARACTER(80), EXTERNAL                    :: FIELD


!!!!!!!!!!!!!!!!!!!!!!
!Begin of the program!
!!!!!!!!!!!!!!!!!!!!!!


!Set a black background and green letters
!Print headline, ask the model file name, clear the display and print the headline again

CALL SYSTEM('color 0A')
CALL HEADLINE_SIR
CALL ASK_MODEL_NAME_SIR(model)
CALL SYSTEM('cls')
CALL HEADLINE_SIR

!Calculate the number of lines in the model file and assign each line to a component of 
!the vector "line"

CALL NUM_REC(TRIM(model),nrec)
ALLOCATE(line(nrec),line_temp(nrec))
CALL READ_FILE(TRIM(model),line)

!Find the lines where "END" and "#FILE_NAME" are

CALL SEARCH_IN_VECT(line,"#END",rec_end)
CALL SEARCH_IN_VECT(line,'#FILE_NAME',rec_name)

!If no line with the file name pattern is found, print an error message and stop the program
!If the line with the file name pattern is found, but not after "#END", print an error message
!and stop the program

IF(rec_name.EQ.0) CALL ERROR_SIR("no_file_name")
IF(rec_name.GE.rec_end) CALL ERROR_SIR("name_not_before_#END")

!Write the label of each variable as character

DO S=1,3
	S_cha(S) = "$0"
	WRITE(S_cha(S)(2:2),'(I1.1)') S
END DO

absent(S) = .FALSE.

!Find the number of the line where each variable is declared and check if it occurs before "#END".
!If the variable is found, but not before "#END", print an error message and stop the program
!If the line is not found at all, set the variable as absent

DO S=1,3
	CALL SEARCH_IN_VECT(line,S_cha(S),rec1(S))
	IF(rec1(S).EQ.rec_name) CALL SEARCH_IN_VECT(line,S_cha(S),rec1(S),start_line=rec1(S)+1) 
	IF(rec1(S).GE.rec_end) CALL ERROR_SIR("var_not_before_#END")
	IF(rec1(S).EQ.0) absent(S) = .TRUE.  		
END DO

!Get the initial and final values of each variable. Get also the grid and the format in which 
!them must be writen. If the variable is absent, don't do anything else then set the grid as 1

DO S=1,3
	IF(.NOT.absent(S))THEN
		par(S,:) = (/(FIELD(line(rec1(S)),p+1,' '), p = 1, 3)/)
		READ(par(S,1),*) par_i(S)
		READ(par(S,2),*) par_f(S)
		READ(par(S,3),*) par_g(S)

		form(S) = FIELD(line(rec1(S)),5,' ')
		SELECT CASE(form(S)(3:3))
			CASE('0':'9')
				READ(form(S)(2:3),'(I2.2)') par_len(S)
			CASE DEFAULT
				READ(form(S)(2:2),'(I1.1)') par_len(S)
		END SELECT
		IF(form(S)(1:1).EQ.'I') form(S) = TRIM(form(S))//'.'//TRIM(form(S)(2:))
		form(S) = '('//TRIM(form(S))//')'
	ELSE
		par_g(S) = 1
	END IF
END DO

!Get the name pattern

name = FIELD(line(rec_name),2,' ')

!Copy "name", in order to not to modify it, just the temporary copy  

name_temp = name

!Determine the initial and final position of each variable label in the name

DO S=1,3
	pos_i_name(S) = INDEX(name_temp,S_cha(S))
	pos_f_name(S) = pos_i_name(S) + 1
	name_temp = name_temp(:pos_i_name(S)-1)//blank_str(:par_len(S))//name_temp(pos_f_name(S)+1:73)
END DO

!Determine the number of substitutions to be done for each variable

DO S=1,3
	rec = rec_end + 1
	DO WHILE(rec.NE.0)
		CALL SEARCH_IN_VECT(line,S_cha(S),rec,start_line=rec+1)
		posi = 1
		ind = 1
		DO WHILE ((ind.NE.0).AND.(rec.NE.0))
			ind = INDEX(line(rec)(posi:),S_cha(S))
			posi = posi + ind
			IF(ind.NE.0) subs(S) = subs(S) + 1
		END DO
	END DO
END DO

dim = MAXVAL(subs)

ALLOCATE(rec2(3,dim),pos_i(3,dim),pos_f(3,dim))


!Copy "line", in order to not to modify it, just the temporary copy  

line_temp = line

!Find where are the variable labels along the file and replace them by a string of same 
!length of their corresponding values. 

DO S=1,3
	subst = 1
	rec = rec_end + 1
	DO WHILE(rec.NE.0)
		CALL SEARCH_IN_VECT(line_temp,S_cha(S),rec,start_line=rec+1)
		posi = 1
		ind  = 1
		DO WHILE ((ind.NE.0).AND.(rec.NE.0))
			ind  = INDEX(line_temp(rec)(posi:),S_cha(S))
			posi = posi + ind
			IF(ind.NE.0)THEN
				rec2(S,subst)  = rec
				pos_i(S,subst) = posi - 1
				pos_f(S,subst) = posi
				line_temp(rec) = line_temp(rec)(:pos_i(S,subst)-1)//blank_str(:par_len(S))//line_temp(rec)(pos_f(S,subst)+1:73)
				subst = subst + 1
			END IF
		END DO
	END DO
END DO


n_of_files = par_g(1)*par_g(2)*par_g(3)
algar = 0
div = n_of_files

DO WHILE(div.GE.1)
	div = INT(div/10)
	algar = algar + 1
END DO

WRITE(algar_cha,*) algar
algar_cha = FIELD(algar_cha,1,' ')

CALL FDATE(ffdate)
CALL LASTJOB(last_job)

IF(last_job.EQ.0)THEN
	WRITE(*,FMT='(2/,12X,56("*"),/,11X,A,/,12X,56("*"),/)') ' *** Calculations started at '//TRIM(ffdate)//' ***'
ELSE
	WRITE(*,'(/,12X,A,I'//TRIM(algar_cha)//'.'//TRIM(algar_cha)//',A)') " !!! Restarting interrupted job from file number ",last_job+1," !!!" 
	WRITE(*,FMT='(2/,12X,58("*"),/,11X,A,/,12X,58("*"),/)') ' *** Calculations restarted at '//TRIM(ffdate)//' ***'
END IF

WRITE(*,FMT='(A)',ADVANCE = 'NO') ''


CALL TIME_IN_S(time1)

counter = 0

DO t=1,par_g(1)

	CALL PAR_NEW(absent(1),par_i(1),par_f(1),par_g(1),form(1),t,par_len(1),par_new_cha(1))
	
	DO v=1,par_g(2)
		
		CALL PAR_NEW(absent(2),par_i(2),par_f(2),par_g(2),form(2),v,par_len(2),par_new_cha(2))
		
		DO k=1,par_g(3)

			CALL PAR_NEW(absent(3),par_i(3),par_f(3),par_g(3),form(3),k,par_len(3),par_new_cha(3))
						
			
			IF(counter.LT.last_job)THEN
				counter = counter + 1
				CYCLE
			END IF
			
			CALL CREATE_RES(algar_cha,counter)
												
			
			counter = counter + 1
			name_temp = name
		
			DO S = 1,3
				IF(.NOT.absent(S)) name_temp = name_temp(:pos_i_name(S)-1)//TRIM(par_new_cha(S))//name_temp(pos_f_name(S)+1:73)
			END DO

			OPEN(UNIT=10,FILE=TRIM(name_temp)//'.gin',STATUS="REPLACE")

			line_temp = line

			DO l=rec_end+1,nrec

				DO S = 1,3
					DO subst=1,subs(S)
						IF((l.EQ.rec2(S,subst)).AND.(.NOT.absent(S))) &
						& line_temp(l) = line_temp(l)(:pos_i(S,subst)-1)//TRIM(par_new_cha(S))//line_temp(l)(pos_f(S,subst)+1:73)
					END DO
				END DO

				WRITE(10,FMT='(A)') line_temp(l)		
						   
			END DO

			CLOSE(10)

			WRITE(*,'(80("-"),3A)') '--> running "'//TRIM(name_temp)//'"...'
			WRITE(*,FMT='(A)',ADVANCE="NO") ''

			CALL SYSTEM('gulp.exe < "'//TRIM(name_temp)//'.gin" > "'//TRIM(name_temp)//'.out"')
			WRITE(*,'(A,2(I'//TRIM(algar_cha)//'.'//TRIM(algar_cha)//',A))')   &
			& '    ...Done! [',counter,' calculation(s) of ',n_of_files,']'
		
			n = counter - last_job
			done = counter.EQ.n_of_files
			n_of_files_left = n_of_files - counter

			CALL PRINT_TIME_STAFF(n,done,n_of_files_left)
					 
		END DO

	END DO	

END DO

DEALLOCATE(line, line_temp)

CALL SYSTEM("del continue_interrupted_job.res continue_interrupted_job.temp")
READ(*,*) S	

END