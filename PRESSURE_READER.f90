SUBROUTINE PRESSURE_READER(P1,P2,g_P,errors,err_conf,variables,var_conf)


USE FIND_ENTHALPY_V__MODULE 
USE FIND_CELL__MODULE 
USE FIND_SDLC__MODULE
USE FIND_HFDLC__MODULE
USE FIND_COORD__MODULE
USE FIND_FREQ__MODULE


IMPLICIT NONE

!Passed variables

CHARACTER(*),  OPTIONAL,    DIMENSION(:), INTENT(IN)      :: variables, errors  
INTEGER,       OPTIONAL,    DIMENSION(:), INTENT(IN)      :: var_conf, err_conf
REAL,                                     INTENT(IN)      :: P1, P2
INTEGER,                                  INTENT(IN)      :: g_P

!Internal variables
	
CHARACTER(80), ALLOCATABLE, DIMENSION(:)                  :: out_vect
CHARACTER(80)                                             :: temp
CHARACTER(30)                                             :: name
CHARACTER(14),              DIMENSION(7)                  :: full_cell, prim_cell
CHARACTER(10),              DIMENSION(3,3)                :: hfdlc, sdlc 
CHARACTER(6)										      :: p_new_c
CHARACTER(8),               DIMENSION(7)                  :: perc_err
CHARACTER(8)                                              :: fin_val
CHARACTER(20)                                             :: enthalpy
CHARACTER(8)                                              :: freq
CHARACTER(6)                                              :: coord_err
CHARACTER(3)			 							      :: AD
CHARACTER(2),               DIMENSION(SIZE(variables))    :: var_conf_c
CHARACTER(2),               DIMENSION(SIZE(errors))       :: err_conf_c
CHARACTER(1)                                              :: axis
CHARACTER(LEN(variables)),  DIMENSION(SIZE(variables))    :: variables_2
CHARACTER(LEN(errors)),     DIMENSION(SIZE(errors))       :: errors_2
REAL                                                      :: p_new
LOGICAL                                                   :: pres, pres2
INTEGER							                	      :: atom, num_of_var, num_of_err, pos_number
INTEGER                                                   :: i, j,  k, l, m, n, conf, nrec 
INTEGER,                    DIMENSION(SIZE(variables))    :: var_2
INTEGER,                    DIMENSION(SIZE(errors))       :: err_2

!External function

CHARACTER(2), EXTERNAL                                    :: FIELD



num_of_var = SIZE(variables)
num_of_err = SIZE(errors)

var_2 = 0
err_2 = 0

DO i=1,num_of_var
	pos_number = SCAN(variables(i),'0123456789')
	IF(pos_number.NE.0) THEN	
		temp = variables(i)(pos_number:)
		READ(temp,'(I4.1)') var_2(i)
		variables_2(i) = variables(i)(1:pos_number-1)
	ELSE
		variables_2(i) = variables(i)(1:)
	END IF
END DO




DO i=1,num_of_err
	pos_number = SCAN(errors(i),'0123456789')
	IF(pos_number.NE.0) THEN	
		temp = errors(i)(pos_number:)
		READ(temp,'(I4.1)') err_2(i)
		errors_2(i) = errors(i)(1:pos_number-1)
	ELSE
		errors_2(i) = errors(i)(1:)
	END IF
END DO




OPEN (UNIT=007, FILE='results.dat',BLOCKSIZE=100000000)


pres = PRESENT(errors)
pres2 = PRESENT(variables)

WRITE(007,'(A," ")',ADVANCE='NO') '    P'


AD = 'NO'

IF (pres2) THEN
	WRITE(var_conf_c, FMT='(I2.1)') var_conf
	var_conf_c = (/(FIELD(var_conf_c(n),1,' '), n = 1, num_of_var)/) 
	DO n=1, num_of_var
		IF ((n.EQ.num_of_var).AND.(.NOT.pres)) AD = 'YES'
		WRITE (007,FMT='(A," ")',ADVANCE=TRIM(AD) ) TRIM(variables(n))//'_'//TRIM(var_conf_c(n)) 
	END DO
END IF


IF (pres) THEN
	WRITE(err_conf_c, FMT='(I2.1)') err_conf
	err_conf_c = (/(FIELD(err_conf_c(n),1,' '), n = 1, num_of_err)/)
	DO n=1, num_of_err
		IF (n.EQ.num_of_err) AD = 'YES'
		WRITE (007,FMT='(A," ")',ADVANCE=TRIM(AD) ) TRIM(errors(n))//'_'//TRIM(err_conf_c(n))
	END DO
END IF


CLOSE(007)


DO j=1, g_P

	P_new = P1 + REAL(j - 1)*(P2 - P1)/REAL(g_P - 1)

	IF (j.EQ.1) P_new = 0.0001 

	WRITE(P_new_c,'(F6.4)') P_new

	IF(P_new_c(1:1).EQ.' ')THEN
		P_new_c = '0'//TRIM(P_new_c(2:))
	END IF

		OPEN (UNIT=007, FILE='results.dat',POSITION='APPEND',BLOCKSIZE=100000000)

		
		WRITE(007,'(A,"  ")',ADVANCE='NO') P_new_c


		name = 'Pressure_'//TRIM(P_new_c)//'.out'



		CALL NUM_REC(name,nrec)

		ALLOCATE(out_vect(nrec)) 

		CALL READ_FILE(name,out_vect)

		AD = 'NO'

		print*," *** Reading file ",TRIM(name),' ***'
				
		IF (pres2) THEN
		
			DO i = 1,num_of_var

				IF ((i.EQ.num_of_var).AND.(.NOT.pres)) AD = 'YES'

				conf =  var_conf(i)
				
				temp = variables_2(i)
				
				
				SELECT CASE(TRIM(temp))
					CASE ('enthalpy')
						CALL FIND_ENTHALPY_V(out_vect,enthalpy,conf)
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) enthalpy
					CASE ('freq')
						CALL FIND_FREQ(out_vect,freq,conf)
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) freq
					CASE ('a','b','c','alpha','beta','gamma','v')
						CALL FIND_CELL(out_vect,cell_full=full_cell,conf=conf)
						SELECT CASE (TRIM(temp))
							CASE ('a') 
								l = 1
							CASE ('b')
								l = 2
							CASE ('c') 
								l = 3
							CASE ('alpha') 
								l = 4
							CASE ('beta') 
								l = 5
							CASE ('gamma') 
								l = 6
							CASE ('v') 
								l = 7
						END SELECT
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) full_cell(l) 
					
					CASE ('a_prim','b_prim','c_prim','alpha_prim','beta_prim','gamma_prim','v_prim')

						CALL FIND_CELL(out_vect,cell_prim=prim_cell,conf=conf)
						SELECT CASE (TRIM(temp))
							CASE ('a_prim') 
								l = 1
							CASE ('b_prim') 
								l = 2
							CASE ('c_prim') 
								l = 3
							CASE ('alpha_prim') 
								l = 4
							CASE ('beta_prim') 
								l = 5
							CASE ('gamma_prim') 
								l = 6
							CASE ('v_prim') 
								l = 7
						END SELECT
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) prim_cell(l) 

					CASE ('sdlc')
						CALL FIND_SDLC(out_vect,sdlc,conf)
							l = INT(var_2(i)/10)
							m = MOD(var_2(i),10)
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) sdlc(l,m)


					CASE ('hfdlc')
						CALL FIND_HFDLC(out_vect,hfdlc,conf=conf)
							l = INT(var_2(i)/10)
							m = MOD(var_2(i),10)
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) hfdlc(l,m)
				
					CASE ('x', 'y', 'z')
							axis = variables_2(i)
							atom = var_2(i)
							CALL FIND_COORD(out_vect,TRIM(axis),atom,coord_val=fin_val,conf=conf)
							WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) fin_val
							
				
				
				END SELECT
				
			END DO

		END IF

		IF (pres) THEN
		
			DO k=1, num_of_err

				IF (k.EQ.num_of_err) AD = 'YES'

				conf = err_conf(k)
				
				temp = errors_2(k)
				
				SELECT CASE(TRIM(temp))

					CASE ('a','b','c','alpha','beta','gamma','v')
						CALL FIND_CELL(out_vect,perc_err,conf)
						SELECT CASE (TRIM(temp))
							
							CASE ('a') 
								l = 1
							CASE ('b') 
								l = 2
							CASE ('c') 
								l = 3
							CASE ('alpha') 
								l = 4
							CASE ('beta') 
								l = 5
							CASE ('gamma') 
								l = 6
							CASE ('v') 
								l = 7
						END SELECT

						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) perc_err(l)


					CASE ('x', 'y', 'z')
						axis = errors_2(k)
						atom = err_2(k)
						CALL FIND_COORD(out_vect,TRIM(axis),atom,coord_err,conf)
						WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) coord_err
									
				END SELECT

				

			END DO

		END IF

		DEALLOCATE(out_vect)

		CLOSE(007)


END DO

RETURN


END	


MODULE PRESSURE_READER__MODULE

INTERFACE
	SUBROUTINE PRESSURE_READER(P1, P2,g_P,errors,err_conf,variables,var_conf)
		CHARACTER(*),  OPTIONAL,    DIMENSION(:), INTENT(IN)      :: variables, errors  
		INTEGER,       OPTIONAL,    DIMENSION(:), INTENT(IN)      :: var_conf, err_conf
		REAL,                                     INTENT(IN)      :: P1, P2
		INTEGER,                                  INTENT(IN)      :: g_P
	END SUBROUTINE
END INTERFACE

END MODULE