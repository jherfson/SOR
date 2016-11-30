SUBROUTINE VARY_BUCK_READER(A1,A2,g_A,rho1,rho2,g_rho,errors,err_conf,variables,var_conf)


USE FIND_CELL__MODULE 
USE FIND_SDLC__MODULE
USE FIND_HFDLC__MODULE
USE FIND_COORD__MODULE
USE FIND_FREQ__MODULE


IMPLICIT NONE

!Passed variables

CHARACTER(*),  OPTIONAL,    DIMENSION(:), INTENT(IN)      :: variables, errors  
INTEGER,       OPTIONAL,    DIMENSION(:), INTENT(IN)      :: var_conf, err_conf
REAL,                                     INTENT(IN)      :: A1, A2, rho1, rho2 
INTEGER,                                  INTENT(IN)      :: g_A, g_rho

!Internal variables
	
CHARACTER(80), ALLOCATABLE, DIMENSION(:)                  :: out_vect
CHARACTER(80)                                             :: temp
CHARACTER(30)                                             :: name
CHARACTER(14),              DIMENSION(7)                  :: full_cell, prim_cell
CHARACTER(10),              DIMENSION(3,3)                :: hfdlc, sdlc 
CHARACTER(9)                                              :: A_new_c
CHARACTER(8),               DIMENSION(7)                  :: perc_err
CHARACTER(8)                                              :: fin_val
CHARACTER(8)                                              :: freq
CHARACTER(6)                                              :: coord_err, rho_new_c
CHARACTER(3)			 							      :: AD
CHARACTER(2),               DIMENSION(SIZE(variables))    :: var_conf_c
CHARACTER(2),               DIMENSION(SIZE(errors))       :: err_conf_c
CHARACTER(1)                                              :: axis
CHARACTER(LEN(variables)),  DIMENSION(SIZE(variables))    :: variables_2
CHARACTER(LEN(errors)),     DIMENSION(SIZE(errors))       :: errors_2
REAL                                                      :: A_new, rho_new
LOGICAL                                                   :: pres, pres2
INTEGER                                                   :: atom, num_of_var, num_of_err, pos_number
INTEGER                                                   :: i, j, h, k, l, m, n, conf, nrec 
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

WRITE(007,'(A," ",A," ")',ADVANCE='NO') '    A','    rho'


AD = 'NO'

IF (pres2) THEN
	WRITE(var_conf_c, FMT='(I2.1)') var_conf
	var_conf_c = (/(FIELD(var_conf_c(n),1,' '), n = 1, num_of_var)/) 
	DO n=1, num_of_var
		IF ((n.EQ.num_of_var).AND.(.NOT.pres)) AD = 'YES'
		WRITE (007,FMT='(A," ")',ADVANCE=TRIM(AD) ) TRIM(variables(n))//'_'//var_conf_c(n) 
	END DO
END IF


IF (pres) THEN
	WRITE(err_conf_c, FMT='(I2.1)') err_conf
	err_conf_c = (/(FIELD(err_conf_c(n),1,' '), n = 1, num_of_err)/)
	DO n=1, num_of_err
		IF (n.EQ.num_of_err) AD = 'YES'
		WRITE (007,FMT='(A," ")',ADVANCE=TRIM(AD) ) errors(n)//'_'//err_conf_c(n) 
	END DO
END IF


CLOSE(007)


DO h=1, g_A

	A_new = A1 + REAL(h - 1)*(A2 - A1)/REAL(g_A - 1)

	WRITE(A_new_c,'(F9.3)') A_new

	DO j=1, g_rho

		OPEN (UNIT=007, FILE='results.dat',POSITION='APPEND',BLOCKSIZE=100000000)


		rho_new = rho1 + REAL(j - 1)*(rho2 - rho1)/REAL(g_rho - 1)

		
		WRITE(rho_new_c,'(F6.4)') rho_new
		rho_new_c = '0'//rho_new_c(2:6)


		
		WRITE(007,'(A,"  ",A,"  ")',ADVANCE='NO') A_new_c, rho_new_c


		name = TRIM(A_new_c)//'_'//TRIM(rho_new_c)//'.out'

		IF(name(1:1).EQ.' ')THEN
			name(1:1) = '0'
		END IF

		IF(name(2:2).EQ.' ')THEN
			name(2:2) = '0'
		END IF

		IF(name(3:3).EQ.' ')THEN
			name(3:3) = '0'
		END IF



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
							WRITE (007,FMT='(A)', ADVANCE = TRIM(AD)) fin_val
							
				
				
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
						WRITE (007,FMT='(A)', ADVANCE = TRIM(AD)) coord_err
									
				END SELECT

				

			END DO

		END IF

		DEALLOCATE(out_vect)

		CLOSE(007)

	END DO 

END DO

RETURN


END	


MODULE VARY_BUCK_READER__MODULE

INTERFACE
	SUBROUTINE VARY_BUCK_READER(A1,A2,g_A,rho1,rho2,g_rho,errors,err_conf,variables,var_conf)
		CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT (IN) :: variables, errors  
		INTEGER, DIMENSION(:), OPTIONAL, INTENT (IN)      :: var_conf, err_conf
		REAL, INTENT (IN)                                 :: A1, A2, rho1, rho2 
		INTEGER, INTENT(IN)                               :: g_A, g_rho
	END SUBROUTINE
END INTERFACE

END MODULE