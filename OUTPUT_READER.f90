
! 05/11/2012 - Created
! 08/11/2012 - Now the Helmholtz energy and the defect Helmholtz energy can be searched too
! 18/12/2012 - Now the gradient's norm, defect gradient's norm and the free energy can be searched too
! 18/12/2012 - Now messages can be searched too
! 18/12/2012 - Now it is possible to search for the message that says that the CPU time limit has been reached
! 18/12/2012 - Problems	regarding spacing in the results file were fixed



SUBROUTINE OUTPUT_READER(filename,par_cha,err_cha,err_int,err_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)


USE FIND_CELL__MODULE
USE FIND_FREQ__MODULE
USE FIND_SDLC__MODULE
USE FIND_HFDLC__MODULE
USE FIND_GNORM__MODULE
USE FIND_COORD__MODULE
USE FIND_CPU_LIM__MODULE
USE FIND_ENTROPY__MODULE
USE FIND_ENTHALPY__MODULE
USE FIND_HELMHOLTZ__MODULE
USE FIND_DEF_GNORM__MODULE
USE FIND_DEF_ENERGY__MODULE
USE FIND_DEF_ENTROPY__MODULE
USE FIND_FREE_ENERGY__MODULE
USE FIND_DEF_HELMHOLTZ__MODULE


IMPLICIT NONE

!Passed observables

CHARACTER(*),                             INTENT(IN)      :: filename
CHARACTER(*),               DIMENSION(:), INTENT(IN)      :: par_cha
CHARACTER(*),  OPTIONAL,    DIMENSION(:), INTENT(IN)      :: err_cha, obser_cha, mes_cha
INTEGER,       OPTIONAL,    DIMENSION(:), INTENT(IN)      :: err_int, err_conf, obser_int, obser_conf, mes_int, mes_conf

!Internal observables
	
CHARACTER(80), ALLOCATABLE, DIMENSION(:)                  :: out_vect
CHARACTER(50)                                             :: cha
CHARACTER(20)                                             :: enthalpy, def_energy, helmholtz, def_helmholtz, free_energy 
CHARACTER(10),              DIMENSION(7)                  :: full_cell, prim_cell, perc_err
CHARACTER(10),              DIMENSION(3,3)                :: hfdlc, sdlc
CHARACTER(10)                                             :: gnorm, def_gnorm, coord_val, freq, entropy, def_entropy, coord_err 
CHARACTER(3)			 							      :: AD
CHARACTER(1)											  :: CPU_lim
LOGICAL                                                   :: err_is_present, obser_is_present, mes_is_present
INTEGER                                                   :: num_of_obser, num_of_err, num_of_mes, num_of_par
INTEGER                                                   :: i, l, m, conf, nrec, integ 


num_of_obser = SIZE(obser_cha)
num_of_err   = SIZE(err_cha)
num_of_mes   = SIZE(mes_cha)
num_of_par   = SIZE(par_cha)

CALL NUM_REC(filename,nrec)
ALLOCATE(out_vect(nrec)) 
CALL READ_FILE(filename,out_vect)

IF(PRESENT(err_cha).AND.PRESENT(err_int).AND.PRESENT(err_conf))THEN
	err_is_present = .TRUE.
ELSE
	err_is_present = .FALSE.
END IF

IF(PRESENT(obser_cha).AND.PRESENT(obser_int).AND.PRESENT(obser_conf))THEN
	obser_is_present = .TRUE.
ELSE
	obser_is_present = .FALSE.
END IF

IF(PRESENT(mes_cha).AND.PRESENT(mes_int).AND.PRESENT(mes_conf))THEN
	mes_is_present = .TRUE.
ELSE
	mes_is_present = .FALSE.
END IF


OPEN (UNIT=007, FILE='results.dat',POSITION='APPEND',BLOCKSIZE=100000000,RECL=100000000)

DO i = 1, num_of_par
	WRITE(007,'(A," ")',ADVANCE='NO') TRIM(par_cha(i))
END DO


AD = 'NO'
				
IF(obser_is_present)THEN
		
	DO i = 1,num_of_obser


		IF((i.EQ.num_of_obser).AND.(.NOT.(err_is_present)).AND.(.NOT.(mes_is_present))) AD = 'YES'
		cha   = obser_cha(i)
		integ = obser_int(i)
		conf  = obser_conf(i)
		IF(conf.EQ.0) conf = 1
				
		SELECT CASE(TRIM(cha))
			CASE ('freq')
				CALL FIND_FREQ(out_vect,freq,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) freq
			CASE ('a','b','c','alpha','beta','gamma','v')
				CALL FIND_CELL(out_vect,cell_full=full_cell,conf=conf)
				SELECT CASE (TRIM(cha))
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
				SELECT CASE (TRIM(cha))
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
				l = INT(integ/10)
				m = MOD(integ,10)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) sdlc(l,m)
			CASE ('hfdlc')
				CALL FIND_HFDLC(out_vect,hfdlc,conf=conf)
				l = INT(integ/10)
				m = MOD(integ,10)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) hfdlc(l,m)
			CASE ('x', 'y', 'z')
				CALL FIND_COORD(out_vect,TRIM(cha),integ,coord_val=coord_val,conf=conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) coord_val
			CASE ('enthalpy')
				CALL FIND_ENTHALPY(out_vect,enthalpy,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) enthalpy
			CASE ('free_energy')
				CALL FIND_FREE_ENERGY(out_vect,free_energy,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) free_energy
			CASE ('gnorm')
				CALL FIND_GNORM(out_vect,gnorm,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) gnorm
			CASE ('def_gnorm')
				CALL FIND_DEF_GNORM(out_vect,def_gnorm,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) def_gnorm
			CASE ('def_energy')
				CALL FIND_DEF_ENERGY(out_vect,def_energy,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) def_energy
			CASE ('entropy')
				CALL FIND_ENTROPY(out_vect,entropy,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) entropy
			CASE ('def_entropy')
				CALL FIND_DEF_ENTROPY(out_vect,def_entropy,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) def_entropy
			CASE ('helmholtz')
				CALL FIND_HELMHOLTZ(out_vect,helmholtz,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) helmholtz
			CASE ('def_helmholtz')
				CALL FIND_DEF_HELMHOLTZ(out_vect,def_helmholtz,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) def_helmholtz
		END SELECT
				
	END DO

END IF

IF (err_is_present) THEN
		
	DO i=1, num_of_err

		IF ((i.EQ.num_of_err).AND.(.NOT.(mes_is_present))) AD = 'YES'

		cha   = err_cha(i)
		integ = err_int(i)
		conf  = err_conf(i)
		IF(conf.EQ.0) conf = 1
				
		SELECT CASE(TRIM(cha))

			CASE ('a','b','c','alpha','beta','gamma','v')
				CALL FIND_CELL(out_vect,perc_err,conf)
				SELECT CASE (TRIM(cha))			
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
				CALL FIND_COORD(out_vect,TRIM(cha),integ,coord_err,conf)
				WRITE (007,FMT='(A," ")', ADVANCE = TRIM(AD)) coord_err
							
		END SELECT

	END DO

END IF

IF(mes_is_present)THEN
		
	DO i = 1,num_of_mes


		IF(i.EQ.num_of_mes) AD = 'YES'
		cha   = mes_cha(i)
		integ = mes_int(i)
		conf  = mes_conf(i)
		IF(conf.EQ.0) conf = 1
				
		SELECT CASE(TRIM(cha))
			CASE ('CPU_limit')

				CALL FIND_CPU_LIM(out_vect,CPU_lim)
				WRITE (007,FMT='(A,10(" "))', ADVANCE = TRIM(AD)) CPU_lim
		END SELECT
				
	END DO

END IF





DEALLOCATE(out_vect)

CLOSE(007)

RETURN


END	


MODULE OUTPUT_READER__MODULE

INTERFACE
	SUBROUTINE OUTPUT_READER(filename,par_cha,err_cha,err_int,err_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)
		CHARACTER(*),                             INTENT(IN)      :: filename
		CHARACTER(*),               DIMENSION(:), INTENT(IN)      :: par_cha
		CHARACTER(*),  OPTIONAL,    DIMENSION(:), INTENT(IN)      :: err_cha, obser_cha, mes_cha
		INTEGER,       OPTIONAL,    DIMENSION(:), INTENT(IN)      :: err_int, err_conf, obser_int, obser_conf, mes_int, mes_conf
	END SUBROUTINE
END INTERFACE

END MODULE