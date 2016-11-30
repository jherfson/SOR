SUBROUTINE FIRST_LINE(num_of_par,par_len,err_cha,err_int,err_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)


IMPLICIT NONE

!Passed variables

CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT(IN) :: err_cha, obser_cha, mes_cha 
INTEGER,      DIMENSION(:), OPTIONAL, INTENT(IN) :: obser_conf, obser_int, err_conf, err_int, mes_conf, mes_int
INTEGER,      DIMENSION(:),           INTENT(IN) :: par_len
INTEGER,                              INTENT(IN) :: num_of_par

!Internal variables

LOGICAL                                          :: obser_is_present, err_is_present, mes_is_present
INTEGER                                          :: i, num_of_obser, num_of_err, num_of_mes
CHARACTER(4)                                     :: AD, obser_int_cha, obser_conf_cha, err_int_cha, err_conf_cha, mes_int_cha, mes_conf_cha
CHARACTER(10)                                    :: short_subtitle
CHARACTER(20)                                    :: subtitle
	

CHARACTER(4), EXTERNAL                                    :: FIELD

obser_is_present = PRESENT(obser_cha).AND.PRESENT(obser_int).AND.PRESENT(obser_conf)
err_is_present   = PRESENT(err_cha).AND.PRESENT(err_int).AND.PRESENT(err_conf)
mes_is_present   = PRESENT(mes_cha).AND.PRESENT(mes_int).AND.PRESENT(mes_conf)

OPEN (UNIT=007, FILE='results.dat',STATUS='REPLACE',BLOCKSIZE=100000000,RECL=100000000)

DO i = 1, num_of_par
	WRITE(007,'(A,I1.1,A)',ADVANCE='NO') '$',i,REPEAT(" ",par_len(i) - 1)
END DO

AD = 'NO'

IF(obser_is_present)THEN
	num_of_obser = SIZE(obser_cha)
	DO i = 1,num_of_obser
		IF((i.EQ.num_of_obser).AND.(.NOT.err_is_present).AND.(.NOT.mes_is_present)) AD = 'YES'
		WRITE(obser_int_cha,'(I4.1)') obser_int(i)
		WRITE(obser_conf_cha,'(I4.1)') obser_conf(i)
		obser_int_cha = FIELD(obser_int_cha,1,' ') 
		obser_conf_cha = FIELD(obser_conf_cha,1,' ')
		IF(TRIM(obser_int_cha).EQ.'0') obser_int_cha = ''
		IF(TRIM(obser_conf_cha).NE.'0')THEN
			subtitle = TRIM(obser_cha(i))//TRIM(obser_int_cha)//'_'//TRIM(obser_conf_cha)
		ELSE
			subtitle = TRIM(obser_cha(i))//TRIM(obser_int_cha)
		END IF
		SELECT CASE(TRIM(obser_cha(i)))
			CASE('enthalpy', 'def_energy', 'helmholtz', 'def_helmholtz', 'free_energy')
				WRITE(007,'(A," ")',ADVANCE=TRIM(AD)) subtitle
			CASE DEFAULT
				short_subtitle = subtitle(:10)
				WRITE(007,'(A," ")',ADVANCE=TRIM(AD)) short_subtitle
		END SELECT
	END DO
END IF

IF(err_is_present)THEN
	num_of_err = SIZE(err_cha)
	DO i = 1,num_of_err
		IF((i.EQ.num_of_err).AND.(.NOT.mes_is_present)) AD = 'YES'
		WRITE(err_int_cha,'(I4.1)') err_int(i)
		WRITE(err_conf_cha,'(I4.1)') err_conf(i)
		err_int_cha = FIELD(err_int_cha,1,' ') 
		err_conf_cha = FIELD(err_conf_cha,1,' ') 
		IF(TRIM(err_int_cha).EQ.'0') err_int_cha = ''
		IF(TRIM(err_conf_cha).NE.'0')THEN
			short_subtitle =  TRIM(err_cha(i))//TRIM(err_int_cha)//'_'//TRIM(err_conf_cha)
		ELSE
			short_subtitle =  TRIM(err_cha(i))//TRIM(err_int_cha)
		END IF
		WRITE(007,'(A," ")',ADVANCE=TRIM(AD)) short_subtitle
	END DO
END IF

IF(mes_is_present)THEN
	num_of_mes = SIZE(mes_cha)
	DO i = 1,num_of_mes
		IF(i.EQ.num_of_mes) AD = 'YES'
		WRITE(mes_int_cha,'(I4.1)') mes_int(i)
		WRITE(mes_conf_cha,'(I4.1)') mes_conf(i)
		mes_int_cha = FIELD(mes_int_cha,1,' ') 
		mes_conf_cha = FIELD(mes_conf_cha,1,' ') 
		IF(TRIM(mes_int_cha).EQ.'0') mes_int_cha = ''
		IF(TRIM(mes_conf_cha).NE.'0')THEN
			short_subtitle =  TRIM(mes_cha(i))//TRIM(mes_int_cha)//'_'//TRIM(mes_conf_cha)
		ELSE
			short_subtitle =  TRIM(mes_cha(i))//TRIM(mes_int_cha)
		END IF
		WRITE(007,'(A," ")',ADVANCE=TRIM(AD)) short_subtitle
	END DO
END IF

CLOSE(007)

RETURN


END	


MODULE FIRST_LINE__MODULE

INTERFACE
	SUBROUTINE FIRST_LINE(num_of_par,par_len,err_cha,err_int,err_conf,obser_cha,obser_int,obser_conf,mes_cha,mes_int,mes_conf)
		CHARACTER(*), DIMENSION(:), OPTIONAL, INTENT(IN) :: err_cha, obser_cha, mes_cha 
		INTEGER,      DIMENSION(:), OPTIONAL, INTENT(IN) :: obser_conf, obser_int, err_conf, err_int, mes_conf, mes_int
		INTEGER,      DIMENSION(:),           INTENT(IN) :: par_len
		INTEGER,                              INTENT(IN) :: num_of_par
	END SUBROUTINE
END INTERFACE

END MODULE