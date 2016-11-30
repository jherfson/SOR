!This subroutine returns to 'from' the number of the line of the GULP's output file
!contained in the vector 'vec' where the configuration 'conf' starts and to 'until'
!the number of the line where it finishes. The argument 'conf' is optional. When 
!ommited, it is assumed to be 1. If the given configuration cannot be found, both 
!'from' and 'until' return 0.

!Subroutines required: 
!SEARCH_IN_VECT

!Function required: 
!FIELD	


SUBROUTINE CONF_LIM(vec,from,until,conf)

USE SEARCH_IN_VECT__MODULE

IMPLICIT NONE

!Passed variables

CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec        
INTEGER,                               INTENT(OUT) :: from, until
INTEGER,                     OPTIONAL, INTENT(IN)  :: conf       
                                         
!Internal variables                      
                                         
CHARACTER(80)                                      :: temp_line
CHARACTER(3)                                       :: configuration
INTEGER                                            :: start, rec, wanted_conf, current_conf, num_of_configs
LOGICAL                                            :: key, key2, key3
                                                   
!External function                                 
                                                   
CHARACTER(3), EXTERNAL                             :: FIELD





!If 'conf' is ommited, it will be considered that the configuration is the first one.

IF(PRESENT(conf))THEN
	wanted_conf = conf
ELSE
	wanted_conf = 1
END IF

!Find the total number of configurations

CALL SEARCH_IN_VECT(vec,'Total number of configurations input =',line=temp_line)
configuration = FIELD(temp_line,7,' ')

!Convert it from character to integer

READ(configuration,*) num_of_configs

rec = 0

!Initialize logical keys
key3 = num_of_configs.GE.wanted_conf
key2 = num_of_configs.EQ.wanted_conf
key = .TRUE.

!Start from the first line (vector component, actually)

start = 1

!Keep searching the start of configuration 'conf' until it is found or until
!there are no more configurations left.
 
DO WHILE(key.AND.key3)

	CALL SEARCH_IN_VECT(vec,'Output for configuration',rec,temp_line,start)

	IF(rec.NE.0) start = rec + 1
	configuration = FIELD(temp_line,5,' ')
	READ(configuration,*) current_conf
	key = (current_conf.NE.wanted_conf).AND.(rec.NE.0)
END DO

!Note that if the configuration was not found, the value of 'rec' will be 0.

from = rec

!If the configuration was not found, set 'until' 0. Else, search where the
!configuration finishes.

IF(from.EQ.0)THEN
	until = 0
ELSE
	IF(key2)THEN
		until = SIZE(vec)
	ELSE
		CALL SEARCH_IN_VECT(vec,'Output for configuration',rec,temp_line,start)
	
		!If there are no more configurations after 'conf', set 'until' as the 
		!last component of the vector.

		IF((rec.EQ.0))THEN
			until = SIZE(vec)
		ELSE
			until = rec - 1
		END IF
	END IF
END IF 
			   
RETURN

END	SUBROUTINE




!Interface needed due to the optional arguments

MODULE CONF_LIM__MODULE

INTERFACE

	SUBROUTINE CONF_LIM(vec,from,until,conf)
		CHARACTER(80), DIMENSION(:),           INTENT(IN)  :: vec         
		INTEGER,                               INTENT(OUT) :: from, until 
		INTEGER,                     OPTIONAL, INTENT(IN)  :: conf        
	END SUBROUTINE

END INTERFACE

END MODULE