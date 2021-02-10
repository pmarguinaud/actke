PROGRAM WRAP_ACTKE

USE MODI_TOTO
IMPLICIT NONE

INTEGER :: IBLOCK, NGPBLKS, N, J, JJ

NGPBLKS = 10

N = 1000

!$acc parallel loop gang vector private (IBLOCK,JJ) collapse (2)

DO IBLOCK = 1, NGPBLKS

  DO JJ = 1, N

  CALL TOTO (IBLOCK, JJ)

  ENDDO

ENDDO

!$acc end parallel loop 



END PROGRAM WRAP_ACTKE
