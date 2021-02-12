PROGRAM WRAP_ACTKE

USE YOMDATA
USE MODI_TOTO
IMPLICIT NONE

INTEGER :: IBLOCK, NGPBLKS, N, J, JJ
REAL :: WW

NGPBLKS = 10

N = 10

ALLOCATE (ZZ (N))

DO J = 1, N
  ZZ (J) = REAL (J)
ENDDO

WW = 123

!$acc parallel
YY = WW
!$acc end parallel

YY = 456

!$acc update device (YY)


!$acc parallel loop gang vector private (IBLOCK,JJ) collapse (2) 

DO IBLOCK = 1, NGPBLKS

  DO JJ = 1, N

  CALL TOTO (IBLOCK, JJ)

  ENDDO

ENDDO

!$acc end parallel loop 




END PROGRAM WRAP_ACTKE
