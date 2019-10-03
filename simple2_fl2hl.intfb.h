INTERFACE
#include "simple2.h"
SUBROUTINE SIMPLE2_FL2HL ( KIDIA, KFDIA, KLON, KTDIAT, KLEV, PAPRS, PAPRSF, PXFL,&
 & PXHL, KINI, CDLOCK) 
USE PARKIND1 ,ONLY : JPIM ,JPRB
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIAT
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KINI
REAL(KIND=JPRB) ,INTENT(IN) :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB) ,INTENT(IN) :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(IN) :: PXFL(KLON,KLEV)
REAL(KIND=JPRB) ,INTENT(OUT) :: PXHL(KLON,KINI:KLEV)
CHARACTER(LEN=7) ,INTENT(IN) :: CDLOCK
END SUBROUTINE SIMPLE2_FL2HL
END INTERFACE
