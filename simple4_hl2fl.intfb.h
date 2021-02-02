INTERFACE
SUBROUTINE SIMPLE4_HL2FL ( KIDIA, KFDIA, KLON, KTDIAT, KLEV, PAPRS, PAPRSF, PXHL, &
& KINI, PXFL, CDLOCK, KPSTSZ, KPTRST, PSTACK) 
USE PARKIND1  ,ONLY : JPIM   ,JPRB
IMPLICIT NONE
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAT 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KINI 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PXHL(KLON,KINI:KLEV)

REAL(KIND=JPRB)   ,INTENT(OUT)  :: PXFL(KLON,KLEV)

CHARACTER(LEN=7)  ,INTENT(IN)    :: CDLOCK
REAL(KIND=JPRB),   INTENT(OUT), TARGET :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPTRST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ
END SUBROUTINE SIMPLE4_HL2FL
END INTERFACE
