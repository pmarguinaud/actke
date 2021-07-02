INTERFACE

SUBROUTINE SIMPLE4_ACEVOLET (  KIDIA,  KFDIA,  KLON,  KTDIAT, KTDIAN, KLEV,     &
& PAPHI, PAPHIF, PAPRS, PAPRSF, PECT, PKUROV, PR,PT,  PU, PV, PUSLE, PPRODTH,   &
& PKCLS, PECTCLS, PECT1, PPRDY, PDIFF, PDISS, RG, ECTMIN, ADISE, ALPHAE, ADISI, &
& ECTMAX, TSPHY, KPSTSZ, KPTRST, PSTACK)

USE PARKIND1  ,ONLY : JPIM     ,JPRB

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAT 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PUSLE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT)    :: PPRODTH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PKCLS 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECTCLS 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECT1(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPRDY(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDIFF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PDISS(KLON,KLEV)

REAL(KIND=JPRB), INTENT (   IN) :: RG     
REAL(KIND=JPRB), INTENT (   IN) :: ECTMIN 
REAL(KIND=JPRB), INTENT (   IN) :: ADISE  
REAL(KIND=JPRB), INTENT (   IN) :: ALPHAE 
REAL(KIND=JPRB), INTENT (   IN) :: ADISI  
REAL(KIND=JPRB), INTENT (   IN) :: ECTMAX 
REAL(KIND=JPRB), INTENT (   IN) :: TSPHY
REAL(KIND=JPRB),   INTENT(OUT)   :: PSTACK (KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPTRST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ

END SUBROUTINE SIMPLE4_ACEVOLET

END INTERFACE