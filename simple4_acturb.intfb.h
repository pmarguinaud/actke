INTERFACE
SUBROUTINE SIMPLE4_ACTURB  ( KIDIA,  KFDIA,  KLON,   KTDIAT, KTDIAN, KLEV, PAPHI, &
& PAPHIF, PAPRS,  PAPRSF, PR,     PT, PU, PV, PECT,   PQV,    PLSCPE, PLMECT,     &
& PPHI3, PGZ0,   PTS,    PQS, PQICE,  PQLI, PKTROV, PKUROV, PNBVNO, PPRODTH,      &
& PNEBS, PQCS,   PL3F2, PGKCLS, PECTCLS, RG, RV, RCPV, RETV, RCW, RCS, RLVTT,     &
& RLSTT, RTT, RDT, RALPW, RBETW, RGAMW, RALPS, RBETS, RGAMS, RALPD, RBETD, RGAMD, &
& RKAPPA, RATM, RD, UPRETMIN, VKARMN, UPRETMAX, ARSB2, ECTMIN, AKN, ALPHAT,       &
& ALMAV, UDECT, USHEARM, EDB, EDC, EDD, USURIC, KPSTSZ, KPTRST, PSTACK)
USE PARKIND1  ,ONLY : JPIM     ,JPRB
IMPLICIT NONE
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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSCPE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLMECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPHI3(KLON,KLEV) 
 
 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNBVNO(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPRODTH(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PL3F2(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGKCLS(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECTCLS(KLON)

REAL(KIND=JPRB), INTENT (   IN) :: RG       
REAL(KIND=JPRB), INTENT (   IN) :: RV       
REAL(KIND=JPRB), INTENT (   IN) :: RCPV     
REAL(KIND=JPRB), INTENT (   IN) :: RETV     
REAL(KIND=JPRB), INTENT (   IN) :: RCW      
REAL(KIND=JPRB), INTENT (   IN) :: RCS      
REAL(KIND=JPRB), INTENT (   IN) :: RLVTT    
REAL(KIND=JPRB), INTENT (   IN) :: RLSTT    
REAL(KIND=JPRB), INTENT (   IN) :: RTT      
REAL(KIND=JPRB), INTENT (   IN) :: RDT      
REAL(KIND=JPRB), INTENT (   IN) :: RALPW    
REAL(KIND=JPRB), INTENT (   IN) :: RBETW    
REAL(KIND=JPRB), INTENT (   IN) :: RGAMW    
REAL(KIND=JPRB), INTENT (   IN) :: RALPS    
REAL(KIND=JPRB), INTENT (   IN) :: RBETS    
REAL(KIND=JPRB), INTENT (   IN) :: RGAMS    
REAL(KIND=JPRB), INTENT (   IN) :: RALPD    
REAL(KIND=JPRB), INTENT (   IN) :: RBETD    
REAL(KIND=JPRB), INTENT (   IN) :: RGAMD    
REAL(KIND=JPRB), INTENT (   IN) :: RKAPPA   
REAL(KIND=JPRB), INTENT (   IN) :: RATM     
REAL(KIND=JPRB), INTENT (   IN) :: RD       
REAL(KIND=JPRB), INTENT (   IN) :: UPRETMIN 
REAL(KIND=JPRB), INTENT (   IN) :: VKARMN   
REAL(KIND=JPRB), INTENT (   IN) :: UPRETMAX 
REAL(KIND=JPRB), INTENT (   IN) :: ARSB2    
REAL(KIND=JPRB), INTENT (   IN) :: ECTMIN   
REAL(KIND=JPRB), INTENT (   IN) :: AKN      
REAL(KIND=JPRB), INTENT (   IN) :: ALPHAT   
REAL(KIND=JPRB), INTENT (   IN) :: ALMAV    
REAL(KIND=JPRB), INTENT (   IN) :: UDECT    
REAL(KIND=JPRB), INTENT (   IN) :: USHEARM  
REAL(KIND=JPRB), INTENT (   IN) :: EDB      
REAL(KIND=JPRB), INTENT (   IN) :: EDC      
REAL(KIND=JPRB), INTENT (   IN) :: EDD      
REAL(KIND=JPRB), INTENT (   IN) :: USURIC
REAL(KIND=JPRB),   INTENT(OUT), TARGET :: PSTACK (KLON, KPSTSZ)
INTEGER(KIND=JPIM),INTENT(IN)    :: KPTRST
INTEGER(KIND=JPIM),INTENT(IN)    :: KPSTSZ
END SUBROUTINE SIMPLE4_ACTURB
END INTERFACE
