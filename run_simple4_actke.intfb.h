INTERFACE
#ifdef GPU
ATTRIBUTES (GLOBAL) &
#endif
SUBROUTINE RUN_SIMPLE4_ACTKE(KPROMA, KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV, PAPHI,&
 & PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PLSCPE, PCD, PCH, PGZ0, PTS,&
 & PQS, PQICE, PQLI, PECT, PPRODTH, PNLAB, PNLABCVP, PKTROV, PKUROV, PXTROV,&
 & PXUROV, PNBVNO, PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN, PFECT, PECT1, PTPRDY, PEDR,&
 & RG, RPRTH, ECTMIN, TSPHY, ACBRPHIM, ADISE, ADISI, AKN, ALD, ALMAV, ALMAVE,&
 & ALMAVX, ALPHAE, ALPHAT, ARSB2, ARSC1, ECTMAX, EDB, EDC, EDD, RALPD, RALPS,&
 & RALPW, RATM, RBETD, RBETS, RBETW, RCPV, RCS, RCW, RD, RDT, RETV, RGAMD, RGAMS,&
 & RGAMW, RKAPPA, RLSTT, RLVTT, RTT, RV, UDECT, UPRETMAX, UPRETMIN, USHEARM,&
 & USURIC, VKARMN, KSIZST, KPTRST, PSTACK) 
USE PARKIND1, ONLY : JPIM, JPRB
INTEGER(KIND=JPIM), INTENT ( IN) :: KPROMA
INTEGER(KIND=JPIM), INTENT ( IN) :: KIDIA
INTEGER(KIND=JPIM), INTENT ( IN) :: KFDIA
INTEGER(KIND=JPIM), INTENT ( IN) :: KLON
INTEGER(KIND=JPIM), INTENT ( IN) :: KTDIAT
INTEGER(KIND=JPIM), INTENT ( IN) :: KTDIAN
INTEGER(KIND=JPIM), INTENT ( IN) :: KLEV
REAL(KIND=JPRB) , INTENT ( IN) :: PAPHI (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PAPHIF (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PAPRS (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PAPRSF (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PDELP (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PR (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PT (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PU (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PV (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PQ (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PLSCPE (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PCD (KLON)
REAL(KIND=JPRB) , INTENT ( IN) :: PCH (KLON)
REAL(KIND=JPRB) , INTENT ( IN) :: PGZ0 (KLON)
REAL(KIND=JPRB) , INTENT ( IN) :: PTS (KLON)
REAL(KIND=JPRB) , INTENT ( IN) :: PQS (KLON)
REAL(KIND=JPRB) , INTENT (INOUT) :: PQICE (KLON, KLEV)
REAL(KIND=JPRB) , INTENT (INOUT) :: PQLI (KLON, KLEV)
REAL(KIND=JPRB) , INTENT (INOUT) :: PECT (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PPRODTH (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PNLAB (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: PNLABCVP (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PKTROV (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PKUROV (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PXTROV (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PXUROV (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PNBVNO (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PNEBS (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PQCS (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PNEBS0 (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PQCS0 (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PCOEFN (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PFECT (KLON, 0:KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PECT1 (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PTPRDY (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( OUT) :: PEDR (KLON, KLEV)
REAL(KIND=JPRB) , INTENT ( IN) :: RG
REAL(KIND=JPRB) , INTENT ( IN) :: RPRTH
REAL(KIND=JPRB) , INTENT ( IN) :: ECTMIN
REAL(KIND=JPRB) , INTENT ( IN) :: TSPHY
REAL(KIND=JPRB) , INTENT ( IN) :: ACBRPHIM
REAL(KIND=JPRB) , INTENT ( IN) :: ADISE
REAL(KIND=JPRB) , INTENT ( IN) :: ADISI
REAL(KIND=JPRB) , INTENT ( IN) :: AKN
REAL(KIND=JPRB) , INTENT ( IN) :: ALD
REAL(KIND=JPRB) , INTENT ( IN) :: ALMAV
REAL(KIND=JPRB) , INTENT ( IN) :: ALMAVE
REAL(KIND=JPRB) , INTENT ( IN) :: ALMAVX
REAL(KIND=JPRB) , INTENT ( IN) :: ALPHAE
REAL(KIND=JPRB) , INTENT ( IN) :: ALPHAT
REAL(KIND=JPRB) , INTENT ( IN) :: ARSB2
REAL(KIND=JPRB) , INTENT ( IN) :: ARSC1
REAL(KIND=JPRB) , INTENT ( IN) :: ECTMAX
REAL(KIND=JPRB) , INTENT ( IN) :: EDB
REAL(KIND=JPRB) , INTENT ( IN) :: EDC
REAL(KIND=JPRB) , INTENT ( IN) :: EDD
REAL(KIND=JPRB) , INTENT ( IN) :: RALPD
REAL(KIND=JPRB) , INTENT ( IN) :: RALPS
REAL(KIND=JPRB) , INTENT ( IN) :: RALPW
REAL(KIND=JPRB) , INTENT ( IN) :: RATM
REAL(KIND=JPRB) , INTENT ( IN) :: RBETD
REAL(KIND=JPRB) , INTENT ( IN) :: RBETS
REAL(KIND=JPRB) , INTENT ( IN) :: RBETW
REAL(KIND=JPRB) , INTENT ( IN) :: RCPV
REAL(KIND=JPRB) , INTENT ( IN) :: RCS
REAL(KIND=JPRB) , INTENT ( IN) :: RCW
REAL(KIND=JPRB) , INTENT ( IN) :: RD
REAL(KIND=JPRB) , INTENT ( IN) :: RDT
REAL(KIND=JPRB) , INTENT ( IN) :: RETV
REAL(KIND=JPRB) , INTENT ( IN) :: RGAMD
REAL(KIND=JPRB) , INTENT ( IN) :: RGAMS
REAL(KIND=JPRB) , INTENT ( IN) :: RGAMW
REAL(KIND=JPRB) , INTENT ( IN) :: RKAPPA
REAL(KIND=JPRB) , INTENT ( IN) :: RLSTT
REAL(KIND=JPRB) , INTENT ( IN) :: RLVTT
REAL(KIND=JPRB) , INTENT ( IN) :: RTT
REAL(KIND=JPRB) , INTENT ( IN) :: RV
REAL(KIND=JPRB) , INTENT ( IN) :: UDECT
REAL(KIND=JPRB) , INTENT ( IN) :: UPRETMAX
REAL(KIND=JPRB) , INTENT ( IN) :: UPRETMIN
REAL(KIND=JPRB) , INTENT ( IN) :: USHEARM
REAL(KIND=JPRB) , INTENT ( IN) :: USURIC
REAL(KIND=JPRB) , INTENT ( IN) :: VKARMN
INTEGER(KIND=JPIM), INTENT ( IN) :: KSIZST
INTEGER(KIND=JPIM), INTENT ( IN) :: KPTRST
REAL(KIND=JPRB) , INTENT (INOUT) :: PSTACK (KLON, KSIZST)
END SUBROUTINE RUN_SIMPLE4_ACTKE
END INTERFACE
