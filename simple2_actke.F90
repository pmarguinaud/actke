SUBROUTINE SIMPLE2_ACTKE ( KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV, PAPHI,      &
& PAPHIF, PAPRS, PAPRSF, PDELP, PR, PT, PU, PV, PQ, PLSCPE, PCD, PCH, PGZ0, PTS, &
& PQS, PQICE, PQLI, PECT, PPRODTH, PNLAB, PNLABCVP , PKTROV, PKUROV, PXTROV,     &
& PXUROV, PNBVNO, PNEBS, PQCS, PNEBS0, PQCS0, PCOEFN , PFECT , PECT1 , PTPRDY,   &
& PEDR, RG, RPRTH, ECTMIN, TSPHY, ACBRPHIM, ADISE, ADISI, AKN, ALD, ALMAV,       &
& ALMAVE, ALMAVX, ALPHAE, ALPHAT, ARSB2, ARSC1, ECTMAX, EDB, EDC, EDD, RALPD,    &
& RALPS, RALPW, RATM, RBETD, RBETS, RBETW, RCPV, RCS, RCW, RD, RDT, RETV, RGAMD, &
& RGAMS, RGAMW, RKAPPA, RLSTT, RLVTT, RTT, RV, UDECT, UPRETMAX, UPRETMIN,        &
& USHEARM, USURIC, VKARMN)

!**** *ACTKE * - SCHEMA DE TURBULENCE TKE

!     Sujet.
!     ------
!     - APPEL DES SOUS-PROGRAMMES ACBL89, ACTURB, ACEVOLET

!**   Interface.
!     ----------
!        *CALL* *ACTKE*

!-----------------------------------------------------------------------

!     Auteur.
!     -------
!       04-11, Francois Bouyssel

!   Modifications.
!   --------------
!      2006-04-11 E. Bazile : Ajout de CDLOCK.
!      2006-05-04 E. Bazile : Possibilite de passer l'ECT sur Full-level 
!                             (LECTFL) pour l'advection.
!      2007-05-10 E. Bazile : PXTXX en sortie, PQC0, PNEBS0 pour rayt.
!      2008-04-28 E. Bazile : Introduction of PPROTH and removal PRS, PSTAB
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      2009-11-10 E. Bazile : Input of PDELP for ACEVOLET.
!      2010-12-01 E. Bazile : Output for AROCLDIA of TKE(t+dt)
!      2011-10-01 E. Bazile : TKE fluxes for DDH and use of HL2FL and FL2HL
!      2014-10-14 E. Bazile : EDR : Output similar to TKE dissipation (>0)
!      2015-03-11 J.M Piriou: fix bug in case of LFLEXDIA=T: introduce ZDIAG array.
!-----------------------------------------------------------------------


! -   ARGUMENTS D'ENTREE.
!   -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE.
! KTDIAN     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE + NEBULOSITE.
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".

! - 2D (0:KLEV) .

! PAPHI      : GEOPOTENTIEL AUX DEMI-NIVEAUX.
! PAPRS      : PRESSION AUX DEMI-NIVEAUX.
! PPRODTH    : PRODUCTION THERMIQUE DU A AU SCHEMA SHALLOW.

! - 2D (1:KLEV) .

! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PDELP      : EPAISSEUR EN PRESSION DE LA COUCHE.
! PR         : CONSTANTE DES GAZ POUR L'AIR.
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.
! PQ         : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
! PECT       : ENERGIE CINETIQUE TURBULENTE.

! - 1D (DIAGNOSTIQUE) .

! PCD        : COEFFICIENT D'ECHANGE EN SURFACE POUR U ET V
! PCH        : COEFFICIENT D'ECHANGE EN SURFACE POUR T ET Q
! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE
! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.

!-----------------------------------------------------------------------

! -   ARGUMENTS EN ENTREE/SORTIE.
!     ---------------------------

! - 2D (1:KLEV) .

! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "PRONOSTIQUE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "PRONOSTIQUE".
! PNLAB      : Si 1 Presence d'un nuage Shallow (used in ACBL89)
! PNLABCVP   : Si 1 Presence d'un nuage Deep    (used in ACBL89)


!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T ET Q EN KG/(M*M*S).
! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
! PXTROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKTROV.
! PXUROV     : MULTIPLICATEUR "ANTI-FIBRILLATION" DE PKUROV.
! PNBVNO     : CARRE DE FR. BRUNT-VAISALA DIVISEE PAR G FOIS LA DENSITE.

! - 2D (1:KLEV) .

! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME.
! PQCS       : EAU CONDENSEE STRATIFORME.
! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME APRES AJUSTEMENT.
!            : STRATIFORM FRACTIONAL CLOUDINESS APRES AJUSTEMENT.
! PQCS0      : CONTENU "STRATIFORME" EN CONDENSAT NUAGEUX POUR RAYT.
!            : STRATIRORM CLOUD WATER (LIQUID + SOLID) FOR RADIATION.
! PNEBS0     : NEBULOSITE PARTIELLE STRATIFORME POUR RAYT.
! PCOEFN     : COEFFICIENT STATISTIQUE POUR LES FLUX D'EAUX CONDENSEES.
! PTPRDY     : tendance de TKE due a la production dynamique.
! PEDR       : EDR tendance de TKE due a la dissipation (calcul specifique)

!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM   ,JPRB








 

!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAT 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PU(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSCPE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCD(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PCH(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPRODTH(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLAB(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLABCVP(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PXUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNBVNO(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS0(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PCOEFN(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECT1(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PFECT(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PTPRDY(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PEDR(KLON,KLEV)
REAL(KIND=JPRB), INTENT (   IN) :: ACBRPHIM 
REAL(KIND=JPRB), INTENT (   IN) :: ADISE    
REAL(KIND=JPRB), INTENT (   IN) :: ADISI    
REAL(KIND=JPRB), INTENT (   IN) :: AKN      
REAL(KIND=JPRB), INTENT (   IN) :: ALD      
REAL(KIND=JPRB), INTENT (   IN) :: ALMAV    
REAL(KIND=JPRB), INTENT (   IN) :: ALMAVE   
REAL(KIND=JPRB), INTENT (   IN) :: ALMAVX   
REAL(KIND=JPRB), INTENT (   IN) :: ALPHAE   
REAL(KIND=JPRB), INTENT (   IN) :: ALPHAT   
REAL(KIND=JPRB), INTENT (   IN) :: ARSB2    
REAL(KIND=JPRB), INTENT (   IN) :: ARSC1    
REAL(KIND=JPRB), INTENT (   IN) :: ECTMAX   
REAL(KIND=JPRB), INTENT (   IN) :: EDB      
REAL(KIND=JPRB), INTENT (   IN) :: EDC      
REAL(KIND=JPRB), INTENT (   IN) :: EDD      
REAL(KIND=JPRB), INTENT (   IN) :: RALPD    
REAL(KIND=JPRB), INTENT (   IN) :: RALPS    
REAL(KIND=JPRB), INTENT (   IN) :: RALPW    
REAL(KIND=JPRB), INTENT (   IN) :: RATM     
REAL(KIND=JPRB), INTENT (   IN) :: RBETD    
REAL(KIND=JPRB), INTENT (   IN) :: RBETS    
REAL(KIND=JPRB), INTENT (   IN) :: RBETW    
REAL(KIND=JPRB), INTENT (   IN) :: RCPV     
REAL(KIND=JPRB), INTENT (   IN) :: RCS      
REAL(KIND=JPRB), INTENT (   IN) :: RCW      
REAL(KIND=JPRB), INTENT (   IN) :: RD       
REAL(KIND=JPRB), INTENT (   IN) :: RDT      
REAL(KIND=JPRB), INTENT (   IN) :: RETV     
REAL(KIND=JPRB), INTENT (   IN) :: RGAMD    
REAL(KIND=JPRB), INTENT (   IN) :: RGAMS    
REAL(KIND=JPRB), INTENT (   IN) :: RGAMW    
REAL(KIND=JPRB), INTENT (   IN) :: RKAPPA   
REAL(KIND=JPRB), INTENT (   IN) :: RLSTT    
REAL(KIND=JPRB), INTENT (   IN) :: RLVTT    
REAL(KIND=JPRB), INTENT (   IN) :: RTT      
REAL(KIND=JPRB), INTENT (   IN) :: RV       
REAL(KIND=JPRB), INTENT (   IN) :: UDECT    
REAL(KIND=JPRB), INTENT (   IN) :: UPRETMAX 
REAL(KIND=JPRB), INTENT (   IN) :: UPRETMIN 
REAL(KIND=JPRB), INTENT (   IN) :: USHEARM  
REAL(KIND=JPRB), INTENT (   IN) :: USURIC   
REAL(KIND=JPRB), INTENT (   IN) :: VKARMN   

REAL(KIND=JPRB), INTENT (   IN) :: RG     
REAL(KIND=JPRB), INTENT (   IN) :: RPRTH  
REAL(KIND=JPRB), INTENT (   IN) :: ECTMIN 
REAL(KIND=JPRB), INTENT (   IN) :: TSPHY  
 


!-----------------------------------------------------------------------

INTEGER(KIND=JPIM) :: JLEV, JLON
! Attention tableau pour acturb, acbl89, et acevolet de dim KLEV mais sur 
! les 1/2 niveaux
REAL(KIND=JPRB) :: ZUSLE  (KIDIA:KFDIA,KLEV), ZLMECT(KIDIA:KFDIA,KLEV), ZPHI3(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZPRODTH(KIDIA:KFDIA,KLEV),ZPRDY(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZDIAG(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZDIFF(KIDIA:KFDIA,KLEV),ZDISS(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZECT(KIDIA:KFDIA,KLEV), ZECT1(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZDELPSG(KIDIA:KFDIA,KLEV)
!--------------------------------------------------------------------------
REAL(KIND=JPRB) :: ZDET(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZKCLS(KIDIA:KFDIA), ZECTCLS(KIDIA:KFDIA)
REAL(KIND=JPRB) :: ZTABHL(KIDIA:KFDIA,0:KLEV),ZTABFL(KIDIA:KFDIA,KLEV),ZDIFFAR(KIDIA:KFDIA,KLEV)
! For DDH and MUSC and in future in CPTEND_NEW ???
REAL(KIND=JPRB) :: ZFPRTH(KIDIA:KFDIA,0:KLEV),ZFPRDY(KIDIA:KFDIA,0:KLEV),&
    & ZFDISS(KIDIA:KFDIA,0:KLEV),ZFDIFF(KIDIA:KFDIA,0:KLEV),ZFCORTKE(KIDIA:KFDIA,0:KLEV)
REAL(KIND=JPRB) :: ZTPRTH(KIDIA:KFDIA,KLEV),&
    & ZTDISS(KIDIA:KFDIA,KLEV),ZTDIFF(KIDIA:KFDIA,KLEV),ZTCORTKE(KIDIA:KFDIA,KLEV),ZEDR(KIDIA:KFDIA,KLEV)

REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------

#include "simple2_acbl89.intfb.h"
#include "simple2_acturb.intfb.h"
#include "simple2_acevolet.intfb.h"
#include "simple2_hl2fl.intfb.h"
#include "simple2_fl2hl.intfb.h"



!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
DO JLON = KIDIA, KFDIA
  ZTDIFF(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  PEDR(JLON,:)    =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZTDISS(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZTPRTH(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  PTPRDY(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZTCORTKE(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZFDIFF(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZFDISS(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZFPRTH(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZFPRDY(JLON,:)  =0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZFCORTKE(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZDET(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZDELPSG(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZDIAG(JLON,:)=0.0_JPRB
ENDDO
!     ------------------------------------------------------------------
! 0.   Passage eventuel sur les demi-niveaux dans le cas ou LECTFL=.TRUE.
!      en effet en sortie d'ACTKE on a passe l'ECT sur les niveaux pleins pour 
!      l'advecter.
DO JLON = KIDIA, KFDIA
  ZDIFFAR(JLON,:)=0._JPRB
ENDDO

! PECT en entree sur les FL donc passage sur les HL pour les calculs physiques 
CALL SIMPLE2_FL2HL ( KIDIA, KFDIA, KLON, 1, KLEV, PAPRS, PAPRSF, PECT, ZECT, 1, &
& 'FL2HL  ')

!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.   Trois parties : ACBL89 (longueur de melange de
!      Bougeault-Lacarerre-1989) + ACTURB (calculs de
!      l'ect et de ses evolutions) + ACEVOLET (evolution
!      proprement dite de l'ect + calcul des flux
!      des tendances physiques pour CPTEND). 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

DO JLEV = 1, KLEV
  DO JLON = KIDIA,KFDIA
    ZUSLE  (JLON,JLEV) = 0.0_JPRB
    ZLMECT (JLON,JLEV) = 0.0_JPRB
    ZPHI3  (JLON,JLEV) = 0.0_JPRB
  ENDDO
ENDDO

DO JLON = KIDIA,KFDIA
  ZECTCLS (JLON) = 0.0_JPRB
  ZKCLS   (JLON) = 0.0_JPRB
ENDDO

DO JLEV=KTDIAT,KLEV
  DO JLON=KIDIA,KFDIA
    ZECT(JLON,JLEV) = MAX( ECTMIN, ZECT(JLON,JLEV) )
    ZDELPSG(JLON,JLEV)=PDELP(JLON,JLEV)/RG
  ENDDO
ENDDO
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.1   Calculs : des longueurs de melange (ZLMECT, ZUSLE)
!       de l'ect en surface (ZECTCLS)
!       de la production thermique (ZPRODTH)
!       des coefficients d'echange (PKTROV,PKUROV, ZKCLS)
!       de la frequence de Brunt-Vaisala (PNBVNO)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     

CALL SIMPLE2_ACBL89   ( KIDIA, KFDIA, KLON, KTDIAN, KLEV,  PAPHI, PAPHIF, PAPRS, &
& PAPRSF, PT,  ZECT, PQ, PQICE, PQLI, PNLAB, PNLABCVP,  PGZ0, PTS,  ZUSLE,       &
& ZLMECT, ZPHI3, RG, RETV, RKAPPA, RATM, ALMAVX, ACBRPHIM, VKARMN, ALD, ECTMIN,  &
& ALMAVE, ARSC1)

CALL SIMPLE2_ACTURB   ( KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,  PAPHI, PAPHIF, &
& PAPRS, PAPRSF, PR, PT,  PU, PV, ZECT, PQ, PLSCPE,  ZLMECT, ZPHI3,  PGZ0, PTS,   &
& PQS,   PQICE, PQLI,  PKTROV, PKUROV, PNBVNO,  ZPRODTH, PNEBS, PQCS, PCOEFN,     &
& ZKCLS, ZECTCLS, RG, RV, RCPV, RETV, RCW, RCS, RLVTT, RLSTT, RTT, RDT, RALPW,    &
& RBETW, RGAMW, RALPS, RBETS, RGAMS, RALPD, RBETD, RGAMD, RKAPPA, RATM, RD,       &
& UPRETMIN, VKARMN, UPRETMAX, ARSB2, ECTMIN, AKN, ALPHAT, ALMAV, UDECT, USHEARM,  &
& EDB, EDC, EDD, USURIC )

DO JLON = KIDIA, KFDIA
  PNEBS0(JLON,:)=PNEBS(JLON,:)
ENDDO
DO JLON = KIDIA, KFDIA
  PQCS0(JLON,:)=PQCS(JLON,:)
ENDDO

DO JLON = KIDIA, KFDIA
  PXTROV(JLON,:)=1.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  PXUROV(JLON,:)=1.0_JPRB
ENDDO

DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZPRODTH(JLON,JLEV)=ZPRODTH(JLON,JLEV)+ &
          &              RPRTH*MAX(0._JPRB,PPRODTH(JLON,JLEV))
   ENDDO
ENDDO
!- - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.2 Calcul d'evolution de l'ECT
!- - - - - - - - - - - - - - - - - - - - - - - - - -
CALL SIMPLE2_ACEVOLET ( KIDIA, KFDIA, KLON, KTDIAT, KTDIAN, KLEV,   PAPHI,       &
& PAPHIF, PAPRS, PAPRSF, ZECT,   PKUROV, PR, PT, PU, PV, ZUSLE, ZPRODTH,  ZKCLS, &
& ZECTCLS, ZECT1, ZPRDY, ZDIFF, ZDISS, RG, ECTMIN, ADISE, ALPHAE, ADISI, ECTMAX, &
& TSPHY)

!     ------------------------------------------------------------------
! 2.   Passage eventuel sur les niveaux pleins dans le cas ou LECTFL=.TRUE.
!      pour l'advecter.
DO JLON = KIDIA, KFDIA
  PECT1(JLON,:)=ECTMIN
ENDDO
! Calcul de l'EDR avec protection de la valeur min de L a 0.01
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZEDR(JLON,JLEV)= MIN(100._JPRB,ZUSLE(JLON,JLEV)*RG)* &
           & (0.5_JPRB*(ZECT1(JLON,JLEV)+PECT(JLON,JLEV)))**1.5
   ENDDO
ENDDO

! Passage de ZECT1 sur les niveaux pleins pour calculer la tendance sur les FL
! puis le flux  
CALL SIMPLE2_HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,  PAPRS, PAPRSF, ZECT1, 1, &
& PECT1, 'HL2FL  ') 
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      ZDET(JLON,JLEV)=(PECT1(JLON,JLEV)-PECT(JLON,JLEV))/TSPHY
   ENDDO
ENDDO
! Production dynamique
CALL SIMPLE2_HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,  PAPRS, PAPRSF, ZPRDY, 1, &
& PTPRDY, 'HL2FL  ') 
! EDR
CALL SIMPLE2_HL2FL ( KIDIA, KFDIA, KLON, 1, KLEV,  PAPRS, PAPRSF, ZEDR, 1, PEDR, &
& 'HL2FL  ') 


! CALCUL DU FLUX PAR INTEGRATION DE LA TENDANCE DE HAUTS EN BAS
!         LES FLUX SONT SUPPOSES NULS AU PREMIER NIVEAU (KTDIA) DE
!         CALCUL (FLUX AU NIVEAU DU MODELE).
DO JLEV = KTDIAT, KLEV
   DO JLON = KIDIA,KFDIA
      PFECT(JLON,JLEV)=PFECT(JLON,JLEV-1)- ZDET(JLON,JLEV) &
           & * ZDELPSG(JLON,JLEV)
   ENDDO
ENDDO
!-----------------------------------------------------------------------


END SUBROUTINE SIMPLE2_ACTKE
