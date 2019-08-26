SUBROUTINE SIMPLE2_ACTURB  ( KIDIA,  KFDIA,  KLON,   KTDIAT, KTDIAN, KLEV, PAPHI, &
& PAPHIF, PAPRS,  PAPRSF, PR,     PT, PU, PV, PECT,   PQV,    PLSCPE, PLMECT,     &
& PPHI3, PGZ0,   PTS,    PQS, PQICE,  PQLI, PKTROV, PKUROV, PNBVNO, PPRODTH,      &
& PNEBS, PQCS,   PL3F2, PGKCLS, PECTCLS, RG, RV, RCPV, RETV, RCW, RCS, RLVTT,     &
& RLSTT, RTT, RDT, RALPW, RBETW, RGAMW, RALPS, RBETS, RGAMS, RALPD, RBETD, RGAMD, &
& RKAPPA, RATM, RD, UPRETMIN, VKARMN, UPRETMAX, ARSB2, ECTMIN, AKN, ALPHAT,       &
& ALMAV, UDECT, USHEARM, EDB, EDC, EDD, USURIC)

!**** *ACTURB - CALCUL DES COEFFICIENTS D'ECHANGE VERTICAL TURBULENT ET
!               DE LA PRODUCTION THERMIQUE HUMIDE <w'(THETA)vl'> (QUI
!               DEPEND DE LA FONCTION STATISTIQUE ASYMETRIQUE F2 DE 
!               BOUGEAULT, PLUS L'AJOUT DU TERME "LAMBDA3" DE BECHTOLD).
!               CALCULS DES NEBULOSITE ET EAU CONDENSEE STRATIFORMES,
!               QUI DEPENDENT DES FONCTIONS STATISTIQUES ASYMETRIQUES 
!               F0 ET F1 DE BOUGEAULT. CES FONCTIONS (F0,F1,F2) SONT 
!               TABULEES, COMME DANS MESO-NH.

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCUL DES COEFFICIENTS D'ECHANGES VERTICAUX TURBULENTS (DIMEN-
!       ON (DP/(G*DT)) ET DE LA STABILITE STATIQUE (DIMENSION (U/DP)**2)

!     - COMPUTATION OF VERTICAL TURBULENT EXCHANGE COEFFICIENTS
!       (DIMENSION (DP/(G*DT)) AND OF STATIC STABILITY (DIMENSION
!       (U/DP)**2) .

!**   Interface.
!     ----------
!        *CALL* *ACTURB*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE, EXCEPT FOR KTDIAT AND KTDIAN.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
! KTDIAT     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE.
! KTDIAN     : INDICE DE DEPART DES BOUCLES VERTICALES (1 EN GENERAL)
!              POUR LES CALCULS DE TURBULENCE + NEBULOSITE.
! KLEV       : DIMENSION VERTICALE DES TABLEAUX "FULL LEVEL".

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PAPHI      : GEOPOTENTIEL AUX DEMI-NIVEAUX.
! PAPRS      : PRESSION AUX DEMI-NIVEAUX.

! - 2D (1:KLEV) .

! PAPHIF     : GEOPOTENTIEL AUX NIVEAUX DES COUCHES.
! PAPRSF     : PRESSION AUX NIVEAUX DES COUCHES.
! PR         : CONSTANTE DES GAZ POUR L'AIR.
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PU         : COMPOSANTE EN X DU VENT.
! PV         : COMPOSANTE EN Y DU VENT.
! PECT       : ENERGIE CINETIQUE TURBULENTE.
! PQV        : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PLSCPE     : RAPPORT EFECTIF DES L ET CP EN CONDENSATION/EVAPORATION.
! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "PRONOSTIQUE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "PRONOSTIQUE".
! PLMECT     : UNE LONGUEUR DE MELANGE (FOIS G) POUR ACNEBR

! - 1D (DIAGNOSTIQUE) .

! PCD        : COEFFICIENT D'ECHANGE EN SURFACE POUR U ET V
! PCH        : COEFFICIENT D'ECHANGE EN SURFACE POUR T ET Q
! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE
! PQS        : HUMIDITE SPECIFIQUE DE SURFACE.

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (0:KLEV) .

! PKTROV     : COEFFICIENT D'ECHANGE VERTICAL DE T ET Q EN KG/(M*M*S).
! PKUROV     : COEFFICIENT D'ECHANGE VERTICAL DE U ET V EN KG/(M*M*S).
!              !! PKUROV et PKTROV : egaux a g*K*P/(R*T*d(Phi))
! PNBVNO     : CARRE DE FR. BRUNT-VAISALA DIVISEE PAR G FOIS LA DENSITE.

! - 2D (1:KLEV) .

! PQICE      : HUMIDITE SPECIFIQUE  SOLIDE "RADIATIVE".
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE "RADIATIVE".
! PPRODTH    : LA PRODUCTION THERMIQUE : +(g/T)*(w'X') avec X=(THETA)vl
! PNEBS      : NEBULOSITE PARTIELLE STRATIFORME.
! PQCS       : EAU CONDENSEE STRATIFORME.
! PL3F3      : PRODUIT DES FONCTIONS "LAMBDA3" ET "F2" DE BECHTOLD ET DE 
!              BOUGEAULT, INTERVENANT DANS LA PONDERATION DES PARTIES
!              "AIR SEC" ET "AIR SATURE" (PRODUCTION THERMIQUE, FLUX
!              TURBULENTS "HUMIDES", etc ...)

! - 1D (KLON) .

! PGKCLS     : COEFFICIENT D'ECHANGE A LA SURFACE !! en (m/s)**3 (=g*K)
! PECTCLS    : ENERGIE CINETIQUE TURBULENTE A LA SURFACE.

!-----------------------------------------------------------------------

! -   ARGUMENTS IMPLICITES.
!     ---------------------

! COMMON/YOMCST /
! COMMON/YOMPHY0/

!-----------------------------------------------------------------------

!     Externes.
!     ---------

!     Methode.
!     --------

!     Auteur.
!     -------
!      2002-03, P. Marquet.
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -
!               From the LAST part of the old ACCOEFKE code,
!               written in 1993 in ARPEGE format by P. Lacarrere
!               from the old PERIDOT code, then tested by C. Bossuet
!               in 1997/98 using an Eulerian T42 Arpege GCM, then 
!               improved and tested by P. Marquet with the next 
!               versions of Arpege GCM (semi-lagrangian, SCM,
!               EUROCS Shallow convection case, with the use of
!               the new ideas coming from Meso-NH developments).
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -

!     Modifications.
!     --------------
!      2003-11-05, P. Marquet : LLIMQ1 switch 
!      2004-03-19, P. Marquet : PQLI and PQICE = prognostic as input,
!                                              = statiform  as output.
!      2005-02-02, P. Marquet : Top PBL Entrainment (if LPBLE), from
!                  the ideas tested in ACCOFGY (H. Grenier, F. Gueremy)
!      2005-02-18, P. Marquet : ZECTBLK in limited by PECTCLS.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2007-05-09, E. Bazile : ZEPSIG and no cloud with ZSTAB=0 at the surface.
!      2007-05-09, Y. Bouteloup : AGRE2 for LPBLE.
!      2008-02-18, P. Marquet : set ILEVT to ILEVBI(JLON)
!                               and no more  ILEVBI(JLON)-1
!      2008-02-21, Y. Bouteloup : LECTREP + LECTQ1
!      2008-03-19, P. Marquet : change the definition of ZTHETAVL 
!                  (see page 360 and the appendix-B in Grenier 
!                   and Bretherton, MWR 2001)
!      2008-04-25, E. Bazile and P. Marquet : Correction for the AGRE2 term
!                  with  A1*[1+A2*L*qc/(cp*d(theta_vl))] instead of
!                   A1*[1+A2/d(theta_vl)], with "L*qc/cp" missing...)
!      2008-10-06, E. Bazile : Computation of a 'unified' PBL height for 
!                  the TKEcls, the top-entrainment and the diagnostic
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      2011-06: M. Jerczynski - some cleaning to meet norms
!      2012-01-28, E. Bazile Correction for ZDTL and new option  LECTFL0
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB







!-----------------------------------------------------------------------

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
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KIDIA:KFDIA,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PLSCPE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PLMECT(KIDIA:KFDIA,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PPHI3(KIDIA:KFDIA,KLEV) 
 
 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQS(KLON) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(INOUT) :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKTROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PKUROV(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNBVNO(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPRODTH(KIDIA:KFDIA,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PNEBS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PQCS(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PL3F2(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PGKCLS(KIDIA:KFDIA) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PECTCLS(KIDIA:KFDIA)

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
 

!-----------------------------------------------------------------------

REAL(KIND=JPRB) :: ZSTAB (KIDIA:KFDIA), ZRS(KIDIA:KFDIA),ZBLH(KIDIA:KFDIA)
REAL(KIND=JPRB) :: ZRTV   (KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZGDZF   (KIDIA:KFDIA,KLEV),ZZ     (KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZTHETA  (KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZTHETALF(KIDIA:KFDIA,KLEV),ZLOCPEXF(KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZTHETAVL(KIDIA:KFDIA,KLEV)

REAL(KIND=JPRB) :: ZLMECTF(KIDIA:KFDIA,KLEV),ZECTF  (KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZGKTF  (KIDIA:KFDIA,KLEV)
REAL(KIND=JPRB) :: ZGKTH  (KIDIA:KFDIA,0:KLEV), ZGKUH  (KIDIA:KFDIA,0:KLEV)
REAL(KIND=JPRB) :: ZLM  (KIDIA:KFDIA,0:KLEV) 

REAL(KIND=JPRB) :: ZUSTAR(KIDIA:KFDIA),ZWSTAR(KIDIA:KFDIA)

!- - - - - - - - - - - - - - - -
! For the Top-PBL Entrainment :
!- - - - - - - - - - - - - - - -
REAL(KIND=JPRB) :: ZECTINT(KIDIA:KFDIA)     ! ect moyenne de la cla (sans surf)
REAL(KIND=JPRB) :: ZQCINT (KIDIA:KFDIA)     ! qc_cloud moyen de la cla (sans surf)
INTEGER(KIND=JPIM) :: ICM(KIDIA:KFDIA,KLEV)    ! indice de couche melangee
INTEGER(KIND=JPIM) :: ILEVBI(KIDIA:KFDIA)      ! niveau de la base de l'inversion
REAL(KIND=JPRB) :: ZBI, ZDEN, ZECTBLK, ZNUM, ZLINV, ZQCBLK
REAL(KIND=JPRB) :: ZGKENT
INTEGER(KIND=JPIM) :: ICLA, ILEVM1, ILEVT

! Tableaux pour les fonctions de "condens.f90" de Meso-NH
REAL(KIND=JPRB) :: ZN1D(-22:11), ZRC1D(-22:11), ZSRC1D(-22:11)

INTEGER(KIND=JPIM) :: IHCLPMAX, IHCLPMIN, IJLEVM1, IJLEVP1,&
 & INIV, INQ1, JLEV, JLON  

LOGICAL :: LLIMQ1

REAL(KIND=JPRB) ::  Z2B, Z3B, Z3BCF, ZA, ZAA, ZCE1, ZCIS, ZCK, ZCTO, &
 & ZDD, ZDELTQF, ZDELTQF1, ZDELTQF2, ZDELTQH, &
 & ZDI, ZDIFFC, ZDIFFH, ZDLEWF, ZDLEWF1, ZDLEWF2, &
 & ZDPHI, ZDPHI0, ZDQLST, ZDQW, ZDS, &
 & ZDSTA, ZDT, ZDTETA, ZDTL, ZDU2, &
 & ZECTH, ZEPDELT, ZEPNEBS, ZECTBLH,&
 & ZEPS, ZEPS1, ZEPSQ, ZEPSQ1, ZEPSV, &
 & ZEW, ZEW1, ZEW2, ZFACT, ZGALP2, &
 & ZGLMT2, ZGLMU2, ZGLT, ZGLTZ, ZGLU, ZGLUZ, &
 & ZGZ, ZH, ZH1, ZH2, ZIGMAS, ZIGMAS2, &
 & ZINC, ZIS, ZLCPM1, ZLCPP1, ZLMECT, ZLOI, ZLOS, &
 & ZLSCPEF, ZLSCPEF1, ZLSCPEF2, ZLSCPEH, ZMAXQ1, &
 & ZMODU, ZNEBLOW, ZPHI3MIN, &
 & ZPHMAX, ZPHMIN, ZPREF, ZPRODC, ZPRODH, &
 & ZQ11, ZQ1MAX, ZQ1MIN, ZQC, ZQLF, ZQCF1, ZQCF2, ZQLH, &
 & ZQLM1, ZQLP1, ZQSATF, ZQSATF1, ZQSATF2, ZQSLTLF, &
 & ZQSLTLF1, ZQSLTLF2, ZQSLTLH, &
 & ZQWF, ZQWF1, ZQWF2, ZQWH, ZRESUL, ZRH, ZRHOH, &
 & ZRIC, ZRICHF, ZRIH, ZRLTLU, ZROSDPHI, ZRQZERO, &
 & ZRTI, ZSIGMAS, ZSIGMAS2, ZSRC, ZSTA, ZSURSAT, &
 & ZTETA, ZTF, ZTF1, ZTF2, ZTH, ZTHETAOT, &
 & ZTLF, ZTLF1, ZTLF2, ZU, ZUSTAR2, &
 & ZWDIFF, ZWQW, ZWTL, ZZETF, ZZF0, ZZF1, &
 & ZZKTH, ZZLMF, ZZN1D, &
 & ZZQC, ZZRT, ZZT, ZL3F2, ZILIMQ1, ZEPSIG, & 
 & ZHTOP, ZHBOT
REAL(KIND=JPRB) ::  ZPLS, ZDELTA, ZGAUSS,ZQV
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-----------------------------------------------------------------------
!     INTRODUCTION DE FONCTIONS.

!     FUNCTIONS THERMODYNAMIQUES DE BASE
#include "fcttrm.func.h"


!-----------------------------------------------------------------------

DATA ZN1D /&
 & 0.0_JPRB            ,  0.0_JPRB            ,  1.7225742E-05_JPRB,  0.275373E-04_JPRB ,&
 & 4.5657158E-05_JPRB,  0.748634E-04_JPRB ,  1.2344122E-04_JPRB,  0.203788E-03_JPRB ,&
 & 3.3539534E-04_JPRB,  0.553310E-03_JPRB ,  9.1189146E-04_JPRB,  0.150353E-02_JPRB ,&
 & 2.4784803E-03_JPRB,  0.408673E-02_JPRB ,  6.7381263E-03_JPRB,  0.111092E-01_JPRB ,&
 & 1.8315554E-02_JPRB,  0.301974E-01_JPRB ,  4.9787164E-02_JPRB,  0.831191E-01_JPRB ,&
 & 0.1512039_JPRB    ,  0.286653E+00_JPRB ,  0.5000000_JPRB    ,  0.691489E+00_JPRB ,&
 & 0.8413813_JPRB    ,  0.933222E+00_JPRB ,  0.9772662_JPRB    ,  0.993797E+00_JPRB ,&
 & 0.9986521_JPRB    ,  0.999768E+00_JPRB ,  0.9999684_JPRB    ,  0.9999997_JPRB    ,&
 & 1.0000000_JPRB    ,  1.000000_JPRB     /  

DATA ZRC1D /&
 & 0.0_JPRB            ,  0.0_JPRB            ,  1.1461278E-05_JPRB,  0.275279E-04_JPRB ,&
 & 4.3084903E-05_JPRB,  0.747532E-04_JPRB ,  1.2315845E-04_JPRB,  0.201069E-03_JPRB ,&
 & 3.3593364E-04_JPRB,  0.551618E-03_JPRB ,  9.1182487E-04_JPRB,  0.150296E-02_JPRB ,&
 & 2.4801120E-03_JPRB,  0.408695E-02_JPRB ,  6.7372285E-03_JPRB,  0.111084E-01_JPRB ,&
 & 1.8315896E-02_JPRB,  0.301974E-01_JPRB ,  4.9786866E-02_JPRB,  0.721706E-01_JPRB ,&
 & 0.1165014_JPRB    ,  0.210263E+00_JPRB ,  0.3990000_JPRB    ,  0.697847E+00_JPRB ,&
 & 1.0833505_JPRB    ,  0.152933E+01_JPRB ,  2.0084987_JPRB    ,  0.250201E+01_JPRB ,&
 & 3.0003829_JPRB    ,  0.350006E+01_JPRB ,  4.0000072_JPRB    ,  0.450000E+01_JPRB ,&
 & 5.0000000_JPRB    ,  5.500000_JPRB     /  

DATA ZSRC1D /&
 & 0.0_JPRB            ,  0.0_JPRB            ,  2.0094444E-04_JPRB,   0.316670E-03_JPRB,&
 & 4.9965648E-04_JPRB,  0.785956E-03_JPRB ,  1.2341294E-03_JPRB,   0.193327E-02_JPRB,&
 & 3.0190963E-03_JPRB,  0.470144E-02_JPRB ,  7.2950651E-03_JPRB,   0.112759E-01_JPRB,&
 & 1.7350994E-02_JPRB,  0.265640E-01_JPRB ,  4.0427860E-02_JPRB,   0.610997E-01_JPRB,&
 & 9.1578111E-02_JPRB,  0.135888E+00_JPRB ,  0.1991484_JPRB    ,   0.230756E+00_JPRB,&
 & 0.2850565_JPRB    ,  0.375050E+00_JPRB ,  0.5000000_JPRB    ,   0.691489E+00_JPRB,&
 & 0.8413813_JPRB    ,  0.933222E+00_JPRB ,  0.9772662_JPRB    ,   0.993797E+00_JPRB,&
 & 0.9986521_JPRB    ,  0.999768E+00_JPRB ,  0.9999684_JPRB    ,   0.999997E+00_JPRB,&
 & 1.0000000_JPRB    ,  1.000000_JPRB     /  

!-----------------------------------------------------------------------


!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     0 - CALCULS PRELIMINAIRES
!     ------------------------------------------------------------------

!     ZCE1     : FACTEUR DE DECROISSANCE DE L'E.C.T.
!     ZPHMAX   : PRESSION PAR DEFAUT AU SOMMET DE LA COUCHE LIMITE, PMIN
!     ZPHMIN   : PRESSION PAR DEFAUT A LA BASE DE LA COUCHE LIMITE, PMAX
!     IHCLPMAX : NIVEAU MAXIMUM DE LA COUCHE LIMITE
!     IHCLPMIN : NIVEAU MINIMUN DE LA COUCHE LIMITE
!     ZEPS     : VALEUR MINIMALE DE L'E.C.T.
!     ZEPS1    : VALEUR MINIMALE DU CISAILLEMENT DU VENT
!     ZLMIN    : VALEUR MINIMALE POUR ZLMUP ET ZLMDN

ZCE1     = UDECT
ZPHMAX   = UPRETMIN
ZPHMIN   = UPRETMAX
IHCLPMAX = KTDIAN
IHCLPMIN = KLEV-2
ZEPS     = ECTMIN
ZEPS1    = USHEARM

DO JLON = KIDIA, KFDIA
  PKTROV(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  PKUROV(JLON,:)=0.0_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  PPRODTH(JLON,:)=0.0_JPRB
ENDDO

! ZSTAB      : INDICE DE STABILITE A LA SURFACE (1 SI STABLE, 0 SINON).
! ZRS        : CONSTANTE DES GAZ PARFAITS EN SURFACE.
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Compute the value for ZRS, ZSTAB (from ACHMT usually...
! but not available if LMSE and no call to ACHMT)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLON=KIDIA,KFDIA
  ZRS  (JLON) = RD + (RV-RD)*PQS(JLON)
  ZDPHI0      = PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV)
  ZRTI        = 2.0_JPRB/(PR(JLON,KLEV)*PT(JLON,KLEV) &
             &            +RKAPPA*ZDPHI0 &
             &            +ZRS(JLON)*PTS(JLON))  
  ZSTA        = ZDPHI0*( PR(JLON,KLEV)*PT(JLON,KLEV) &
             &          +RKAPPA*ZDPHI0 &
             &          -ZRS(JLON)*PTS(JLON) )*ZRTI
  ZSTAB(JLON) = MAX(0.0_JPRB,SIGN(1.0_JPRB,ZSTA))
ENDDO

!   CONSTANTES DE SECURITE ET DE PARAMETRISATION. (Schema stat.)
!   SECURITY AND PARAMETRIZATION CONSTANTS.       (Stat. Scheme)

ZEPSQ   = 1.E-10_JPRB
ZEPNEBS = 1.E-12_JPRB
ZEPDELT = 1.E-12_JPRB
ZEPSV   = 1.E-10_JPRB
ZEPSIG  = 1.E-10_JPRB

ZMAXQ1  = 20._JPRB
ZEPSQ1  = 1.E-6_JPRB
LLIMQ1  = .TRUE.

IF (LLIMQ1) THEN
  ZILIMQ1 = 1.0_JPRB
ELSE
  ZILIMQ1 = 0.0_JPRB
ENDIF

ZGAUSS=1.0_JPRB/(2.0_JPRB*RDT**2)

!   TABLEAUX DE TRAVAIL
!   WORK ARRAYS ONCE FOR ALL

DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZZ(JLON,JLEV)    = PAPHIF(JLON,JLEV)  -PAPHI (JLON,KLEV)
    ZRTV (JLON,JLEV) = PR(JLON,JLEV)*PT(JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV
DO JLEV=KTDIAN+1,KLEV
  DO JLON=KIDIA,KFDIA
    ZGDZF(JLON,JLEV) = PAPHIF(JLON,JLEV-1)-PAPHIF(JLON,JLEV)
  ENDDO ! JLON
ENDDO   ! JLEV

!*
!     ------------------------------------------------------------------
!     I - ACCOEFK SIMPLIFIE AU DESSUS DE KTDIAN.

!     CALCULS DE PARAMETRES AUXILIAIRES ET DE CONSTANTES
!     DE SECURITE (POUR LE CARRE DU CISAILLEMENT DU VENT).

!     COMPUTATION OF DERIVED PARAMETERS AND SECURITY
!     CONSTANTS (FOR THE SQUARE OF THE WIND SHEAR).
!     ------------------------------------------------------------------

Z2B=2.0_JPRB*EDB
Z3B=3._JPRB*EDB
Z3BCF=EDB*EDC*VKARMN**2/SQRT(3._JPRB)
ZRLTLU=SQRT(1.5_JPRB*EDD)

ZGLU=RG*ALMAV
ZGLT=ZGLU*ZRLTLU

!     BOUCLE PASSIVE SUR LES NIVEAUX VERTICAUX.
!     PASSIVE LOOP ON VERTICAL LEVELS.

DO JLEV=KTDIAT,KTDIAN-1

!       CALCULS PROPREMENT DITS.

  ZGLUZ=ZGLU
  ZGLMU2=ZGLUZ**2
  ZGLTZ=ZGLT
  ZGLMT2=ZGLTZ**2

  DO JLON=KIDIA,KFDIA

!         PROFIL DE LONGUEUR DE MELANGE CONSTANT
!         CONSTANT MIXING LENGTH PROFILE

    ZGZ=PAPHI(JLON,JLEV)-PAPHI(JLON,KLEV)+PGZ0(JLON)
    ZCK=Z3BCF*(ZGLTZ/(VKARMN*ZGZ))**2
    ZDPHI0=PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)

!         CISAILLEMENT DE VENT.
!         WIND SHEAR.

    ZCIS=MAX(ZEPS1,(PU(JLON,JLEV)-PU(JLON,JLEV+1))**2 &
     & +(PV(JLON,JLEV)-PV(JLON,JLEV+1))**2)  
    ZU=SQRT(ZCIS)

!         PRECALCUL DE STABILITE.
!         PRELIMINARY STABILITY COMPUTATION.

    ZDTETA=PR(JLON,JLEV)  *PT(JLON,JLEV)&
     & -PR(JLON,JLEV+1)*PT(JLON,JLEV+1)+RKAPPA*ZDPHI0  

!         CALCUL DE STABILITE.
!         STABILITY COMPUTATION.

    ZRTI=2.0_JPRB/(PR(JLON,JLEV)  *PT(JLON,JLEV)&
     & +PR(JLON,JLEV+1)*PT(JLON,JLEV+1))  
    ZSTA=ZDPHI0*ZDTETA*ZRTI
    ZSTA=ZSTA/(1.0_JPRB+MAX(0.0_JPRB,ZSTA)*USURIC/ZCIS)
    ZIS=MAX(0.0_JPRB,SIGN(1.0_JPRB,ZSTA))

!         CALCULS COMMUNS POUR QUANTITE DE MOUVEMENT ET ENERGIE.
!         COMMON COMPUTATIONS FOR MOMENTUM AND ENERGY.

    ZDS=SQRT(ZCIS+EDD*ABS(ZSTA))
    ZDI=1.0_JPRB/(ZU+ZCK*SQRT(ABS(ZSTA)))

!         CALCULS POUR LES COMPOSANTES DU VENT.
!         COMPUTATIONS FOR THE WIND COMPONENTS.

    ZLOS=ZCIS*ZDS/(ZU*ZDS+Z2B*ABS(ZSTA))
    ZLOI=ZU-Z2B*ZSTA*ZDI
    PKUROV(JLON,JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*&
     & ZGLMU2*PAPRS(JLON,JLEV)*ZRTI/ZDPHI0**2  

!         CALCULS POUR LA TEMPERATURE ET L'HUMIDITE.
!         COMPUTATIONS FOR TEMPERATURE AND HUMIDITY.

    ZLOS=ZCIS**2/(ZU*ZCIS+Z3B*ABS(ZSTA)*ZDS)
    ZLOI=ZU-Z3B*ZSTA*ZDI
    PKTROV(JLON,JLEV)=(ZLOI+ZIS*(ZLOS-ZLOI))*&
     & ZGLMT2*PAPRS(JLON,JLEV)*ZRTI/ZDPHI0**2  

!         CALCUL DE UN SUR G FOIS LA FREQUENCE DE BRUNT-VAISALA
!         DIVISEE PAR  LA DENSITE LE TOUT AU CARRE.
!         COMPUTATION OF ONE OVER G TIME THE BRUNT-VAISALA FREQUENCY
!         DIVIDED BY DENSITY THE WHOLE BEING SQUARED.

    PNBVNO(JLON,JLEV)=ZSTA/(PAPRS(JLON,JLEV)*ZRTI*ZDPHI0)**2

  ENDDO
ENDDO

!     FIN DE ACCOEFK SIMPLIFIE


!     CALCUL DE LA HAUTEUR DE COUCHE LIMITE:
!          PREMIER NIVEAU EN PARTANT DE LA SURFACE OU LA TKE <0.01
!          SI LPBLE (Top. Entr. ) ou TKECLS (not LECTREP)

DO JLON = KIDIA, KFDIA
  ZBLH(JLON) =0._JPRB
ENDDO



DO JLON=KIDIA,KFDIA
   PECTCLS(JLON)  = PECT(JLON,KLEV-1)
ENDDO   
 ! LECREP
!*
!     ------------------------------------------------------------------
!     IV - DEFINITION DE TABLEAUX DE TRAVAIL POUR LES CALCULS A SUIVRE.
!          (CALCUL DES TEMPERATURES POTENTIELLES SECHES ET HUMIDES)
!     ------------------------------------------------------------------

! - - - - - - - - - - -
! CALCULS DE THETA (sec)
! - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZPREF              = PAPRSF(JLON,JLEV)
    ZTHETA(JLON,JLEV)  = PT(JLON,JLEV)*(RATM/ZPREF)**(RKAPPA)
  ENDDO
ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! CALCUL DE (THETA)l = THETA * [1-Lv*(Ql+Qi)/(Cp*T)]
!           (THETA)l = THETA - ZLOCPEXF*(Ql+Qi)
! AVEC      ZLOCPEXF = (Lv/Cp)*(THETA/T)
! ET DONC   ZLOCPEXF =  Lv/Cp/(T/THETA)
! LA FONCTION D'EXNER ETANT : PI=T/THETA
! CALCUL DE  ZCOEFJF = [Lv(Tl)*qsat(Tl)]/[Rv*Tl*(Theta)l]
! - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZZT                 = PT  (JLON,JLEV)
    ZQC                 = PQLI(JLON,JLEV) +PQICE(JLON,JLEV)
    ZTHETAOT            = ZTHETA(JLON,JLEV)/ZZT
    ZLOCPEXF(JLON,JLEV) = PLSCPE(JLON,JLEV)*ZTHETAOT
    ZTHETALF(JLON,JLEV) = ZTHETA(JLON,JLEV)-ZQC*ZLOCPEXF(JLON,JLEV)
  ENDDO
ENDDO
!!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!      si : (THETA)l  =  THETA * [ 1 - L*(Ql+Qi)/(Cp*T) ]
! CALCUL DE (THETA)vl = (THETA)l * [ 1 + RETV*(Ql+Qi) ]
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO JLON=KIDIA,KFDIA
    ZQV               =   PQV (JLON,JLEV)
    ZQC               =   PQLI(JLON,JLEV)+PQICE(JLON,JLEV)
    ! ZTHETAVL(JLON,JLEV) = ZTHETALF(JLON,JLEV)*(1.0_JPRB+RETV*ZQC)
    ! ancien calcul
    ZTHETAVL(JLON,JLEV) = ZTHETA(JLON,JLEV)*(1.0_JPRB+RETV*ZQV-ZQC)
  ENDDO
ENDDO

!*
!     ------------------------------------------------------------------
!     V - CALCUL DES COEFFICIENTS DE MELANGE "PKUROV" ET "PKTROV".
!     ------------------------------------------------------------------
DO JLON = KIDIA, KFDIA
  ZGKUH(JLON,:)=1.E-14_JPRB
ENDDO
DO JLON = KIDIA, KFDIA
  ZGKTH(JLON,:)=1.E-14_JPRB
ENDDO
DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA

    ZDPHI    =ZGDZF(JLON,JLEV+1)
    ZZRT     =0.5_JPRB*(ZRTV(JLON,JLEV)+ZRTV(JLON,JLEV+1))
    ZROSDPHI =PAPRS(JLON,JLEV)/(ZDPHI*ZZRT)

    ZGKUH(JLON,JLEV)=AKN*PLMECT(JLON,JLEV)*SQRT(PECT(JLON,JLEV))
    ZGKTH(JLON,JLEV)=ZGKUH(JLON,JLEV)*PPHI3(JLON,JLEV)*ALPHAT

    PKTROV(JLON,JLEV)=ZGKTH(JLON,JLEV)*ZROSDPHI
    PKUROV(JLON,JLEV)=ZGKUH(JLON,JLEV)*ZROSDPHI

  ENDDO ! JLON
ENDDO ! JLEV

!*
!     ------------------------------------------------------------------
!     VI - AU DERNIER DEMI-NIVEAU (C'EST LE SOL): CALCULS POUR ACEVOLET 
!          (EQUATION D'EVOLUTION DE L'ECT) DU COEFFICIENT DE MELANGE
!          PGKCLS=g*KUCLS ET DE LA LONGEUR DE MELANGE PLMECT (A KLEV).
!     ------------------------------------------------------------------

DO JLON=KIDIA,KFDIA

  PGKCLS(JLON) = 0._JPRB
  ZGKTH (JLON,KLEV)= 0._JPRB
  ZGKUH (JLON,KLEV)= 0._JPRB
ENDDO ! JLON

   

!*
!     ------------------------------------------------------------------
!     VII - AUX DEMI-NIVEAUX : CALCUL DE PNBVNO (->GRAVITY WAVE DRAG).
!           > CISAILLEMENT DE VENT (ZCIS), CALCULS DE STABILITE (ZDTETA
!           > ZSTA), CALCUL DE "UN SUR G FOIS LA FREQUENCE DE BRUNT
!           > VAISALA DIVISEE PAR LA DENSITE", LE TOUT AU CARRE (PNBVNO)
!     ------------------------------------------------------------------

DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA
    ZDPHI =PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)
    ZCIS  =MAX(ZEPS1,(PU(JLON,JLEV)-PU(JLON,JLEV+1))**2 &
     & +(PV(JLON,JLEV)-PV(JLON,JLEV+1))**2)  
    ZDTETA=ZRTV(JLON,JLEV)-ZRTV(JLON,JLEV+1)+RKAPPA*ZDPHI
    ZZRT  =2.0_JPRB/(ZRTV(JLON,JLEV)+ZRTV(JLON,JLEV+1))
    ZSTA  =ZDPHI*ZDTETA*ZZRT
    ZSTA  =ZSTA/(1.0_JPRB+MAX(0.0_JPRB,ZSTA)*USURIC/ZCIS)
    PNBVNO(JLON,JLEV)=ZSTA/(PAPRS(JLON,JLEV)*ZZRT*ZDPHI)**2
  ENDDO ! JLON
ENDDO ! JLEV

!*
!     ------------------------------------------------------------------
!     VIII(a) - CALCUL DE "PPRODTH" SUR LES DEMI-NIVEAUX.
!     ------------------------------------------------------------------
!            - LA PRODUCTION THERMIQUE "PPRODTH" EST CALCULEE COMME
!            LA CORRELATION (g/T)*(w'X'), OU ON UTILISE LA VARIABLE
!            "X=(THETA)vl" QUI CORRESPOND A "T(1+0.608Qv-Ql)". LA
!            PRODUCTION THERMIQUE EST OBTENUE PAR UNE PONDERATION PAR
!            "L3*F2" D'UN TERME "SEC" ET D'UN TERME "HUMIDE"
!                       -------------------------------------------
!                        PPRODTH = ZPRODH + (LAMBDA_3*F_2)* ZPRODC.
!                       -------------------------------------------
!     ------------------------------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - -
! ** DEBUT DES BOUCLES VERTICALES ET HORIZONTALES   **
! ** START OF VERTICAL AND HORIZONTAL NESTED LOOPS  **
! - - - - - - - - - - - - - - - - - - - - - - - - - - -

DO JLEV=KTDIAN,KLEV-1
  DO JLON=KIDIA,KFDIA

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.1 - VARIABLES AUXILIAIRES ET DE TRAVAIL (demi-niveaux).
!                - AUXILIARY VARIABLES AND WORK VALUES (half levels).
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZTF1     = PT (JLON,JLEV  )
    ZTF2     = PT (JLON,JLEV+1)
    ZQWF1    = PQV(JLON,JLEV  ) +PQLI(JLON,JLEV  ) +PQICE(JLON,JLEV  )
    ZQWF2    = PQV(JLON,JLEV+1) +PQLI(JLON,JLEV+1) +PQICE(JLON,JLEV+1)
    ZQWF1    = MAX(ABS(ZQWF1),ZEPSQ)
    ZQWF2    = MAX(ABS(ZQWF2),ZEPSQ)
    ZLSCPEF1 = PLSCPE(JLON,JLEV  )
    ZLSCPEF2 = PLSCPE(JLON,JLEV+1)
    ZQCF1    = PQLI(JLON,JLEV  )+PQICE(JLON,JLEV  )
    ZQCF2    = PQLI(JLON,JLEV+1)+PQICE(JLON,JLEV+1)
    ZTLF1    = ZTF1-ZLSCPEF1*ZQCF1
    ZTLF2    = ZTF2-ZLSCPEF2*ZQCF2

    ZH1      = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLF1))
    ZEW1     = FOEW(ZTLF1,ZH1)/PAPRSF(JLON,JLEV)
    ZQSATF1  = FOQS(ZEW1)
    ZDLEWF1  = FODLEW(ZTLF1,ZH1)
    ZQSLTLF1 = FODQS(ZQSATF1,ZEW1,ZDLEWF1)
    ZDELTQF1 = ZQWF1-ZQSATF1
    ZDELTQF1 = SIGN(MAX(ABS(ZDELTQF1),ZEPDELT),ZDELTQF1)

    ZH2      = MAX(0.0_JPRB,SIGN(1.0_JPRB,RTT-ZTLF2))
    ZEW2     = FOEW(ZTLF2,ZH2)/PAPRSF(JLON,JLEV+1)
    ZQSATF2  = FOQS(ZEW2)
    ZDLEWF2  = FODLEW(ZTLF2,ZH2)
    ZQSLTLF2 = FODQS(ZQSATF2,ZEW2,ZDLEWF2)
    ZDELTQF2 = ZQWF2-ZQSATF2
    ZDELTQF2 = SIGN(MAX(ABS(ZDELTQF2),ZEPDELT),ZDELTQF2)

    ZRH     = (PR(JLON,JLEV)+PR(JLON,JLEV+1))/2.0_JPRB
    ZQWH    = (ZQWF1   +ZQWF2   )/2.0_JPRB
    ZTH     = (ZTF1    +ZTF2    )/2.0_JPRB
    ZQLH    = (ZQCF1   +ZQCF2   )/2.0_JPRB
    ZQSLTLH = (ZQSLTLF1+ZQSLTLF2)/2.0_JPRB
    ZLSCPEH = (ZLSCPEF1+ZLSCPEF2)/2.0_JPRB
    ZDELTQH = (ZDELTQF1+ZDELTQF2)/2.0_JPRB

    ZAA  = 1.0_JPRB/(1.0_JPRB+ZLSCPEH*ZQSLTLH)
    ZDD  = ZLSCPEH-(1.0_JPRB+RETV)*ZTH
    ZCTO = RETV*ZTH

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.2 - CALCUL DES GRADIENTS VERTICAUX    D/DZ = RG * D/DPHI
!                - COMPUTATION OF VERTICAL GRADIENTS D/DZ = RG * D/DPHI
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZDPHI  = PAPHIF(JLON,JLEV)-PAPHIF(JLON,JLEV+1)
    ZDQW   = PQV   (JLON,JLEV)-PQV   (JLON,JLEV+1)&
     & +  PQLI  (JLON,JLEV)-PQLI  (JLON,JLEV+1)&
     & +  PQICE (JLON,JLEV)-PQICE (JLON,JLEV+1)  
    ZDT    = PT    (JLON,JLEV)-PT    (JLON,JLEV+1)
!   Ancien calcul avec bug
!    ZDQLST = ZQCF1*ZLSCPEF1/ZTF1-ZQCF2*ZLSCPEF2/ZTF2
!    ZDSTA  = ZDT+ZDPHI*RKAPPA/ZRH
!    ZDTL   = ZDSTA*(1.0_JPRB-ZLSCPEH*ZQLH/ZTH)-ZTH*ZDQLST
!   En fait il faut 
    ZDTL   = (ZTHETALF(JLON,JLEV)-ZTHETALF(JLON,JLEV+1))*ZTH / &
            & (ZTHETALF(JLON,JLEV)+ZTHETALF(JLON,JLEV+1)) * 2._JPRB

    ZDIFFH = ZDTL +  ZCTO  *ZDQW
    ZDIFFC = ZDQW - ZQSLTLH*ZDTL

!         Attention, ici : ZZKTH=RG*KTH/ZDPHI
    ZRHOH  = PAPRS (JLON,JLEV)/ZRH/ZTH
    ZZKTH  = PKTROV(JLON,JLEV)/ZRHOH

!         - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.3 - CALCUL DE SIGMAQ ET DE SIGMAQL=Q/STTBMIN
!                - COMPUTATION OF SIGMAQ AND SIGMAQL=Q/STTBMIN
!         - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZECTH  = MAX(ECTMIN,PECT(JLON,JLEV))
    ZLMECT = PLMECT(JLON,JLEV)
    ZWQW   = -ZZKTH*ZDQW
    ZWTL   = -ZZKTH*ZDTL
    ZWDIFF = ZWQW-ZQSLTLH*ZWTL

!         - - - - - - - - - - - - - - - - - - - - - -
!         VIII.4 - CALCUL DE SIGMA_S, PUIS DE Q11 :
!         - - - - - - - - - - - - - - - - - - - - - -
    ZIGMAS2 = -ZAA*ZAA*ARSB2*ZLMECT/4._JPRB/SQRT(ZECTH)*ZWDIFF*ZDIFFC/ZDPHI
    ZIGMAS  = MAX(ZEPSIG,SQRT(ABS(ZIGMAS2)))
    ZQ11    = ZAA*ZDELTQH/(2*ZIGMAS)

!         - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.5 - CALCUL DE Q1MAX (LIMITATION SUR LA LONGUEUR
!                  MELANGE ET, EN FAIT, ICI SUR "PHI3") :
!         - - - - - - - - - - - - - - - - - - - - - - - - - - -

       ! LECTQ1

!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!         VIII.9 - CALCUL DE ZPRODH, ZPRODC, PUIS DE LA PRODUCTION
!                  THERMIQUE : PPRODTH = ZPRODH + (LAMBDA_3*F_2)*ZPRODC
!         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ZPRODH = -RG*ZZKTH*ZDIFFH/ZTH
    ZPRODC = -RG*ZZKTH*ZDIFFC*ZAA*ZDD/ZTH

    INQ1   = MIN( MAX(-22,FLOOR(2*ZQ11) ), 10)
    ZINC   = 2.0_JPRB*ZQ11 - INQ1
    ZSRC   = (1.0_JPRB-ZINC)*ZSRC1D(INQ1)+ZINC*ZSRC1D(INQ1+1)
    ZL3F2  = MIN(1.0_JPRB,ZSRC)* MIN( MAX(1.0_JPRB, 1.0_JPRB-ZQ11), 3._JPRB )

    PL3F2  (JLON,JLEV) = ZL3F2

    PPRODTH(JLON,JLEV) = ZPRODH+ ZL3F2*ZPRODC

  ENDDO !  JLON=KIDIA, KFDIA
ENDDO   !  JLEV=KTDIAN,KLEV-1
DO JLON=KIDIA,KFDIA
   PPRODTH(JLON,KLEV)=PPRODTH(JLON,KLEV-1)
ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - -
! ** FIN DES BOUCLES VERTICALES ET HORIZONTALES  **
! ** END OF VERTICAL AND HORIZONTAL NESTED LOOPS **
! - - - - - - - - - - - - - - - - - - - - - - - - -

!*
!     ------------------------------------------------------------------
!     VIII(b) - CALCUL DES COEFFICIENTS AU NIVEAU DE L'ENTRAINEMENT EN
!             SOMMET DE COUCHE LIMITE, DEFINIE PAR LE DERNIER NIVEAU
!             OU, PARTANT DU SOL, ON EST ENCORE EN COUCHE INSTABLE,
!             AU SENS OU LE RICHARDSON DEPASSE POUR LA PREMIERE FOIS
!             LE SEUIL "AGRERICR".
!     ------------------------------------------------------------------
!----------------
  ! LPBLE
!------
!*
!     ------------------------------------------------------------------
!     IX - CALCUL DE "Q1" ET DE "SIGMAS" SUR LES NIVEAUX DU MODELE.
!        > POUR CALCULS DE "PQCS" ET "PNEBS" (SUR LES "FULL-LEVELS" =
!        > SUR LES NIVEAUX DU MODELE).
!     ------------------------------------------------------------------
 
DO JLON = KIDIA, KFDIA
  PNEBS(JLON,:) = ZEPNEBS
ENDDO
DO JLON = KIDIA, KFDIA
  PQCS(JLON,:)  = 0.0_JPRB
ENDDO
 ! KEY LNEBECT

!         - - - - - - - - - - - - - - - - - - - -
!         * VARIABLES AUXILIAIRES ET DE TRAVAIL.
!         * WORK ARRAYS AND VARIABLES
!         - - - - - - - - - - - - - - - - - - - -

!     - - - - - - - - - - - - - - - - - - - - - - - - -
!     ** FIN DES BOUCLES VERTICALES ET HORIZONTALES  **
!     ** END OF VERTICAL AND HORIZONTAL NESTED LOOPS **
!     - - - - - - - - - - - - - - - - - - - - - - - - -

!*
!     ------------------------------------------------------------------



END SUBROUTINE SIMPLE2_ACTURB
