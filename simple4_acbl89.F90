#include "simple4.h"
SUBROUTINE SIMPLE4_ACBL89  ( KIDIA,  KFDIA,    KLON,   KTDIAN, KLEV, PAPHI,     &
& PAPHIF, PAPRS,  PAPRSF, PT, PECT,   PQV,      PQICE,  PQLI, PNLAB, PNLABCVP , &
& PGZ0, PTS, PUSLE,  PLMECT,   PPHI3, RG, RETV, RKAPPA, RATM, ALMAVX, ACBRPHIM, &
& VKARMN, ALD, ECTMIN, ALMAVE, ARSC1, KSIZST, KPTRST, PSTACK)

!**** *ACBL89 - CALCUL DES LONGUEURS DE MELANGE ET DE DISSIPATION,
!               D'APRES LA METHODE DE BOUGEAULT-LACARRERRE (1989),
!               MODIFIEE EN 2000 COMME DANS MESO-NH (LES EXPOSANTS
!               -2/3 ET -3/2), AVEC DES SECURITES POUR LE CLIMAT :    
!               L > MIN(RG*ALMAVE, VKARMN*ZPHIH). 

!     Sujet.
!     ------
!     - ROUTINE DE CALCUL ACTIF .
!       CALCUL DE LA LONGUEUR DE MELANGE "PLMECT=g*L_mix" ET DE 
!       L'INVERSE DE LA LONGUEUR DE DISSIPATION "PUSLE=1/(ALD*g*L_diss)"

!     - COMPUTATION OF THE MIXING LENGTH "PLMECT=g*L_mix"
!       AND THE DISSIPATIVE LENGTH "PUSLE=1/(ALD*g*L_diss)".

!**   Interface.
!     ----------
!        *CALL* *ACBL89*

!-----------------------------------------------------------------------
! WARNING: THE ENGLISH VERSION OF VARIABLES' NAMES IS TO BE READ IN THE
!          "APLPAR" CODE, EXCEPT FOR KTDIAN.
!-----------------------------------------------------------------------

! -   ARGUMENTS D'ENTREE.
!     -------------------

! - NOM DES PARAMETRES DE DIMENSIONNEMENT DE LA PHYSIQUE.

! KIDIA      : INDICE DE DEPART DES BOUCLES VECTORISEES SUR L'HORIZONT.
! KFDIA      : INDICE DE FIN DES BOUCLES VECTORISEES SUR L'HORIZONTALE.
! KLON       : DIMENSION HORIZONTALE DES TABLEAUX.
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
! PT         : TEMPERATURE (APRES AJUSTEMENT CONVECTIF).
! PECT       : ENERGIE CINETIQUE TURBULENTE.
! PQV        : HUMIDITE SPECIFIQUE DE LA VAPEUR D'EAU.
! PQICE      : HUMIDITE SPECIFIQUE SOLIDE.
! PQLI       : HUMIDITE SPECIFIQUE LIQUIDE.
! PNLAB      : Si 1 Presence d'un nuage Shallow
! PNLABCVP   : Si 1 Presence d'un nuage Deep

! - 1D (DIAGNOSTIQUE) .

! PGZ0       : G FOIS LA LONGUEUR DE RUGOSITE COURANTE.
! PTS        : TEMPERATURE DE SURFACE

!-----------------------------------------------------------------------

! -   ARGUMENTS DE SORTIE.
!     --------------------

! - NOM DES VARIABLES DE LA PHYSIQUE (PAR ORDRE ALPHABETIQUE DANS CHAQUE
!   CATEGORIE).

! - 2D (1:KLEV) .

! PUSLE      : INVERSE DE LA LONGUEUR DE DISSIPATION MULTIPLIEE PAR G
! PLMECT     : UNE LONGUEUR DE MELANGE (FOIS G) POUR ACNEBR
! PPHI3      : LA FONCTION DE REDESLPERGER POUR K_T

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
!      2003-03, P. Marquet.
!              - - - - - - - - - - - - - - - - - - - - - - - - - - -
!               From the FIRST part of the old ACCOEFKE code,
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
!      2004-06-03, P. Marquet : a limitation of ZG2L2SLD2 according to
!                  the use of ALMAVE>0, for which the minimal value of
!                  2.*ARSC1 must be granted in stable cases.
!                  If ALMAVE>0, then (ZGLMCBR)*2 is no longer equal to 
!                  2*e*Theta/[dTheta/dphi] and the MIN(_TWO_, ...) in
!                  factor of ARSC1 ensure the limit value of 2.*ARSC1,
!                  even if ZGLMCBR=ALMAVE>0.
!        M.Hamrud      01-Oct-2003 CY28 Cleaning
!      2007-10-26, E. Bazile and S. Malardel : bugs correction for Lm_down
!                  ZDLUP1 replaced by ZDLDN1 and add deltaZ to Lm_down   
!      2008-07-18  Y. Bouteloup : 1) Modification of mixing length in case of 
!                                    shallow or deep convection cloud
!                                 2) : lup(j) = max(lup(j),lup(j+1)-delta_phi)
!                                 3) : ldn(j) = max(ldn(j),ldn(j-1)-delta_phi)
!      2008-10-03 E. Bazile : optimisation du calcul L=Lup**-2/3+Ldw**-2/3
!      K. Yessad (Jul 2009): remove CDLOCK + some cleanings
!      R. El Khatib 22-Jul-2014 Vectorizations
!-----------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRB

  



!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER(KIND=JPIM),INTENT(IN)    :: KLON 
INTEGER(KIND=JPIM),INTENT(IN)    :: KLEV 
INTEGER(KIND=JPIM),INTENT(IN)    :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN)    :: KTDIAN 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLAB(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PNLABCVP(KLON,KLEV)
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHI(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPHIF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQV(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PGZ0(KLON) 
REAL(KIND=JPRB)   ,INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PUSLE(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PLMECT(KLON,KLEV) 
REAL(KIND=JPRB)   ,INTENT(OUT)   :: PPHI3(KLON,KLEV)

REAL(KIND=JPRB), INTENT (   IN) :: RG       
REAL(KIND=JPRB), INTENT (   IN) :: RETV     
REAL(KIND=JPRB), INTENT (   IN) :: RKAPPA   
REAL(KIND=JPRB), INTENT (   IN) :: RATM     
REAL(KIND=JPRB), INTENT (   IN) :: ALMAVX   
REAL(KIND=JPRB), INTENT (   IN) :: ACBRPHIM 
REAL(KIND=JPRB), INTENT (   IN) :: VKARMN   
REAL(KIND=JPRB), INTENT (   IN) :: ALD      
REAL(KIND=JPRB), INTENT (   IN) :: ECTMIN   
REAL(KIND=JPRB), INTENT (   IN) :: ALMAVE   
REAL(KIND=JPRB), INTENT (   IN) :: ARSC1
INTEGER(KIND=JPIM),INTENT(IN)    :: KPTRST
INTEGER(KIND=JPIM),INTENT(IN)    :: KSIZST
REAL(KIND=JPRB)   ,INTENT(INOUT)    :: PSTACK(KLON,KSIZST)    
 

!-----------------------------------------------------------------------








! Tableaux pour les sorties sur listing (1D seulement)

INTEGER(KIND=JPIM) :: JJLEV, JLEV, JLON

REAL(KIND=JPRB) ::  ZDLDN,  ZDLDN1,  ZDLDN2,   ZQV, &
 & ZDLUP,  ZDLUP1,  ZDLUP2,   ZDPHI, &
 & ZEPSX,  ZINCR,   ZG2L2SLD2, &
 & ZGLDIS, ZGLKARMN,ZGLMCBR, &
 & ZGLMINF,ZGLMIX,  ZPHI3MAX, &
 & ZPREF,   ZQC,      ZTEST,   ZTEST0, &
 & ZUSX,    ZX,       ZZDTHVL, ZZDTHVLP, &
 & ZZTHVL, ZZTHVLP, ZGZLCVPUP, ZGZLCVPDN, &
 & Z2SQRT2, ZLDN,ZLWK0


!-----------------------------------------------------------------------


!-----------------------------------------------------------------------

!*
!     ------------------------------------------------------------------
!     1 - CALCULS PRELIMINAIRES
!     ------------------------------------------------------------------

INTEGER (KIND=JPIM) :: IPTRST

INTEGER (KIND=JPIM) :: IPTRST_ZGZTOPCVP

INTEGER (KIND=JPIM) :: IPTRST_ZDTHETA

INTEGER (KIND=JPIM) :: IPTRST_ZGDZH

INTEGER (KIND=JPIM) :: IPTRST_ZPHIH

INTEGER (KIND=JPIM) :: IPTRST_ZLWK1

INTEGER (KIND=JPIM) :: IPTRST_ZGLMUP

INTEGER (KIND=JPIM) :: IPTRST_ZTHETA

INTEGER (KIND=JPIM) :: IPTRST_ZGDZF

INTEGER (KIND=JPIM) :: IPTRST_ZLUP

INTEGER (KIND=JPIM) :: IPTRST_ZTHETAH

INTEGER (KIND=JPIM) :: IPTRST_ZGZBOTCVP

INTEGER (KIND=JPIM) :: IPTRST_ZGZTOP

INTEGER (KIND=JPIM) :: IPTRST_ZEN

INTEGER (KIND=JPIM) :: IPTRST_ZTHETAP

INTEGER (KIND=JPIM) :: IPTRST_ZGZBOT

INTEGER (KIND=JPIM) :: IPTRST_ZGLMDN

IPTRST = KPTRST

IPTRST_ZGZTOPCVP = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZDTHETA = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZGDZH = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZPHIH = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZLWK1 = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZGLMUP = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZTHETA = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZGDZF = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZLUP = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZTHETAH = IPTRST

IPTRST = IPTRST + (KLEV - 0) + 1

IPTRST_ZGZBOTCVP = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZGZTOP = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZEN = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZTHETAP = IPTRST

IPTRST = IPTRST + KLEV

IPTRST_ZGZBOT = IPTRST

IPTRST = IPTRST + 1

IPTRST_ZGLMDN = IPTRST

IPTRST = IPTRST + KLEV


ZEPSX    = ECTMIN

!   UNE CONSTANTE POUR LA FONCTION "PHI3" (Cuxart Bougeault Redels.)
!   A CONSTANT FOR "PHI3" FUNCTION        (Quart. J. Roy. Met. Soc.)

ZPHI3MAX= (1.0_JPRB-ACBRPHIM)/ACBRPHIM

INIT_JLON

!   TABLEAUX DE TRAVAIL
!   WORK ARRAYS ONCE FOR ALL

DO JLEV=KTDIAN,KLEV
  DO_JLON
    PSTACK(JLON,IPTRST_ZGDZH+JLEV-1) = PAPHIF(JLON,JLEV)  -PAPHI (JLON,JLEV)
  ENDDO_JLON
ENDDO   ! JLEV
DO JLEV=KTDIAN+1,KLEV
  DO_JLON
    PSTACK(JLON,IPTRST_ZGDZF+JLEV-1) = PAPHIF(JLON,JLEV-1)-PAPHIF(JLON,JLEV)
  ENDDO_JLON
ENDDO   ! JLEV


!!  Calcul de ZGZTOP et ZGZBOT de la cvpp et de la cvp

DO_JLON
  PSTACK(JLON, IPTRST_ZGZTOP) = 0.0_JPRB
ENDDO_JLON
DO_JLON
  PSTACK(JLON, IPTRST_ZGZBOT) = 100000.0_JPRB
ENDDO_JLON

DO_JLON
  PSTACK(JLON, IPTRST_ZGZTOPCVP) = 0.0_JPRB
ENDDO_JLON
DO_JLON
  PSTACK(JLON, IPTRST_ZGZBOTCVP) = 100000.0_JPRB
ENDDO_JLON

DO JLEV=KTDIAN,KLEV
  DO_JLON
     PSTACK(JLON, IPTRST_ZGZTOP) = MAX(PSTACK(JLON, IPTRST_ZGZTOP),PAPHIF(JLON,JLEV)*PNLAB(JLON,JLEV))
     PSTACK(JLON, IPTRST_ZGZBOT) = (1.0_JPRB-PNLAB(JLON,JLEV))*PSTACK(JLON, IPTRST_ZGZBOT) &
    &             + PNLAB(JLON,JLEV)*MIN(PSTACK(JLON, IPTRST_ZGZBOT),PAPHIF(JLON,JLEV))
  ENDDO_JLON
ENDDO    

DO JLEV=KTDIAN,KLEV
  DO_JLON
     PSTACK(JLON, IPTRST_ZGZTOPCVP) = MAX(PSTACK(JLON, IPTRST_ZGZTOPCVP),PAPHIF(JLON,JLEV)*PNLABCVP(JLON,JLEV))
     PSTACK(JLON, IPTRST_ZGZBOTCVP) = (1.0_JPRB-PNLABCVP(JLON,JLEV))*PSTACK(JLON, IPTRST_ZGZBOTCVP) &
    &             + PNLABCVP(JLON,JLEV)*MIN(PSTACK(JLON, IPTRST_ZGZBOTCVP),PAPHIF(JLON,JLEV))
  ENDDO_JLON
ENDDO    

!*
!     ------------------------------------------------------------------
!     IV - DEFINITION DE TABLEAUX DE TRAVAIL POUR LES CALCULS A SUIVRE.
!          (CALCUL DES TEMPERATURES POTENTIELLES SECHES ET HUMIDES)
!     ------------------------------------------------------------------

! - - - - - - - - - - -
! CALCULS DE THETA (sec)
! - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO_JLON
    ZPREF              = PAPRSF(JLON,JLEV)
    PSTACK(JLON,IPTRST_ZTHETA+JLEV-1)  = PT(JLON,JLEV)*(RATM/ZPREF)**(RKAPPA)
  ENDDO_JLON
ENDDO

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! CALCUL DE (THETA)vl = THETA * ( 1 + RETV*Qv - (Ql+Qi) )
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV
  DO_JLON
    ZQV               =   PQV (JLON,JLEV)
    ZQC               =   PQLI(JLON,JLEV)+PQICE(JLON,JLEV)
    PSTACK(JLON,IPTRST_ZTHETA+JLEV-1) = PSTACK(JLON,IPTRST_ZTHETA+JLEV-1)*(1.0_JPRB+RETV*ZQV-ZQC)
  ENDDO_JLON
ENDDO

! - - - - - - - - - - - - - - - - -
! CALCULS DE (THETA)vl/Half-level
! - - - - - - - - - - - - - - - - -
DO JLEV=KTDIAN,KLEV-1
  DO_JLON
    PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0) = 0.5_JPRB*(PSTACK(JLON,IPTRST_ZTHETA+JLEV-1)+PSTACK(JLON,IPTRST_ZTHETA+JLEV+1-1))
  ENDDO_JLON
ENDDO
DO_JLON
  PSTACK(JLON,IPTRST_ZTHETAH+KLEV-0)   = PTS(JLON)*(RATM/PAPRS(JLON,KLEV))**(RKAPPA)
ENDDO_JLON

! - - - - - - - - - - - -
! CALCULS DE d(THETA)vl
! - - - - - - - - - - - -
!DO JLON=KIDIA,KFDIA
!  ZDTHETA(JLON,KTDIAN) = ZTHETA(JLON,MAX(KTDIAN-1,1))&
!                    &   -ZTHETA(JLON,MAX(KTDIAN  ,1))
!ENDDO
DO JLEV=KTDIAN+1,KLEV
  DO_JLON
    PSTACK(JLON,IPTRST_ZDTHETA+JLEV-1) = PSTACK(JLON,IPTRST_ZTHETA+JLEV-1-1)-PSTACK(JLON,IPTRST_ZTHETA+JLEV-1)
  ENDDO_JLON
ENDDO

!*
!     ------------------------------------------------------------------
!     V - CALCUL DES LONGUEURS DE MELANGE ET DE DISSIPATION.
!         ON ATTRIBUT A UNE PARTICULE L'ENERGIE CINETIQUE MOYENNE
!         DONT ELLE EST ISSUE, ON REGARDE SES DEPLACEMENTS MAXIMA
!         VERS LE HAUT "ZGLMUP" ET VERS LE BAS "ZGLMDN". ON FABRIQUERA
!         "PLMECT" ET "PUSLE" EN MOYENNANT CES DEUX QUANTITES.
!     ------------------------------------------------------------------

!         Pour ne pas ruiner les vectorisations on calcule les deux cas
!         ZEN(JLON)>ZINCR ==> ZDLUP1 (on atteint le niveau cible)
!         ZEN(JLON)<ZINCR ==> ZDLUP2 (on s'arrete entre 2 niveaux)
!         Et on utilise : ZTEST=1 pour garder ZDLUP1 et ZTEST=0
!         pour garder ZDLUP2.

!         De plus, il ne faut incrementer ZGLMUP(JLON) ou ZGLMDN(JLON)
!         que si ZEN(JLON) est positif au depart : ZTEST0=1., car
!         on est amene a continuer les boucles vers le haut et
!         vers le bas jusqu'a ce que TOUS les points ont atteint
!         leur seuil de flottabilite. ZEN(JLON) peut donc etre negatif
!         pour les premiers points a atteindre ce seuil...

!         Le calcul de ZDLDN2 est general sauf si ZUSX=1/ZX est proche
!         de 0 (si ZINCR -> 0). Dans ce cas, comme la fonction est
!         continue et egale a SQRT(PZEN(JLON)/ZINCR)*ZDLDN1 avec
!         0<PZEN(JLON)/ZINCR<1, il suffit de prendre une valeur
!         seuil ZEPSX pour la variable ZUSX afin d'assurer la
!         continuite de ZDLDN2(ZUSX,ZEN(JLON)/ZINCR).

!         Les divers ABS() qui interviennent dans les SQRT() ne
!         devraient pas etre necessaires, sinon pour remedier au
!         probleme de faire tous les calculs ZDLUP1/ZDLUP2 et
!         ZDLDN1/ZDLDN2 pour garantir la vectorisation, y compris
!         dans des cas ou les formules degenerent mal (mais
!         seules les formules pertinantes sont retenues ensuite
!         en fonction de ZTEST).

!     ------------------------------------------------------------------



!     --------------------------------------
DO JLEV=KTDIAN,KLEV-1 ! BOUCLE GENERALE
!     --------------------------------------

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Pour les modifications "wet-BL89" de l'INM
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! Calcul de (Theta)vl de la particule apres deplacement
  ! du demi-niveaux (JK) vers autres niveaux pleins, en
  ! conservant (Theta)l et Qtot au cours du deplacement.
  ! On a (THETA)vl = [ (THETA)l + Lv*THETA/Cp/T*(Ql+Qi) ]
  !                 *[ 1 + (1+RETV)*Qv - Qtot ]
  ! et ici : JLEV+1 varie entre KTDIAN+1 et KLEV (donc OK)
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  DO JJLEV=KTDIAN,KLEV
    DO_JLON
      PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1) = PSTACK(JLON, IPTRST_ZTHETAH+JLEV-0)
    ENDDO_JLON
  ENDDO ! JJLEV

  ! - - - - - - - - - - - - - - - - - - - - -
  ! BOUCLE VERS LE HAUT QUI CALCULE ZGLMUP :
  ! - - - - - - - - - - - - - - - - - - - - -

  ! L'energie (ZEN) et valeur initiale de "Lup"

  DO_JLON
    PSTACK   (JLON, IPTRST_ZEN)=PECT(JLON,JLEV)
    PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1)=0.0_JPRB
  ENDDO_JLON

  ! Le passage du demi-niveau JLEV au niveau
  ! JLEV juste au dessus :

  DO_JLON
    ZDLUP1 = PSTACK(JLON,IPTRST_ZGDZH+JLEV-1)
    ZINCR  =(PSTACK (JLON,IPTRST_ZTHETA+JLEV-1)-PSTACK(JLON,IPTRST_ZTHETAP+JLEV-1))&
     & /PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)/2.0_JPRB*ZDLUP1  
    ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
    ZTEST0 =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN))
    ZTEST  =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN)-ZINCR)
    ZDLUP2 =SQRT(ABS(PSTACK(JLON, IPTRST_ZEN)/ZINCR))*ZDLUP1
    ZDLUP  =ZTEST*ZDLUP1+(1.0_JPRB-ZTEST)*ZDLUP2
    PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1)=PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1)+ZDLUP*ZTEST0
    PSTACK   (JLON, IPTRST_ZEN)=PSTACK  (JLON, IPTRST_ZEN)-ZINCR*ZTEST0
  ENDDO_JLON

  ! On boucle ensuite en passant du niveau JJLEV
  ! au niveau JJLEV-1 :

  DO JJLEV=JLEV,KTDIAN+1,-1
      DO_JLON
        ZDLUP1   = PSTACK(JLON,IPTRST_ZGDZF+JJLEV-1)
        ZZTHVL   =(PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1)+PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1-1))/2.0_JPRB
        ZZTHVLP  =(PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1)+PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1-1))/2.0_JPRB
        ZINCR    =(ZZTHVL-ZZTHVLP)/PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)*ZDLUP1
        ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
        ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN))
        ZTEST    =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN)-ZINCR)
        ZZDTHVL  = PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1-1)-PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1)
        ZZDTHVLP = PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1-1)-PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1)
        ZX    =(ZZDTHVL-ZZDTHVLP)/PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)/ZINCR*ZDLUP1
        ZUSX  =1.0_JPRB/(SIGN(1.0_JPRB,ZX)*MAX(ZEPSX,ABS(ZX)))
        ZDLUP2=( -(ZUSX-0.5_JPRB) +SIGN(1.0_JPRB,ZX)*&
         & SQRT(ABS((ZUSX-0.5_JPRB)*(ZUSX-0.5_JPRB)&
         & +2.0_JPRB*ZUSX*ABS(PSTACK(JLON, IPTRST_ZEN)/ZINCR))) )*ZDLUP1  
        ZDLUP =ZTEST*ZDLUP1+(1.0_JPRB-ZTEST)*ZDLUP2
        PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1)=PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1)+ZDLUP*ZTEST0
        PSTACK   (JLON, IPTRST_ZEN)=PSTACK  (JLON, IPTRST_ZEN)-ZINCR*ZTEST0
      ENDDO_JLON
  ENDDO

  ! - - - - - - - - - - - - - - - - - - - -
  ! BOUCLE VERS LE BAS QUI CALCULE ZGLMDN :
  ! - - - - - - - - - - - - - - - - - - - -

  ! L'energie (ZEN) et valeur initiale de "Ldown"

  DO_JLON
    PSTACK   (JLON, IPTRST_ZEN)=PECT(JLON,JLEV)
    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)=0.0_JPRB
  ENDDO_JLON

  ! Le passage du demi-niveau JLEV au niveau
  ! JLEV+1 juste au dessous :

  DO_JLON
    ZDLDN1 = PSTACK  (JLON,IPTRST_ZGDZF+JLEV+1-1)-PSTACK (JLON,IPTRST_ZGDZH+JLEV-1)
    ZINCR  =(PSTACK(JLON,IPTRST_ZTHETAP+JLEV+1-1)-PSTACK(JLON,IPTRST_ZTHETA+JLEV+1-1))&
     & /PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)/2.0_JPRB*ZDLDN1  
    ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
    ZTEST0 =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN))
    ZTEST  =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN)-ZINCR)
    ZDLDN2 =SQRT(ABS(PSTACK(JLON, IPTRST_ZEN)/ZINCR))*ZDLDN1
    ZDLDN  =ZTEST*ZDLDN1+(1.0_JPRB-ZTEST)*ZDLDN2
    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)=PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)+ZDLDN*ZTEST0
    PSTACK   (JLON, IPTRST_ZEN)=PSTACK  (JLON, IPTRST_ZEN)-ZINCR*ZTEST0
  ENDDO_JLON

  ! On boucle ensuite en passant du niveau JJLEV
  ! au niveau JJLEV+1 :
  DO JJLEV=JLEV+1,KLEV-1
      DO_JLON
        ZDLDN1   = PSTACK(JLON,IPTRST_ZGDZF+JJLEV+1-1)
        ZZTHVL   =(PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1)+PSTACK (JLON,IPTRST_ZTHETA+JJLEV+1-1))/2.0_JPRB
        ZZTHVLP  =(PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1)+PSTACK(JLON,IPTRST_ZTHETAP+JJLEV+1-1))/2.0_JPRB
        ZINCR    =(ZZTHVLP-ZZTHVL)/PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)*ZDLDN1
        ZINCR=SIGN(1._JPRB,ZINCR)*MAX(1.E-10_JPRB,ABS(ZINCR))
        ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN))
        ZTEST    =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN)-ZINCR)
        ZZDTHVL  = PSTACK (JLON,IPTRST_ZTHETA+JJLEV-1)-PSTACK (JLON,IPTRST_ZTHETA+JJLEV+1-1)
        ZZDTHVLP = PSTACK(JLON,IPTRST_ZTHETAP+JJLEV-1)-PSTACK(JLON,IPTRST_ZTHETAP+JJLEV+1-1)
        ZX    =(ZZDTHVL-ZZDTHVLP)/PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0)/ZINCR*ZDLDN1
        ZUSX  =1.0_JPRB/(SIGN(1.0_JPRB,ZX)*MAX(ZEPSX,ABS(ZX)))
        ZDLDN2=( -(ZUSX-0.5_JPRB) +SIGN(1.0_JPRB,ZX)*&
         & SQRT(ABS((ZUSX-0.5_JPRB)*(ZUSX-0.5_JPRB)&
         & +2.0_JPRB*ZUSX*ABS(PSTACK(JLON, IPTRST_ZEN)/ZINCR))) )*ZDLDN1  
        ZDLDN=ZTEST*ZDLDN1+(1.0_JPRB-ZTEST)*ZDLDN2
        PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)=PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)+ZDLDN*ZTEST0
        PSTACK   (JLON, IPTRST_ZEN)=PSTACK  (JLON, IPTRST_ZEN)-ZINCR*ZTEST0
      ENDDO_JLON
  ENDDO

  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! CALCUL EFFECTIF DES LONGUEURS DE MELANGE : "Lmel" et "Ldiss"
  ! ET CALCUL DES COEFFICIENTS DE MELANGE "PKUROV" ET "PKTROV".
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! ON PREVOIT UNE LIMITATION INFERIEURE (PAS SUPERIEURE CAR LA
  ! LOI POUR FAIBLES "z" EST PLUTOT EN "2.8*z" QU'EN "0.4*z") :
  !    -> ALMAVE       :  DANS L'ATMOSPHERE LIBRE
  !    -> KARMANN*"z"  :  DANS LA CLP
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  
  DO_JLON

    ! On limite Ldown par la hauteur :

    PSTACK(JLON,IPTRST_ZPHIH+JLEV-1)   =PAPHI(JLON,JLEV)-PAPHI(JLON,KLEV)+PGZ0(JLON)
    ZTEST0   =0.5_JPRB+SIGN(0.5_JPRB,PSTACK(JLON, IPTRST_ZEN))
    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)=PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1)+ZTEST0*(PAPHIF(JLON,KLEV)-PAPHI(JLON,KLEV))

    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1) =MIN(PSTACK(JLON,IPTRST_ZPHIH+JLEV-1), PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1))
    
    ! On majore par la hauteur de cvpp et de cvp
    

    PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1),PNLAB(JLON,JLEV)*(PSTACK(JLON, IPTRST_ZGZTOP)-PAPHI(JLON,JLEV)))
    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1),PNLAB(JLON,JLEV)*(PAPHI(JLON,JLEV)-PSTACK(JLON, IPTRST_ZGZBOT)))
    
  
    ZGZLCVPUP = MIN(RG*ALMAVX,PSTACK(JLON, IPTRST_ZGZTOPCVP)-PAPHI(JLON,JLEV))
    ZGZLCVPDN = MIN(RG*ALMAVX,PAPHI(JLON,JLEV)-PSTACK(JLON, IPTRST_ZGZBOTCVP))
    PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1),PNLABCVP(JLON,JLEV)*ZGZLCVPUP)
    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1),PNLABCVP(JLON,JLEV)*ZGZLCVPDN)
      

  ENDDO_JLON
ENDDO ! Jlev    

!     --------------------------------------
DO JLEV=KLEV-2,KTDIAN,-1 ! BOUCLE GENERALE Nr 2
!     --------------------------------------
  DO_JLON

    !  On veut que ça monte et descende au moins à la même hauteur !!
    
    PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1),PSTACK(JLON,IPTRST_ZGLMUP+JLEV+1-1) &
    &  + PAPHI(JLON,JLEV+1) - PAPHI(JLON,JLEV))

  ENDDO_JLON
ENDDO ! Jlev    

!     --------------------------------------
DO JLEV=KTDIAN+1,KLEV-1 ! BOUCLE GENERALE Nr 3
!     --------------------------------------
  DO_JLON

    PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1) = MAX(PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1),PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1-1) &
    &  + PAPHI(JLON,JLEV)   - PAPHI(JLON,JLEV-1))

  ENDDO_JLON
ENDDO ! Jlev    

Z2SQRT2=2._JPRB*SQRT(2._JPRB)

!     --------------------------------------
DO JLEV=KTDIAN,KLEV-1 ! BOUCLE GENERALE Nr 4
!     --------------------------------------
  DO_JLON
    
    ! La "nouvelle" longueur de melange de meso-NH 
    ! (devant approcher "2.8*g*z" pres de la surface) :

     PSTACK(JLON, IPTRST_ZLUP)=MAX(PSTACK(JLON,IPTRST_ZGLMUP+JLEV-1),1.E-10_JPRB)
     ZLDN=MAX(PSTACK(JLON,IPTRST_ZGLMDN+JLEV-1),1.E-10_JPRB)
     ZLWK0=PSTACK(JLON, IPTRST_ZLUP)/ZLDN
     PSTACK(JLON, IPTRST_ZLWK1)=1._JPRB+ZLWK0**(2.0_JPRB/3._JPRB)

  ENDDO_JLON

  DO_JLON

     ZGLMCBR=Z2SQRT2*PSTACK(JLON, IPTRST_ZLUP)/(PSTACK(JLON, IPTRST_ZLWK1)*SQRT(PSTACK(JLON, IPTRST_ZLWK1)))

    ! La limitation : L > LINF = MIN( 0.4*(G*z) , ALMAVE )

    ZGLKARMN= VKARMN*PSTACK(JLON,IPTRST_ZPHIH+JLEV-1)
    ZGLMINF = MIN(RG*ALMAVE, ZGLKARMN)

    ZGLMIX   = MAX(ZGLMINF, ZGLMCBR)
    ZGLDIS   = MAX(ZGLMINF, ZGLMCBR)

    ! Les deux longueurs : (1) de melange ; (2) de dissipation

    PLMECT(JLON,JLEV)=ZGLMIX
    PUSLE (JLON,JLEV)=1.0_JPRB/(ALD*ZGLDIS)

    ! La fonction "PHI3" de Redeslperger ; neutre=1./(1.+2.*ARSC1)

    ZDPHI    =PSTACK(JLON,IPTRST_ZGDZF+JLEV+1-1)
    ZG2L2SLD2=MAX( ZPHI3MAX, ARSC1* MIN( 2.0_JPRB, ZGLMCBR*ZGLMCBR &
     & *PSTACK(JLON,IPTRST_ZDTHETA+JLEV+1-1)/ZDPHI &
     & /PECT(JLON,JLEV)/PSTACK(JLON,IPTRST_ZTHETAH+JLEV-0) ) &
     & )  
    PPHI3 (JLON,JLEV)=1.0_JPRB/(1.0_JPRB+ZG2L2SLD2)

  ENDDO_JLON

!     ------------------------------
ENDDO ! JLEV (BOUCLE GENERALE)
!     ------------------------------

!*
!     --------------------------------------------------------------------
!     VI  - AU DERNIER NIVEAU : CALCULS DE LA LONGEUR DE MELANGE "PLMECT"
!           DE L'INVERSE DE LA LONGUEUR DE DISSIPATION "PUSLE" ET DE LA
!           FONCTION "PPHI3" DE REDELSPERGER.
!     --------------------------------------------------------------------

DO_JLON
  PLMECT(JLON,KLEV)= 0.5_JPRB*PLMECT(JLON,KLEV-1)
  PUSLE (JLON,KLEV)= 1.0_JPRB/(ALD*0.5_JPRB*PLMECT(JLON,KLEV-1))
  PPHI3 (JLON,KLEV)= PPHI3(JLON,KLEV-1)
ENDDO_JLON
!*
!     ------------------------------------------------------------------



END SUBROUTINE SIMPLE4_ACBL89
