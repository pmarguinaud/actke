!$acc routine(SIMPLE4_ACBL89) seq
!$acc routine(SIMPLE4_ACEVOLET) seq
!$acc routine(SIMPLE4_ACTKE) seq
!$acc routine(SIMPLE4_ACTURB) seq
!$acc routine(SIMPLE4_FL2HL) seq
!$acc routine(SIMPLE4_HL2FL) seq

#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON  JLON = KIDIA
