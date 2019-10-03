!$acc routine(SIMPLE2_ACBL89) seq
!$acc routine(SIMPLE2_ACEVOLET) seq
!$acc routine(SIMPLE2_ACTKE) seq
!$acc routine(SIMPLE2_ACTURB) seq
!$acc routine(SIMPLE2_FL2HL) seq
!$acc routine(SIMPLE2_HL2FL) seq

#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON  JLON = KIDIA
