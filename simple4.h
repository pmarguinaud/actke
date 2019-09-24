!$acc routine(SIMPLE4_ACBL89) worker
!$acc routine(SIMPLE4_ACEVOLET) worker
!$acc routine(SIMPLE4_ACTKE) worker
!$acc routine(SIMPLE4_ACTURB) worker
!$acc routine(SIMPLE4_FL2HL) worker
!$acc routine(SIMPLE4_HL2FL) worker

#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON  JLON = KIDIA
