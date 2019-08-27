#ifdef CPU
#define DO_JLON DO JLON = KIDIA, KFDIA
#define ENDDO_JLON ENDDO
#define INIT_JLON 
#ifdef D_LAST_LEVEL
!nothing
#else
!$acc routine(SIMPLE4_ACBL89) vector
!$acc routine(SIMPLE4_ACEVOLET) worker
!$acc routine(SIMPLE4_ACTKE) worker
!$acc routine(SIMPLE4_ACTURB) worker
!$acc routine(SIMPLE4_FL2HL) worker
!$acc routine(SIMPLE4_HL2FL) worker
#endif
#endif

#ifdef GPU
#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON JLON = KIDIA
#endif

#ifdef GPU
ATTRIBUTES (DEVICE) &
#endif


