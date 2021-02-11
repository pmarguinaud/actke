
!$acc routine(SIMPLE4_ACBL89) seq
!$acc routine(SIMPLE4_ACEVOLET) seq
!$acc routine(SIMPLE4_ACTKE) seq
!$acc routine(SIMPLE4_ACTURB) seq
!$acc routine(SIMPLE4_FL2HL) seq
!$acc routine(SIMPLE4_HL2FL) seq


#ifdef USE_STACK
#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)
#define init_stack() INTEGER :: IPTRST; IPTRST = KPTRST
#define alloc(n) IP_##n##_ = LOC (PSTACK (IPTRST)); IPTRST = IPTRST + SIZE (n); IF (IPTRST > KPSTSZ) STOP "STACK OF: "//__FILE__
#else
#define temp(t, n, s) t :: n s
#define alloc(n)
#define init_stack() INTEGER :: IPTRST
#endif

#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON  JLON = KIDIA
