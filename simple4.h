
!$acc routine(SIMPLE2_ACBL89) seq
!$acc routine(SIMPLE2_ACEVOLET) seq
!$acc routine(SIMPLE2_ACTKE) seq
!$acc routine(SIMPLE2_ACTURB) seq
!$acc routine(SIMPLE2_FL2HL) seq
!$acc routine(SIMPLE2_HL2FL) seq

#define alloc0(a) a => PSTACK (1:KLON, IPTRST); IPTRST = IPTRST + 1
#define alloc1(a, l1, u1) a (1:,l1:) => PSTACK (1:KLON, IPTRST:IPTRST+((u1)-(l1))); IPTRST=IPTRST+(u1)-(l1)+1

#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON  JLON = KIDIA
