#ifdef CPU
#define DO_JLON DO JLON = KIDIA, KFDIA
#define ENDDO_JLON ENDDO
#define INIT_JLON 
#endif

#ifdef GPU
#define DO_JLON 
#define ENDDO_JLON 
#define INIT_JLON JLON = KIDIA
#endif

#define DOLOG(f,i,x) IPTR(1)=IPTR(1)+1;NFILE(IPTR(1))=f;NLINE(IPTR(1))=__LINE__;NIND(IPTR(1))=i;RVALUE(IPTR(1))=x;

#ifdef GPU
ATTRIBUTES (DEVICE) &
#endif
