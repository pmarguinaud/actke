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

#ifdef GPU
ATTRIBUTES (DEVICE) &
#endif
