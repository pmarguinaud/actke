MODULE YOMDATA

#define create(x) !$acc declare create(x)

REAL, POINTER :: ZZ (:) => NULL () 
create (ZZ)

REAL :: YY  
create (YY)

INTEGER, PARAMETER :: JP = 123
create (JP)

END MODULE
