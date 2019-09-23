#include <stdio.h>

extern double __intel_cpu_feature_indicator_x;
extern double __intel_cpu_feature_indicator;

void set_ ()
{
  unsigned char * p;

  p = (unsigned char *)&__intel_cpu_feature_indicator_x;
  printf (" p = 0x%llx\n", p);
  p[0] = 0xff; p[1] = 0xff; p[2] = 0xff; p[3] = 0x30;
  p[4] = 0x00; p[5] = 0x00; p[6] = 0x00; p[7] = 0x00;

  p = (unsigned char *)&__intel_cpu_feature_indicator;
  printf (" p = 0x%llx\n", p);
  p[0] = 0xff; p[1] = 0xff; p[2] = 0xff; p[3] = 0x30;
  p[4] = 0x00; p[5] = 0x00; p[6] = 0x00; p[7] = 0x00;
}
