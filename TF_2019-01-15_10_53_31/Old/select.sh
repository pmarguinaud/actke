#!/bin/bash

d=$1

perl -e ' 
use File::Basename;
for my $f (@ARGV) 
{
  my $d = &dirname ($f);
  my $g = &basename ($f);
  $g =~ s/\.\d+//o;
  print "$f -> $g\n";
  unlink ($g);
  symlink ($f, $g);
} ' $d/*


