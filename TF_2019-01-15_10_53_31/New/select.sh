#!/bin/bash

set -x

d=$1

/usr/bin/perl -e ' 
use File::Basename;
print "ARGV=@ARGV\n";
for my $f (@ARGV) 
{
print "f=$f\n";

  my $d = &dirname ($f);
  my $g = &basename ($f);
  $g =~ s/\.\d+//o;
  print "$f -> $g\n";
  unlink ($g);
  symlink ($f, $g);
} ' $d/*


