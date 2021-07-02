#!/usr/bin/perl -w
#
use strict;
use FileHandle;
use Data::Dumper;

use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;

my $f = shift;

my $d = &Fxtran::fxtran (location => $f);




my @as = &f ('.//f:EN-decl/f:array-spec'
           . '[./f:shape-spec-LT/f:shape-spec[string (.)="KLON"]]' 
           . '[count (./f:shape-spec-LT/f:shape-spec)=1]', $d);


for my $as (@as)
  {
    $as->unbindNode ();
  }


my @ar = &f ('.//f:R-LT[./f:parens-R/f:element-LT/f:element[string (.)="JLON"]]'
           . '[count (./f:parens-R/f:element-LT/f:element)=1]', $d);



for my $ar (@ar)
  {  
    $ar->unbindNode ();
  }  

my @do = &f ('.//f:do-construct[./f:do-stmt/f:do-V[string (.)="JLON"]]', $d);

for my $do (@do)
  {
    $do->firstChild->unbindNode ();
    $do->lastChild->unbindNode ();
    my @n = $do->childNodes;

    for my $n (@n)
      {
        $do->parentNode->insertBefore ($n, $do);
      }

    $do->unbindNode ();

  }


'FileHandle'->new (">$f.new")->print ($d->textContent);
