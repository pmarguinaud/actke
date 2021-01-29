#!/home/gmap/mrpm/marguina/install/perl-5.32.0/bin/perl -w
#
use FindBin qw ($Bin);
use lib $Bin;
use Fxtran;
use Data::Dumper;
use FileHandle;
use File::Copy;
use File::Basename;
use strict;

my $F90 = 'simple2_acturb.F90';

my $doc = &Fxtran::fxtran (location => $F90);
    
my $fun = do { my $fh = 'FileHandle'->new ("<fcttrm.func.h"); local $/ = undef; <$fh> } . "\nEND\n";
$fun = &Fxtran::fxtran (string => $fun);

# print $fun->toString (), "\n";

my @name = qw (FODLEW FODQS FOEW FOQS);


for my $name (@name)
  {

    my @E = &f ('.//f:named-E[./f:N/f:n[text ()="' . $name . '"]]', $doc);

# <a-stmt><E-1><named-E><N><n>FOEW</n></N> <R-LT><parens-R>( <element-LT><element><named-E><N><n>PTARG</n></N></named-E></element>,<element><named-E><N><n>PDELARG</n></N></named-E></element></element-LT> )</parens-R></R-LT></named-E></E-1> <a>=</a> <E-2>

    my ($F) = &f ('.//f:a-stmt[./f:E-1/f:named-E/f:N/f:n[text ()="' . $name . '"]]', $fun);

    &Fxtran::expand ($F);
  
    print $F->textContent (), "\n";
    print $F->toString (), "\n";

    my @dumm = &f ('./f:E-1/f:named-E/f:R-LT/f:parens-R/f:element-LT/f:element/f:named-E/f:N/f:n', $F);

    for my $dumm (@dumm)
      {
        print "   ", $dumm->textContent (), "\n";
      }
    @dumm = map { $_->textContent () } @dumm;

    my ($V) = &f ('./f:E-2/*', $F);
   
    print $V->textContent (), "\n";

    for my $E (@E)
      {
        my $stmt = &Fxtran::stmt ($E);

        print " stmt = ", $stmt->textContent (), "\n";

        my $v = $V->cloneNode (1);

        print " v = ", $v->textContent (), "\n";

        print $E->textContent (), "\n";
        my @actu = &f ('./f:R-LT/f:parens-R/f:element-LT/f:element/f:named-E/f:N/f:n', $E);

        for my $actu (@actu)
          {
            print "   ", $actu->textContent (), "\n";
          }

        for my $i (0 .. $#dumm)
          {
            my $dumm = $dumm[$i];
            my $actu = $actu[$i];

            my @e = &f ('.//f:named-E[./f:N/f:n[text ()="' . $dumm . '"]]', $v);

            print " e = ", join (' ', map { $_->textContent () } @e), "\n";

            for my $e (@e)
              {
                $e->replaceNode ($actu->cloneNode (1));
              }
          }

        $E->replaceNode ($v);

        &Fxtran::fold ($stmt);

        print " stmt = ", $stmt->textContent (), "\n";

      }

  }

=pod

'FileHandle'->new (">$F90")->print ($doc->textContent ());

=cut

print $doc->textContent ();



