#!/usr/bin/perl -w
#==========================================================================
# bsc-trace.pl
#
# Author : Christopher Batten (cbatten@mit.edu)
# Date   : April 12, 2005
#
(our $usageMsg = <<'ENDMSG') =~ s/^\#//gm;
#
# Simple script which converts bsv "one-per-line" trace output into
# a more compact and readable column format.
#
ENDMSG

use strict "vars";
use warnings;
no  warnings("once");
use Getopt::Long;
use File::Basename;

#--------------------------------------------------------------------------
# Command line processing
#--------------------------------------------------------------------------

our %opts;

sub usage()
{

  print "\n";
  print " Usage: bsc-trace.pl <format-file> [trace-file]\n";
  print "\n";
  print " Options:\n";
  print "  --help        print this message\n";
  print "  <format-str>  format string\n";
  print "  [trace-file]  output trace from BSV simulation (default is STDIN)\n";
  print "$usageMsg";

  exit();
}

sub processCommandLine()
{

  $opts{"help"}        = 0;

  Getopt::Long::GetOptions( \%opts, 'help|?' ) or usage();

  ($opts{"format"}     = shift(@ARGV)) or usage();
  ($opts{"trace-file"} = shift(@ARGV)) or ($opts{"trace-file"} = "-");
  $opts{"help"} and usage();

}

#--------------------------------------------------------------------------
# Helper Functions
#--------------------------------------------------------------------------

sub printTraceLine()
{
  my $traceString  = $_[0];
  my $traceHashRef = $_[1];

  foreach my $tag ( @settings::fieldNames ) {
    if ( defined($traceHashRef->{$tag}) ) {
      $traceString =~ s/$tag/$traceHashRef->{$tag}/;
    }
    else {
      $traceString =~ s/$tag/$settings::emptyFields->{$tag}/;
    }
  }

}

#--------------------------------------------------------------------------
# Main
#--------------------------------------------------------------------------

sub main()
{

  processCommandLine();
  require $opts{"format"};

  my $cycle = 0;
  my %traceHash;

  my $traceFile = $opts{"trace-file"};
  open( FIN, "<$traceFile" ) or die("Could not open BSV trace file ($traceFile)!");
  while ( my $line = <FIN> ) {
  
    if ( $line =~ /^ => Cycle =\s+(\d+)$/ ) {

      my $tempTraceLine = $settings::traceString;
      foreach my $tag ( keys %settings::fields ) {

        my $theTraceString = $traceHash{$tag};
        my $theEmptyString = $settings::fields{$tag};

        # Substitute the trace field in ...
        if ( defined($theTraceString) ) {

          # If the trace string is shorter than the empty string then
          # add some spaces at the end so things line up ...
          my $theTraceStringLen = length($theTraceString);
          my $theEmptyStringLen = length($theEmptyString);
          if ( $theTraceStringLen < $theEmptyStringLen ) {
            $theTraceString .= (" "x ($theEmptyStringLen-$theTraceStringLen));
          }

          $tempTraceLine =~ s/{$tag}/$theTraceString/;

        }

        # Substitute the empty field in ...
        else {
          $tempTraceLine =~ s/{$tag}/$theEmptyString/;
        }

      }

      print " CYC: ".sprintf("%4d",$cycle)."  $tempTraceLine\n";

      $cycle = $1;
      %traceHash = ();
    }
    elsif ( $line =~ /^ => (\S+) (.*)$/ ) {
      $traceHash{$1} = $2;
    }
    else {
      print $line;
    }

  }
  close( FIN );
  
}

main();
