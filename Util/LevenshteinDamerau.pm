package Rolebot::Util::LevenshteinDamerau;
use strict;
use warnings;
use List::Util qw(min);

use base qw(Exporter);
our @EXPORT_OK  = qw(&distance);

sub distance($$) {
	my ($source,$target) = @_;

	my $m = length($source);
	my $n = length($target);
	my $INF = $m + $n;
	my %H;
	$H{0}{0} = $INF;

	for(my $i = 0; $i <= $m; $i++) { $H{$i + 1}{1} = $i; $H{$i + 1}{0} = $INF; }
	for(my $j = 0; $j <= $n; $j++) { $H{1}{$j + 1} = $j; $H{0}{$j + 1} = $INF; }

	my %sd;
	for(my $key = 0; $key < ($m + $n); $key++) {
		my $letter = substr($source . $target, $key-1, 1);
		$sd{$letter} = 0;
	}
	

	for(my $i = 1; $i <= $m; $i++) {
		my $DB = 0;

		for(my $j = 1; $j <= $n; $j++) {
			my $i1 = $sd{substr($target, $j-1, 1)};
			my $j1 = $DB;

			if( substr($source, $i-1, 1) eq substr($target, $j-1, 1) ) {
				$H{$i + 1}{$j + 1} = $H{$i}{$j};
				$DB = $j;
			}
			else {
				$H{$i + 1}{$j + 1} = min($H{$i}{$j}, min($H{$i + 1}{$j}, $H{$i}{$j + 1})) + 1;
			}

			$H{$i + 1}{$j + 1} = min($H{$i + 1}{$j + 1}, $H{$i1}{$j1} + ($i - $i1 - 1) + 1 + ($j - $j1 - 1));
		}

		$sd{substr($source, $i-1, 1)} = $i;
	}

	return $H{$m + 1}{$n + 1};
}

1;
