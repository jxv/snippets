use strict;
use warnings;

my @arr = (
	"this is the first elem",
	"this is the sec elem",
	"3rd",
	"4th",
	"5th",
	"6th",
);

for (my $i = 0; $i < scalar @arr; $i++) {
	print $i, ": ", $arr[$i], "\n";
}


print "@arr";
