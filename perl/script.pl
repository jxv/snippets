use warnings;
use strict;
use 5.010;

say "Hello, World!";


# user def. sub routine
sub hyphenate {
  my $word = shift @_; # extract the first argument from teh array, ignore everything else.
  $word = join "-", map { substr $word, $_, 1 } (0 .. (length $word) - 1); # an overly clever list comprehension
  return $word;
}


say hyphenate("exterminate"); # "e-x-t-e-r-m-i-n-a-t-e"

my %hash = (
  1 => "one",
  2 => "two",
  3 => "three",
  4 => "four",
  5 => "five",
  6 => "six",
  7 => "seven",
  8 => "eight",
  9 => "nine",
  10 => "ten",
);

my @arr = %hash;
print "@arr\n";


