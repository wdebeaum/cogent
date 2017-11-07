package TextTagger::Misspellings;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_misspellings ready_misspellings tag_misspellings fini_misspellings);

use IPC::Open2;
use List::Util qw(sum0);

use TextTagger::Util qw(structurally_equal);
use TextTagger::Normalize qw(normalize);

use strict vars;

my $debug = 0;

# get at most 3 options for correcting the spelling of a misspelled word
my $max_corrections = 3;

my ($aspell_in, $aspell_out, $aspell_pid);

sub init_misspellings {
  my $self = shift;
  die "aspell not configured. Please install aspell and re-run the configure script."
    if ($TextTagger::Config::ASPELL eq '');
  # give aspell back its own local-data-dir setting (see note in Makefile)
  open $aspell_in, "-|", $TextTagger::Config::ASPELL, 'config', 'local-data-dir';
  my $local_data_dir = <$aspell_in>;
  close $aspell_in;
  chomp $local_data_dir;
  $aspell_pid = open2($aspell_in, $aspell_out,
		      $TextTagger::Config::ASPELL, 'pipe', '--encoding=utf-8',
		      "--local-data-dir=$local_data_dir",
		      ($self->{aspell_dict} ?
		        ('--master=' . $self->{aspell_dict}) : ()));
  binmode $aspell_in, ':utf8';
  binmode $aspell_out, ':utf8';
}

sub fini_misspellings {
  close($aspell_in);
  close($aspell_out);
  waitpid $aspell_pid, 0;
}

sub ready_misspellings {
  my $header = <$aspell_in>; # discard header
}

sub tag_misspellings {
  my ($self, $text) = @_;
  my @typos = ();
  # change whitespace to simple spaces to avoid embedded newlines
  $text =~ s/\s/ /;
  # send (escaped) text to aspell
  print $aspell_out "^$text\n";
  # read its output a line at a time, until a blank line
  my $line;
  while (($line = <$aspell_in>) and $line !~ /^\s*$/) {
    next unless ($line =~ /^& (\S+) \d+ (\d+): /); # first number is number of corrections, unused because we can count commas
    my ($misspelled_word, $start, $corrections) = ($1, $2, $');
    $start--; # aspell uses 1-based character indices
    # split correction options
    my @corrections = split(/, /, $corrections);
    print STDERR Data::Dumper->Dump([$misspelled_word, \@corrections], [qw(misspelled_word *raw_corrections)])
      if ($debug);
    # simple case/dash variants don't count as misspellings to us
    my $norm_missp_word = normalize($misspelled_word);
    # remove "corrections" that are just space/dash insertions, when we have
    # other options (they're usually more trouble than they're worth)
    my @unsplit_corrections =
      grep {
	my $norm_no_space = normalize($_);
	$norm_no_space =~ s/\s//g;
	$norm_missp_word ne $norm_no_space or $_ !~ /[\s-]/
      } @corrections;
    @corrections = @unsplit_corrections if (@unsplit_corrections);
    # if any "corrections" remain that are just case changes, skip this whole
    # word because it isn't really misspelled
    next if (grep { $norm_missp_word eq normalize($_) } @corrections);
    # split words within corrections
    @corrections = map { [split(/\s|-/)] } @corrections;
    # take only the first few unique corrections, ignoring case
    my @new_corrections = ();
    while (@new_corrections < $max_corrections and @corrections > 0) {
      my $c1 = shift @corrections;
      my $c1lc = [map { lc($_) } @$c1];
      # Special case to make sure that if we get two corrections next to each
      # other that differ only in case, one of them is all lowercase, and the
      # original is all lowercase, then we keep the all lowercase correction.
      # E.g. "mdoel" -> "Model", "model" gets swapped.
      next if ($misspelled_word !~ /\p{Uppercase}/ and 
	       (grep /\p{Uppercase}/, @$c1) and
	       @corrections > 0 and
	       (not grep /\p{Uppercase}/, @{$corrections[0]}) and
	       structurally_equal($c1lc, [map { lc($_) } @{$corrections[0]}]));
      push @new_corrections, $c1
        unless (grep { structurally_equal($c1lc, [map { lc($_) } @$_]) }
	             @new_corrections);
    }
    @corrections = @new_corrections;
    my %common = (
      type => 'alternate-spelling', # TODO or make a new type?
      start => $start,
      end => $start + length($misspelled_word)
    );
    print STDERR Data::Dumper->Dump([\@corrections], ['*corrections'])
      if ($debug);
    for my $c (@corrections) {
      last unless (defined($c)); # we got fewer than $max_corrections
      if (@$c == 1) { # unsplit word
	push @typos, +{ %common, lex => $c->[0] };
      } else { # split words
        if (length($misspelled_word) == sum0(map { length($_) } @$c)) {
	  # length of misspelled word = total length of the corrected words,
	  # so just split the tags based on each corrected word's length
	  my $start = $common{start};
	  for my $w (@$c) {
	    my $end = $start + length($w);
	    push @typos,
	      +{ %common, lex => $w, start => $start, end => $end },
	      # also output a subword tag so that CombineTags won't output a
	      # word message with a NIL word
	      +{ type => 'subword',
	         lex => substr($text, $start, $end - $start),
		 start => $start, end => $end
	      };
	    $start = $end;
	  }
	} else {
	  # can't handle this case because we don't know how to adjust the
	  # character offsets to fit within the original word
	  print STDERR "Warning: dropping spelling correction that involves both word splitting and a change in length: \@$start $misspelled_word => " . join(' ', @$c) . "\n";
	}
      }
    }
  }
  return [@typos];
}

push @TextTagger::taggers, {
  name => 'misspellings',
  init_function => \&init_misspellings,
  ready_function => \&ready_misspellings,
  tag_function => \&tag_misspellings,
  fini_function => \&fini_misspellings,
  output_types => [qw(alternate-spelling subword)],
  input_text => 1
};

1;
