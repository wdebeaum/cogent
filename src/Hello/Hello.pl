#!/usr/bin/env perl

package Hello;
use TripsModule::TripsModule;
@ISA = qw(TripsModule);

use strict vars;

sub init {
  my $self = shift;
  $self->{name} = 'Hello';
  $self->SUPER::init();
  $self->send_msg('(subscribe :content (request &key :content (hello . *)))');
}

sub receive_request {
  my ($self, $msg, $content) = @_;
  my $verb = lc($content->{verb});
  if ($verb eq 'hello') {
    $self->reply_to_msg($msg,
      '(tell :content (hello' .
      (exists($content->{sender}) ? ' ' . $content->{sender} : '') . '))'
    );
  } else {
    $self->reply_to_msg($msg,
      qq/(error :comment "unknown request verb $verb")/);
  }
}

Hello->new(@ARGV)->run() if ($0 eq __FILE__);

1;

