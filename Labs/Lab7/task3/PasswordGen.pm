package PasswordGen;

use Exporter qw(import);
@EXPORT_OK = qw(gen_pass);

sub gen_pass {
    my @symbols = ('a'...'z', 'A'...'Z', 0...9);
    my $length = shift;
    my $randpass = join "", map $symbols[rand @symbols], 1..$length;
    return $randpass;
}

1;
