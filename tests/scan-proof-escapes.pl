#!/usr/bin/env perl
# SPDX-License-Identifier: MPL-2.0
# Fail-closed lexical guard for known proof escape tokens in tracked sources.
# Not a compiler, theorem prover or proof of type soundness.
use strict;
use warnings;

if (@ARGV != 1 || !-f $ARGV[0] || !-r $ARGV[0]) {
    print STDERR "NOT_RUN: expected one readable proof source file\n";
    exit 2;
}
my $file = $ARGV[0];
my ($ext) = $file =~ /\.([^.\/]+)$/;
$ext //= '';
my %tokens = (
  idr  => qr/\b(?:believe_me|really_believe_me|assert_total)\b/,
  lean => qr/\b(?:sorry|unsafeCoerce)\b/,
  agda => qr/\b(?:believe_me|really_believe_me|assert_total|sorry|unsafeCoerce)\b/,
  thy  => qr/\b(?:sorry|Admitted)\b/,
  v    => qr/\b(?:Admitted\s*\.|admit\b)/,
);
if (!exists $tokens{$ext}) {
    print STDERR "NOT_RUN: unsupported proof-source extension: $file\n";
    exit 2;
}
my %block = (idr => ['{-','-}'], agda => ['{-','-}'],
             lean => ['/-','-/'], thy => ['(*','*)'], v => ['(*','*)']);
my %line = (idr => '--', agda => '--', lean => '--', thy => '', v => '//');
open my $fh, '<', $file or die "NOT_RUN: unreadable $file\n";
local $/;
my $raw = <$fh>;
defined $raw or die "NOT_RUN: unreadable $file\n";
close $fh;
my ($start, $end) = @{$block{$ext}};
my ($visible, $depth, $quoted) = ('', 0, 0);
my $length = length($raw);
for (my $i = 0; $i < $length;) {
    my $pair = substr($raw, $i, 2);
    my $c = substr($raw, $i, 1);
    if ($depth) {
        if ($pair eq $start) { $depth++; $visible .= '  '; $i += 2; next; }
        if ($pair eq $end) { $depth--; $visible .= '  '; $i += 2; next; }
        $visible .= ($c eq "\n" ? "\n" : ' ');
        $i++;
        next;
    }
    if ($quoted) {
        # Coq escapes quote by doubling it; Idris/Lean/Agda also use \.
        if ($ext eq 'v' && $pair eq '""') { $visible .= '  '; $i += 2; next; }
        if ($c eq '\\' && $ext ne 'v' && $i + 1 < $length) {
            $visible .= '  '; $i += 2; next;
        }
        if ($c eq '"') { $quoted = 0; }
        $visible .= ($c eq "\n" ? "\n" : ' ');
        $i++;
        next;
    }
    if ($pair eq $start) { $depth = 1; $visible .= '  '; $i += 2; next; }
    if ($c eq '"') { $quoted = 1; $visible .= ' '; $i++; next; }
    if ($line{$ext} ne '' && substr($raw, $i, length($line{$ext})) eq $line{$ext}) {
        while ($i < $length && substr($raw, $i, 1) ne "\n") {
            $visible .= ' ';
            $i++;
        }
        next;
    }
    $visible .= $c;
    $i++;
}
if ($depth || $quoted) {
    print STDERR "NOT_RUN: unterminated comment or quoted string: $file\n";
    exit 2;
}
my $failed = 0;
my $pattern = $tokens{$ext};
while ($visible =~ /$pattern/g) {
    my $offset = $-[0];
    my $prefix = substr($visible, 0, $offset);
    my $line_number = 1 + ($prefix =~ tr/\n/\n/);
    print STDERR "FAIL: $file:$line_number: executable proof escape token\n";
    $failed = 1;
}
exit($failed ? 1 : 0);
