#!/usr/bin/env perl

use strict; use warnings;
use File::Find;
use File::Slurp;
use File::Spec;
use Symbol qw(gensym);
use IPC::Open3;

# TODO - USAGE - Run from spec directory
# probably want to have this overriden via passed option
my $dir = "syntax-examples";
my $header = "";
my $sub_header = "";

open(NULL, ">", File::Spec->devnull);
# our output markdown file
open(MD, ">", "syntax.md");

# Print automated message and caveat
print MD <<CAVEAT
# Please NOTE: This is *_NOT_* the final syntax.  
This is simply showing some examples that have 
been proposed to start a discussion.  
# REPEAT: THERE IS NOTHING FINAL ABOUT ANYTHING IN THIS DOC

This doc was AUTOGENERATED via the files in `syntax-examples` tree 
CAVEAT
;

# Find all our examples
find(\&add_to_markdown, $dir);

sub add_to_markdown
{
	# we only want files
	return unless -f $_; 
	my $full_path = $File::Find::name;

	# Find our relative path within the syntax-example dirs
	# by removing all directories up through syntax-examples
	$full_path =~ s/.*$dir[\\|\/]//g;
	#print "$full_path\n";
	my @path_pieces = split(/[\\|\/]/, $full_path);

	# check if we are in a new header
	if( $path_pieces[0] ne $header)
	{
		$header = $path_pieces[0];
		print MD "\n## $header\n\n";
	}
	# check if we have a subdir / subgroup
	if(scalar @path_pieces == 3)
	{
		# see if this is a different subdir
		if($path_pieces[1] ne $sub_header)
		{
			$sub_header = $path_pieces[1];
			print MD "### $sub_header\n\n";
		}
	}
	# read in the example
	my @content = read_file($path_pieces[-1]);
	my $err = '';
	# run perl -c on the example file to see if the syntax is valid
	my $pid = open3(gensym, ">&NULL", \*PH, "perl -c $path_pieces[-1]");
	# captures stderr
	while(<PH>) { $err .= $_;}
	waitpid($pid, 0);
	
	print MD "    # $full_path - ";
	if (defined $err && $err =~ /syntax error/)
	{
		print MD "contains invalid perl5 syntax\n";
	}
	else
	{
		print MD "valid perl5 syntax\n";
	}
	foreach my $line (@content)
	{
		print MD "    $line";
	}
	print MD "\n";
}

# close everything up
close(NULL);
close(MD);