#!/usr/bin/perl -w

# this script patches a config.status file, to use our own perl script
# in the main loop
# we do it this way to circumvent hacking (and thereby including)
# autoconf function (which are GPL) into our LGPL acinclude.m4.in
# written by Michael Matz <matz@ifh.de>

# we have to change two places
# 1. the splitting of the substitutions into chunks of 90 (or even 48 in
#    later autoconf's
# 2. the big main loop which patches all Makefile.in's
use File::Basename;

my $ac_aux_dir = dirname($0);
my ($flag);
my $ac_version = 0;
my $vpath_seen = 0;
$flag = 0;

while (<>) {
# usage of $flag: 0 -- we have seen nothing yet
#   1 -- we are in (1)
#   2 -- we have ended (1)
#   3 -- we are in (2)
#   4 -- we ended (2)

    if ($flag == 4) {
        print;
    } elsif ($flag == 0) {
# 1. begins with (including): "ac_max_sed_\S+\s*=\s*[0-9]+..."
#    ends with (excluding) "CONFIG_FILE=..."
#    in later autoconf (2.14.1) there is no CONFIG_FILES= line,
#    but instead the (2) directly follow (1)
        if (/^\s*ac_max_sed_([a-z]+).*=\s*[0-9]+/ ) {
	    $flag = 1;
	    if ($1 eq 'lines') {
	        $ac_version = 2141;
	    } elsif ($1 eq 'cmds') {
	        $ac_version = 213;
	    }
	    # hmm, we don't know the autoconf version, but we try anyway
	} else {
	    print;
	}
    } elsif ($flag == 1) {
        if (/^\s*CONFIG_FILES=/ ) {
	     print;
	     $flag = 2;
	} elsif (/^\s*for\s+ac_file\s+in\s+.*CONFIG_FILES/ ) {
	     $flag = 3;
	     if ($ac_version != 2141) {
	         $ac_version = 2141;
	     }
	}
    } elsif ($flag == 2) {
# 2. begins with: "for ac_file in.*CONFIG_FILES"  (the next 'for' after (1))
#    end with: "rm -f conftest.s\*"
	if (/^\s*for\s+ac_file\s+in\s+.*CONFIG_FILES/ ) {
	    $flag = 3;
	} else {
	    print;
	}
    } elsif ($flag == 3) {
        if (/^\s*rm\s+-f\s+conftest/ ) {
	    $flag = 4;
	    insert_main_loop();
	} elsif (/^\s*rm\s+-f\s+.*ac_cs_root/ ) {
	    $flag = 4;
	    insert_main_loop();
	    #die "hhhhhhh";
	    if ($ac_version != 2141) {
	        print STDERR "hmm, don't know autoconf version\n";
	    }
	} elsif (/VPATH/ ) {
	    $vpath_seen = 1;
	}
    }
}

die "wrong input (flag != 4)" unless $flag == 4;
print STDERR "hmm, don't know autoconf version\n" unless $ac_version;

sub insert_main_loop {
    print <<EOF;
#echo Doing the fast build of Makefiles -- autoconf $ac_version
if test "x\$ac_cs_root" = "x" ; then
    ac_cs_root=conftest
fi
EOF
    if ($vpath_seen) {
        print <<EOF;
# VPATH subst was seen in original config.status main loop
echo '/^[ 	]*VPATH[ 	]*=[^:]*\$/d' >> \$ac_cs_root.subs
EOF
    }
    print <<EOF;
rm -f \$ac_cs_root.sacfiles
for ac_file in .. \$CONFIG_FILES ; do
    if test "x\$ac_file" != x..; then
        echo \$ac_file >> \$ac_cs_root.sacfiles
    fi
done
if test -f \$ac_cs_root.sacfiles ; then
    perl $ac_aux_dir/config.pl "\$ac_cs_root" "\$ac_given_srcdir" "\$ac_given_INSTALL"
fi
rm -f \$ac_cs_root.s*

EOF
    return;
}
