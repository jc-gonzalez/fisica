#!/usr/local/bin/perl -w
#############################################################
# JOBS-MC.daemon v. 0.01
# Copyright (c) J C Gonzalez, 1998 - All rights reserved
#------------------------------------------------------------
# Perl script for running automatically the MC generation
# program.
#############################################################
#

use strict;
use vars qw(
            $VERSION $PROGRAM $SYSDIR $COPYRIGHT $USER $EXECCOR
            $uptime $DISK $MOTHERDIR $NICE $TOP $TEL
            $MINN $SITE
            $Spectral_Index
            @energies @energies2
            @nshowers
            @primaries
            @Theta
            @Phi
            $Energy1 $Energy2
            $Primary
            $Set
            $MACH_CRITICAL
            $MACH_BAD     
            $MACH_GOOD    
            $MACH_VERYGOOD
            $MAXDISK $MAXTAPE
            $verbose      
            $debug        
            $minfree      
            $cpu
            $cpumsg
            $icpu
            );
use Carp;

require 5.001;

$TEL = 'MAGIC';

$PROGRAM = "$TEL-MC.DAEMON";
$COPYRIGHT = 'Copyright (c) J C Gonzalez, 1998 - All rights reserved';
$VERSION = '0.01';
$USER = '#USERADD#';
$DISK = '#DATADISK#';
$SYSDIR = '#ADMDISK#';
$MOTHERDIR = '#PATHEXE#';
$EXECCOR = '#NAMEEXE#';
$NICE = '#NICE#';
$SITE = '#SITE#';
$TOP = '/usr/local/bin/top';

# parameters 

# Note: each CORSIKA run launched from this script will generate
#       [nshowers] showers in the energy bin [energies:energies2], 
#       with a **differential** spectral index [Spectral_Index].
#       The range in angles are given in @Theta and @Phi

$Spectral_Index = -1.5;         # spectral index ( **differential** )
@energies = qw ( 1 );           # lower limits in energy bins
@energies2 = qw ( 30000 );      # corresponding upper limits
@nshowers = qw ( 1000 );           # number of showers to generate
@primaries = ( 1 );             # primaries (GEANT codes)
@Theta = qw ( 5. 25. );         # Theta range
@Phi = qw ( 0. 360. );          # Phi range

$MINN = {};                     # minimum number of showers for energy

# currently we use the MACH factor to see how loaded is
# the machine. It goes from 0 to 1, 0 is dead, 1 is free

$MACH_CRITICAL = 0.0;       # critical 
$MACH_BAD      = 0.2;       # bad
$MACH_GOOD     = 0.4;       # good
$MACH_VERYGOOD = 0.9;       # verygood

# maximum disk and tape space allowed
$MAXDISK = (10 * 1024 * 1024);  # max. disk
$MAXTAPE = (6 * 1024 * 1024);   # max. space
    
# some flags (some of them not yet used)
$verbose = 1;           # verbose output flag
$debug = 0;         # debugging flag
$minfree = 1;           # minimum number of free jobs
$uptime = 0;
$cpumsg = '';

###
# subroutines
###

# presentation
sub hello {
    print <<"_eom_" if ( $verbose );
============================================================
$PROGRAM version $VERSION
$COPYRIGHT
============================================================

_eom_
}

# initialize
sub init {
    my (@line,$df,$e,$p,$msg,$l);

    for ($l=0; $l<=$#energies; $l++) {
        $$MINN{$energies[$l]} = $nshowers[$l];
    }
    
    $msg = "";
    if (! -f "$SYSDIR/last-primary") {
        system("echo $#primaries > $SYSDIR/last-primary");
        $msg .= "\n\t\tlast-primary file initialized.";
    }
    if (! -f "$SYSDIR/last-energy") {
        system("echo $#energies > $SYSDIR/last-energy");
        $msg .= "\n\t\tlast-energy file initialized.";
    }                           
    if (! -f "$SYSDIR/disk-space") {
        system("echo $MAXDISK > $SYSDIR/disk-space") == 0
            or croak "Cannot write $SYSDIR/disk-space";
        $msg .= "\n\t\tdisk-space file initialized.";
    }
    if (! -f "$SYSDIR/tape-space") {
        system("echo $MAXTAPE > $SYSDIR/tape-space") == 0
            or croak "Cannot write $SYSDIR/tape-space";
        $msg .= "\n\t\ttape-space file initialized.";
        $msg .= "\n\t\tA new tape for disk $DISK is required.";
    }
    if (! -f "$SYSDIR/last-sets") {
        open(FILESETS,"> $SYSDIR/last-sets") 
            or croak "Cannot open file $SYSDIR/last-sets";
        foreach $p (@primaries) {
            foreach $e (@energies) {
                $l = $p . "-" . $e . "  0\n";
                print FILESETS $l;
            }
        } 
        close(FILESETS);
        $msg .= "\n\t\tlast-sets file initialized.";
    }       
    send_mail("System Initialization Procedure:" . $msg, 0) 
        if ($msg ne "");
}

# clear the system
sub sys_clear {
    exit system('yes | rm -r ' . 
                $SYSDIR . '/last-* ' . 
                $SYSDIR . '/*space ' . 
                $SYSDIR . '/to-save ');
}

# get options for the program
sub get_options {
    my ($op);
    foreach $op ( @ARGV ) { 
        shift;
        last if ($op =~ /^--$/);
        if ($op =~ /^-q/) { $verbose = 0 }
        if ($op =~ /^-D/) { $debug = 1 }
        if ($op =~ /^-u/) { $uptime = 1 }
        if ($op =~ /^-n(.*)/) { $NICE = $1 }
        if ($op =~ /^-t(.*):(.*)/) { @Theta = ( $1, $2, );}
        if ($op =~ /^-p(.*):(.*)/) { @Phi = ( $1, $2, );}
        if ($op =~ /^-c/) { &sys_clear }
    }
    1;
}

# get machine load
sub read_cpu_load {
    my ($line, @upline, $users, @mach);

    $line = `uptime -m`;
    @mach  = split (
                    /^.* factor: ([0-9\.]*), ([0-9\.]*), ([0-9\.]*)$/,
                    $line);
    return $mach[1];
}

# check the cpu load
sub check_cpu_load {
    my ($cpu, $msg, $running, $l);

    $cpu = read_cpu_load;

    if ( $cpu < $MACH_CRITICAL ) {
        $cpumsg = "CPU is critically overloaded:  MACH = $cpu";
        $icpu = 0;
    } elsif ( $cpu < $MACH_BAD ) {
        $cpumsg = "CPU load is too high:  MACH = $cpu";
        $icpu = 1;
    } elsif ( $cpu < $MACH_GOOD ) {
        $cpumsg = "CPU load is not good enough:  MACH = $cpu";
        $icpu = 2;
    } elsif ( $cpu < $MACH_VERYGOOD ) {
        $cpumsg = "CPU load is very good:  MACH = $cpu";
        $icpu = 3;
    } else {
        $cpumsg = "CPU load is excellent!:  MACH = $cpu";
        $icpu = 4;
    }
	
	open(C520RUNNING, "ps x|") 
		or die "Cannot execute ps x: $!"; 	
	$running = 0;
	while ( $l = <C520RUNNING> ) {
		if ( $l =~ /c520/ ) {
			$running++;
		}
	}	
	close(C520RUNNING);
	
	if ( $running > 0 ) {
		$cpumsg .= "\nCORSIKA still running $running time(s):\n$l";
        $icpu = 0;
	}		
	
	$icpu;
}

# get primary to be used
sub get_primary {
    my ($prim);

    $prim = `cat $SYSDIR/last-primary`
        or croak "Cannot read data file $SYSDIR/last-primary"; 

    $prim = ($prim == $#primaries ) ? 0 : $prim+1;
    
    system("echo $prim > $SYSDIR/last-primary") == 0
        or croak "Cannot write data file $SYSDIR/last-primary";

    $primaries[$prim];
}

# get primary to be used
sub get_energy {
    my ($prim) = @_;
    my ($ener);

    $ener = `cat $SYSDIR/last-energy`
        or croak "Cannot read data file $SYSDIR/last-energy"; 
    
    if ($prim == $primaries[0]) {
        $ener = ($ener == $#energies ) ? 0 : $ener+1;
        system("echo $ener > $SYSDIR/last-energy") == 0
            or croak "Cannot write data file $SYSDIR/last-energy";
    }

    return ( $energies[$ener], $energies2[$ener],);
}

# get set (run) number for this run
sub get_set {
    my ($energy, $primary) = @_;
    my (@line, $lastset, $l);

    open(FILESETS,"< $SYSDIR/last-sets") 
        or croak "Cannot open file $SYSDIR/last-sets";
    open(FILESETS2,"> $SYSDIR/last-sets.bak") 
        or croak "Cannot open file $SYSDIR/last-sets.bak";
    while ($l = <FILESETS>) {
        if ($l =~ /^($primary-$energy) /) {
            chomp $l;
            @line = split ' ', $l;
            $lastset = $line[1];
            $lastset++;
            $l = "$primary-$energy     $lastset\n";
        }
        print FILESETS2 $l;
    }
    close(FILESETS2);
    close(FILESETS);
    system("mv $SYSDIR/last-sets.bak $SYSDIR/last-sets") == 0
        or croak "Cannot modify file $SYSDIR/last-sets";
    
    $lastset;
}

# send an e-mail
sub send_mail {
    my ($msg,$err)=@_;
    my ($date,$fullmsg,$time);

    $date = scalar localtime;
    $time = time;

    $fullmsg = "Subject: $PROGRAM $VERSION - log\n";
    $fullmsg .= "=" x 60 . "\n";
    $fullmsg .= "$PROGRAM version $VERSION\n";
    $fullmsg .= "$COPYRIGHT\n";
    $fullmsg .= "=" x 60 . "\n\n";
    $fullmsg .= "  User:   $USER\n";
    $fullmsg .= "  Date:   $date ($time)\n\n";
    $fullmsg .= "  Msg:    $msg\n\n";
    open(MSGFILE, "> $SYSDIR/last-msg") 
        or croak "Cannot write data file $SYSDIR/last-msg";
    print MSGFILE $fullmsg;
    close(MSGFILE);
    open(OVERLOAD, ">> $SYSDIR/last-logs")
        or croak "Cannot write data file $SYSDIR/last-logs";
    if ($err < 1) {
        system("mail $USER < $SYSDIR/last-msg") == 0
            or croak "Cannot send e-mail file $SYSDIR/last-msg";
        print OVERLOAD "+$date : $cpumsg\n";
    } else {
        print OVERLOAD " $date : $cpumsg\n";
    }
    close(OVERLOAD);
}

# construct the job file
sub make_job {
    my ($job,$nrun,$date,$dir,$fulldir,$nshow);
    my ($seed1,$seed2,$seed3);
    
    $Primary = get_primary();
    ($Energy1,$Energy2) = get_energy($Primary);
    $Set = get_set($Energy1, $Primary);
    $nshow = $$MINN{$Energy1};
    $dir = "mc$TEL-$Primary-$Energy1:$Energy2-$Set";
    $fulldir = "$DISK/$dir";
    $job = "job.cmds";

    # change seed for random numbers
    srand ( time() ^ ($$ + ($$ << 15)) );
    $seed1 = int(rand 100000) + 1;
    $seed2 = int(rand 100000) + 1;
    $seed3 = int(rand 100000) + 1;

### make job commands file

    open(JOBFILE,"> $SYSDIR/$job") 
        or croak "Cannot write job file $SYSDIR/job";
    $date = scalar localtime;

    print JOBFILE <<"_eoj_";
#!/bin/sh
#############################################################
# JOBS script
# Automaticaly generated by $PROGRAM v. $VERSION 
#
# $COPYRIGHT
# $date
#############################################################
#

# begin

# initialize variables 

MAIN_DIR="$MOTHERDIR"
TARGET_DIR="$fulldir"
DATA_FILES="ATM75 ATM80 ATM84 ATM85 ATM86 ATM87 ATM88 ATM89 ATM90"
DATA_FILES="\$DATA_FILES EGSDAT2 NUCNUCCS VENUSDAT"
PROG="$EXECCOR"
INPUT="input"
OUTPUT="output"
ERROR="error"

# create target directory
mkdir \$TARGET_DIR || \
{ echo "Cannot create directory \$TARGET_DIR"; exit 1; }
cd \$TARGET_DIR || \
{ echo "Cannot move to directory \$TARGET_DIR"; exit 1; }

# make symbolic links
for i in \$DATA_FILES; do
  ln -s \$MAIN_DIR/\$i \$TARGET_DIR/. || \
    { echo "Cannot create symbolic link"; exit 1; }
done

# copy this file to the target directory
cp $SYSDIR/$job $SYSDIR/\$INPUT \$TARGET_DIR/.

# before it starts
datebef=`date`
timebef=`times`

#------------------------------------------------------------
# execute CORSIKA
nice -n $NICE \$MAIN_DIR/\$PROG < \$INPUT 1> \$OUTPUT 2> \$ERROR
# \$MAIN_DIR/\$PROG < \$INPUT 1> \$OUTPUT 2> \$ERROR 
#------------------------------------------------------------

# after it finishes
dateaft=`date`
timeaft=`times`
dspace=`du -sk \$TARGET_DIR | cut -f 1`

# now update information in the system
odspace=`cat "$SYSDIR/disk-space"`
otspace=`cat "$SYSDIR/tape-space"`
ndspace=`expr "\$odspace" - "\$dspace"`
ntspace=`expr "\$otspace" - "\$dspace"`
echo \$ndspace > $SYSDIR/disk-space
echo \$ntspace > $SYSDIR/tape-space

# save the directory name in the table to be saved to tape
echo \$TARGET_DIR \$dspace >> $SYSDIR/to-save

# build report

cat << EOM > $SYSDIR/last-mail
Subject: $PROGRAM v $VERSION - END OF JOB
============================================================
$PROGRAM version $VERSION
$COPYRIGHT
============================================================

  User:   $USER
  Date:   \$dateaft

  Msg:    The job has finished
          Follows statistics of the job:

  Running with nice: $NICE
           CPU load: $cpumsg
   Command line was: "\$MAIN_DIR/\$PROG < \$INPUT"
   Target directory: \$TARGET_DIR
    Used disk space: \$dspace
   Start date(time): \$datebef ( \$timebef )
     End date(time): \$dateaft ( \$timeaft )
  Disk space avail.: \$ndspace
  Tape space avail.: \$ntspace

EOM

echo '-- List of directories to save --------' >> $SYSDIR/last-mail 
cat $SYSDIR/to-save >> $SYSDIR/last-mail 
echo '-- EOF --------------------------------' >> $SYSDIR/last-mail 

echo '' >> $SYSDIR/last-mail 

echo '-- Input file used --------------------' >> $SYSDIR/last-mail 
cat \$INPUT >> $SYSDIR/last-mail 
echo '-- EOF --------------------------------' >> $SYSDIR/last-mail 

echo '' >> $SYSDIR/last-mail 

echo '-- Job file used ----------------------' >> $SYSDIR/last-mail 
cat job.cmds >> $SYSDIR/last-mail
echo '-- EOF --------------------------------' >> $SYSDIR/last-mail 

# send the report 

mail $USER < $SYSDIR/last-mail

# bye

exit 0
 
_eoj_

    close(JOBFILE);
    chmod(0755, "$SYSDIR/job.cmds"); # make it executable

###  make input file

    open(INFILE, "> $SYSDIR/input") 
    or croak "Cannot write input file $SYSDIR/input";
    print INFILE <<"_eoj_";
RUNNR   $Set                           number of run
EVTNR   1                              number of first shower event
NSHOW   $nshow                         number of showers to generate
PRMPAR  $Primary                       particle type of prim. particle
ESLOPE  0.0                            slope of primary energy spectrum
ERANGE  $Energy1    $Energy2             energy range of primary particle
THETAP  $Theta[0]  $Theta[1]               range of zenith angle (degree)
PHIP    $Phi[0]  $Phi[1]                   range of azimuth angle (degree)
SEED    $seed1   $SITE   0                seed for 1. random number sequence
SEED    $seed2   $SITE   0                seed for 2. random number sequence
SEED    $seed3   $SITE   0                seed for 3. random number sequence
OBSLEV  2200.E2                        observation level (in cm)
ELMFLG  F   F                          em. interaction flags (NKG,EGS)
RADNKG  200.E2                         outer radius for NKG lat.dens.determ.
ARRANG  0.                             rotation of array to north
FIXHEI  0.  0                          first interaction height & target
FIXCHI  0.                             starting altitude (g/cm**2)
MAGNET  20.0  42.8                     magnetic field centr. europe
HADFLG  0  0  0  0  0  0               flags for hadr. interaction
GHEISH  T                              use gheisha for low energy hadrons
VENUS   T                              use venus for high energy hadrons
VENSIG  T                              use VENUS hadronic cross sections
ECUTS   0.3  0.3  0.02  0.02         e.cuts: had, mu, elec y fot
MUADDI  F                              additional info for muons
MUMULT  T                              muon multiple scattering angle
LONGI   T  10.  T                      longit.distr. & step size & fit
MAXPRT  0                              max. number of printed events
ECTMAP  1.E4                           cut on gamma factor for printout
STEPFC  10.0                           mult. scattering step length fact.
DEBUG   F  6  F  1000000               debug flag and log.unit for out
VENDBG  0                              venus debug option
DIRECT  ./                   
CWAVLG  290.  600.                     Cherenkov wavelength band
CSCAT   1  0.  35000.                  scatter Cherenkov events
CERSIZ  1.                             bunch size Cherenkov photons
CERFIL  T                              Cherenkov output to extra file
CERTEL  1
        0. 0. 0. 0. 0. 1800. 1700.     Location and size of each CT
EXIT                                   terminates input
_eoj_
    close(INFILE);
}

sub bye {
    print "\nbye.\n\n";
}



###
#  main procedure
###

# get command line options
init;

# get command line options
get_options;

# say hello
hello;



