#! /bin/sh

readvalue () {
    dfl="$1"
    msg="$2"
    desc="$3"

    echo ''
    echo $msg
    echo -n "[ $dfl ] ? "
    read value
    value=${value:="$dfl"}
    echo "$desc <- $value"
}

cat <<EOF    

MAGIC.MC.DAEMON Configuration Script
========================================

NOTE:: This script WILL NOT create any directory in
       the case that it does not exist. 
       You MUST have created all the directories for yourself.
       You will need :

        - an [executables directory]: where I will look for 
                    the CORSIKA executables and data files, and
                    for the MMD program.

        - a [data directory]: where I will save the data.

        - an [administration directory]: where some log-files
                    will be created.

       If you DO NOT have yet these directories created, press
       now Ctrl-C, create them, and write afterwards again

       $ make script

Press Return to continue, or Ctrl-C to stop this script.
EOF
read ans

cat <<EOF
+---------------------------------------------------+
| This procedure will ask you about some parameters | 
| of your system. Please, answer all the questions. | 
| If you have any doubt, press Ctrl-C.              | 
+---------------------------------------------------+
EOF

readvalue `pwd` \
    "Directory where the CORSIKA and daemon executables should be located:" path-exe 
pathexe="$value"

readvalue "c520-osf" \
    "Enter the executable name:" name-exe 
nameexe="$value"

readvalue "/data" \
    "Disk/path where the bulk of data will be stored:" data-disk
datadisk="$value"

readvalue "/data" \
    "Disk/path where the administration files will be stored:" adm-disk
admdisk="$value"

echo ''

readvalue "0" \
    "Final NICE value to use when running CORSIKA" niceval
niceval="$value"

readvalue "0" \
    "What is your site seed generator?" siteval
siteval="$value"

readvalue "no" \
    "Do you want to check the system load before running CORSIKA (yes/no)" MACH
MACH="$value"

echo ''

readvalue "$USER@$HOST" \
    "Enter the e-mail address to sent the log-mailing to" email-add
useradd="$value"

echo ''

echo 'Preparing daemon . . .'

cat <<EOF >dmy.sed
s=#USERADD#=$useradd=g
s=#DATADISK#=$datadisk=g
s=#ADMDISK#=$admdisk=g
s=#PATHEXE#=$pathexe=g
s=#NAMEEXE#=$nameexe=g
s=#NICE#=$niceval=g
s=#SITE#=$siteval=g
EOF

sed -f dmy.sed magic-mc.daemon.tpl > mmd


if [ $MACH = "yes" ]; then

    cat <<EOF >> mmd
# get information about system load
if ( check_cpu_load() < 3 ) {
    send_mail( "\n## Top output:\n\n" . \`\$TOP -b\`, 1 );
    exit;
}

# make job
make_job;

# say bye, execute job and leave this program
exec "\$SYSDIR/job.cmds" 
    or croak "Cannot execute job file";
EOF

else

    cat <<EOF >> mmd
# get information about system load
check_cpu_load();

# make job
make_job;

# say bye, execute job and leave this program
exec "\$SYSDIR/job.cmds" 
    or croak "Cannot execute job file";
EOF

fi

chmod 744 ${pathexe}/mmd

echo 'MMD (MAGIC-MC.DAEMON) was generated.'

echo ''

echo 'The MAGIC-MC.DAEMON is designed to run from the crontab.'

readvalue "yes" \
    "Do you want me to change your crontab (yes/no)?" crntb
crntb="$value"

echo ''

echo 'OK, I will change your crontab. I will asume you are using C-shell'

if [ $crntb = "yes" ]; then
    crontab -l > dmy
    echo '0,15,30,45 * * * * '${pathexe}'/mmd -n'${niceval}' -q >& '${admdisk}'/last-crontab-out' >> dmy
    crontab dmy
    rm -f dmy
fi

echo ''
echo 'Preparing your administration directory (do not worry about'
echo 'some messages like "[file]: No such file or directory"'
echo ''

${pathexe}/mmd -c

echo 'Done.'

cat <<EOF

MAGIC-MC.DAEMON is ready to run.

If you did not ask me to modify your crontab,
you must run yourself the program, with the following 
command line:

$ mmd -n$niceval -q >& ${admdisk}/last-crontab-out

However, it's much better if let my do the dirty work
for you.

Good luck.

In case of problems, please contact to <gonzales@hegra1.mppmu.mpg.de>

EOF



