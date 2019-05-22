#!/bin/bash
######################################################################################
# cumulate_full_showers
######################################################################################

#- This script path and name
SCRIPT_PATH="${BASH_SOURCE[0]}";
SCRIPT_NAME=$(basename "${SCRIPT_PATH}")
if [ -h "${SCRIPT_PATH}" ]; then
    while [ -h "${SCRIPT_PATH}" ]; do
        SCRIPT_PATH=$(readlink "${SCRIPT_PATH}")
    done
fi
pushd . > /dev/null
cd $(dirname ${SCRIPT_PATH}) > /dev/null
SCRIPT_PATH=$(pwd)
popd  > /dev/null

#- Script variables
DOCKER_EXE=docker 
DOCKER_CMD=run 
DOCKER_OPTS="-t --rm" 
DOCKER_VOL_IN="-v $(pwd)/io:/c520.io" 
DOCKER_VOL_OUT_TPL="-v @OUT_FOLDER@/@RUNNR@:/c520.data" 
DOCKER_IMG=c520:0.1

OUT_FOLDER=""
TOTAL=0
STEP=0
PRIMARY=1
ENERGY=1000

#-- Other
DATE=$(date +"%Y%m%d%H%M%S")
LOG_FILE=./${DATE}.log

#-- Define functions
usage () {
    echo "Usage:"
    echo "        ${SCRIPT_NAME} -N <total> -n <group> -e <energy> -p <primary> -o <out_folder>"
    echo ""
}

#-- Get options from command line
while getopts :hN:n:e:p:o: OPT; do
    case $OPT in
        h|+h) usage ;;
        o|+o) OUT_FOLDER="$OPTARG" ;;
        N|+N) TOTAL=$OPTARG ;;
        n|+n) STEP=$OPTARG ;;
        e|+e) ENERGY=$OPTARG ;;
        p|+p) PRIMARY=$OPTARG ;;
        *)    usage ; exit 2
    esac
done
shift `expr $OPTIND - 1`
OPTIND=1

if [ -z "$OUT_FOLDER" ]; then
    usage
    echo "Please, specify the output folder"
    exit 3
fi

#-- Loop
run=1
total=0

while [ $total -lt $TOTAL ]; do

    DOCKER_VOL_OUT=$(echo $DOCKER_VOL_OUT_TPL | sed -e "s#@OUT_FOLDER@#$OUT_FOLDER#g" -e "s#@RUNNR@#$run#g")

    DOCKER_CMD_LINE="$DOCKER_EXE $DOCKER_CMD $DOCKER_OPTS "
    DOCKER_CMD_LINE="$DOCKER_CMD_LINE $DOCKER_VOL_IN $DOCKER_VOL_OUT $DOCKER_IMG"

    mkdir -p $OUT_FOLDER/$run

    # Create input file, and set random numbers
    sed -e "s#@RUNNR#$run#g" \
        -e "s#@STEP#$STEP#g" \
        -e "s#@PRIM#$PRIMARY#g" \
        -e "s#@ENERGY#$ENERGY#g" ./io/in.tpl > ./io/in.rnd

    for i in 11 12 21 22 31 32 ; do 
        cat io/in.rnd |sed -e "s#@RND$i#$RANDOM#" > io/out.rnd
        mv io/out.rnd io/in.rnd
    done
    mv io/in.rnd  io/in

    echo $DOCKER_CMD_LINE

    $DOCKER_CMD_LINE | tee ${LOG_FILE}.${run}

    python3 ./process_run.py ${LOG_FILE}.${run} out/${run}/cer*

    #rm -rf ${LOG_FILE}.${run} out/${run}

    run=$(( run + 1 ))
    total=$(( total + $STEP ))

done

mv log_data.pickle log_data.$PRIMARY.$ENERGY.pickle
mv cer_data.pickle cer_data.$PRIMARY.$ENERGY.pickle

ls -l log_data.$PRIMARY.$ENERGY.pickle cer_data.$PRIMARY.$ENERGY.pickle
