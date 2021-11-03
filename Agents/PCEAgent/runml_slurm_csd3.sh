#!/bin/bash
echo "This is a wrapper script for Slurm submission of the"
echo "ML models on CSD3."
echo

# Store command-line arguments for passing to Slurm script
args=$@

function usage {
    echo "Usage:"
    echo
    echo "./runml_slurm_csd3 (JOB_OPTIONS) [ML_OPTIONS]"
    echo
    echo "JOB_OTPIONS:"
    echo "  -c               : config file path                            "
    echo "                                                                 "
    echo "  -t  (hh:mm:ss)   : estimated total wall-time                   "
    echo "                     warning: Underestimate the run-time and your"
    echo "                     job will be killed pre-maturely...          "
    echo "                                                                 "
    echo "  -n               : job name (optional)                         "
    echo "  -h               : display this help and exit                  "
    echo
    echo "ML_OPTIONS"
    echo "    please see the oscml readme"
    echo
    echo
    echo "Example call:"
    echo
    echo "         this will become the first <configFilePath>"
    echo "         argument passed to the ml code"
    echo "                           |           "
    echo "                           |  this will be appended to the name"
    echo "                           |   of slurm stdout/stderr file        this will be passed as"
    echo "                           |               |                      is to the ml code"
    echo "                      _____|_______     ___|_____               ______|__"
    echo "                     /              \  /         \             /         \ "
    echo " ./runml_slurm_csd3 -c configFilePath -n my_ml_job -t 00:05:00 --trials 20  [other options]"
    echo "                                                   \__________/"
    echo "                                                         |"
    echo "                        this will be used as slurm job time and also converted to"
    echo "                        --timeout 300 argument to the ml code (300s = 5* 60s)"
    echo
    read -n 1 -s -r -p "Press any key to continue"
    exit
}

# for debugging the bash script
function testfunc {
    printf "%s\n" "$@"
}

JOBNAME="ML_run"
# Scan command-line arguments
if [[ $# = 0 ]]
then
   usage
fi

#----------------------------------------------------------------
# Process all arguments and split them into args that will be used for sbatch command
# and args that will be passed to the ml code.
# Note that the
# --timeout     argument to the ml code is removed and replaced by the slurm --time
#               argument, where its value is set as 99% of the slurm job time
# <configFile>  this ml argument is set via -c argument to this bash script
#
i=0
ORIGINAL_ARGS=("$@")
ML_ARGS=()
OTHER_ARGS=()
while [ $i -lt ${#ORIGINAL_ARGS[@]} ]; do
  arg=${ORIGINAL_ARGS[$i]}
  if [ $arg == "-n" ] || [ $arg == "-c" ] || [ $arg == "-t" ] ; then
    OTHER_ARGS+=($arg)
    i=$((i + 1)) # careful!  ((i++)) will kill your script if you use "set -e"
    OTHER_ARGS+=(${ORIGINAL_ARGS[$i]})
  elif [ $arg == "-h" ]; then
    OTHER_ARGS+=($arg)
    i=$((i + 1))
  elif [ $arg == "--timeout" ]; then
    i=$((i + 1))
  else
    ML_ARGS+=($arg)
  fi
  i=$((i + 1)) # careful!  ((i++)) will kill your script if you use "set -e"
done
# for debuging
#echo "ML_ARGS = ${ML_ARGS[@]}; length = ${#ML_ARGS[@]}"
#echo "OTHER_ARGS = ${OTHER_ARGS[@]}; length = ${#OTHER_ARGS[@]}"


#----------------------------------------------------------------
# now process the JOB_OPTIONS arguments
i=0
while [ $i -lt ${#OTHER_ARGS[@]} ];
do
key=${OTHER_ARGS[$i]}
i=$((i + 1))
case $key in
    -h)
     usage;;
    -c)
        CONFIG=${OTHER_ARGS[$i]};
        i=$((i + 1));;
    -t)
        WALLT=${OTHER_ARGS[$i]};
        i=$((i + 1));;
    -n)
        JOBNAME=${OTHER_ARGS[$i]};
        i=$((i + 1));;
    *)
    usage;;
esac
done

#----------------------------------------------------------------
# calculate the --timeout argument
# split time into hh, mm and ss
hms=(${WALLT//:/ })
# calculate total time in ss, or the timeout parameter
WALLTS=$((3600 * ${hms[0]} + 60 * ${hms[1]} + ${hms[2]}))
# make timeout 1% smaller than the total slurm time
WALLTS=$(($WALLTS * 99/100))


echo "The tests will be run on a single node of the skylake partition using 20 cores."
echo

usremailadr=$(git config user.email)

echo "Notification emails will be sent to: $usremailadr"
echo "(NB Edit your git config in order to change this.)"
echo

ml_exec=oscml/driver.py

echo "Submitting job to Slurm..."

STDOUT1=$JOBNAME"_"slurm.%u.%j.%N.stdout.txt
STDERR1=$JOBNAME"_"slurm.%u.%j.%N.errout.txt

sbatch --mail-user=$usremailadr --time=$WALLT --output=$STDOUT1 --error=$STDERR1 ./SLURM_runhpo_csd3.sh $ml_exec $CONFIG --timeout $WALLTS ${ML_ARGS[@]}
# for debugging the bash script
#testfunc --mail-user=$usremailadr --time=$WALLT --output=$STDOUT1 --error=$STDERR1 ./SLURM_runhpo_csd3.sh $ml_exec $CONFIG --timeout $WALLTS ${ML_ARGS[@]}

exit


echo "Type \"squeue --jobs=<JOBID>\" or \"squeue -u $USER\" to watch it."
echo
echo "Done."

