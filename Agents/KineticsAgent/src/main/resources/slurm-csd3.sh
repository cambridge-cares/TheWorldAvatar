#!/bin/bash
echo "This is a wrapper script for Slurm submission of the"
echo "Kinetics & SRM Engine Suite regression tests."
echo

# Store command-line arguments for passing to Slurm script
args=$@

# Scan command-line arguments
while [[ $# > 0 ]]
do
key="$1"
case $key in
    --help)
    echo "Usage:"
    echo "  -c, --cases <CASEFILE>       <CASEFILE> needs to contain a list of available"
    echo "                               cases. The script defaults to using the 'cases'"
    echo "                               file if the argument is omitted."
    echo "  -t, --time hh:mm:ss          Estimated total wall-time."
    echo "                               Warning: Underestimate the run-time and your"
    echo "                               job will be killed pre-maturely..."
    echo "  -n, --ntasks <number>        Total number of tasks to run. Maximal values:"
    echo "                               20 per CARES node"
    echo "                               16 per Darwin node"
    echo "  -w, --nodelist <namelist>    List of node names to run the job on:"
    echo "                               cpu-g-[1-20] are the CARES nodes"
    echo "                               cpu-g-[21-36] are Darwin nodes"
    echo "                               (type sinfo to see currently idle nodes)"
    echo "  -d, --debug                  Test only debug versions of the code."
    echo "  -r, --release                Test only release versions of the code."
    echo "      --help                   Display this help and exit."
    echo
    exit
    ;;
    -t)         WALLT=$2; shift;;
    --time)     WALLT=$2; shift;;
    -n)         NTASKS=$2; shift;;
    --ntasks)   NTASKS=$2; shift;;
    -w)         NODELIST=$2; shift;;
    --nodelist) NODELIST=$2; shift;;
    *)
    # otherwise do nothing
    ;;
esac
shift # past argument
done

usremailadr=$(git config user.email)

echo "Notification emails will be sent to: $usremailadr"
echo "(NB Edit your git config in order to change this.)"
echo

usrworkdir=$(pwd)

echo "Submitting job to Slurm..."

sbatch --mail-user=$usremailadr --chdir=$usrworkdir --job-name="SRM Regression Tests" --time=$WALLT --nodelist=$NODELIST --ntasks=$NTASKS ./scripts/regression_tests_slurm_como.sh $args

echo "Type \"squeue --jobs=<JOBID>\" or \"squeue -u $USER\" to watch it."
echo
echo "Done."
echo
