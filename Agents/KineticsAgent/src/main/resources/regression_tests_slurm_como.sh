#!/bin/bash
# This is a Slurm submission script.
# It is being used by a wrapper script in the folder above.
# DO NOT EXECUTE THIS DIRECTLY ON THE COMMAND LINE!
# DO NOT SUBMIT THIS DIRECTLY TO SLURM!

#SBATCH -p como
#SBATCH -A COMO-SL2-CPU
#SBATCH --mem=32000
#SBATCH --output slurm.%u.%j.%N.stdout.txt   # (%u,%j,%N)=(user, job allocation #, node)
#SBATCH --error slurm.%u.%j.%N.errout.txt    #
#SBATCH --mail-type=END,FAIL                 # notifications for job done & fail

#! Number of nodes and tasks per node allocated by SLURM (do not change):
numnodes=$SLURM_JOB_NUM_NODES
mpi_tasks_per_node=$(echo "$SLURM_TASKS_PER_NODE" | sed -e  's/^\([0-9][0-9]*\).*$/\1/')

#! Number of MPI tasks to be started by the application per node and in total (do not change):
np=$[${numnodes}*${mpi_tasks_per_node}]

CMD="mpirun -ppn $mpi_tasks_per_node -np $np "

# SRM post build test script (Linux)
SCRIPT="$(basename $0)"

function casefile_error {
    echo "$SCRIPT: error        - cannot find CASEFILE: $casefile"
}

# Set default values for optional arguments
debug=0
release=0
casefile="cases"

# Parse optional arguments using getopt
# A colon (:) indicates that the option takes an argument
SOPT="c:drt:n:w:"
LOPT="cases:,debug,release,time:,help,ntasks:,nodelist:"
ARGS=`getopt -o $SOPT -l $LOPT -- $* 2> ~/"$SCRIPT"_err`
if [ $? -ne "0" ]; then
    while read LINE
    do
        echo $SCRIPT $LINE | sed s/getopt//g
    done < ~/"$SCRIPT"_err
    echo "Try '"$SCRIPT" --help' for more information."
    exit
fi
set -- $ARGS
for i; do
  case "$i" in
    -c  )           casefile=$(echo $2 | sed s/\'//g); shift; shift;;
    --cases  )      casefile=$(echo $2 | sed s/\'//g); shift; shift;;
    -d  )           debug=1; shift;;
    --debug  )      debug=1; shift;;
    -r  )           release=1; shift;;
    --release  )    release=1; shift;;
    -t  )           shift; shift;;
    --time  )       shift; shift;;
    -n  )           shift; shift;;
    --ntasks  )     shift; shift;;
    -w  )           shift; shift;;
    --nodelist  )   shift; shift;;
    --  )           shift; break ;;
  esac
done
rm -rf ~/"$SCRIPT"_err

# Output arguments
#echo Arguments
#echo "  $ARGS"
#echo "  cases      $casefile"
#echo "  debug      $debug"
#echo "  release    $release"
#exit

# Parse mandatory arguments

# Check argument syntax
if [ ! -f $casefile ]; then
    casefile_error; exit
fi

# Display startup message
echo "Host:         $HOSTNAME"
echo "OS Name:      $(uname) $(uname -r)"
echo
echo Running SRM post build test script...

# Set directory names
tstdir=$(pwd)
script=scripts
exedir=executables
refdir=ref
wrkdir=WorkingDir
pstdir=postbuild
mchdir=../../mechanisms

# Set file prefixes
rltpfx=Output

# Create master case directory
masdir=$(date +%Y_%m_%d)_Job$SLURM_JOBID
if [ $debug -gt 0 ]; then
    # Append debug identifier to master case directory
    masdir=$(echo $masdir)_d
fi
if [ $release -gt 0 ]; then
    # Append release identifier to master case directory
    masdir=$(echo $masdir)_r
fi
rm -rf $masdir
mkdir $masdir

# Set script to check all files by default
if [ $release == 0 ] && [ $debug == 0 ]
then
    debug=1
    release=1
fi

# Set git info
branch=$(git branch | head -n1 | sed s/'* '/''/g)
commit=$(git log | head -n1 | sed s/'commit '/''/g)

# Create case list
i=0
echo
echo Writing case list...
cat $casefile | sed s/'\\'/'\/'/g > temp_cases
cat builds > temp_builds
dos2unix -q temp_cases temp_builds
while read caseid srcdir
do
    # Check that source directory exists
    if [[ ! -z $srcdir ]]; then

        # Read case ID and source directory
        for builds in `cat temp_builds`
        do
            # Look for matching executables
            for exefle in `ls $exedir/$builds | sed s/$exedir'\/'//g`
            do

                # Check whether to run debug/release executables
                useexe=0
                chkfle=$(echo $exefle | sed s/_d[0-9]//g)
                if [ $exefle == $chkfle ]; then
                    # A release executable
                    if [ $release -gt 0 ]; then
                        useexe=1
                    fi
                else
                    # A debug executable
                    if [ $debug -gt 0 ]; then
                        useexe=1
                    fi
                fi

                if [ $useexe -gt 0 ]; then
                    # Set counter
                    i=$((i+1))

                    # Set mechanism converter executable
                    sprfle=`echo $exefle | sed s/srm_driver/mechanism_convert/g`

                    # Set chempp executable
                    pstfle=`echo $exefle | sed s/driver/chempp/g`

                    # Set api executable
                    apifle=`echo $exefle | sed s/driver/apiexa/g`

                    # Set api c executable
                    apicfle=`echo $exefle | sed s/driver/apicexa/g`

                    # Set target directory
                    tardir=$exefle

                    # Write case list
                    echo $i $caseid $tardir $refdir $exefle $pstfle $apifle $apicfle $sprfle $srcdir >> $masdir/postbuild_caselist
                    if [ $i -le "9" ]; then
                        echo "  Case $i:     $exefle   $caseid"
                    else
                        if [ $i -le "99" ]; then
                            echo "  Case $i:    $exefle   $caseid"
                        else
                            echo "  Case $i:   $exefle   $caseid"
                        fi
                    fi
                fi
            done
        done
    fi
done < temp_cases
rm -f temp_cases temp_builds
if [ ! -f $masdir/postbuild_caselist ]; then
    echo "  No cases found."
    exit
else
    numcas=$i
fi

# Run cases
echo
echo Running cases...
for (( i=1; i<=$numcas; i++ ))
do

    # Read case list
    #
    # Use the counting loops over i and j to read the caselist
    # because running the SRM using MPI breaks any loops based
    # based on while read.
    for (( j=1; j<=$i; j++ ))
    do
        read casnum caseid tardir refdir exefle pstfle apifle apicfle sprfle srcdir
    done  < $masdir/postbuild_caselist

    # Set case directory
    # casdir=$masdir/$casnum
    casdir=$masdir/`echo $srcdir | sed s/'.*\/'/''/g`

    # Write case data to standard out
    echo
    echo "  Case:       $caseid"
    echo "  Target:     $tardir"
    echo "  Folder:     $casdir"

    # Create case directories and copy source files
    rm -rf $casdir/$refdir $casdir/$tardir
    mkdir -p $casdir/$refdir $casdir/$tardir/$pstdir

    # Copy source files
    cp -r $srcdir/* $casdir/$refdir
    cp -r $srcdir/* $casdir/$tardir

    # Copy binary mechanism files to executables folder
    # These are required to run driver cases that use internal mechanisms from the executables folder
    cp ../SRMAux*.* $exedir

    # Delete executable and output files
    rm -f $casdir/$tardir/*.exe $casdir/$tardir/$wrkdir/$rltpfx*

    # Re-copy any output files labelled 'Cyc0000' - these are actually inputs
    ls $srcdir/$wrkdir/ > temp_working_files
    grep Output.*Cyc0000.* temp_working_files > temp_restore_input_files
    for file in `cat temp_restore_input_files`
    do
        cp $srcdir/$wrkdir/$file $casdir/$tardir/$wrkdir/
    done
    rm -f temp_working_files temp_restore_input_files

    # Copy unit dictionary
    cp $exedir/Unit*.xml $casdir/$tardir/

    # Create analysis log file
    echo "Analysis of SRM post build test results"  >> $casdir/$tardir/$pstdir/analysis
    echo "  Case:       $caseid"                    >> $casdir/$tardir/$pstdir/analysis
    echo "  Source:     $srcdir"                    >> $casdir/$tardir/$pstdir/analysis
    echo "  Target:     $tardir"                    >> $casdir/$tardir/$pstdir/analysis
    echo "  Folder:     $casdir"                    >> $casdir/$tardir/$pstdir/analysis
    echo                                            >> $casdir/$tardir/$pstdir/analysis
    echo "Version control"                          >> $casdir/$tardir/$pstdir/analysis
    echo "  Git branch: $branch"                    >> $casdir/$tardir/$pstdir/analysis
    echo "  Commit:     $commit"                    >> $casdir/$tardir/$pstdir/analysis
    echo                                            >> $casdir/$tardir/$pstdir/analysis
    echo "Test platform"                            >> $casdir/$tardir/$pstdir/analysis
    echo "  OS Name:    $(uname) $(uname -r)"       >> $casdir/$tardir/$pstdir/analysis
    echo                                            >> $casdir/$tardir/$pstdir/analysis

    # Run pre process script
    if [ -f $casdir/$tardir/preprocess.sh ]; then
        title="Running pre process script..."
        echo 'cd "'$casdir/$tardir'";./"'preprocess.sh'" --mechs "'$tstdir/$mchdir'" --folder "'$tstdir/$exedir'" --converter "'$sprfle'"' > temp_run_srm
		echo 'cd "'$tstdir'"' >> temp_run_srm
        dos2unix -q $casdir/$tardir/preprocess.sh
        chmod u+x temp_run_srm $casdir/$tardir/preprocess.sh
        ./temp_run_srm 1> $casdir/$tardir/$pstdir/srm_log 2>&1
        rm -f temp_run_srm
    fi

    # Run case
    title="Running $exefle..."
    echo 'cd "'$exedir'";'$CMD' "'$exefle'" -w "'$tstdir/$casdir/$tardir/$wrkdir/'"' > temp_run_srm
    echo 'cd "'$tstdir'"' >> temp_run_srm
    chmod u+x temp_run_srm
    ./temp_run_srm 1>> $casdir/$tardir/$pstdir/srm_log 2>&1
    rm -f temp_run_srm

    # Run post process script
    if [ -f $casdir/$tardir/postprocess.sh ]; then
        title="Running post process script..."
        echo 'cd "'$casdir/$tardir'";./"'postprocess.sh'" --folder "'$tstdir/$exedir'" --chempp "'$pstfle'" --apif "'$apifle'" --apic "'$apicfle'" --driver "'$exefle'" --driver_prefix "'$CMD'"' > temp_run_srm
        dos2unix -q $casdir/$tardir/postprocess.sh
        chmod u+x temp_run_srm $casdir/$tardir/postprocess.sh
        ./temp_run_srm 1>> $casdir/$tardir/$pstdir/srm_log 2>&1
        rm -f temp_run_srm
    fi

    # Write reference results files file
    cd $casdir/$refdir/$wrkdir/
    ls -X $rltpfx*.csv >  "$tstdir/$casdir/$tardir/$pstdir/files"
    cd "$tstdir"

    # Write reference results folders file
    echo $tstdir/$casdir/$tardir/$wrkdir >> $casdir/$tardir/$pstdir/folders
    echo $tstdir/$casdir/$refdir/$wrkdir >> $casdir/$tardir/$pstdir/folders

    # Run parse execeutable
    title="Running post build test parse script..."
    echo 'cd "'$casdir/$tardir/$pstdir'";"'$tstdir/$script/runtests_parseresults'"'  > temp_run_parse
    echo 'cd "'$tstdir'";echo;echo See the "parse_log" file to view these results in more detail.' >> temp_run_parse
    chmod u+x temp_run_parse $tstdir/$script/runtests_parseresults
    ./temp_run_parse 1> $casdir/$tardir/$pstdir/parse_log 2>&1
    rm -f temp_run_parse

    # Get parse data
    if [ -f $casdir/$tardir/$pstdir/parse_summary ]; then
        cat $casdir/$tardir/$pstdir/parse_summary >> $casdir/$tardir/$pstdir/analysis
        cat $casdir/$tardir/$pstdir/parse_result
    fi

    # Display analysis
    # cat $casdir/$tardir/$pstdir/analysis

done

# Write final messages to screen
echo
echo
echo "  More information is given in each case folder:"
echo "  $pstdir/analysis    - detailed log of test results."
echo "  $pstdir/diff_macros - shortcuts to difference files that failed the test."
echo
echo

echo "Job diagnostics:"
sacct --job $SLURM_JOBID --format "JobName,Submit,Elapsed,AveCPU,CPUTime,UserCPU,TotalCPU,NodeList,NTasks,AveDiskRead,AveDiskWrite"

echo
echo "SRM post build tests complete."
echo  $(date)
