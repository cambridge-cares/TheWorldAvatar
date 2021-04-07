#!/bin/bash
#------------------------------------------------------------------------------#
#                    Script to run EPISODE CityChem On CSD3                    #
#                    Author: Feroz Farazi (msff2@cam.ac.uk)                    #
#------------------------------------------------------------------------------#
mv *.zip input.zip
unzip -o input.zip -d input
homeDir=$HOME
echo $homeDir
inputDir=$PWD
#Reads the value of the attribute runWholeScript
runWholeScript=$(cat input.json | jq .runWholeScript)
echo "Running the whole script."
#unzip -o code.zip
#codeDir=$PWD/code
#Copying main code
#cd code/main
#cp csubpr.f90 csubpx.f90 mod_psrc.f90 roldp.f90 rpsrc.f90 tspsrc.f90 ${HOME}/citychem-1.3/src/point
#cp mod_readnc.f90 ${HOME}/citychem-1.3/src/main
#cp ricon.f90 tsgrid.f90 ${HOME}/citychem-1.3/src/grid
#cp wstatm.f90 wstatr.f90 ${HOME}/citychem-1.3/src/stat
#Copying preprocessing code
#cd ${codeDir}/pre-processor/uect
#cp emission_points.for get_user_input.for module_uect_exe.for module_uect_io.for output_citychem_pse.for ${HOME}/citychem-1.3/preproc/uect2.3/src
#cp ${codeDir}/pre-processor/auxiliary/aermap_reader.for ${HOME}/citychem-1.3/preproc/auxiliary/src
#Copying input files N22E113.hgt and N22E114.hgt
# clean space
cd ${HOME}/citychem-1.3
rm OUTPUT/*
rm INPUT/other/*
rm -r INPUT/emis/*
rm -r INPUT/mcwind/*
rm preproc/auxiliary/srtm3/*
rm preproc/auxiliary/output/*
rm preproc/mcwind/input/*
rm preproc/mcwind/other/*
rm preproc/uect2.3/input/*
rm receptor_raster_kang/output/*
rm receptor_raster_kang/input/*
# copy restart files to INPUT/other
cd ${inputDir}
rm -r output*
cd input
cp plume* icmhou* ${HOME}/citychem-1.3/INPUT/other
# finish copying restart files
#cd ${inputDir}/input
#cp *.hgt ${HOME}/citychem-1.3/preproc/auxiliary/srtm3
#Copying the srtm file conversion script
#cp ${inputDir}/input/srtm_generate_hdr.sh ${HOME}/citychem-1.3/preproc/auxiliary
##cd ${HOME}/citychem-1.3/preproc/auxiliary
#chmod +x srtm_generate_hdr.sh
##Changing directory to srtm3
#cd ${HOME}/citychem-1.3/preproc/auxiliary/srtm3
#files=$( ls *.hgt )
#for i in $files ; do
#	echo Next: $i
#	zip $i.zip $i
#	cd ..
#	./srtm_generate_hdr.sh ./srtm3/$i.zip
#	fileName=${i%.*}
#	mv $fileName.* ./srtm3
#	cd srtm3
#done
#cd ${HOME}/citychem-1.3/preproc/auxiliary
##Copying aermap.inp from input to preproc/auxiliary
#cp ${inputDir}/input/aermap.inp ./
#./aermap.exe
#mv *.OUT *.out *.REC TiffDebug* ./output
#cp ./output/AERMAP*.REC ${HOME}/citychem-1.3/INPUT/other
#cp ./output/AERMAP*.REC ${HOME}/citychem-1.3/INPUT/emis
cp ${inputDir}/input/cctapm_meta_PSE.inp ${HOME}/citychem-1.3/preproc/cctapm_meta.inp
#cd ..
#./static4cc.exe
#cp ${HOME}/citychem-1.3/INPUT/emis/*.asc ${HOME}/citychem-1.3/INPUT/other
#cp ${HOME}/citychem-1.3/INPUT/other/*.asc ${HOME}/citychem-1.3/preproc/mcwind/input
#Copying run_file.asc from input
cp ${inputDir}/input/run_file.asc ${HOME}/citychem-1.3/preproc/mcwind
cp ${inputDir}/input/mcwind_input*.txt ${HOME}/citychem-1.3/preproc/mcwind/input
cd ${HOME}/citychem-1.3/preproc/mcwind
./MCWIND.exe
cp ./output/*.* ${HOME}/citychem-1.3/INPUT/mcwind	
cd ${inputDir}
cp input/lines*.csv ${HOME}/citychem-1.3/preproc/uect2.3/input
cp input/points*.csv ${HOME}/citychem-1.3/preproc/uect2.3/input
cp input/cctapm_meta_PSE.inp ${HOME}/citychem-1.3/preproc/cctapm_meta.inp
LD_LIBRARY_PATH=${HOME}/netcdf4/lib
cd ${HOME}/citychem-1.3/preproc
./uect.exe
cd ${inputDir}
cp input/cctapm_meta_LSE.inp ${HOME}/citychem-1.3/preproc/cctapm_meta.inp
cd ${HOME}/citychem-1.3/preproc
./uect.exe
cd ${inputDir}
cp input/receptor_input.txt ${HOME}/citychem-1.3/receptor_raster_kang/input
cd ${HOME}/citychem-1.3/receptor_raster_kang
gfortran receptor_raster.f90
./a.out
cp output/receptor_stations_raster.txt ${HOME}/citychem-1.3/INPUT/other
cp ${inputDir}/input/citychem_restart.txt ${HOME}/citychem-1.3/SIMU 
cd ${HOME}/citychem-1.3/SIMU
./citychem.exe citychem_restart.txt
cd ${HOME}/citychem-1.3/OUTPUT
zip -r output.zip *.*
cp output.zip ${inputDir}
rm -r output.zip
echo "Simulation Completed."
