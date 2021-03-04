#! /bin/csh -f

# for the Users' Guide example
# >    ./installcc.csh city example
# >    ./installcc.csh cityopt example
# >    ./installcc.csh util example
# >    ./installcc.csh uninstall
# >    ./installcc.csh cleandata
#
# for your own boundary conditions
# >    ./installcc.csh city bcon
# >    ./installcc.csh cityopt bcon
# >    ./installcc.csh util bcon


set ACTION       = $1
set EXPERIMENT   = $2

# directories
set uect    = uect2.3
set mcwind  = mcwind
set tapm4cc = tapm4cc2.0
set auxil   = auxiliary
set bconcc  = bconcc2.2

source config.user


echo $ACTION
echo $EXPERIMENT


# do the following if 2nd argument is bcon

if ($EXPERIMENT == "bcon") then

# check if ioapi Makeinclude exist

    #echo ${IOAPIDIR}/ioapi/Makeinclude.${IOAPI}

    set ioapifile = ${IOAPIDIR}/ioapi/Makeinclude.${IOAPI}

    echo ${ioapifile}

    if ( ! -f ${ioapifile} ) then
       echo "File '${ioapifile}' not found."
       echo "please check IOAPI and IOAPIDIR in config.user"
    endif

    echo ${HOMEPATH}

    set tmpfile1 = ${HOMEPATH}/tmpfile1.txt
    set tmpfile2 = ${HOMEPATH}/tmpfile2.txt

    cp ${ioapifile} ${tmpfile1}

    #echo ${tmpfile}

# insert "set " in the temporary Makeinclude file
# if not "#" or space at beginning of line

    sed -i "s/^[^# ].*=/set &/;s/(/{/;s/)/}/g" ${tmpfile1}

    sed '/^set/ s/$/+/g' ${tmpfile1} > ${tmpfile2}

    #sed -i "/^set/ s/=/= \'/;s/+/\'/" ${tmpfile2}

# for set variable with continuation line
# do not add "'" if last sign in line is "\"
#sed -i "/^set/ s/=/= \'/;{/ARCHF/! s/+/\'/}" ${tmpfile2}

    sed -i "/^set/ s/=/= \'/;{/\\/! s/+/\'/}" ${tmpfile2}

    sed -i '/^[^set]/ {/\\/! s/$/*/}' ${tmpfile2}

    sed -i "s/*/\'/;s/+//g" ${tmpfile2}
    #sed -i "s/*/\'/;s/=/ =/g" ${tmpfile2}

    source ${tmpfile2}

    echo $FOPTFLAGS
    echo $OMPFLAGS

    rm -f ${tmpfile1}
    rm -f ${tmpfile2}

# somewhere in the flag variable should be "big_endian"

    #echo $FOPTFLAGS:s|-O3||
    #echo $OMPFLAGS:s|-O3||

    set finds = 'big_endian'  # replace by big_endian
    set ncount = 0

    foreach x ($FOPTFLAGS)
        #echo $x
        if ( "${x}" == ${finds} ) then
          echo $x
          set ncount = 1
          break
        endif
     end

    foreach y ($OMPFLAGS)
        #echo $y
        if ( "${y}" == ${finds} ) then
          echo $y
          set ncount = 1
          break
        endif
     end

    echo $ncount


# if big-endian was there, then add it to F90FLAGS and F90OPTFLAGS

    if ( $ncount == 1 ) then
      echo "F90: Append big-endian flag"
      set F90FLAGS = "$F90FLAGS -convert big_endian" 
      set F90OPTFLAGS = "$F90OPTFLAGS -convert big_endian" 
    endif

    #echo $F90FLAGS
    #echo $F90OPTFLAGS


endif



switch ($ACTION)


#################################################
# build CityChem
#################################################

case city:

     if ($EXPERIMENT != "example" && $EXPERIMENT != "bcon") then
       echo "second argument has to be 'example' or 'bcon'"
       exit 1
     endif

     echo $F90
     echo $F90FLAGS

     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{NCINC}%${NCINC}%g" \
     <./src/Makefile.mk.tmpl>./src/Makefile.mk


     cd src/
     make -f Makefile.mk cleanall
     make -f Makefile.mk

     cd ../SIMU/
     ln -sf ../bin/citychem.exe


  breaksw


#################################################
# build CityChem with optimization
#################################################

case cityopt:

     if ($EXPERIMENT != "example" && $EXPERIMENT != "bcon") then
       echo "second argument has to be 'example' or 'bcon'"
       exit 1
     endif

     echo $F90
     echo $F90OPTFLAGS

     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90OPTFLAGS}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{NCINC}%${NCINC}%g" \
     <./src/Makefile.mk.tmpl>./src/Makefile.mk


     cd src/
     make -f Makefile.mk cleanall
     make -f Makefile.mk

     cd ../SIMU/
     ln -s ../bin/citychem.exe


  breaksw

#################################################
# build the preprocessing utilities
#################################################

case util:

     if ($EXPERIMENT != "example" && $EXPERIMENT != "bcon") then
       echo "second argument has to be 'example' or 'bcon'"
       exit 1
     endif

     echo $F90
     echo $F90FLAGS

     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/ATDSC3.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/CONST3.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/FDESC3.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/IODECL3.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/NETCDF.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/NOTICE.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/PARMS3.EXT
     dos2unix ${HOME}/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fixed_src/STATE3.EXT
     chmod +x ~/citychem-1.3/preproc/bconcc2.2/ioapi3.2/ioapi/fix_src.csh
     chmod +x ~/citychem-1.3/preproc/auxiliary/srtm_generate_hdr.sh
#> Write the makefiles using the *.tmpl templates
#     sed \
#       -e "s%@{F90}%${F90}%g" \
#       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
#       -e "s%@{NCLIB}%${NCLIB}%g" \
#       -e "s%@{NCINC}%${NCINC}%g" \
#       -e "s%@{HOMEPATH}%${HOMEPATH}%g" \
#       -e "s%@{IOAPI}%${IOAPI}%g" \
#     <./preproc/util/Makefile.tmpl>./preproc/util/Makefile

#> 1 MCWIND
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
     <./preproc/${mcwind}/Makefile.mc.tmpl>./preproc/${mcwind}/Makefile.mc

#> 2 UECT
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{NCINC}%${NCINC}%g" \
     <./preproc/${uect}/Makefile.uect.tmpl>./preproc/${uect}/Makefile.uect

#> 3 TAPM4CC
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{NCINC}%${NCINC}%g" \
     <./preproc/${tapm4cc}/Makefile.tapm.tmpl>./preproc/${tapm4cc}/Makefile.tapm

#> 4 AUXIL
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
     <./preproc/${auxil}/Makefile.aux.tmpl>./preproc/${auxil}/Makefile.aux
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{F90FLAGS}%${F90FLAGS}%g" \
     <./preproc/${auxil}/Makefile.zo.tmpl>./preproc/${auxil}/Makefile.zo
     # AERMAP does only work with gfortran or g95

#> 5 IOAPI
     echo "build ioapi library with gfortran or provide ioapi library for intel fortran"
     echo "in config.user:"
     echo " set IOAPI (name of Makeinclude)"
     echo " set IOAPIDIR (pathname of Makeinclude file)"
     sed \
       -e "s%@{IOAPI}%${IOAPI}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{BASEDIR}%${HOMEPATH}%g" \
       -e "s%@{IOAPIDIR}%${IOAPIDIR}%g" \
     <./preproc/${bconcc}/ioapi3.2/Makefile.ioapi.tmpl>./preproc/${bconcc}/ioapi3.2/Makefile.ioapi

#> 6 CMAQ4CC
     sed \
       -e "s%@{F90}%${F90}%g" \
       -e "s%@{NCLIB}%${NCLIB}%g" \
       -e "s%@{NCINC}%${NCINC}%g" \
       -e "s%@{IOAPI}%${IOAPI}%g" \
       -e "s%@{IOAPIDIR}%${IOAPIDIR}%g" \
     <./preproc/${bconcc}/cmaq4cc/Makefile.cmaq.tmpl>./preproc/${bconcc}/cmaq4cc/Makefile.cmaq


#> Execute the generated makefiles

#> 1 MCWIND
     cd preproc/${mcwind}/
     make -f Makefile.mc clean
     make -f Makefile.mc
     ln -s ./bin/MCWIND.exe

#> 2 UECT
     cd .. &&  cd ./${uect}/
     make -f Makefile.uect cleanall
     make -f Makefile.uect

#> 3 TAPM4CC
     cd .. &&  cd ./${tapm4cc}/
     make -f Makefile.tapm cleanall
     make -f Makefile.tapm

#> 4 AUXIL
     cd .. &&  cd ./${auxil}/
     make -f Makefile.aux cleanall
     make -f Makefile.aux
     make -f Makefile.zo cleanall
     make -f Makefile.zo
     make -f Makefile.aermap cleanall
     make -f Makefile.aermap

#> 5 IOAPI
     cd .. &&  cd ./${bconcc}/
     if (${F90} == "gfortran") then
       cd ./ioapi3.2/
       make -f Makefile.ioapi clean
       make -f Makefile.ioapi all
       make -f Makefile.ioapi m3p
       cd ..
     else
       cd ./ioapi3.2/
       make -f Makefile.ioapi m3p
       cd ..
     endif

#> 6 CMAQ4CC
     cd ./cmaq4cc/
     make -f Makefile.cmaq
     cd ..


#> Create symbolic links to the executables
     sleep 2   #(sleep 2 seconds)
     cd .. &&     pwd
     ln -s ./${mcwind}/bin/MCWIND.exe
     ln -s ./${tapm4cc}/bin/tapm4cc.exe
     ln -s ./${uect}/bin/uect.exe
     ln -s ./${auxil}/bin/static4cc.exe
     ln -s ./${auxil}/bin/z0top4cc.exe
     cd ./${auxil}/
     pwd
     ln -s ./bin/aermap.exe
     cd ..
     cd ./${bconcc}/
     ln -s ./ioapi3.2/bin/m3cpleplus.exe
     ln -s ./cmaq4cc/cmaq4cc.exe


  breaksw

#################################################
# uninstall all programs
#################################################

case uninstall:

#> remove symbolic links
     cd SIMU/
     rm *.exe
     cd .. && cd preproc/
     rm *.exe
     cd ${auxil}/
     rm *.exe
     cd .. && cd ${bconcc}/
     rm *.exe
     cd ../..

#> 1 MCWIND
     cd ./preproc/${mcwind}/
     make -f Makefile.mc clean
#> 2 UECT
     cd .. &&  cd ./${uect}/
     make -f Makefile.uect cleanall
#> 3 TAPM4CC
     cd .. &&  cd ./${tapm4cc}/
     make -f Makefile.tapm cleanall
#> 4 AUXIL
     cd .. &&  cd ./${auxil}/
     make -f Makefile.aux cleanall
     make -f Makefile.zo cleanall
     make -f Makefile.aermap cleanall

#> 5 IOAPI and CMAQ4cc
     cd .. &&  cd ./${bconcc}/ioapi3.2/
     if (${F90} == "gfortran") then
       make -f Makefile.ioapi clean
     endif 
     rm -f ./bin/*.exe
     rm -f ./m3plus/*.o
     rm -f ../cmaq4cc/cmaq4cc.exe
     rm -f ../cmaq4cc/*.o
     rm -f ../cmaq4cc/*.mod
     cd ..

#> 6 CITYCHEM
     cd ../../src/
     make -f Makefile.mk cleanall


  breaksw


#################################################
# remove all output data
#################################################

case cleandata:

#> remove files in INPUT
     cd INPUT/emis
     rm *.txt
     rm *.nc
     cd ../tapm/
     rm *.asc
     rm *.fld
     rm *.nc
     cd ../other/
     rm *.asc
     cd ../mcwind/
     rm *.*
     cd ../..

#> remove files in OUTPUT
     cd OUTPUT/
     rm *
     cd ..

#> remove files in testdata
     cd testdata
     rm *.*
     rmdir -r cmaq/
     cd ncfiles/
     rm *.*
     


  breaksw


#################################################
# invalid parameter and help
#################################################
case *:
    echo "installcc error: no valid option chosen"
    echo "usage for Users' Guide example:"
    echo "to install CityChem"
    echo "  ./installcc.csh city example"
    echo "to install CityChem with optimization"
    echo "  ./installcc.csh cityopt example"
    echo "to install all utilities"
    echo "  ./installcc.csh util  example"
    echo "to remove all installations"
    echo "  ./installcc.csh uninstall"
    echo "to remove all output data"
    echo "  ./installcc.csh cleandata"
    echo ""
    echo "usage for own boundary conditions:"
    echo "to install CityChem"
    echo "  ./installcc.csh city bcon"
    echo "to install CityChem with optimization"
    echo "  ./installcc.csh cityopt bcon"
    echo "to install all utilities"
    echo "  ./installcc.csh util bcon"

endsw
  
exit(0)
