#!/bin/bash

cd scan
files=( *.xyz )
Nscan=${#files[@]};
cd ..

cd validation

for((j=1; j<=Nscan; j++))
do

	mkdir $j
	cp CONTROL DLPOLY.Z SETEVB $j
	for state in 1 2
	do
		if [ $state -eq 1 ]; then
			cp ../state$state/$j/CONFIG ../state$state/$j/FIELD $j
		else
			cp ../state$state/$j/CONFIG $j/CONFIG$state
			cp ../state$state/$j/FIELD $j/FIELD$state		
		fi
	done
		
	cd $j
	chmod 777 DLPOLY.Z
	./DLPOLY.Z
	N_out=`awk '/         step     eng_tot    temp_tot     eng_cfg     eng_src     eng_cou     eng_bnd     eng_ang     eng_dih     eng_tet/{ print NR; exit }' OUTPUT`
	awk -v c=`expr $N_out + 4` 'NR==c { print $4 }' OUTPUT >> ../E_evb.txt
	cd ..

done
	
cd ..



	
	
