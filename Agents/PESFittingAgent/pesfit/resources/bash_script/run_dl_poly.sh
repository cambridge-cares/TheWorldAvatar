#!/bin/bash

for state in 1 2
do
	#MODIFY FIELD FILE - charges
	
	FILE=scan/charges_state$state.txt
			
	if [ -f "$FILE" ]; then
		
		cp dl_field/output$state/dl_poly.FIELD state$state/FIELD.old
		cp scan/charges_state$state.txt state$state/charges_state$state.txt
		
		cd state$state
		N=`awk 'END { print NR }' charges_state$state.txt`
		awk 'NR>=1 && NR<=6 { print $0 }' FIELD.old > FIELD.1
		awk -v c=`expr $N + 6` 'NR>=7 && NR<=c { print $0}' FIELD.old > FIELD.2
		awk -v c=`expr $N + 7` 'NR>=c { print $0}' FIELD.old > FIELD.3
		awk 'NR==FNR{a[NR]=$0;next}{$3=a[FNR]}1' charges_state$state.txt FIELD.2 > FIELD.4
		
		cat FIELD.1 FIELD.4 FIELD.3 > FIELD
		rm FIELD.*
		
		cd .. 
		
	else 
		cp dl_field/output$state/dl_poly.FIELD state$state/FIELD
	fi
	
	#CREATE CONFIG FILES
	
	cp scan/*.xyz state$state
	cp dl_field/output$state/dl_poly.CONFIG state$state/CONFIG.old
	cd state$state
	awk 'NR>=1 && NR<=5 { print $0 }' CONFIG.old > CONFIG.1
	
	files=( *.xyz )
	Nscan=${#files[@]};
	echo "E$state" >> E$state.txt
	for((j=1; j<=Nscan; j++))
	do
		k=6
		filename='scan_'$j'.xyz'
		N=`awk 'END { print NR }' $filename`
		for ((i=1+2; i<=N; i++))
		do	
			x=`awk -v c=$i 'NR==c { print $2 }' $filename`
			y=`awk -v c=$i 'NR==c { print $3 }' $filename`
			z=`awk -v c=$i 'NR==c { print $4 }' $filename`
			sed -n "${k}p" CONFIG.old >> CONFIG.2
			echo "$x $y $z" >> CONFIG.2
			k=`expr $k + 2`
		done
		cat CONFIG.1 CONFIG.2 > CONFIG
		rm CONFIG.2
		mkdir $j
		mv CONFIG $j
		cp CONTROL DLPOLY.Z FIELD $j
		cd $j
		chmod 777 DLPOLY.Z
		./DLPOLY.Z
		N_out=`awk '/         step     eng_tot    temp_tot     eng_cfg     eng_src     eng_cou     eng_bnd     eng_ang     eng_dih     eng_tet/{ print NR; exit }' OUTPUT`
		awk -v c=`expr $N_out + 4` 'NR==c { print $4 }' OUTPUT >> ../E$state.txt
		cd ..
	done
	
	rm *.xyz CONFIG.*
	
	cd ..
done
	




