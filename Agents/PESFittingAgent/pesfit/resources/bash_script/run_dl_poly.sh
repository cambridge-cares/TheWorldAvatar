#!/bin/bash

for state in 1 2
do
	cp scan/*.xyz state$state
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

	#MODIFY FIELD FILE - vdw
	
	FILE=scan/TABLE
			
	if [ -f "$FILE" ]; then
		
		cp state$state/FIELD state$state/FIELD.old
		cp scan/TABLE state$state/TABLE
		
		cd state$state
		filename='scan_1.xyz'
		N=`awk 'END { print NR }' $filename`
		awk 'NR>=1 && NR<=6 { print $0 }' FIELD.old > FIELD.1
		awk -v c=`expr $N + 4` 'NR>=7 && NR<=c { print $0}' FIELD.old > FIELD.2
		awk -v c=`expr $N + 5` 'NR>=c { print $0}' FIELD.old > FIELD.3

		for ((i=1+2; i<=N; i++))
		do	
			atom=`awk -v c=$i 'NR==c { print $1 }' $filename`
			k=`expr $i - 2`
			p1=`awk -v c=$k 'NR==c { print $2 }' FIELD.2`
			p2=`awk -v c=$k 'NR==c { print $3 }' FIELD.2`
			p3=`awk -v c=$k 'NR==c { print $4 }' FIELD.2`
			p4=`awk -v c=$k 'NR==c { print $5 }' FIELD.2`
			echo "$atom $p1 $p2 $p3 $p4" >> FIELD.5
		done

		N=`grep -n "finish" FIELD.3 | awk -F  ":" '{print $1}'`
		awk -v c=`expr $N` 'NR<=c { print $0}' FIELD.3 > FIELD.4
		echo "vdw 3" >> FIELD.4
		echo "C         C         tab" >> FIELD.4
		echo "H         H         tab" >> FIELD.4
		echo "C         H         tab" >> FIELD.4
		echo "close" >> FIELD.4
		
		cat FIELD.1 FIELD.5 FIELD.4 > FIELD
		rm FIELD.*
		
		cd .. 
	fi
	
	#CREATE CONFIG FILES
	
	cp dl_field/output$state/dl_poly.CONFIG state$state/CONFIG.old
	cd state$state
	awk 'NR>=1 && NR<=5 { print $0 }' CONFIG.old > CONFIG.1
	
	files=( *.xyz )
	Nscan=${#files[@]};
	echo "E$state" >> E$state.txt
	for((j=1; j<=Nscan; j++))
	do
		k=1
		kk=6
		filename='scan_'$j'.xyz'
		N=`awk 'END { print NR }' $filename`
		for ((i=1+2; i<=N; i++))
		do	
			FILE=TABLE
			if [ -f "$FILE" ]; then	
				atom=`awk -v c=$i 'NR==c { print $1 }' $filename`
				x=`awk -v c=$i 'NR==c { print $2 }' $filename`
				y=`awk -v c=$i 'NR==c { print $3 }' $filename`
				z=`awk -v c=$i 'NR==c { print $4 }' $filename`
				echo "$atom $k" >> CONFIG.2
				echo "$x $y $z" >> CONFIG.2
				k=`expr $k + 1`
			else
				x=`awk -v c=$i 'NR==c { print $2 }' $filename`
				y=`awk -v c=$i 'NR==c { print $3 }' $filename`
				z=`awk -v c=$i 'NR==c { print $4 }' $filename`
				sed -n "${kk}p" CONFIG.old >> CONFIG.2
				echo "$x $y $z" >> CONFIG.2
				kk=`expr $kk + 2`
			fi
		done
		cat CONFIG.1 CONFIG.2 > CONFIG
		rm CONFIG.2
		mkdir $j
		mv CONFIG $j
		cp CONTROL DLPOLY.Z FIELD $j
		FILE=TABLE
		if [ -f "$FILE" ]; then
			cp TABLE $j
		fi
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
	




