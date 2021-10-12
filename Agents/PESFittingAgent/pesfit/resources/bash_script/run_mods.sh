#!/bin/bash

cp scan/scan.csv mods/Initial
cp state1/E1.txt mods/Initial
cp state2/E2.txt mods/Initial

cd scan
files=( *.xyz )
Nscan=${#files[@]};
filename='scan_data.txt'
s1=`awk 'NR==1 { print $2 }' $filename`
s2=`awk 'NR==2 { print $2 }' $filename`
cd ..

cd mods

cd Initial

filename='MODS_SIM_INITFILE__AIVarInitReadFile.csv'
echo "Case names,A1,A2,A3,A4" >> $filename
for((j=1; j<=Nscan; j++))
do
	echo "CaseGroup1_Case$j,0.0,0.0,0.0,0.0" >> $filename
done

filename='MODS_SIM_INITFILE__cases.csv'
echo "Case names,F1,F2,Eg" >> $filename

s1=`expr $s1 + 1`
s2=`expr $s2 + 1`
E1_min=`awk -v c=$s1 'NR==c {printf "%.2f\n", $1}' E1.txt`
E2_end=`awk -v c=$s2 'NR==c {printf "%.2f\n", $1}' E2.txt`
Eg_end=`awk -F',' -v c=$s2 'NR==c { printf "%.2f\n", $2 }' scan.csv`
Eshift=$(bc -l <<<"${Eg_end}-${E2_end}+${E1_min}")

for((j=1; j<=Nscan; j++))
do
	i=`expr $j + 1`
	E1=`awk -v c=$i 'NR==c {printf "%.2f\n", $1 }' E1.txt`
	E1=$(bc -l <<<"${E1}-${E1_min}")
	E2=`awk -v c=$i 'NR==c { printf "%.2f\n", $1 }' E2.txt`
	E2=$(bc -l <<<"${E2}-${E2_end}+${Eg_end}")
	Eg=`awk -F',' -v c=$i 'NR==c { printf "%.2f\n", $2 }' scan.csv`
	echo "CaseGroup1_Case$j,$E1,$E2,$Eg" >> $filename
done
	
cd ..

cd Working_dir

for((j=1; j<=Nscan; j++))
do
	echo -e "\t\t\t\t<case>CaseGroup1_Case$j</case>" >> CASES
done
row='0'
lb_abs='-1.0'
ub_abs='1.0'
for((j=1; j<Nscan; j++))
do
	row=$(echo "$row;$j")
	lb_abs=$(echo "$lb_abs;-1.0")
	ub_abs=$(echo "$ub_abs;1.0")
done

awk 'NR<=56 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
for((j=1; j<=Nscan; j++))
do
	line1='<case name="CaseGroup1_Case'
	line2='">'
	echo -e "\t\t$line1$j$line2" >> MoDS_inputs.xml
	echo -e "\t\t\t<models>" >> MoDS_inputs.xml
	echo -e "\t\t\t\t<model>FormulaCaseGroup1</model>" >> MoDS_inputs.xml
	echo -e "\t\t\t</models>" >> MoDS_inputs.xml
	echo -e "\t\t</case>" >> MoDS_inputs.xml
done

awk 'NR>=57 && NR<=86 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml

awk 'NR>=87 && NR<=93 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="row">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$row$line2" >> MoDS_inputs.xml
awk 'NR>=95 && NR<=97 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="lb_abs">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$lb_abs$line2" >> MoDS_inputs.xml
line1='<detail name="ub_abs">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$ub_abs$line2" >> MoDS_inputs.xml
awk 'NR>=100 && NR<=106 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml

awk 'NR>=107 && NR<=113 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="row">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$row$line2" >> MoDS_inputs.xml
awk 'NR>=115 && NR<=117 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="lb_abs">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$lb_abs$line2" >> MoDS_inputs.xml
line1='<detail name="ub_abs">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$ub_abs$line2" >> MoDS_inputs.xml
awk 'NR>=120 && NR<=126 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml

awk 'NR>=127 && NR<=146 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml
awk 'NR>=147 && NR<=166 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml
awk 'NR>=167 && NR<=186 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml
awk 'NR>=187 && NR<=206 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
cp MoDS_inputs.xml tmp
cat tmp CASES > MoDS_inputs.xml


awk 'NR>=207 && NR<=213 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="row">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$row$line2" >> MoDS_inputs.xml
awk 'NR>=215 && NR<=217 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml
line1='<detail name="lb_factor">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$ub_abs$line2" >> MoDS_inputs.xml
line1='<detail name="ub_factor">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$ub_abs$line2" >> MoDS_inputs.xml
line1='<detail name="lb_addend">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$lb_abs$line2" >> MoDS_inputs.xml
line1='<detail name="ub_addend">'
line2='</detail>'
echo -e "\t\t\t\t\t\t$line1$ub_abs$line2" >> MoDS_inputs.xml
awk 'NR>=222 && NR<=227 { print $0}' MoDS_inputs_sample.xml >> MoDS_inputs.xml

cd ..

chmod 777 MoDS_mpi
./MoDS_mpi

cd CalibrationAlg

N=`awk 'END { print NR }' CalibrationAlg_Summary.csv`
OFmin=`awk -F',' 'NR==2 { printf "%.2f\n", $4 }' CalibrationAlg_Summary.csv`
for((j=2; j<=N; j++))
do
	OFnew=`awk -F',' -v c=$j 'NR==c { printf "%.2f\n", $4 }' CalibrationAlg_Summary.csv`
	
	if (( $(echo "$OFnew < $OFmin" |bc -l) )); then
		OFmin=$OFnew
		A1=`awk -F',' -v c=$j 'NR==c { printf "%.2f\n", $5 }' CalibrationAlg_Summary.csv`
		A2=`awk -F',' -v c=$j 'NR==c { printf "%.2f\n", $6 }' CalibrationAlg_Summary.csv`
		A3=`awk -F',' -v c=$j 'NR==c { printf "%.2f\n", $7 }' CalibrationAlg_Summary.csv`
		A4=`awk -F',' -v c=$j 'NR==c { printf "%.2f\n", $8 }' CalibrationAlg_Summary.csv`
	fi
	
done

cd ..

echo "# evb settings" >> SETEVB
echo "evbtypemols	1 1" >> SETEVB
echo "evbcoupl 	1 2 gauss $A1 $A2 $A3 $A4" >> SETEVB
echo "evbshift	1 0.0" >> SETEVB
echo "evbshift	2 $Eshift" >> SETEVB

mv SETEVB ../validation

cd ..		




