#!/bin/bash

cd scan

filename='scan_data.txt'
s1=`awk 'NR==1 { print $2 }' $filename`
s2=`awk 'NR==2 { print $2 }' $filename`
filename='scan_'$s1'.xyz'
cp $filename ../dl_field/state1.xyz
filename='scan_'$s2'.xyz'
cp $filename ../dl_field/state2.xyz

cd ..

cd dl_field
chmod 777 dl_field

cp dl_f_path1 dl_f_path 
./dl_field
cp dl_field.output output1/dl_field.output

cp dl_f_path2 dl_f_path
./dl_field
cp dl_field.output output2/dl_field.output

rm dl_f_path dl_field.output

cd ..