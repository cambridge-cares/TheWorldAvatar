#!/bin/bash
# cleaning target directories
echo "Cleaning the old data in output_files"
rm -r ./output_files/*
echo "Cleaning the old data in csv_files"
rm -r ./csv_files/*

# converting the large xml file to xml chunks using the python tool
echo "Converting the large XML to XML chunks:"
for file in ./input_files/*; do
    FILE_NAME=${file##*/}
    python3 filesplitter.py $FILE_NAME

done


# start converting the xml chunks to csvs using the xslt tempelates
if [[ "$?" -ne 1 ]]; then
echo "Converting xml chunks to csv chunks:"
for file in ./output_files/*; do
    FILE_NAME=${file##*/}
    echo "converting $FILE_NAME to $FILE_NAME.csv"
    xsltproc xml2csv.xslt $file > ./csv_files/"$FILE_NAME.csv"
    echo "done converting $FILE_NAME to $FILE_NAME.csv"
    echo "deleting $FILE_NAME"
    rm $file
done
fi
