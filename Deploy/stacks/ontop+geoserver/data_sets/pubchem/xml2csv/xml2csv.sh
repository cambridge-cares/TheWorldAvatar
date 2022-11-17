#!/bin/bash
# create directory root
pubchem_dir=./data/inputs/pubchem

# create the required directories
mkdir $pubchem_dir/output_files
mkdir $pubchem_dir/csv_files

# cleaning target directories
echo "1.------------------------"
echo "Cleaning the old csv files"
rm $pubchem_dir/*.csv
rm $pubchem_dir/output_files*
rm $pubchem_dir/csv_files/*

# converting the large xml file to xml chunks using the python tool
echo "2.-------------------------------------"
echo "Converting the large XML to XML chunks:"
for file in $pubchem_dir/*.xml; do
    XML_FILE_NAME=${file##*/}

    python3 fileSplitter.py $XML_FILE_NAME

# start converting the xml chunks to csvs using the xslt tempelates
if [[ "$?" -ne 1 ]]; then
echo "3.----------------------------------"
echo "Converting xml chunks to csv chunks:"
for file in $pubchem_dir/output_files/*; do
    FILE_NAME=${file##*/}
    echo "converting $FILE_NAME to $FILE_NAME.csv"
    xsltproc xml2csv.xslt $file > $pubchem_dir/csv_files/"$FILE_NAME.csv"
    echo "done converting $FILE_NAME to $FILE_NAME.csv"
    echo "deleting $FILE_NAME"
    rm $file
done
fi

# start merging all the csv chunks to a single csv file
if [[ "$?" -ne 1 ]]; then
    echo "4.-------------------------------------"
    echo "merging the csv files into a single one"

    echo "Create the aggregate file $XML_FILE_NAME.csv in output_files"
    touch $pubchem_dir/"${XML_FILE_NAME}.csv"

    firstFile=true
    for csvFile in $pubchem_dir/csv_files/*; do
        echo "current file is: ${csvFile##*/}"
        if $firstFile; then
           tail -n +1 $csvFile >> $pubchem_dir/"${XML_FILE_NAME}.csv"
           firstFile=false
        else
           tail -n +2 $csvFile >> $pubchem_dir/"${XML_FILE_NAME}.csv"
        fi
        echo "deleting the current csv file:"
        rm $csvFile   
        
    done

fi


# end of loop over large xml files
done




# deleting the extra directories
rmdir $pubchem_dir/csv_files
rmdir $pubchem_dir/output_files
