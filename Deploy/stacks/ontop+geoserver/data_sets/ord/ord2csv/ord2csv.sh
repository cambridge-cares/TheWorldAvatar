#!/bin/bash
RESULTS=./results

# cleaning target directories
echo "1.------------------------"
echo "Cleaning the old csv files"
rm -r $RESULTS/*

# running the code
echo "2.------------------------"
echo "Creating the new csv files"
python3 ord2csv.py