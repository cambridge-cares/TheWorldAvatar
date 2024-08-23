from os import name
import time as tm
import sys
import re

try:
    # receive the name of the input_file from the arguments
    input_name = sys.argv[1]
except:
    # if the input_file is not provided
    raise Exception("fileSplitter: please provide a file name argument")
    


def read_in_chunks(file_object):
    """Lazy function (generator) to read a file piece by piece.
    Default chunk size: 1k."""
    while True:
        data = file_object.readline()
        if not data:
            break
        yield data


counter = 0
name_counter = 1
compound_count = 50000 # number of compounds added per file
File_name = './data/inputs/pubchem/'+input_name
with open(File_name) as f:
    # if the main xml file is opened properly print the the code is running
    if(f):
        print("fileSplitter: is running")
    # create the outputfile
    output_file_name = './data/inputs/pubchem/output_files/'+input_name+str((name_counter-1)*compound_count)+'-'+str(name_counter*compound_count)+'.xml' 
    output = open(output_file_name,'w',newline='',encoding='UTF8')
    print("fileSplitter: "+output_file_name+" is created")

    for piece in read_in_chunks(f):

        print(str.rstrip(piece), file=output)
        
        if '</PC-Compound>'in piece:
            counter+=1

        if counter >= compound_count-1:
            print('</PC-Compounds>', file=output)
            output.close()
            counter = 0
            name_counter += 1
            output_file_name = './data/inputs/pubchem/output_files/'+input_name+str((name_counter-1)*compound_count)+'-'+str(name_counter*compound_count)+'.xml' 
            output = open(output_file_name,'w',newline='',encoding='UTF8')
            print("fileSplitter: "+output_file_name+" is created")


            string = \
            '''<?xml version="1.0"?>
<PC-Compounds
    xmlns="http://www.ncbi.nlm.nih.gov"
    xmlns:xs="http://www.w3.org/2001/XMLSchema-instance"
    xs:schemaLocation="http://www.ncbi.nlm.nih.gov ftp://ftp.ncbi.nlm.nih.gov/pubchem/specifications/pubchem.xsd"
>
            '''
            print(string, file=output)


print("fileSplitter: done")
