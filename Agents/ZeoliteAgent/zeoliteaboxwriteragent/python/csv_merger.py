"""
Combine several csvs.

TODO
1) ??? Verification that new classes are not defined twice. 
   Though not strictly necessary. This is alreaty done in the abox_creator.
2) 

"""

import os
import argparse
import logging
logging.basicConfig( level = logging.INFO )

def read_command_line():
    inDir = ""
    pathOut = ""

    inDir = os.path.join( "ontozeolite", "zeocsv" )
    pathOut = "all.csv"
    pathOut = os.path.join( inDir, pathOut )
    logging.info( pathOut )

    return inDir, pathOut
    pass # read_command_line()

def get_input_files( inDir, pathOut ):

    if not os.path.isdir( inDir ):
        logging.error( "Input dir in not a directory" )
    files = os.listdir( inDir )
    logging.info( str(files) )

    pathsIn = []
    for f in files:
        d, _e = os.path.splitext( f )
        if ".csv" == _e.lower():
            pathsIn.append( os.path.join( inDir, f ) )

    logging.info( str(pathsIn ) )
    return pathsIn
    pass # get_input_files()

def merge_files( pathsIn, pathOut ):
    fOut = open( pathOut, "w" )
    pathOut = os.path.abspath( pathOut )

    for count,f in enumerate( pathsIn ):
        if not os.path.isfile( f ):
            logging.error( "Failed to open file '" + str(f) + "'." ) 
            continue

        if os.path.abspath( f ) == pathOut:
            # Don't include the output file to the list of inputs
            continue

        with open(f) as fIn:
            for line in fIn:
                words = line.split( "," )
                # Skip the header line of all except the first file:
                #if count > 0 and line.lower().startswith("source,type,target,relation") :
                if count > 0 and "source"   == words[0].lower() \
                             and "type"     == words[1].lower() \
                             and "target"   == words[2].lower() \
                             and "relation" == words[3].lower(): 
                    continue
                if count > 0 and "ontology" == words[1].lower():
                    continue
                fOut.write( line )

    fOut.close()


if __name__ == "__main__":

    # Find all file paths to be merged (input)
    inDir, pathOut = read_command_line()

    pathsIn = get_input_files( inDir, pathOut )

    # Find the path of the output file (it should not be in the list of input)

    # merge
    merge_files( pathsIn, pathOut )

