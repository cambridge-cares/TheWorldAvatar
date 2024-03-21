"""
Combine several csvs.

TODO
1) ??? Verification that new classes are not defined twice. 
   Though not strictly necessary. This is alreaty done in the abox_creator.
2) 

"""

import os
import sys
import argparse
import logging
logging.basicConfig( level = logging.INFO )

def read_command_line():
    inDir = ""
    pathOut = ""

    if len(sys.argv) == 2:
        #print( "Got command line argv: ", sys.argv )
        inDir = sys.argv[1]
    else:
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
    # logging.info( str(files) )

    pathsIn = []
    for f in files:
        d, _e = os.path.splitext( f )
        if ".csv" == _e.lower():
            pathsIn.append( os.path.join( inDir, f ) )

    logging.info( str(pathsIn ) )
    return pathsIn
    pass # get_input_files()


def is_singleton(wordsIn, singletons):
    #print( "is singleton?", entity )
    #if entity.startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
    #    return True
    line = [x.strip() for x in wordsIn]
    #words = [x.strip() for x in wordsIn]

    for s in singletons:
        if line[0] == s[0] and line[1] == s[1] and line[2] == s[2] and \
           line[3] == s[3] and line[4] == s[4] and line[5] == s[5]:
            return True

    if "source" == line[0].lower() and "type"      == line[1].lower() and \
       "target" == line[2].lower() and "relation"  == line[3].lower() and \
       "value"  == line[4].lower() and "data type" == line[5].lower(): 
        singletons.append( line )
        return True
 
    if line[0].startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ) and \
       line[1] == "Instance" and \
       line[2].startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
        #print( "In merger found om-2" )
        singletons.append( line )
        #return True

    if line[0].startswith( "rdfs:label" ) and \
       line[1] == "Data Property" and \
       line[2].startswith( "http://www.ontology-of-units-of-measure.org/resource/om-2/" ):
        #print( "In merger found om-2" )
        singletons.append( line )
        #return True

    # Instance for Element
    if line[0].startswith("http://www.theworldavatar.com/kb/ontospecies/Element") and \
       line[1] == "Instance" and \
       line[2].startswith("http://www.daml.org/2003/01/periodictable/PeriodicTable#Element" ):
        #print( "In merger found os:element" )
        singletons.append(line )

    # Instance for ElementSymbol
    if line[0].startswith("http://www.theworldavatar.com/kb/ontospecies/ElementSymbol_") and \
       line[1] == "Instance" and \
       line[2].startswith("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#ElementSymbol" ):
        singletons.append(line )

    # Relation between Element and ElementSymbol
    if line[0].startswith("http://www.theworldavatar.com/kb/ontospecies/Element_") and \
       line[1] == "Instance" and \
       line[2].startswith("http://www.theworldavatar.com/kb/ontospecies/ElementSymbol_") and \
       line[3].startswith("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#hasElementSymbol" ):
        singletons.append( line )

    # Name of the Element symbol
    if line[0].startswith( "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#value" ) and \
       line[1] == "Data Property" and \
       line[2].startswith( "http://www.theworldavatar.com/kb/ontospecies/ElementSymbol_" ) and \
       line[5] == "string":
        singletons.append( line )

    # Instance for Species
    if line[0].startswith( "http://www.theworldavatar.com/kb/ontospecies/Species_" ) and \
       line[1] == "Instance" and \
       line[2].startswith( "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#Species" ):
        singletons.append( line )

    # Name for the Species
    if line[0].startswith( "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#name" ) and \
       line[1] == "Data Property" and \
       line[2].startswith( "http://www.theworldavatar.com/kb/ontospecies/Species_" ):
       #and \
       #line[5] == "string":
        #print(len(line), line[5])
        singletons.append( line )

    # Formula for the Species
    if line[0].startswith( "http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#formula" ) and \
       line[1] == "Data Property" and \
       line[2].startswith( "http://www.theworldavatar.com/kb/ontospecies/Species_" ) and \
       line[5] == "string":
        singletons.append( line )

    return False

def merge_files( pathsIn, pathOut ):
    fOut = open( pathOut, "w", encoding="utf-8" )
    pathOut = os.path.abspath( pathOut )

    singletons = []
    count = 0
    for f in pathsIn:
        if not os.path.isfile( f ):
            logging.error( "Failed to open file '" + str(f) + "'." ) 
            continue

        if os.path.abspath( f ) == pathOut:
            # Don't include the output file to the list of inputs
            continue

        words = os.path.split(f) 
        #print("words =", words)
        if words[-1].lower().strip() == "default.csv":
            #print("aaaaaaaaaa")
            continue

        #print( "fileIn = ", f )
        with open(f, encoding="utf-8") as fIn:
            for il, line in enumerate(fIn):
                #print( "line: ", f, il, line )
                words = line.split( "," )
                # Skip the header line of all except the first file:
                #if count > 0 and line.lower().startswith("source,type,target,relation") :
                #print("count =",  count )
                if len(words) > 1:
                    """
                    if count > 0 and "source"    == words[0].strip().lower() \
                                 and "type"      == words[1].strip().lower() \
                                 and "target"    == words[2].strip().lower() \
                                 and "relation"  == words[3].strip().lower() \
                                 and "value"     == words[4].strip().lower() \
                                 and "data type" == words[5].strip().lower(): 
                        continue
                    """
                    if count > 0 and "ontology" == words[1].lower():
                        continue
                    if is_singleton(words, singletons):
                        if count > 0:
                            continue
                        #if words[0] in singletons:
                        #    continue
                        #else:
                        #    singletons.append( words[0] )
                    #else:
                        #singletons.append( words[0] )

                    fOut.write( line )
                    #if il > 30:
                    #    print(">>>>>>>>>scv_merger break <<<<<<<<<<<<<<<<<<<<<")
                    #    break
            count += 1

    fOut.close()

    #print( "singletons", )
    #for s in singletons:
    #    print("   ", s)


if __name__ == "__main__":

    # Find all file paths to be merged (input)
    inDir, pathOut = read_command_line()

    pathsIn = get_input_files( inDir, pathOut )

    # Find the path of the output file (it should not be in the list of input)

    # merge
    merge_files( pathsIn, pathOut )

