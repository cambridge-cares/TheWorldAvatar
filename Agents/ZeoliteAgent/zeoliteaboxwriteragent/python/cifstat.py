# This script measures the occurance frequency of various properties in CIF files.
# Also it is possible to check for valid CIF entries.
# Requires files:
# CIF_standard_2.4.5.txt
# cif_non_standard.txt
# in __main__ specify the directories with CIFs.

# FIXME 
"""
Some cif files in ccdc database is made of multiple cif files.
I need to divide it in separate cif files and process individually.

"""

import os
import logging
logging.basicConfig( level = logging.WARNING )
#logging.basicConfig( level = logging.INFO )
import datetime

USE_MULTIPROC = False
#USE_MULTIPROC = True

if USE_MULTIPROC:
    from multiprocessing import Pool

def isEmpty( line ):
    if not isinstance(line,str):
        logging.error( "Input line '" + str(line) + "' is not a string in isEmpty()." )
        return True
    if len( line.strip() ) == 0:
        return True

    # Removing comment from the line
    pos = line.find( "#" )
    if pos >= 0:
        if len( line[:pos].strip() ) == 0:
            return True

    return False
    pass # isEmpty()

def cleanString( line ):
    """
    Remove extra symbols, comments, etc from a stirng. 
    Specific for this python file, so it is not in the common tools.py 
    """
    if not isinstance(line,str):
        logging.error( "Input line '" + str(line) + "' is not a string in cleanString()." )
        return True
 
    pos = line.find( "#" )
    if pos < 0:
        tmp = line.strip()
    else:
        tmp = line[:pos].strip()

    tmp2 = tmp.replace( "<br>", "" ) #.strip()
    return tmp2
    pass # cleanString()

class Entry:
    def __init__( self ):
        self.count = 0
        self.paths = []
        pass 

    pass # class Entry


class CIFStat:
    def __init__(self):
        self.entries = dict() #
        self.nCIF    = 0      # Number of CIF files checked (to measure the statistics in %)
        self.files = []

        # List of official CIF entries, defined in the 2.4.5 standard:
        self.cifStandard = self.readCifStandard( "2.4.5" )
        pass # __init__()


    def readStandardFile( self, path ):
        output = []
        if not os.path.isfile( path ):
            logging.error( "CIF standard file does not exist: '" + path + "'" )
            return []

        f = open( path, encoding="utf8" )
        for line in f:
            short = cleanString( line )
            #print( short )
            if short.startswith( "_" ):
                #logging.info( "CIF category with '_' symbol at the beginning" )
                if "_[]" not in short:
                    output.append( short )
                else:
                    #logging.info( "CIF category array, not a category" )
                    pass

        f.close()
        return output
        pass # readStandardFile()
   

    def readStandardFileOrig( self, path ):
        output = []
        if not os.path.isfile( path ):
            logging.error( "CIF standard file does not exist: '" + path + "'" )
            return []

        f = open( path, encoding="utf8" )
        for line in f:
            short = cleanString( line )
            #print( short )
            pos = short.find( "_" )
            if 0 == pos:
                #logging.info( "CIF category with '_' symbol at the beginning" )
                pos = short.find( "_[]" )
                if pos < 0:
                    output.append( short )
                else:
                    #logging.info( "CIF category array, not a category" )
                    pass

        f.close()
        return output
        pass # readStandardFile()
   
    def correctEntrySpelling( self, entry ):
        """
_atom_site_charge
_pd_proc_ls_prof_R_I_factor
_pd_meas_wavelength
    """

        if       "_symmetry_int_tables_number" == entry:
            return "_symmetry.Int_Tables_number"
        elif     "_cell_formula_units" == entry:
            return "_cell_formula_units_Z"
        elif     "_refine_ls_weighting_sheme" == entry:
            return "_refine_ls_weighting_scheme"
        elif     "_reflns_Friedel_fraction" == entry:
        #  This appears inside a COMMENT in file 10.1021_acs.inorgchem.1c03632.cif
        #  so it is added to the list of non-standard entries.
            return "_reflns_Friedel_fraction"
        elif     "" == entry:
            return ""
        elif     "" == entry:
            return ""
        else:
            #logging.error( "Unknown entry in cif, cannot correct spelling: '" + entry + "'." )
            return entry
        pass # correctEntrySpelling()

    def readCifStandard( self, version ):
        output = []
        if "2.4.5" == version:
            path = "CIF_standard_" + version + ".txt"
            output += self.readStandardFile( path )

        else:
            logging.error( "Unknown standard of CIF: " + str(version) )
            return []

        # Non-standard entries, defined by diferent software:
        output += self.readStandardFile( "cif_non_standard.txt" )

        logging.info( "Loaded " + str(len(output)) + " categories for CIF standard " + version )
        #for e in output:
        #  print( e )
        return output
        pass # readCifStandard()

    def isInCifStandard( self, entry ):
        if entry in self.cifStandard:
            return True
        else:
            logging.error( "Entry is not in the CIF standard: '" + entry + "'" )
            return False
        pass # isInCifStandard()

    def addDir( self, dirName ):
        logging.info( "Start addDir() " )
        if not os.path.isdir( dirName ):
            logging.error( "'" + dirName + "' is not a directory in addDir()" )
            return 

        files = []
        for f in os.listdir( dirName ):
            path = os.path.join( dirName, f ) 
            #print( f, path )
            if os.path.isdir( path ):
                logging.warning( "Checking a sub-dir '" + path + "'." )
                self.addDir( path )

            elif os.path.isfile( path ):
            #if True:
                _, ext = os.path.splitext( path )
                #print ( "    this is a file" )
                if ".cif" == ext.lower():
                    files.append( path )
                    #print ( "        and has cif extension" )
                else: 
                    logging.warning( "File '" + path + "' is not .cif extension, I skip it." )
            else:
                logging.warning( "Not a file, not a dir: '" + path + "'. " )
                pass

        logging.info( "Detected the following files: " )
        logging.info( files )
        for f in files:
            #print( "file = ", f )
            pass
        self.addCifArr( files )

        pass # addDir()

    # cifArr - is a list of CIF file paths
    def addCifArr( self, cifArr ):

        # TODO
        if USE_MULTIPROC:
            nProc = 2
            pool = Pool( nProc )
            gotCif = pool.map( self.readCif, cifArr )
            for path,lst in zip(cifArr, gotCif):
                if lst:
                    self.addEntry( lst, path )
        else:
            for path in cifArr:
                lst = self.readCif( path )
                if lst:
                    self.addEntry( lst, path )

        pass # addCifArr()

    # Returns a list of all categories in the given CIF file
    def readCif( self, filename ):
        #logging.info( "Going to open file '" + filename + "'" )

        if not os.path.isfile( filename ):
            logging.error( "File '" + filename + "' does not exist, skipping it in readCif()." )
            return None

        #if filename in self.files:
        #    # This particular file has been processed, skip it
        #    logging.warning( "File '" + filename + "' appears in the list multiple times in readCif()" )
        #    return None
        #self.files.append( filename )

        output1 = []
        output2 = set()
        f = open( filename, encoding="utf8" )
        for il, line in enumerate(f):
            #print( il, line )
            #logging.info( "  line " )
            short = cleanString( line )
            if len( short ) == 0:
                continue
            words = short.split()
            #print( words )
            for w in words:
                if w.lstrip().startswith( "_" ):
                    corrected = self.correctEntrySpelling( w )
                    output2.add( corrected )

                """
                pos = w.strip().find( "_" )
                if 0 == pos:
                    # FIXME QQQQQ
                    if not w in output1:
                        corrected = self.correctEntrySpelling( w )
                        output1.append( corrected )
                """

        f.close()

        #print( output1 )
        return list(output2)
        pass # readCif()

    # Adds the 
    def addEntry( self, entryList, path ):
        self.nCIF += 1
        if None == entryList:
            return

        for e in entryList:
            if None == e:
                logging.warning( "Skipping entry" )
                continue
            # TODO this makes it slow:     
            if e not in list(self.entries.keys()):
                self.entries[e]  = Entry()

            #self.entries[e]  = Entry()
            #    self.entries[e].count += 1
            #    self.entries[e].files.append( path )
            #else:   
            #    self.entries[e].count += 1
            #    self.entries[e].files.append( path )
            self.entries[e].count += 1
            self.entries[e].paths.append( path )

        pass # addEntry()

    # Print the statistics of the entries
    def stat( self, filename = "" ):
        d = dict(sorted(self.entries.items(), key=lambda item: item[1].count, reverse=True))
        #print( d )
        if filename != "":
            f = open( filename, "w", encoding="utf8" )
            #f.write( str(0) + "\t" + "None" + "\t" + str(0) + "\t" + str(0) + "\n" )

        print( "Total number of CIFs:", self.nCIF )
        total = 0.0
        for k in d:
            total += d[k].count
        print( "total = ", total )

        cumul = 0.0
        for ik,k in enumerate(d):
            value = d[k].count
            cumul += value
            #print( "cumul = ", cumul )
            print( k.ljust(40), "=>", round( value * 100. / self.nCIF, 4 ), 
                              "% =>", round( cumul * 100. / total,     4 ) ) 
            #print( "%20s => %f %" % ( k, d[k] * 100 / self.nCIF ) )

            if filename != "":
                f.write( str(ik+1) + "\t" + k + "\t" + 
                         str(round( value * 100. / self.nCIF, 4 )) + "\t" +
                         str(round( cumul * 100. / total,     4 )) + "\n" )
            if d[k].count < 2:
                #print( "   Less than 10 entries in dir:", d[k].paths )
                pass

        if filename != "":
            f.close()

        pass # stat()

    def checkValidEntries( self ):
        # Checking for correctness of the CIF:
        for k in self.entries.keys():
            if not self.isInCifStandard( k ):
                print( "   In file(s):", self.entries[k].paths )
                pass 

        pass # checkValidEntries()

    pass # class CIFStat

if __name__ == "__main__":
    t_start  = datetime.datetime.now()
    cs = CIFStat()

    cifArr = []
    #cifArr.append( os.path.join( "CIF", "ABW.cif" ) )

    #cs.addCifArr( cifArr )

    #cs.addDir( "CIF" )
    #cs.addDir( "LI-CIF" )
    #cs.addDir( "ccdcfiles" )
    #cs.addDir( "test" )

    #d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "1", "01" )
    #d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "1" )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "testbig", "1", "00" )
    """
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "1" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "2" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "3" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "4" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "5" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "6" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "7" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "8" )
    cs.addDir( d )
    d = os.path.join( "C:", os.sep, "Users", "PRUT01", "COD_DATA", "cif", "9" )
    cs.addDir( d )
    """

    cs.stat( filename = "COD-stat.dat" )

    t_finish = datetime.datetime.now()

    print( "Time spent:", (t_finish - t_start).total_seconds() )

    #cs.stat( filename = "stat.dat" )

    #cs.checkValidEntries()

    # For testing:
    #cleanString( 2 )

