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

def cleanString( line ):
  if not isinstance(line,str):
    logging.error( "Input line '" + str(line) + "' is not a string in cleanString()." )
    return True
 
  pos = line.find( "#" )
  if pos < 0:
    tmp = line.strip()
  else:
    tmp = line[:pos].strip()

  tmp2 = tmp.replace( "<br>", "" )
  return tmp2



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
    pass

  def readStandardFile( self, path ):
      output = []
      if not os.path.isfile( path ):
        logging.error( "CIF file does not exist: '" + path + "'" )
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
    pass

  def isInCifStandard( self, entry ):
    if entry in self.cifStandard:
      return True
    else:
      logging.error( "Entry is not in the CIF standard: '" + entry + "'" )
      return False
    pass

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
    for path in cifArr:
      l = self.readCif( path )
      self.addEntry( l, path )
    pass

  # Returns a list of all categories in the given CIF file
  def readCif( self, filename ):
    #logging.info( "Going to open file '" + filename + "'" )
    if filename in self.files:
      # This particular file has been processed, skip it
      logging.warning( "File '" + filename + "' appears in the list multiple times in readCif()" )
      return None
    if not os.path.isfile( filename ):
      logging.error( "File '" + filename + "' does not exist, skippint it in readCif()" )
      return None


    output = []
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
        pos = w.strip().find( "_" )
        if 0 == pos:
          if not w in output:
            corrected = self.correctEntrySpelling( w )
            output.append( corrected )

    f.close()

    #print( output )
    return output


  # Adds the 
  def addEntry( self, entryList, path ):
    self.nCIF += 1
    if None == entryList:
      return

    for e in entryList:
      if None == e:
        logging.warning( "Skipping entry" )
        continue
      if e not in list(self.entries.keys()):
        self.entries[e]  = Entry()

      #  self.entries[e].count += 1
      #  self.entries[e].files.append( path )
      #else:   
      #  self.entries[e].count += 1
      #  self.entries[e].files.append( path )
      self.entries[e].count += 1
      self.entries[e].paths.append( path )

  # Print the statistics of the entries
  def stat( self ):
    d = dict(sorted(self.entries.items(), key=lambda item: item[1].count, reverse=True))
    #print( d )
    print( "Total number of CIFs:", self.nCIF )
    for k in d:
      print( k.ljust(40), "=>", round(d[k].count * 100 / self.nCIF, 5 ), "%" ) 
      #print( "%20s => %f %" % ( k, d[k] * 100 / self.nCIF ) )
      if d[k].count < 2:
        #print( "   Less than 10 entries in dir:", d[k].paths )
        pass

  def checkValidEntries( self ):
    # Checking for correctness of the CIF:
    for k in self.entries.keys():
      if not self.isInCifStandard( k ):
        print( "   In file(s):", self.entries[k].paths )
        pass 

    pass

if __name__ == "__main__":
  cs = CIFStat()

  cifArr = []
  #cifArr.append( os.path.join( "CIF", "ABW.cif" ) )

  #cs.addCifArr( cifArr )
  cs.addDir( "CIF" )
  cs.addDir( "LI-CIF" )
  cs.addDir( "ccdcfiles" )
  cs.stat( )

  #cs.checkValidEntries()

  # For testing:
  #cleanString( 2 )

