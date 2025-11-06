"""
    Part of ontozeolite package.
    Author: Rutkevych Pavlo, rutkevych.cares@gmail.com
    Date: 2024/04/01
"""

"""
Extract tile face labels and symmetry from PDF files

"""
import os
import logging
logging.basicConfig( level=logging.INFO )
import re
import zeolist
import tools
import tilesignature

import urllib.request # for file download

def findAll( string, substr ):
  output = []
  start = 0
  pos = 0
  while pos >= 0:
    pos = string[start:].find( substr )
    if pos >= 0:
      output.append( pos + start )
      start += pos + 1
  return output


class Tiling:

  def __init__(self):
    
    #self.extractData( lines )

    pass # Tiling.__init__()

  def extractData( self, lines ):
    logging.info(" Extracting data from " + str( len(lines) ) + " lines" )

    nLines = len(lines)
    iTiles = nLines
    il = 0
    self.tiles = []
    self.faceSymbols = []

    while il < nLines:
    #for il, line in enumerate(lines):
      line = lines[il]
      if 0 == il:
        words = line.split()
        self.framework = words[0]
        self.symmetry  = words[1]
        self.signature = words[2]
        if   "TILES" == lines[il+2].strip():
          self.numbers    = lines[il+1].strip()
          pass
        elif "TILES" == lines[il+3].strip():
          self.signature += lines[il+1].strip()
          self.numbers    = lines[il+2].strip()
        elif "TILES" == lines[il+4].strip():
          self.signature += lines[il+1].strip()
          self.signature += lines[il+2].strip()
          self.numbers    = lines[il+3].strip()
        elif "TILES" == lines[il+5].strip():
          self.signature += lines[il+1].strip()
          self.signature += lines[il+2].strip()
          self.signature += lines[il+3].strip()
          self.numbers    = lines[il+4].strip()
        else:
          logging.error( "Did not find signature where expected for " + self.framework )

      if "TILES" == line.strip():
        iTiles = il
        cages = tilesignature.cellToCages( self.signature )
        #print( "cages =", cages )

      if il > iTiles and line.startswith( "Face symbol:" ):
        words = line.split()
        #print( "words =", words )
        nFace = len(words) - 2
        self.faceSymbols += words[2:]
        iFace = 0
        il += 1
        if not "V, E, F:"  == lines[il].strip():
          logging.error( "Did not find V,E,F as expected" )
        il += 1
        if not "Symmetry:" == lines[il].strip():
          logging.error( "Did not find Symmetry as expected" )
        il += 1
        if not "Wyckoff:"  == lines[il].strip():
          logging.error( "Did not find Wyckoff as expected" )
        il += 1
        if not "Label:"    == lines[il].strip():
          logging.error( "Did not find Label as expected" )

        #print( "line = ", lines[il] )
        #print( "Number of tiles = ", nFace, ". Words =", words )
        for i in range(nFace):
          #print( "i = ", i )
          face = {}
          face["sig"] = words[i+2]
          il += 1
          face["vef"] = lines[il].strip()
          il += 1
          face["sym"] = lines[il].strip()
          il += 1
          face["wyc"] = lines[il].strip()
          il += 1
          face["lab"] = lines[il].strip().strip( " _" )
          if i < len(cages):
            face["num"] = cages[i][0]
          else:
            face["num"] = 0
          #print( "Adding face num =",  
          self.tiles.append( face )

      #logging.info(" Increasing il from " + str(il) + ": " + lines[il]  )
      il += 1
      pass # while()

    # Correction of the individual tile signature:

    cages = tilesignature.cellToCages( self.signature )
    #print( "tiles =", self.tiles )
    #print( "cages =", cages )
    for i, t in enumerate(self.tiles):
          if i < len(cages):
            #print( "  Compare:", cages[i][1], "vs", t["sig"] )
            if cages[i][1].replace( "^", "" ) == t["sig"]:
              t["sig"] = cages[i][1]
              #logging.info(" >>> Corrected cages for framework " + self.framework )
            else:
              logging.error( "In framework " + self.framework + " wrong tile signature: " 
                             + t["sig"] + " vs " + cages[i][1] )
          else:
            # Do nothing
            pass

    #print( "  new tiles =", self.tiles )

    if not "TILING" == lines[il-1].strip():
      # For verification purpose only
      logging.error( "Failed to find 'TILING' as expected" )

    #self.print()
    self.verify()

    pass # Tiling.extractData()

  def getNumbersStr( self ):
    output = "["
    words = self.numbers.split()
    for w in words:
      if 1 == len(w):
        output += w
      else: 
        output += "(" + w + ")"

    output += "]"
    return output
    pass # Tiling.getNumbersStr()


  def print( self ):
    print( ">> Starting tile information for one framewok" )

    print( "  Framework =", self.framework )
    print( "  Symmetry  =", self.symmetry )
    print( "  Numbers   =", self.numbers )
    print( "  Signature =", self.signature )
    print( "  Faces (in total " +str( len(self.tiles) ) + "):" )
    for i,f in enumerate(self.tiles):
      print( "    Face " +str(i+1) + ": ", f["sig"], f["vef"], f["sym"], f["wyc"], f["lab"] )
    print( "-- End of tile information.------------------" )
    pass # Tiling.print()

  def verify( self ):
    # 1. The self.numbers must contain 4 numbers:
    tmp = self.numbers.split()
    if len(tmp) != 4:
      logging.error( "The numbers must contain 4 elements:" + self.numbers + " for " + self.numbers )

    # 2. VEF must obey the Euler rule
    #pattern = r'\((\d+(?:,\s*\d+)*)\)'
    #re.
    for t in self.tiles:
      tmp = t["vef"].strip("() ")
      #print( "vef = ", tmp )
      v,e,f = tuple(  tmp.split("," ) )
      #print( v,e,f )
      t["v"] = int( v )
      t["e"] = int( e )
      t["f"] = int( f )
      if int(v) + int(f) - int(e) != 2:
        logging.error( "V.E.F. does not satisfy the Euler formula: " + 
                       v + " " + e + " " + f + " for " + self.framework )

    #    2a. Number of faces can be computed from the signature for each tile
    #    2b. Number of edges can be computed from the signature for each tile
    #words = self.signature.split("+")
    words = tilesignature.cellToCages( self.signature )
    it = 0
    for i, t in enumerate(self.tiles):
      #print( "i =", i, ", it =", it, ", of", len(words) )
      tile = words[it][1]
      #print( "Tile = ", tile, " need to compare to ", t["vef"] )
      if i < len(self.tiles)-1:
        #print ( words[it][1] , " vs ", words[it+1][1] ):
        #print( self.tiles[i]["vef"] , self.tiles[i+1]["vef"] )
        #if ( self.tiles[i]["vef"] != self.tiles[i+1]["vef"] ) and \
        if (self.faceSymbols[i] != self.faceSymbols[i+1] ):
           #( words[it][1] != words[it+1][1] ):
          it += 1
      else:
        it += 1

      faces = tilesignature.cageToFaces( tile )
      #tileInner = tile.strip("]").split("[")[1]
      #tileInner.split(".")
      nFaces = 0
      nEdges = 0
      for f in faces:
        nFaces += f[1]
        nEdges += int( f[0] ) * f[1]
      nEdges /= 2
      #print( faces, tile, "vs", t["vef"], nFaces, nEdges )
      if nFaces != t["f"]:
        logging.error( "Wrong number of faces in tile " + self.framework 
                      + "-" + tile +
           ": written " + str(t["f"]) + " from signature: " + str(nFaces) )
      if nEdges != t["e"]:
        logging.error( "Wrong number of edges in tile " + self.framework 
                      + "-" + tile +
           ": written " + str(t["e"]) + " from signature: " + str(nEdges) )

    # 3. The tile signature must be without errors: 
    #    3a. no "++", 
    pos = self.signature.find("++")
    if pos >= 0:
      logging.error( "Singature contains '++' for " + self.framework + ": '"
                    + self.signature + "'." )

    #    3.b. In most cases must contain at least one "^"
    pos = self.signature.find( "^" )
    if pos < 0:
      logging.warning(" for " + self.framework + ": Singature should " +
                     "usually contain '^' but got '" + self.signature + "'." )

    #    3c. Check the Landau's rule, and the number of "[]" equals to number of tiles
    bra = findAll( self.signature, "[" )
    ket = findAll( self.signature, "]" )
    if len(bra) != len(ket):
      logging.error(" Wrong number of brackets for " + self.framework + ": " 
                    + str(len(bra)) + " vs " +str(len(ket)) + ": " + self.signature )
    if len(bra) != len( self.tiles):
      logging.warning(" Number of brackets is diff from nTiles for " + self.framework + ": " 
                    + str(len(bra)) + " vs " + str(len(self.tiles)) + ": " + self.signature )
                      
    #    3d. number of "[]" equals to number of tiles
    for i in range( len(ket) ):
      if i < len(bra)-1:
        if ket[i] >= bra[i+1]:
          logging.error(" Wrong order of brackets for " + self.framework + ": " 
                         + ": " + self.signature)

    #    3e. no illegal characters, only 0:9,[,],+,^,"."
    tmp = str( self.signature )
    tmp = tmp.replace( "0", "" ).replace( "1", "" ).replace( "2", "" ).replace( "3", "" )
    tmp = tmp.replace( "4", "" ).replace( "5", "" ).replace( "6", "" ).replace( "7", "" )
    tmp = tmp.replace( "8", "" ).replace( "9", "" ).replace( "[", "" ).replace( "]", "" )
    tmp = tmp.replace( "^", "" ).replace( "+", "" ).replace( ".", "" )
    if len( tmp ) > 0:
      logging.error( "Signature contains illegal characters: '" + tmp + "' in "
                     + "framework " + self.framework + ": '" + self.signature
                     + "'." )

    #    3f. no 0,1,2 as number of edges in the face, no power 0,1 
    # TODO
    isFactor = ""
    isNEdges = ""
    isPower  = ""
    for ch in self.signature:
      pass

    # 4. The tile signature must coincide with the one from excel (with '^' characters)
    # TODO

    # Compare the self.number to those in the csv file
    # Symmetry must be a known code (I need a database of space groups)
    # 4. What else?

    pass # Tiling.verify()

  pass # class Tiling

class TileSet:
  def __init__( self ):
    self.tiles = []
    pass # TileSet.__init__()

  def getChunks( self, filename ):
    output = []

    if not os.path.isfile( filename ):
      logging.error( "File '" + filename + "' does not exist" )
      return output

    chunk = []
    with open( filename, "r", encoding="utf8" ) as f:
      #lines = f.readlines()
      #for line in lines:
      #while line = f.readline():
      for il, line in enumerate(f):
        #print( "Reading line", il+1, line.strip() )
        chunk.append( line )
        if "TILING" == line.strip():
          output.append( chunk )
          chunk = []
          #logging.info( "Got a chunk of lines" )

    return output
    pass # TileSet.readLines()

  def run( self ):

    #fname = os.path.join("ontozeolite", "data", "tiles", "CON.tiles") 
    #fname = os.path.join("ontozeolite", "data", "tiles", "Tiles2010.tiles") 
    fname = os.path.join("ontozeolite", "data", "tiles", "TilesAdd2023.tiles") 
    chunks = self.getChunks( fname ) 
    for chunk in chunks:
      logging.info(" Starting extractData from chunk '" + str(chunk[0][:12]) + "...'")
      t = Tiling()
      t.extractData( chunk )
      self.tiles.append( t )
   
    pass # TileSet.run()

  def updateCsv( self, fileIn, fileOut ):
    #self.save()
    # 1. Get the list of zeolites (all known)
    zeoList = zeolist.getZeoList( ["main", "new"] )
    logging.info(" There are " + str( len(zeoList) ) + " zeolites framworks" )
    #print( zeoList )

    # 2. Read the fileIn csv
    zeoIn = tools.readCsv( fileIn )
    #print( zeoIn[:3] )

    # 3. Check which zeolites are missing in the fileIn and manually add to the csv with signature
    count = 0
    zeoAdd = []
    for z in zeoList:
      #zName = z
      found = False
      for zIn in zeoIn[ 1: ]: # 0-row is the header
        if zIn[0].startswith( z ):
          found = True
          break
        #pass
      if False == found:
        logging.warning( "Missing in csvIn zeolite '" + z + "'" )
        count += 1
        #t = Tiling()
        #t.numbers = 
        #numStr = z.getNumbersStr( self )
        zeoAdd.append( [ z, "", "", "" ] )
    print( "Missing " + str( count ) + " zeolites in the csvIn file" )

    zArr = []
    for t in self.tiles:
      #print( "z.framework =", t.framework )
      numArr  = []
      tileArr = []
      codeArr = []
      vefArr  = []
      vArr    = []
      eArr    = []
      fArr    = []
      symmArr = []
      for tt in t.tiles:
        #print( "tt signature =", tt["sig"] )
        #print( "number = ", tt["num"] )
          #face["sig"] = words[i+2]
          #face["vef"] = lines[il].strip()
          #face["wyc"] = lines[il].strip()
        numArr .append( str(tt["num"]) )
        tileArr.append( tt["sig"] )
        codeArr.append( tt["lab"] )
        vefArr .append( tt["vef"] )
        vArr   .append( str(tt["v"]) )
        eArr   .append( str(tt["e"]) )
        fArr   .append( str(tt["f"]) )
        symmArr.append( tt["sym"] )
      #print( "Before saving symm =", symmArr )

      zArr.append( [ t.framework, t.getNumbersStr(), t.signature, "", t.symmetry,
                     "+".join(numArr), "+".join(tileArr), "+".join(codeArr), 
                     "+".join(vefArr), "+".join(vArr), "+".join(eArr), "+".join(fArr), 
                     "+".join(symmArr) ] )
    #print( "Before saving to csv" )
    tools.writeCsv( "temp3-add.csv", zArr )

    """
    This needs to be run only once
    zeolist.writeCsv( "temp-add.csv", zeoAdd )
    for z in zeoAdd:
      if "-" == z[0][0]:
        nameShort = z[0][1:]
      else:
        nameShort = z[0]
      print( nameShort )
      urllib.request.urlretrieve(
        "https://europe.iza-structure.org/IZA-SC/Tilings/" + nameShort+".pdf",
                                                             nameShort+".pdf" )
    """

    # 4. For all zeolites create a new array and append data from Tiling()

    # 5. Save to fileOut

    pass # TileSet.updateCsv()

  pass # class TileSet


if __name__ == "__main__":
  ts = TileSet()
  ts.run()
  csvIn  = os.path.join( "ontozeolite", "data", "Tile-signature-2023.csv" )
  #csvIn  = os.path.join( "ontozeolite", "data", "additional.csv" )
  csvOut = os.path.join( "ontozeolite", "data", "Tile-signature-xxxx.csv" )
  ts.updateCsv( csvIn, csvOut )

  #print( "Tesing findAll (expect [0,6]) :", findAll( "123asd123", "12" ) )

