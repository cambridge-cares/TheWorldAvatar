
import csv
import os
import tools
import zeolist
import tilesignature
import logging
logging.basicConfig( level = logging.INFO )

from pymatgen.core.structure import Structure, Lattice


class CsvMaker:

  def __init__( self ):
    self.zeoOption = "test"
    self.zeoList = []

    self.ontoBase = "ontoZeolite"
    self.ontoPrefix = "http://www.theworldavatar.com/kg/ontozeolite/"

    self.inputDir  = os.path.join( "ontozeolite", "data" )
    self.outputDir = os.path.join( "ontozeolite", "zeocsv" )

    #baseName = self.ontoBase + "-" + zeoname
    #self.ontoHttp = ""
    
    pass # CsvMaker.__init__()

  def prepare( self ):
    self.zeoList = zeolist.getZeoList( self.zeoOption )

    # May be command line arguments here:

  def arrInit( self, zeoname ):
    output = [ ]
    baseName = self.ontoBase # + "-" + zeoname

    output.append( [ "Source", "Type", "Target", "Relation", "Value", "Data Type" ] )

    output.append( [ baseName, "Ontology", 
                     "http://www.theworldavatar.com/kg/ontozeolite/OntoZeolite.owl", 
                     "http://www.w3.org/2002/07/owl#imports", "", "" ] )

    output.append( [ baseName, "Ontology", 
                     "http://www.theworldavatar.com/kg/ontozeolite",
                     "base", "", "" ] )

    # The framework should be initialized only once.
    uuidDB = tools.loadUUID( )

    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    output.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )
 
    output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
                     uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrInit()

  def arrAtomSite( self, zeoname ):
    output = []

    uuidDB = tools.loadUUID( )

    path = os.path.join( "CIF", zeoname.upper() + ".cif")
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )

    #print( zeoname )
    #print( "  ", structure.lattice ) # These are 3 unit cell vectors 
    #print( "  ", structure.lattice.gamma ) # angles, similarly a,b,c
    #print( "  ", structure.lattice.volume ) # volume
    #print( "  ", structure.lattice.reciprocal_lattice.a ) # reciprocal parameters

    # Define class instances:
    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    #output.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )
 
    #output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
    #                 uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    uuid_cif = tools.getUUID( uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
    output.append( [ uuid_cif, "Instance", "CIFCore", "", "", "" ] )
 
    uuid_cif_uc = tools.getUUID( uuidDB, "CIFCoreCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "CIFCoreCell", "", "", "" ] )

    # Define relation between the class instances:
    output.append( [ uuid_zeolite, "Instance", uuid_cif,  
                     self.ontoPrefix + "hasCIF", "", "" ] )

    output.append( [ uuid_cif, "Instance", uuid_cif_uc,  
                     self.ontoPrefix + "hasCIFCoreCell", "", "" ] )

    # Vector for Unit Cell length parameters (a,b,c):
    uuid_uc_abc = tools.getUUID( uuidDB, "UnitCellLengths", "UnitCellLengths_" + zeoname )
    output.append( [ uuid_uc_abc, "Instance", "UnitCellLengths", "", "", "" ] )
 
    uuid_uc_a = tools.getUUID( uuidDB, "Length", "UnitCellLengthA_" + zeoname )
    output.append( [ uuid_uc_a, "Instance", "Length", "", "", "" ] )
 
    uuid_uc_b = tools.getUUID( uuidDB, "Length", "UnitCellLengthB_" + zeoname )
    output.append( [ uuid_uc_b, "Instance", "Length", "", "", "" ] )

    uuid_uc_c = tools.getUUID( uuidDB, "Length", "UnitCellLengthC_" + zeoname )
    output.append( [ uuid_uc_c, "Instance", "Length", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_abc,  
                     self.ontoPrefix + "hasUnitCellLengths", "", "" ] )

    output.append( [ uuid_uc_abc, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ uuid_uc_abc, "Instance", uuid_uc_a,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_abc, "Instance", uuid_uc_b,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_abc, "Instance", uuid_uc_c,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_a, "", "a", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_a, "", round(structure.lattice.a, 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_b, "", "b", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_b, "", round(structure.lattice.b, 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_c, "", "c", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_c, "", round(structure.lattice.c, 10) , "decimal" ] )
    ###########################################

    # Vector for Unit Cell angles parameters (alpha,beta,gamma):
    uuid_uc_abg = tools.getUUID( uuidDB, "UnitCellAngles", "UnitCellAngles_" + zeoname )
    output.append( [ uuid_uc_abg, "Instance", "UnitCellAngles", "", "", "" ] )
 
    uuid_uc_al = tools.getUUID( uuidDB, "VectorComponent", "UnitCellAngleAlpha_" + zeoname )
    output.append( [ uuid_uc_al, "Instance", "VectorComponent", "", "", "" ] )
 
    uuid_uc_be = tools.getUUID( uuidDB, "VectorComponent", "UnitCellAngleBeta_" + zeoname )
    output.append( [ uuid_uc_be, "Instance", "VectorComponent", "", "", "" ] )

    uuid_uc_gm = tools.getUUID( uuidDB, "VectorComponent", "UnitCellAngleGamma_" + zeoname )
    output.append( [ uuid_uc_gm, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_abg,  
                     self.ontoPrefix + "hasUnitCellAngles", "", "" ] )

    #output.append( [ self.ontoPrefix + "", "Data Property", 
    #                 uuid_uc_abg, "", "", "string" ] )
    output.append( [ uuid_uc_abg, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/degree",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )
    output.append( [ uuid_uc_abg, "Instance", uuid_uc_al,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_abg, "Instance", uuid_uc_be,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_abg, "Instance", uuid_uc_gm,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_al, "", "alpha", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_al, "", round(structure.lattice.alpha, 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_be, "", "beta",  "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_be, "", round(structure.lattice.beta,  10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_gm, "", "gamma", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_gm, "", round(structure.lattice.gamma, 10) , "decimal" ] )
    ###########################################


    # Vector for Unit Cell vectors (a,b,c):
    uuid_uc_vec_abc = tools.getUUID( uuidDB, "UnitCellVectors", "UnitCellVectors_" + zeoname )
    output.append( [ uuid_uc_vec_abc, "Instance", "UnitCellVectors", "", "", "" ] )
 
    uuid_uc_vec_a = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthA_" + zeoname )
    output.append( [ uuid_uc_vec_a, "Instance", "VectorComponent", "", "", "" ] )
 
    uuid_uc_vec_b = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthB_" + zeoname )
    output.append( [ uuid_uc_vec_b, "Instance", "VectorComponent", "", "", "" ] )

    uuid_uc_vec_c = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthC_" + zeoname )
    output.append( [ uuid_uc_vec_c, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_abc,  
                     self.ontoPrefix + "hasUnitCellVectors", "", "" ] )

    uuid_vec_a = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorA_" + zeoname )
    uuid_vec_b = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorB_" + zeoname )
    uuid_vec_c = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorC_" + zeoname )

    uuid_vec_ax = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAx_" + zeoname )
    uuid_vec_ay = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAy_" + zeoname )
    uuid_vec_az = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAz_" + zeoname )

    uuid_vec_bx = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBx_" + zeoname )
    uuid_vec_by = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBy_" + zeoname )
    uuid_vec_bz = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBz_" + zeoname )

    uuid_vec_cx = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCx_" + zeoname )
    uuid_vec_cy = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCy_" + zeoname )
    uuid_vec_cz = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCz_" + zeoname )

    output.append( [ uuid_vec_a, "Instance", "UnitCellVector", "", "", "" ] )
    output.append( [ uuid_vec_b, "Instance", "UnitCellVector", "", "", "" ] )
    output.append( [ uuid_vec_c, "Instance", "UnitCellVector", "", "", "" ] )

    output.append( [ uuid_vec_ax, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_ay, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_az, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_bx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_by, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_bz, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_cx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_cy, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_cz, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_uc_vec_abc, "Instance", uuid_uc_vec_a,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_abc, "Instance", uuid_uc_vec_b,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_abc, "Instance", uuid_uc_vec_c,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_vec_a, "", "a", "string" ] )
    output.append( [ uuid_uc_vec_a, "Instance", uuid_vec_a,  
                     self.ontoPrefix + "hasValue", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", uuid_vec_ax,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", uuid_vec_ay,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", uuid_vec_az,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )



    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_vec_b, "", "b", "string" ] )
    output.append( [ uuid_uc_vec_b, "Instance", uuid_vec_b,  
                     self.ontoPrefix + "hasValue", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", uuid_vec_bx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", uuid_vec_by,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", uuid_vec_bz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_vec_c, "", "c", "string" ] )
    output.append( [ uuid_uc_vec_c, "Instance", uuid_vec_c,  
                     self.ontoPrefix + "hasValue", "", "" ] )

    output.append( [ uuid_vec_c, "Instance", uuid_vec_cx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_c, "Instance", uuid_vec_cy,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_c, "Instance", uuid_vec_cz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    
    output.append( [ uuid_vec_c, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )



    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_ax, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_ax, "", round(structure.lattice.matrix[0][0], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_ay, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_ay, "", round(structure.lattice.matrix[0][1], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_az, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_az, "", round(structure.lattice.matrix[0][2], 10) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_bx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_bx, "", round(structure.lattice.matrix[1][0], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_by, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_by, "", round(structure.lattice.matrix[1][1], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_bz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_bz, "", round(structure.lattice.matrix[1][2], 10) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cx, "", round(structure.lattice.matrix[2][0], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cy, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cy, "", round(structure.lattice.matrix[2][1], 10) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cz, "", round(structure.lattice.matrix[2][2], 10) , "decimal" ] )

    ###########################################


    tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrAtomSite()

  def arrTiles( self, zeoname ):
    output = []

    uuidDB = tools.loadUUID()

    path = os.path.join( self.inputDir, "Tile-signature-2023.csv" )
    dataIn = tools.readCsv( path )

    #print( dataIn )
    data = tilesignature.getDataByCode( dataIn, zeoname )
    #print( data)

    baseName = self.ontoBase + "-" + zeoname

    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    #output.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )

    #output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
    #                 uuid_zeolite, "", zeoname.strip(' "'), "string" ] )


    uuid_tstructure = tools.getUUID( uuidDB, "TiledStructure", "TiledStructure_" + zeoname )
    output.append( [ uuid_tstructure, "Instance", "TiledStructure",
                     "", "", "" ] )

    output.append( [ uuid_zeolite, "Instance", uuid_tstructure,  
                     self.ontoPrefix + "hasTiledStructure", "", "" ] )

    output.append( [ self.ontoPrefix + "hasTileSignature", "Data Property", 
                     uuid_tstructure, "", data[2].strip(' "'), "string" ] )

    ### Begin of transitivity
    uuid_tile_trans = tools.getUUID( uuidDB, "Vector", "TileNumber_" + zeoname + "_transitivity" )
    output.append( [ uuid_tile_trans, "Instance", "Vector",
                     "", "", "" ] )

    uuid_tile_transP = tools.getUUID( uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityP" )
    output.append( [ uuid_tile_transP, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transQ = tools.getUUID( uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityQ" )
    output.append( [ uuid_tile_transQ, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transR = tools.getUUID( uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityR" )
    output.append( [ uuid_tile_transR, "Instance", "VectorComponent",
                     "", "", "" ] )

    uuid_tile_transS = tools.getUUID( uuidDB, "VectorComponent", "TileNumber_" + zeoname + "_transitivityS" )
    output.append( [ uuid_tile_transS, "Instance", "VectorComponent",
                     "", "", "" ] )

    output.append( [ uuid_tstructure, "Instance", uuid_tile_trans ,  
                     self.ontoPrefix + "hasTransitivity", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transP,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transQ,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transR,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_tile_trans, "Instance", uuid_tile_transS,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    trans = self.getTransitivity( data[1] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transP, "", "p", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transP, "", trans[0], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transQ, "", "q", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transQ, "", trans[1], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transR, "", "r", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transR, "", trans[2], "integer" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                                        uuid_tile_transS, "", "s", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                                        uuid_tile_transS, "", trans[3], "integer" ] )
    ### End of transitivity

    cages = tilesignature.cellToCages( data[2] )
    signN = data[ 6].split( "+" )
    codeN = data[ 7].split( "+" )
    vertN = data[ 9].split( "+" )
    edgeN = data[10].split( "+" )
    faceN = data[11].split( "+" )

    #print( "faces =", faceN )
    for ic, cage in enumerate(cages):
      #print( "ic = ", ic )
      uuid_tile     = tools.getUUID( uuidDB, "Tile", "Tile-" + zeoname + "-cage" + str(ic+1) )
      uuid_tile_num = tools.getUUID( uuidDB, "TileNumber", "TileNumber-" + zeoname + "-cage" + str(ic+1) )

      output.append( [ uuid_tile, "Instance", "Tile", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile, 
                       self.ontoPrefix + "hasTile", "", "" ] )

      output.append( [ self.ontoPrefix + "hasTileCode", "Data Property", 
                                         uuid_tile, "", codeN[ic], "string" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfFaces", "Data Property", 
                                         uuid_tile, "", faceN[ic], "integer" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfEdgges", "Data Property", 
                                         uuid_tile, "", edgeN[ic], "integer" ] )

      output.append( [ self.ontoPrefix + "hasNumberOfVertices", "Data Property", 
                                         uuid_tile, "", vertN[ic], "integer" ] )

      # TODO
      #output.append( [ self.ontoPrefix + "hasTileSymmetry", "Data Property", 
      #                                   uuid_tile, "", symmN[ic], "string" ] )

      output.append( [ self.ontoPrefix + "hasTileSignature", "Data Property", 
                                         uuid_tile, "", signN[ic], "string" ] )

      output.append( [ uuid_tile_num, "Instance", "TileNumber", 
                       "", "", "" ] )

      output.append( [ uuid_tstructure, "Instance", uuid_tile_num, 
                       self.ontoPrefix + "hasTileNumber", "", "" ] )

      output.append( [ uuid_tile_num, "Instance", uuid_tile, 
                       self.ontoPrefix + "isNumberOf", "", "" ] )

      output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                       #uuid_tile_num, "", cage[0], "" ] )
                       uuid_tile_num, "", cage[0], "integer" ] )
                       #uuid_tile_num, "", cage[0], 
                       #"http://www.w3.org/2001/XMLSchema#integer" ] )



      faces = tilesignature.cageToFaces( cage[1] )

      for f, face in enumerate(faces):
        #print( "face = ", face )
        
        suffix = "_" + zeoname + "_cage" + str(ic+1) + "_face" + str(f+1)
        uuid_tile_face     = tools.getUUID( uuidDB, "TileFace", "TileFace"       + suffix )
        uuid_tile_face_num = tools.getUUID( uuidDB, "TileFaceNumber", "TileFaceNumber" + suffix )

        output.append( [ uuid_tile_face,     "Instance", "TileFace", 
                         "" , "", "" ] )

        output.append( [ uuid_tile,   "Instance", uuid_tile_face,   
                         self.ontoPrefix + "hasTileFace" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", "TileFaceNumber",  
                         "" , "", "" ] )
        
        output.append( [ uuid_tile, "Instance", uuid_tile_face_num,  
                         self.ontoPrefix + "hasTileFaceNumber" , "", "" ] )

        output.append( [ uuid_tile_face_num, "Instance", uuid_tile_face,
                         self.ontoPrefix + "isNumberOf" , "", "" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                              #uuid_tile_face_num, "", face[1], "" ] )
                              uuid_tile_face_num, "", face[1], "integer" ] )
                              #uuid_tile_face_num, "", face[1], 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.ontoPrefix + "hasNumberOfEdges", "Data Property", 
                              #uuid_tile_face, "", int(face[0]), "" ] )
                              uuid_tile_face, "", int(face[0]), "integer" ] )
                              #uuid_tile_face, "", int(face[0]), 
                              #"http://www.w3.org/2001/XMLSchema#integer" ] )

        output.append( [ self.ontoPrefix + "hasFaceCode", "Data Property", 
                                       uuid_tile_face, "", str("UNDEFINED"), "string" ] )

        # TODO add face code (?), like 14a, 14b, etc
        #output.append( [ self.ontoBase + "#hasFaceCode", "Data Property", 
        #                               uuid_tile_face, "", int(cell[0]), "string" ] )


    tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrTiles()

  def getTransitivity( self, strValue ):
    if not isinstance( strValue, str ):
      print( "Error! input must be string" )
      return None

    short = strValue.strip()
    if short[0] != "[" or short[-1] != "]":
      print( "Error! Transitivity must be in [] brackets" )

    trans = [None]*4
    i = 1   # position of character in the string
    #it = 0  # iterator for transitivity vector. 0 to 3.
    for it in range( 4 ):
      if short[i] != "(":
        trans[it] = short[i]
        i += 1
      else:
        i += 1
        trans[it] = ""
        while short[i] != ")":
          trans[it] += short[i]
          i += 1
        else:
          i += 1 # To skip the ")" bracket
      #it += 1
      #print( "it = ", it, ":", trans, ", i =", i )
    #print( trans )
    return trans

  def makeCsvs( self ):
    errCount = 0

    self.prepare()

    for z in self.zeoList:
      #print( "In zeolist z =", z )
      arr  = self.arrInit( z )
      arr += self.arrTiles( z )
      arr += self.arrAtomSite( z )

      #print( "arr =", arr )
      #csvWrite( arr )
      path = os.path.join( self.outputDir, z + ".csv" )
      tools.writeCsv( path, arr )

      #errCount += 1
      #logging.warning( "Not implemented creating zeolite csv" )
      pass

    if errCount > 0:
      logging.warning( "Detected " + str(errCount) + " errors" )

    pass # CsvMaker.makeCsvs()

  pass # class CsvMaker


if __name__ == "__main__":
  
  a = CsvMaker()

  a.makeCsvs()

