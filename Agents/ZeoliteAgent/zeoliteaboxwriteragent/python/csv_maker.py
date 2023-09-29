
import csv
import os

import logging
#logging.basicConfig( level = logging.DEBUG )
#logging.basicConfig( level = logging.INFO )
logging.basicConfig( level = logging.WARNING )
#logging.basicConfig( level = logging.ERROR )

import tools
import zeolist
import tilesignature
import math
import numpy

from pymatgen.core.structure import Structure, Lattice


# FIXME
#http://www.w3.org/2000/01/rdf-schema#Literal

class CsvMaker:

  def __init__( self ):
    #self.zeoOption = "main"
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
    self.zeoList = [ os.path.join( "test", "913885.cif" ) ]
    self.uuidDB = tools.loadUUID( )

    # May be also command line arguments here:


  def finalize( self ):
    tools.saveUUID( self.uuidDB )


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
    #uuidDB = tools.loadUUID( )
    uuidDB = self.uuidDB

    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    output.append( [ uuid_zeolite, "Instance", "ZeoliteFramework", "", "", "" ] )
 
    output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
                     uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrInit()

  def arrTransform( self, zeoname ):
    #print( "arrTransform started" )
    TWOPI = 2 * math.pi
    DIRS = "xyz"

    output = []

    #uuidDB = tools.loadUUID( )
    uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )
    #uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
    uuid_cif = tools.getUUID( uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )

    uuid_cif_core_trans = tools.getUUID( uuidDB, "CIFCoreTransform", "ZeoliteCIFCoreTransform_" + zeoname )
    output.append( [ uuid_cif_core_trans, "Instance", "CIFCoreTransform", "", "", "" ] )
    output.append( [ uuid_cif, "instance", uuid_cif_core_trans,  
                     self.ontoPrefix + "hasCifCoreTransformation", "", "" ] )

    ################# Fractional to Cartesian ########################
    uuid_m_frac_to_cart = tools.getUUID( uuidDB, "CIFCoreTransformationMatrixToCartesian", 
                        "ZeoliteCIFTransformationMatrixToCartesian_" + zeoname )
    output.append( [ uuid_m_frac_to_cart, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_frac_to_cart,  
                     self.ontoPrefix + "hasTransformationMatrixToCartesian", "", "" ] )

    output.append( [ uuid_m_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( uuidDB, "MatrixComponent", 
                        "MatrixComponentToCartesian"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_frac_to_cart, "instance", uuid_m_comp,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


            output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.ontoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.ontoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )

    uuid_v_frac_to_cart = tools.getUUID( uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToCartesian_" + zeoname )
    output.append( [ uuid_v_frac_to_cart, "Instance", 
                           "CIFCoreTransformationVectorToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_v_frac_to_cart,  
                     self.ontoPrefix + "hasTransformationVectorToCartesian", "", "" ] )

    output.append( [ uuid_v_frac_to_cart, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( uuidDB, "VectorComponent", 
                    "VectorComponentToCartesian" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_frac_to_cart, "instance", uuid_v_comp,  
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.ontoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )
                         #round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )


    ################# Cartesian to Fractional ########################
    uuid_m_cart_to_frac = tools.getUUID( uuidDB, "CIFCoreTransformationMatrixToFractional", 
                        "ZeoliteCIFTransformationMatrixToFractional_" + zeoname )
    output.append( [ uuid_m_cart_to_frac, "Instance", 
                           "Matrix", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_m_cart_to_frac,  
                     self.ontoPrefix + "hasTransformationMatrixToFractional", "", "" ] )

    output.append( [ uuid_m_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for iy in range(3):
        for ix in range(3):
            uuid_m_comp = tools.getUUID( uuidDB, "MatrixComponent", 
                        "MatrixComponentToFractional"+str(ix)+str(iy)+"_" + zeoname )
            output.append( [ uuid_m_comp, "Instance", 
                           "MatrixComponent", "", "", "" ] )
                           #"CIFCoreTransformationMatrixToCartesian", "", "", "" ] )
            output.append( [ uuid_m_cart_to_frac, "instance", uuid_m_comp,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

            output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                             uuid_m_comp, "", DIRS[ix]+DIRS[iy], "string" ] )

            output.append( [ self.ontoPrefix + "hasRowIndex", "Data Property", 
                             uuid_m_comp, "", iy, "integer" ] )

            output.append( [ self.ontoPrefix + "hasColumnIndex", "Data Property", 
                             uuid_m_comp, "", ix, "integer" ] )

            output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                             uuid_m_comp, "", 
                             round(structure.lattice.reciprocal_lattice.matrix[iy][ix]/TWOPI, 12), "decimal" ] )

    uuid_v_cart_to_frac = tools.getUUID( uuidDB, "Vector", 
                        "ZeoliteCIFTransformationVectorToFractional" + zeoname )
    output.append( [ uuid_v_cart_to_frac, "Instance", 
                           "Vector", "", "", "" ] )
                           #"CIFCoreTransformationVectorToFractional", "", "", "" ] )
    output.append( [ uuid_cif_core_trans, "instance", uuid_v_cart_to_frac,  
                     self.ontoPrefix + "hasTransformationVectorToFractional", "", "" ] )

    output.append( [ uuid_v_cart_to_frac, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    for ix in range(3):
        uuid_v_comp = tools.getUUID( uuidDB, "VectorComponent", 
                    "VectorComponentToFractional" + str(ix) + "_" + zeoname )
        output.append( [ uuid_v_comp, "Instance", 
                       "VectorComponent", "", "", "" ] )
                       #"CIFCoreTransformationVectorToCartesian", "", "", "" ] )
        output.append( [ uuid_v_cart_to_frac, "instance", uuid_v_comp,  
                         self.ontoPrefix + "hasComponent", "", "" ] )

        output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                         uuid_v_comp, "", DIRS[ix], "string" ] )

        output.append( [ self.ontoPrefix + "hasIndex", "Data Property", 
                         uuid_v_comp, "", ix, "integer" ] )

        output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                         uuid_v_comp, "", 
                         0.0, "decimal" ] )
                         #round(structure.lattice.matrix[ix][iy], 12), "decimal" ] )



    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrTransform()
 
  def arrAtomSite( self, zeoname ):
    logging.warning( "arrAtomSite() is not implemented yet" )

    output = []

    #uuidDB = tools.loadUUID( )
    uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")
    #dataIn = tools.readCsv( path )
    if not os.path.isfile( path ):
      logging.error( "File not found '" + path + "'." )
      return

    structure = Structure.from_file( path )

    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

    uuid_cif = tools.getUUID( uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
 

    return output
    pass # CsvMaker.arrAtomSite()

  def arrUnitCell( self, zeoname ):
    output = []
    TWOPI = 2 * math.pi
    # TODO to add get_crystal_system(), get_space_group_number(), 
    #      https://pymatgen.org/pymatgen.symmetry.html

    #uuidDB = tools.loadUUID( )
    uuidDB = self.uuidDB

    path = os.path.join( "CIF", zeolist.zeoCodeToCode3(zeoname).upper() + ".cif")

    path = os.path.join( "test", "913885.cif" ) 
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
    #print( "  ", structure.lattice.reciprocal_lattice.matrix[0][0] ) # reciprocal parameters
    #print( "  ", structure.lattice.reciprocal_lattice.matrix ) # reciprocal parameters
    #print( "  ", structure.lattice.matrix ) # reciprocal parameters
    #print( TWOPI * numpy.linalg.inv( structure.lattice.matrix ) )

    # Define class instances:
    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )
 
    #output.append( [ self.ontoPrefix + "hasZeoliteCode", "Data Property", 
    #                 uuid_zeolite, "", zeoname.strip(' "'), "string" ] )

    uuid_cif = tools.getUUID( uuidDB, "CIFCore", "ZeoliteCIF_" + zeoname )
    output.append( [ uuid_cif, "Instance", "CIFCore", "", "", "" ] )
 
    uuid_cif_uc = tools.getUUID( uuidDB, "CIFCoreCell", "UnitCell_" + zeoname )
    output.append( [ uuid_cif_uc, "Instance", "CIFCoreCell", "", "", "" ] )

    # Define relation between the class instances:
    output.append( [ uuid_zeolite, "instance", uuid_cif,  
                     self.ontoPrefix + "hasCIF", "", "" ] )

    output.append( [ uuid_cif, "Instance", uuid_cif_uc,  
                     self.ontoPrefix + "hasCIFCoreCell", "", "" ] )
    ###########################################

    # Vector for Unit Cell length parameters (a,b,c):
    uuid_uc_abc = tools.getUUID( uuidDB, "UnitCellLengths", "UnitCellLengths_" + zeoname )
    output.append( [ uuid_uc_abc, "Instance", "UnitCellLengths", "", "", "" ] )
 
    uuid_uc_a = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthA_" + zeoname )
    output.append( [ uuid_uc_a, "Instance", "Length", "", "", "" ] )
 
    uuid_uc_b = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthB_" + zeoname )
    output.append( [ uuid_uc_b, "Instance", "Length", "", "", "" ] )

    uuid_uc_c = tools.getUUID( uuidDB, "VectorComponent", "UnitCellLengthC_" + zeoname )
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
                     uuid_uc_a, "", round(structure.lattice.a, 12) , "decimal" ] )
    #print( round( 10.123456789012345678, 15 ) )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_b, "", "b", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_b, "", round(structure.lattice.b, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_c, "", "c", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_c, "", round(structure.lattice.c, 12) , "decimal" ] )
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
                     uuid_uc_al, "", 
                     round(structure.lattice.alpha, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_be, "", "beta",  "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_be, "", 
                     round(structure.lattice.beta,  12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_gm, "", "gamma", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_gm, "", 
                     round(structure.lattice.gamma, 12) , "decimal" ] )
    ###########################################


    # Vector for reciprocal Unit Cell lengths (a*,b*,c*):
    uuid_uc_r_abc = tools.getUUID( uuidDB, "UnitCellReciprocalLengths", "UnitCellReciprocalLengths_" + zeoname )
    output.append( [ uuid_uc_r_abc, "Instance", "UnitCellLengths", "", "", "" ] )
 
    uuid_uc_r_a = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalLengthA_" + zeoname )
    output.append( [ uuid_uc_r_a, "Instance", "Length", "", "", "" ] )
 
    uuid_uc_r_b = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalLengthB_" + zeoname )
    output.append( [ uuid_uc_r_b, "Instance", "Length", "", "", "" ] )

    uuid_uc_r_c = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalLengthC_" + zeoname )
    output.append( [ uuid_uc_r_c, "Instance", "Length", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_abc,  
                     self.ontoPrefix + "hasUnitCellReciprocalLengths", "", "" ] )

    output.append( [ uuid_uc_r_abc, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ uuid_uc_r_abc, "Instance", uuid_uc_r_a,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_r_abc, "Instance", uuid_uc_r_b,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_r_abc, "Instance", uuid_uc_r_c,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_a, "", "a*", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_a, "", 
                     round(structure.lattice.reciprocal_lattice.a/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_b, "", "b*", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_b, "", 
                     round(structure.lattice.reciprocal_lattice.b/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_c, "", "c*", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_c, "", 
                     round(structure.lattice.reciprocal_lattice.c/TWOPI, 12) , "decimal" ] )
    ###########################################
    ###########################################

    # Vector for Reciprocal Unit Cell angles parameters (alpha*,beta*,gamma*):
    uuid_uc_r_abg = tools.getUUID( uuidDB, "UnitCellReciprocalAngles", "UnitCellReciprocalAngles_" + zeoname )
    output.append( [ uuid_uc_r_abg, "Instance", "UnitCellReciprocalAngles", "", "", "" ] )
 
    uuid_uc_r_al = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalAngleAlpha_" + zeoname )
    output.append( [ uuid_uc_r_al, "Instance", "VectorComponent", "", "", "" ] )
 
    uuid_uc_r_be = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalAngleBeta_" + zeoname )
    output.append( [ uuid_uc_r_be, "Instance", "VectorComponent", "", "", "" ] )

    uuid_uc_r_gm = tools.getUUID( uuidDB, "VectorComponent", "UnitCellReciprocalAngleGamma_" + zeoname )
    output.append( [ uuid_uc_r_gm, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_r_abg,  
                     self.ontoPrefix + "hasUnitCellReciprocalAngles", "", "" ] )

    output.append( [ uuid_uc_r_abg, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/degree",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ uuid_uc_r_abg, "Instance", uuid_uc_r_al,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_r_abg, "Instance", uuid_uc_r_be,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_r_abg, "Instance", uuid_uc_r_gm,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_al, "", "alpha", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_al, "", 
                     round(structure.lattice.reciprocal_lattice.alpha, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_be, "", "beta",  "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_be, "", 
                     round(structure.lattice.reciprocal_lattice.beta,  12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_uc_r_gm, "", "gamma", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_uc_r_gm, "", 
                     round(structure.lattice.reciprocal_lattice.gamma, 12) , "decimal" ] )
    ###########################################

    # Vector to keep three Unit Cell vectors (a,b,c):
    uuid_uc_vec_abc = tools.getUUID( uuidDB, "UnitCellVectorSet", "UnitCellVectors_" + zeoname )
    output.append( [ uuid_uc_vec_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_abc,  
                     self.ontoPrefix + "hasUnitCellVectorSet", "", "" ] )

    uuid_vec_a = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorA_" + zeoname )
    uuid_vec_b = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorB_" + zeoname )
    uuid_vec_c = tools.getUUID( uuidDB, "UnitCellVector", "UnitCellVectorC_" + zeoname )

    output.append( [ uuid_vec_a, "Instance", "UnitCellVector", "", "", "" ] )
    output.append( [ uuid_vec_b, "Instance", "UnitCellVector", "", "", "" ] )
    output.append( [ uuid_vec_c, "Instance", "UnitCellVector", "", "", "" ] )

    output.append( [ uuid_uc_vec_abc, "Instance", uuid_vec_a,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_abc, "Instance", uuid_vec_b,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_abc, "Instance", uuid_vec_c,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_a, "", "a", "string" ] )
    output.append( [ uuid_vec_a, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_b, "", "b", "string" ] )
    output.append( [ uuid_vec_b, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_c, "", "c", "string" ] )
    output.append( [ uuid_vec_c, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/angstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    uuid_vec_ax = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAx_" + zeoname )
    uuid_vec_ay = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAy_" + zeoname )
    uuid_vec_az = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorAz_" + zeoname )

    uuid_vec_bx = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBx_" + zeoname )
    uuid_vec_by = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBy_" + zeoname )
    uuid_vec_bz = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorBz_" + zeoname )

    uuid_vec_cx = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCx_" + zeoname )
    uuid_vec_cy = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCy_" + zeoname )
    uuid_vec_cz = tools.getUUID( uuidDB, "VectorComponent", "UnitCellVectorCz_" + zeoname )


    output.append( [ uuid_vec_ax, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_ay, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_az, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_bx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_by, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_bz, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_cx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_cy, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_cz, "Instance", "VectorComponent", "", "", "" ] )


    output.append( [ uuid_vec_a, "Instance", uuid_vec_ax,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", uuid_vec_ay,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_a, "Instance", uuid_vec_az,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ uuid_vec_b, "Instance", uuid_vec_bx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", uuid_vec_by,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_b, "Instance", uuid_vec_bz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ uuid_vec_c, "Instance", uuid_vec_cx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_c, "Instance", uuid_vec_cy,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_c, "Instance", uuid_vec_cz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    
    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_ax, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_ax, "", 
                     round(structure.lattice.matrix[0][0], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_ay, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_ay, "", 
                     round(structure.lattice.matrix[0][1], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_az, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_az, "", 
                     round(structure.lattice.matrix[0][2], 12) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_bx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_bx, "", 
                     round(structure.lattice.matrix[1][0], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_by, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_by, "", 
                     round(structure.lattice.matrix[1][1], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_bz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_bz, "", 
                     round(structure.lattice.matrix[1][2], 12) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cx, "", 
                     round(structure.lattice.matrix[2][0], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cy, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cy, "", 
                     round(structure.lattice.matrix[2][1], 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_cz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_cz, "", 
                     round(structure.lattice.matrix[2][2], 12) , "decimal" ] )

    ###########################################

    # Vector for Reciprocal Unit Cell vectors (a*,b*,c*):
    uuid_uc_vec_r_abc = tools.getUUID( uuidDB, "UnitCellVectorSet", "ReciprocalUnitCellVectors_" + zeoname )
    output.append( [ uuid_uc_vec_r_abc, "Instance", "UnitCellVectorSet", "", "", "" ] )
 
    output.append( [ uuid_cif_uc, "Instance", uuid_uc_vec_r_abc,  
                     self.ontoPrefix + "hasReciprocalUnitCellVectorSet", "", "" ] )

    uuid_vec_r_a  = tools.getUUID( uuidDB, "UnitCellVector", "ReciprocalUnitCellVectorA_" + zeoname )
    uuid_vec_r_b  = tools.getUUID( uuidDB, "UnitCellVector", "ReciprocalUnitCellVectorB_" + zeoname )
    uuid_vec_r_c  = tools.getUUID( uuidDB, "UnitCellVector", "ReciprocalUnitCellVectorC_" + zeoname )

    output.append( [ uuid_vec_r_a, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_b, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_c, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_uc_vec_r_abc, "Instance", uuid_vec_r_a,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_r_abc, "Instance", uuid_vec_r_b,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    output.append( [ uuid_uc_vec_r_abc, "Instance", uuid_vec_r_c,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_a, "", "a", "string" ] )

    output.append( [ uuid_vec_r_a, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_b, "", "b", "string" ] )
    output.append( [ uuid_vec_r_b, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_c, "", "c", "string" ] )
    output.append( [ uuid_vec_r_c, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/reciprocalAngstrom",
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )


    uuid_vec_r_ax = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorAx_" + zeoname )
    uuid_vec_r_ay = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorAy_" + zeoname )
    uuid_vec_r_az = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorAz_" + zeoname )

    uuid_vec_r_bx = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorBx_" + zeoname )
    uuid_vec_r_by = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorBy_" + zeoname )
    uuid_vec_r_bz = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorBz_" + zeoname )

    uuid_vec_r_cx = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorCx_" + zeoname )
    uuid_vec_r_cy = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorCy_" + zeoname )
    uuid_vec_r_cz = tools.getUUID( uuidDB, "VectorComponent", "ReciprocalUnitCellVectorCz_" + zeoname )

    output.append( [ uuid_vec_r_ax, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_ay, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_az, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_r_bx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_by, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_bz, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_r_cx, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_cy, "Instance", "VectorComponent", "", "", "" ] )
    output.append( [ uuid_vec_r_cz, "Instance", "VectorComponent", "", "", "" ] )

    output.append( [ uuid_vec_r_a, "Instance", uuid_vec_r_ax,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_a, "Instance", uuid_vec_r_ay,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_a, "Instance", uuid_vec_r_az,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ uuid_vec_r_b, "Instance", uuid_vec_r_bx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_b, "Instance", uuid_vec_r_by,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_b, "Instance", uuid_vec_r_bz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )


    output.append( [ uuid_vec_r_c, "Instance", uuid_vec_r_cx,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_c, "Instance", uuid_vec_r_cy,  
                     self.ontoPrefix + "hasComponent", "", "" ] )

    output.append( [ uuid_vec_r_c, "Instance", uuid_vec_r_cz,  
                     self.ontoPrefix + "hasComponent", "", "" ] )
    
    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_ax, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_ax, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[0][0]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_ay, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_ay, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[0][1]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_az, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_az, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[0][2]/TWOPI, 12) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_bx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_bx, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[1][0]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_by, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_by, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[1][1]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_bz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_bz, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[1][2]/TWOPI, 12) , "decimal" ] )


    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_cx, "", "x", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_cx, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[2][0]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_cy, "", "y", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_cy, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[2][1]/TWOPI, 12) , "decimal" ] )

    output.append( [ self.ontoPrefix + "hasLabel", "Data Property", 
                     uuid_vec_r_cz, "", "z", "string" ] )
    output.append( [ self.ontoPrefix + "hasValue", "Data Property", 
                     uuid_vec_r_cz, "", 
                     round(structure.lattice.reciprocal_lattice.matrix[2][2]/TWOPI, 12) , "decimal" ] )

    ###########################################

    # Unit cell volume, single value, not vector:

    uuid_uc_volume = tools.getUUID( uuidDB, 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure", 
                     "UnitCellVolume_" + zeoname )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/Measure",
                     "", "", "" ] )

    output.append( [ uuid_cif_uc, "Instance", uuid_uc_volume,  
                     self.ontoPrefix + "hasVolume", "", "" ] )

    output.append( [ "http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue", 
                     "Data Property", uuid_uc_volume, "", 
                     round(structure.lattice.volume, 12) , "decimal" ] )

    output.append( [ uuid_uc_volume, "Instance", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/cubicAngstrom", 
                     "http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit",
                      "", "" ] )

    ###########################################

    ###########################################

    #tools.saveUUID( uuidDB )

    return output
    pass # CsvMaker.arrUnitCell()

  def arrTiles( self, zeoname ):
    output = []

    #uuidDB = tools.loadUUID()
    uuidDB = self.uuidDB

    path = os.path.join( self.inputDir, "Tile-signature-2023.csv" )
    dataIn = tools.readCsv( path )

    data = tilesignature.getDataByCode( dataIn, zeoname )

    uuid_zeolite = tools.getUUID( uuidDB, "ZeoliteFramework", "Zeolite_" + zeoname )

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
      uuid_tile     = tools.getUUID( uuidDB, "Tile", "Tile_" + zeoname + "_cage" + str(ic+1) )
      uuid_tile_num = tools.getUUID( uuidDB, "TileNumber", "TileNumber_" + zeoname + "_cage" + str(ic+1) )

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


    #tools.saveUUID( uuidDB )

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

  # Remove error bars from the CIF file and save the result to a temporary file
  def splitErrorBarLoop( self, line, headers, file_line ):
    #print( "Running error bar loop", line )
    #words = line.split()
    words = self.strSplit( line )

    if len(words) == 0:
      logging.error( " Empty line " + file_line )
      return line

    if len(words) != len(headers):
      logging.error( " Number of elements on line is different from the " +
                     "definition of the loop: " + str(len(words)) +" vs " + 
                     str(len(headers)) + ": " + str(words) + " vs " + str(headers) +
                     " " + file_line + "." )
      return line

    lineNew = line
    #for i in range( len(headers) ):
    for ih, h in enumerate(headers):
      if h in self.entriesToCheck:
        #logging.warning( " Found one of the entries" )
        vOut, eOut = self.splitErrorBar( words[ih], file_line )
        pos = line.find( words[ih] )
        lineNew = lineNew.replace( words[ih], vOut )

    print( "lineNew =", lineNew )
    return lineNew


  def splitStr( self, value ):

    pos1 = value.find( "(" )
    pos2 = value.find( ")" )
    vOut = value[:pos1] + value[pos2+1:]
    eOut = value[pos1+1:pos2]

    ''' This sometimes return value like 0.007000000001, last digit round effect
    n = len(eOut )
    vOut = vOut
    v = 0
    factor = ""
    for ix, x in enumerate(vOut):
      if ix == len(vOut) - 1:
        factor += "1"
      elif "." == x:
        factor += "."
      else:
        factor += "0"
    eOut = str( float(factor) * float(eOut) )
    '''

    #iv = len(vOut) - 1
    #ie = len(eOut) - 1
    ie = 1
    factor = []
    for ix,x in enumerate(vOut[::-1]):
      if "." == x:
        factor.insert( 0, "." )
      elif ie <= len( eOut ):
        factor.insert( 0, eOut[-ie] )
        ie += 1
      else:
        factor.insert( 0, "0" )
        ie += 1

    #print( factor )
    factor = "".join( factor )
    if factor.find( "." ) < 0:
      eOut = str( int(factor) )
    else:
      eOut = str( float(factor) )
    #for i in range( len( eOut ) ):
      #vOut[-i-1] = eOut[-i-1]
    return vOut, eOut
    pass

  def splitErrorBar( self, value, file_line ):
    #print( "Starting splitErrorBar() ================== " )
    vOut = value 
    eOut = ""

    if not isinstance( value, str ):
      logging.error( " Impossible error: not a string " + line + " " 
                     + file_line )
      return vOut,eOut

    pos1 = value.find( "(" )
    pos2 = value.find( ")" )
    if pos1 < 0 and pos2 < 0:
      logging.info( " Brackets are not detected. " + file_line )
    elif pos1 >= 0 and pos2 >= 0 and pos1 < pos2:
      #print( "pos1 = ", pos1, " pos2 = ", pos2 )
      vOut,eOut = self.splitStr(value) 
    else:
      logging.error( " Something is wrong with brackets: '" + line + 
                     "' . " + file_line )

    #print( "value, vOut, eOut =", value, vOut, eOut )
    return vOut, eOut
    pass

  def cleanCif( self ):
    fileIn  = os.path.join( "test", "913885.cif" )
    fileOut = "after-913885.cif"

    self.entriesToCheck = [ "_cell_length_a", "_cell_length_b", "_cell_length_c", 
                  "_cell_angle_alpha", "_cell_angle_beta", "_cell_angle_gamma",
                  "_cell_volume", "_cell_measurement_temperature",
                  "_diffrn_ambient_temperature",
                  "_atom_site_fract_x", "_atom_site_fract_y", "_atom_site_fract_z",
                  "_atom_site_U_iso_or_equiv",
                  "_atom_site_aniso_U_11", "_atom_site_aniso_U_22", 
                  "_atom_site_aniso_U_33", "_atom_site_aniso_U_23", 
                  "_atom_site_aniso_U_13", "_atom_site_aniso_U_12", 
                  "_geom_bond_distance", "_geom_angle", 
                  "_refine_ls_extinction_coef"
                  ]

    if not os.path.isfile( fileIn ):
      logging.error( " File '" + fileIn + "' does not exist in cleanCif()." )
      return

    f = open( fileIn )
    fOut = open( fileOut, "w", encoding="utf8" )

    inLoop = False

    for il, line in enumerate(f):
      file_line = "In file '" + fileIn + "' line " + str(il+1)
      #print( file_line )

      if il >= 66:
        logging.debug( " Break after 75 lines for testing. " + file_line )
        break
        pass

      pos1 = line.find( "#" )
      pos2 = line.find( ";" )
      if   0 == pos1:
        logging.info( " Found a comment '" + line.strip() + "'. " + file_line )
        #short = line[ :pos] + "\n"
        fOut.write( line )
        continue

      elif 0 == pos2:
        logging.info( " Found a comment '" + line.strip() + "'. " + file_line )
        #short = line[ :pos] + "\n"
        fOut.write( line )
        continue

      elif pos1 > 0 or pos2 > 0:
        logging.error( file_line + " Comment starts not from line start. " + 
                       "This is not supported by cleanCif()." )
        fOut.write( line )
        continue

      #words = line.strip().split()
      words = tools.strSplit( line )
      #print( "on line", il, "words:", words )
      
      if len(words) == 0:
        #logging.info( " Empty string " + file_line )
        fOut.write( line )
        inLoop = False
        continue
        pass

      elif "loop_" == words[0]:
        #logging.info( " In 'loop_' option" )
        inLoop = True
        inLoopHead = True
        inLoopBody = False
        self.loopHeaders = []
        #fOut.write( "added 'loop_': " + line )
        fOut.write( line )
        continue

      elif inLoop:
        #logging.info( " In inLoop option" )
        
        if inLoopHead:
          if "_" == words[0][0]:
            self.loopHeaders.append( words[0] )
            fOut.write( line )
          else:
            inLoopHead = False
            inLoopBody = True

        if inLoopBody:
          if "_" == words[0][0]:
            inLoop = False
            logging.info( " inLoop is set to False" )
            fOut.write( line )
            #self.loopHeaders = []
            
          else:
            lineNew = self.splitErrorBarLoop( line, self.loopHeaders, file_line )
            fOut.write( lineNew )
            continue
            pass

          pass

        print( "headers =", self.loopHeaders )

      elif len(words) > 2:
        #logging.info( " Length of string = " + str(len(words)) + " " + file_line )
        fOut.write( line )
        pass

      elif "_" == words[0][0]:
        if False == inLoop:
          if len(words) == 1:
            #logging.info( " Only 1 entry in '" + line.strip() + "' " + file_line + ". I skip this case." )
            fOut.write( line )
            continue
          elif len(words) > 2:
            #logging.info( " More than 2 entries in '" + line.strip() + "' " + file_line + ". I skip this case." )
            fOut.write( line )
            continue
            
          elif words[0] in self.entriesToCheck:
            #logging.info( " Found one of the entries" )
            vOut, eOut = self.splitErrorBar( words[1], file_line )
            pos = line.find( words[1] )

            newLine = line.replace( words[1], vOut )
            fOut.write( newLine )
            #fOut.write( line[:pos] + vOut + "\n" )
            continue

          else:
            logging.warning( " Unknown situation. Line = '" + line.strip() + 
                             "'. " + file_line + "." )
            fOut.write( line )
        else:
          fOut.write( line )

      elif len(words) == 2:
        #logging.warning( " Length of string = " + str(len(words)) + " " + file_line )
        fOut.write( line )
        pass

      else:
        #logging.warning( " default else option." )
        fOut.write( line )
        pass

    f.close()
    fOut.close()

    pass


  def makeCsvs( self ):
    errCount = 0

    self.prepare()

    for z in self.zeoList:
      #print( "In zeolist z =", z )
      arr  = self.arrInit( z )
      #arr += self.arrTiles( z )
      arr += self.arrUnitCell( z )
      #arr += self.arrAtomSite( z )
      #arr += self.arrTransform( z )

      #print( "arr =", arr )
      #csvWrite( arr )
      path = os.path.join( self.outputDir, z + ".csv" )
      tools.writeCsv( path, arr )

      #errCount += 1
      #logging.warning( "Not implemented creating zeolite csv" )
      pass

    self.finalize() # << Important! Saves the current list of the UUIDs

    if errCount > 0:
      logging.warning( "Detected " + str(errCount) + " errors" )

    pass # CsvMaker.makeCsvs()

  pass # class CsvMaker


if __name__ == "__main__":
  
  a = CsvMaker()

  #a.makeCsvs()

  a.cleanCif()

  #input_strings = [ "123450(16)", "123450(160)", "1234.5(16)", "123.45(16)", "12.345(16)"]
  #for input_string in input_strings:
  #  value, error = a.splitStr( input_string )
  #  print(f"Input: {input_string}, Value: {value}, Error: {error}")
    
  #input_strings = [ "1 2 3 4 5", "1   2  3   4  5\n", "1 '2 3' 4 5", "1 2 3 '4   5'"  ]
  #for input_string in input_strings:
  #  out = tools.strSplit(input_string )
  #  print( f"Input: {input_string}, Out: {str(out)}" )
  #  #print( out )


