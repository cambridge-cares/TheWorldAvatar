"""
TODO
  - Use variable 'verbose' to display error and warning messages.
 
  - Allow to combine several .xlsx files to make a single csv file.
    - There is a problem of property that has the same name but different Domain/Range (use UNION)
      If different parts use the same property this should be addressed properly.
      - One way to fix it: add property "Combined Comment"
    - Need to check the tbox details (header of the file)
    - Does each .xlsx must be valid by itself?
    - Is DataProperty must be defined through UNION? Or is it properly converted to owl/Protege?
      This I need to test. And may be ask Feroz.

  - Check if names or types contain spaces, comma, tab characters.
    - comment is allowed to have different values.

  - Remove warnings related to the code.

  - Add a single file test-case warnings.xlsx to test all the warnings in the code.

  - Fix TODO notes in the code.
  .

  - DONE In validationXXX functions add warning for repeating entry.

  - DONE Rename the module to TBoxTools:
    - DONE name of the class
    - DONE name of the file
    - DONE external calls from other python programs using this module
 

"""

import pandas  # to read xlsx files
import csv     # to save csv files
import math    # to detect NaN values
import sys     # for command line arguments (sys.argv)
import os      # To detect file, to check filename, etc.

import logging # For error/waarning messages
logging.basicConfig( level=logging.INFO )
#logging.basicConfig( level=logging.WARNING )
#logging.basicConfig( level=logging.ERROR )
#logging.disable( logging.CRITICAL )

#import tools

COLUMN_LETTERS = "ABCDEFGHIJ"
COLUMN_HEADERS = [ "Source",      "Type",  "Target",   "Relation", "Domain",
                   "Range", "Quantifier", "Comment", "Defined By", "Label" ]


PREFIX_XSD  = "http://www.w3.org/2001/XMLSchema#"
PREFIX_RDFS = "http://www.w3.org/2000/01/rdf-schema#"
#logging.warning( " Not supported PREFIX_RDF " )
PREFIX_RDF  = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#DATA_TYPES = dict()
DATA_TYPES_XSD  = dict()
DATA_TYPES_XSD["anyuri"] = "anyURI"
DATA_TYPES_XSD["boolean"] = "boolean"
DATA_TYPES_XSD["byte"] = "byte"
DATA_TYPES_XSD["datetime"] = "dateTime"
DATA_TYPES_XSD["datetimestamp"] = "dateTimeStamp"
DATA_TYPES_XSD["decimal"] = "decimal"
DATA_TYPES_XSD["double"] = "double"
DATA_TYPES_XSD["float"] = "float"
DATA_TYPES_XSD["int"] = "int"
DATA_TYPES_XSD["integer"] = "integer"
DATA_TYPES_XSD["long"] = "long"
DATA_TYPES_XSD["short"] = "short"
DATA_TYPES_XSD["string"]  = "string"

DATA_TYPES_RDF  = dict()
DATA_TYPES_RDF["langstring"] = "langString"
DATA_TYPES_RDF["plainliteral"] = "PlainLiteral"
DATA_TYPES_RDF["xmlliteral"] = "XMLLiteral"
DATA_TYPES_RDFS = dict()
DATA_TYPES_RDFS["literal"] = "Literal"

DATA_PROPS = dict() # These all are RDFS
#DATA_TYPES_RDFS["label"]   = "label"
DATA_PROPS['range'       ] = 'range'
DATA_PROPS['domain'      ] = 'domain'
DATA_PROPS['type'        ] = 'type'
DATA_PROPS['subclassof'  ] = 'subClassOf'
DATA_PROPS['subpropertyof'] = 'subPropertyOf'
DATA_PROPS['label'       ] = 'label'
DATA_PROPS['comment'     ] = 'comment'

# These are classes, not properties.
#DATA_PROP['seealso'     ] = 'seealso'
#DATA_PROP['isdefinedby' ] = 'isDefinedBy'
#DATA_PROP['value'       ] = 'value'
#DATA_PROP['member'      ] = 'member'
#DATA_PROP['datatype'    ] = 'Datatype'
#DATA_PROP['resource'    ] = 'Resource'
#DATA_PROP['class'       ] = 'Class'



#df = pandas.read_excel( filename )
#df  = pandas.DataFrame( tmp )

#print( "Columns = ", df.columns )

#print( "Head = ", df.head() )   # Works, but only 5 lines. 

#print( "keys =", vars(df) )

#print( "Source = ", df["Source"] )
#s = df.sheets()
for i in range( 3 ):
    #print( "row i = ", i, df.iloc[i] )
    pass

#rows = df.loc[i]
#values = df.loc[i].to_string(index = False, header=False )
#print( "values = ", values )

#print( "The size of input file '" + filename + "' is", df.shape, "." )

"""
tmp = []
for i in range( 5 ):
    v = df.loc[i].to_string(index = False, header=False )
    tmp.append( v.replace( "\n","").strip() )
print( tmp )
"""

def is_http( name ):
    output = name
    if not isinstance( name, str ):
        return False
    if name.lower().strip().startswith( "http://" ) or \
       name.lower().strip().startswith( "https://" ):
        return True
    return False


class TBoxTools:
    __slots__ = [ "fileIn",  "dataIn", "classes", "clNames", "objProp", "opNames", 
                  "datProp", "dpNames", "nCol", "onto", "ontoNames", "errCount",
                  "dataOut", "headers",
                  "objPropDomain", "objPropRange", "datPropDomain", "datPropRange",
                  "triples", "tbox", "classRel",
                  "verbose"
                ]
    def __init__( self, verbose = True ):
        self.cleanAll()

        self.verbose = verbose

        pass # TBoxTools.__init__()

    def cleanAll( self ):
        self.dataIn    = []
        self.headers   = []
        self.onto      = []
        self.ontoNames = []
        self.classes   = []
        self.clNames   = []
        self.objProp   = []
        self.opNames   = []
        self.datProp   = []
        self.dpNames   = []

        self.nCol    = 10
        self.fileIn  = ""

        self.datPropDomain = []
        self.datPropRange  = []
        self.objPropDomain = []
        self.objPropRange  = []

        self.tbox          = ""  # TBox path

        self.triples       = {}  # Structure of this dictionary is 
                                 # self.triples["hasXXX"]["domain"] = []
                                 # self.triples["hasXXX"]["range" ] = []
                                 #

        self.classRel      = {}  # Structure of this  dictionary is
                                 # self.classRel[shortName]["path"] = full path
                                 # self.classRel[shortName]["is-a"] = [] all paths
                                 # For built-in datatypes for Data Properties 
                                 # use low-case without prefix for 'short'
                                 # and full path for 'path'.
        self.classRel["owl:Thing"] = {}

        # Global pre-defined classes:
        self.classRel["owl:Thing"]["path"] = "owl:Thing"
        self.classRel["owl:Thing"]["is-a"] = []
        self.clNames.append( "owl:Thing" )
 
        self.verbose       = False

        pass # TBoxTools.cleanAll()


    def readExcel( self, filename ):
        fBase, fExt = os.path.splitext( filename )
        if ".xlsx" == fExt.lower():
            self.readXlsx( filename )
        elif ".csv" == fExt.lower():
            self.readCsv( filename )
        else:
            logging.error( " Unknown file extension in '" + filename + "'." )

        pass # TBoxTools.readExcel()

    def readCsv( self, filename ):

        if not os.path.isfile( filename ):
            self.fileIn = ""
            logging.error( " Input file '" + filename + "' does not exist." )
            return 

        self.fileIn = filename
        df = pandas.read_csv( filename, encoding='unicode_escape' )
        logging.info( "The size of input file '" + filename + "' is " + str(df.shape) + "." )
 
        self.dataIn = []
        self.dataIn.append( df.columns )
        for index, row in df.iterrows():
            #print(row)
            #print(row.to_dict())
            r = row.to_dict()
            line = [ r[k] for k in r.keys() ]
            #print( line )
            self.dataIn.append( line )
        #print( self.dataIn )
        pass # TBoxTools.readXlsx()


        pass # TBoxTools.readCsv()

    def readXlsx( self, filename ):

        if not os.path.isfile( filename ):
            self.fileIn = ""
            logging.error( " Input file '" + filename + "' does not exist." )
            return 

        self.fileIn = filename
        df = pandas.read_excel( filename )
        logging.info( "The size of input file '" + filename + "' is " + str(df.shape) + "." )
        
        self.dataIn = []
        self.dataIn.append( df.columns )
        for index, row in df.iterrows():
            #print(row)
            #print(row.to_dict())
            r = row.to_dict()
            line = [ r[k] for k in r.keys() ]
            #print( line )
            self.dataIn.append( line )
        #print( self.dataIn )
        pass # TBoxTools.readXlsx()

    def isEmptyCell( self, cell ):
        """ Returns True if the input string 'cell' is empty or nan.
        """
        #print( "cell value = '" + str(cell) + "', type =", type(cell) )
        if isinstance( cell, float ):
            if math.isnan( cell ):
                return True
        elif isinstance( cell, str ):
            short = cell.strip().lower()
            if "nan" == short or "" == short:
                return True
        else:
            logging.warning( " Unknown type of '" + cell + "': " + type(cell) )
        return False
        #return True

        pass # TBoxTools.isEmptyCell()

    def isCellBValue( self, line, value, file_line ):
        if len(line) < 2:
            return False

        if not isinstance( line[1], str ):
            # Here NaN value of type float if the cell is empty.
            return False

        i = 1
        expect = value

        if expect.lower() == line[i].lower():
            if expect != line[i]:
                logging.warning( " Wrong input " + file_line + " col " + \
                                 COLUMN_LETTERS[i] + ": '" + line[i] + 
                                 "' but expected '" + expect + "'." )
            return True
        else:
            return False
        pass # TBoxTools.isCellBValue()
 
    def isLineOntology( self, line, file_line ):
        return self.isCellBValue( line, "TBox", file_line )
        pass # TBoxTools.isLineOntology()
       
    def isLineClass( self, line, file_line ):
        return self.isCellBValue( line, "Class", file_line )

        pass # TBoxTools.isLineClass()

    def isLineObjProp( self, line, file_line ):
        return self.isCellBValue( line, "Object Property", file_line )
        pass # TBoxTools.isLineObjProp()

    def isLineDataProp( self, line, file_line ):
        return self.isCellBValue( line, "Data Property", file_line )
        pass # TBoxTools.isLineDataProp()

    def isKnownClass( self, value, strict = False ):
        for c in self.clNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # TBoxTools.isKnownClass()

    def isKnownObjProp( self, value, strict = False ):
        for c in self.opNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # TBoxTools.isKnownObjProp()

    def isKnownDataProp( self, value, strict = False ):
        for c in self.dpNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # TBoxTools.isKnownObjProp()

    def validateHeaders( self, headers, file_line ):
        """ Function returns number of errors + warnings detected. 

        """
        errCount = 0

        # Must be exactly 10 headers:
        if len(headers) != len(COLUMN_HEADERS):
            logging.error( " The header row has size " + str(len(headers)) + \
                           " but expected " + str(len(COLUMN_HEADERS)) + \
                           " " + file_line + "." )
            errCount += 1

        # Headers must match the standard values, including capital letters:
        for i in range(min(len(headers),len(COLUMN_HEADERS) )):

            if   headers[i].strip() == COLUMN_HEADERS[i].strip():
                # Do nothing, this header cell is correct.
                pass
            elif headers[i].strip().lower() != COLUMN_HEADERS[i].strip().lower():
                logging.warning( " Wrong header value " + file_line + \
                               " col " + COLUMN_LETTERS[i] + ": '" + headers[i] + \
                               " ', but expected '" + COLUMN_HEADERS[i] + "'." )
                errCount += 1
            else:
                logging.error( " Wrong header value " + file_line + \
                               " col " + COLUMN_LETTERS[i] + ": '" + headers[i] + \
                               " ', but expected '" + COLUMN_HEADERS[i] + "'." )
                errCount += 1

        return errCount
        pass # TBoxTools.validateHeaders()

    def validateOntoLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines.

        """
        errCount = 0

        # TODO see function description
        logging.warning( " ontology validation is not implemented" )
        errCount += 1

        # Check the exact matching the property type:
        if "class" == line[1].strip().lower():
            if not "TBox" == line[1]:
                logging.warning( " Expected 'TBox' but found '" + 
                                 line[1] + "' " + file_line + "." )
                errCount += 1

        # TODO Col 0, 1, 2, 3 must be present, and be checked.


        # Other columns are empty:
        for i in [ 5, 6, 7, 8, 9 ]:
            if not self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + ". ErrOnt." )
                errCount += 1

        return errCount
        pass # TBoxTools.validateOntoLine()

    def validateClassLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """

        # DONE class name should not be repeated, and don't match the obj prop and dat prop

        errCount = 0

        # Check the exact matching the property type:
        if "class" == line[1].strip().lower():
            if not "Class" == line[1]:
                logging.warning( " Expected 'Class' but found '" + 
                                 line[1] + "' " + file_line + "." )
                errCount += 1

        # TODO columns 0,1 are required

        # Check IS-A Relation of the classes (columns 2,3):
        if self.isEmptyCell( line[3] ):
            if not self.isEmptyCell( line[2] ):
                logging.warning( " Found an 'IS-A' class '" + 
                                 line[2] + "' without the 'IS-A' keyword " + \
                                 file_line + "." )
                errCount += 1
            pass
        else:
            short = str(line[3]).strip()
            if "is-a" == short.lower():
                if "IS-A" != short:
                    logging.warning( " Expected 'IS-A' but found '" + 
                                     line[3] + "' " + file_line + "." )
                    errCount += 1
                if self.isEmptyCell( line[2] ):
                    logging.error( " Missing relation class for class '" + line[0] + "' " + \
                                   file_line + " col " + COLUMN_LETTERS[3] + "." )
                    errCount += 1
                else:
                    if not self.isKnownClass( line[2], strict = True ):
                        logging.error( " Unknown 'IS-A' class name '" + line[2] + "' " + \
                                         file_line + "." )
                        errCount += 1
 
            else:
                # Make warning for is-a and error for neither is-a nor IS-A:
                logging.error( "Unknown Relation keyword '" + short + \
                               "' for class '" + line[0] + "' " +\
                               file_line + " col " + COLUMN_LETTERS[3] + "." )
                errCount += 1

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for class '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1
        elif isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Class '" + line[0] + "' may have incompete 'comment': '" + \
                  line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Wrong Comment '" + line[7] + "' for class '" + \
                             line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing '" + COLUMN_HEADERS[8] + "' for class '" + \
                           line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Class '" + line[0] + "' may have incompete '" + COLUMN_HEADERS[8] + "': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Check whether 'Label' exists (column 9):
        if self.isEmptyCell(line[9]):
            logging.error( " Missing '" + COLUMN_HEADERS[9] + "' for class '" + \
                           str(line[0]) + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[9] + "." )
            errCount += 1
        elif isinstance( line[9], str ):
            if len( line[9] ) < 4:
                logging.warning( " Class '" + str(line[0]) + "' may have incompete '" + COLUMN_HEADERS[9] + "': '" + \
                                 line[9] + "' " + file_line + " col " + COLUMN_LETTERS[9] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 4, 5, 6 ]:
            if not self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + ". Err1." )
                errCount += 1


        # Check repeating entries of same category:
        if self.isKnownClass( line[0], strict = True ):
            logging.warning( " Repeating class name '" + line[0] + "' " + \
                             file_line + "." )
            errCount += 1
        elif self.isKnownClass( line[0], strict = False ):
            logging.warning( " Class name '" + line[0] + "' repeats an existing name, " + \
                             "but uses different capital letters " + \
                             file_line + "." )
            errCount += 1

        # Check repeating entries of other category:
        if self.isKnownObjProp ( line[0], strict = False ) or \
           self.isKnownDataProp( line[0], strict = False ):
            logging.warning( " Data Prop name '" + line[0] + "' repeats an existing name " + \
                             "in different category (ObjProp/DataProp) " + \
                             file_line + "." )
            errCount += 1

        return errCount
        pass # TBoxTools.validateClassLine()

    def validateObjLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """

        errCount = 0

        #logging.warning( " object property validation is not implemented" )
        #errCount += 1

        # Check the exact matching the property type:
        if "Object Property" == line[1]:
            # Do nothing, this is correct value
            pass
        elif "object property" == line[1].strip().lower():
            logging.warning( " Expected 'Object Property' but found '" + \
                             line[1] + "' " + file_line + "." )
            errCount += 1

        # TODO Columns 0,1,4,5 are required.

        # TODO Column 6 is optional.
        # TODO cell 6 can be empty or "only"
        if self.isEmptyCell(line[6]):
            pass
        elif "only" == line[6]:
            pass
        else:
            logging.error( " Invalid value '" + line[6] + "' in " + file_line + \
                           " in col " + COLUMN_LETTERS[6] + "." )
            errCount += 1                

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for Obj Property '" + line[0] + \
                           "' " + file_line + " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1
        elif isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Obj Property '" + line[0] + "' may have " + \
                                 "incompete 'comment': '" + line[7] + "' " + \
                                 file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Wrong Comment '" + line[7] + "' for Obj Property '" + \
                             line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing '" + COLUMN_HEADERS[8] + \
                           "' for Obj Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Obj Property '" + line[0] + "' may have incompete '" + COLUMN_HEADERS[8] +"': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Check whether 'Label' exists (column 9):
        if self.isEmptyCell(line[9]):
            logging.error( " Missing '" + COLUMN_HEADERS[9] + "' for Obj Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[9] + "." )
            errCount += 1
        elif isinstance( line[9], str ):
            if len( line[9] ) < 4:
                logging.warning( " Obj Property '" + line[0] + "' may have incompete '" + COLUMN_HEADERS[9] +"': '" + \
                                 line[9] + "' " + file_line + " col " + COLUMN_LETTERS[9] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3 ]:
            if not self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + ". Err2." )
                errCount += 1
 
        # Check repeating entries:

        # Check repeating entries of same category:
        if self.isKnownObjProp( line[0], strict = True ):
            logging.warning( " Repeating Obj Prop name '" + line[0] + "' " + \
                             file_line + "." )
            errCount += 1
        elif self.isKnownObjProp( line[0], strict = False ):
            logging.warning( " Obj Prop name '" + line[0] + "' repeats an existing name, " + \
                             "but uses different capital letters " + \
                             file_line + "." )
            errCount += 1

        # Check repeating entries of other category:
        if self.isKnownClass(    line[0], strict = False ) or \
           self.isKnownDataProp( line[0], strict = False ):
            logging.warning( " Data Prop name '" + line[0] + "' repeats an existing name " + \
                             "in different category (Class/DataProp) " + \
                             file_line + "." )
            errCount += 1


        return errCount
        pass # TBoxTools.validateObjLine()


    def validateDataLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

            Columns 0,1,4,5 are required.
        """
        errCount = 0

        #logging.warning( " data property validation is not implemented" )

        # Check the exact matching the property type:
        if "data property" == line[1].strip().lower():
            if not "Data Property" == line[1]:
                logging.warning( " Expected 'Data Property' but found '" + 
                                 line[1] + "' " + file_line + "." )
                errCount += 1

        # Check whether 'Comment' exists (column 7):
        if self.isEmptyCell(line[7]):
            logging.error( " Missing Comment for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        elif isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning( " Data Property '" + line[0] + "' may have incompete 'comment': '" + \
                  line[7] + "' " + file_line + " col " + COLUMN_LETTERS[7] + "." )
                errCount += 1
        else:
            logging.warning( " Wrong Comment '" + line[7] + "' for Data Property '" + \
                             line[0] + "' " + file_line + \
                             " in col " + COLUMN_LETTERS[7] + "." )
            errCount += 1

        # Check whether 'Defined In' exists (column 8):
        if self.isEmptyCell(line[8]):
            logging.error( " Missing '" + COLUMN_HEADERS[8] + "' for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Data Property '" + line[0] + \
                                 "' may have incompete '" + COLUMN_HEADERS[8] + "': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Check whether 'Label' exists (column 9):
        if self.isEmptyCell(line[9]):
            logging.error( " Missing '" + COLUMN_HEADERS[9] + "' for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[9] + "." )
            errCount += 1
        elif isinstance( line[9], str ):
            if len( line[9] ) < 4:
                logging.warning( " Data Property '" + line[0] + \
                                 "' may have incompete '" + COLUMN_HEADERS[9] + "': '" + \
                                 line[9] + "' " + file_line + " col " + COLUMN_LETTERS[9] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3, 6 ]:
            if not self.isEmptyCell( line[i] ):
                logging.warning( " Expecting empty cell " + COLUMN_LETTERS[i] + \
                                 " but got '" + str(line[i]) + "' " + file_line + ". Err3." )
                errCount += 1

        # Check repeating entries of same category:
        if self.isKnownDataProp( line[0], strict = True ):
            logging.warning( " Repeating Data Prop name '" + line[0] + "' " + \
                             file_line + "." )
            errCount += 1
        elif self.isKnownDataProp( line[0], strict = False ):
            logging.warning( " Data Prop name '" + line[0] + "' repeats an existing name, " + \
                             "but uses different capital letters " + \
                             file_line + "." )
            errCount += 1

        # Check repeating entries of other category:
        if self.isKnownClass(   line[0], strict = False ) or \
           self.isKnownObjProp( line[0], strict = False ):
            logging.warning( " Data Prop name '" + line[0] + "' repeats an existing name " + \
                             "in different category (Class/ObjProp) " + \
                             file_line + "." )
            errCount += 1


        return errCount
        pass # TBoxTools.validateDataLine()


    def parseInputData( self ):
        self.errCount = 0

        # Preparation:
        self.headers = []
        self.onto    = []
        self.classes = []
        self.objProp = []
        self.datProp = []

        if len(self.dataIn) == 0:
            logging.error( " in extractOntology(): dataIn is not loaded, use readExcel()." )
            return

        # TODO Other internal arrays:

        # Header row on Line 0:
        #file_line = self.fileIn + " on " + str(0)
        #self.headers = [ self.dataIn[0] ]

        # The rest of the input data:
        for il, line in enumerate(self.dataIn):
            file_line = "in '" + self.fileIn + "' on " + str(il+1) 

            if 0 == il:
                self.extractHeaders(  line, file_line )

            elif self.isLineOntology( line, file_line ):
                self.errCount += self.extractOntology( line, file_line )

            elif self.isLineClass(   line, file_line ):
                self.errCount += self.extractClasses( line, file_line )

            elif self.isLineObjProp( line, file_line ):
                self.errCount += self.extractObjProp( line, file_line )

            elif self.isLineDataProp( line, file_line ):
                self.errCount += self.extractDataProp( line, file_line )

            elif self.isEmptyLine( line, file_line ):
                # Do nothing
                #logging.info( " Skipping empty line " + file_line )
                pass
            elif self.isLineComment( line, file_line ):
                # Do nothing
                #logging.info( " Skipping comment line " + file_line )
                pass
            else:
                #logging.warning( " xxxxxxxxxxxxxxxxxxxx " )
                logging.warning( " Failed to identify type, skipping line: '" + \
                                 str(line) + "' " + file_line + "." 
                                 #" Check '" + str( words[1] + "'." 
                               )
        pass # TBoxTools.parseInputData()

    def isLineComment( self, line, file_line ):
        """ Now the conditions are either: 
            - the 1st column is empty or equal to Comment
            - the 2nd column is empty or equal to Comment
            .
        """

        short = self.getLowShort( line[0], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        short = self.getLowShort( line[1], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        return False
        pass # TBoxTools.isLineComment()

    def getLowShort( self, word, file_line ):
        if isinstance( word, str ):
            #print( "type = str" )
            short = word.strip().lower()
        elif isinstance( word, float ):
            #print( "type = float" )
            if math.isnan( word ):
                short = ""
            else:
                short = str(word)

        else:
            logging.error( " Unknown value '" + str(word) + "', type " + \
                           str(type(word)) + " " + file_line )
            short = str(word)
        return short
        pass # TBoxTools.getShort()

    def isEmptyLine( self, line, file_line ):
        #isEmpty = True
        for word in line:
            if isinstance( word, float ):
                if not math.isnan( word ):
                    return False
            elif isinstance( word, str ):
                if not word.strip() == "":
                    return False
            else:
                logging.error( " Unknown type of '" + str(word) + "': " + str(type(word)) + "." )
        return True
        pass # TBoxTools.isEmptyLine()
                
    def extractHeaders( self, line, file_line ):
        #print( "sssssssssssssssssssss" )
        #print( "header line =", line )
        self.headers = [line]

        errCount = self.validateHeaders( line, file_line )
        return errCount
        pass # TBoxTools.extractHeaders()
        
    def extractOntology( self, line, file_line ):
        errCount = self.validateOntoLine( line, file_line )

        self.onto.append( line )
        self.ontoNames.append( line[0] )

        #print( "In ontology:", line[3] )
        #print( "In ontology:", line[3].strip().lower() )
        if line[3].strip().lower() == "https://www.w3.org/2007/05/powder-s#hasiri":
            # "http://www.theworldavatar.com/kg/ontocrystal/":
            self.tbox = line[2].strip()
   
        return errCount
        pass # TBoxTools.extractOntology() 

    def extractClasses( self, line, file_line ):

        errCount = self.validateClassLine( line, file_line )

        #self.addClass( line )
        self.classes.append( line )
        self.clNames.append( str(line[0]).strip() )

        # Recorde the classes and their relations:
        self.classRel[line[0]] = {}
        if is_http( line[0] ):
            path = line[0]
        else:
            path = line[8] + line[0]
        self.classRel[line[0]]["path"] = path
        self.classRel[line[0]]["is-a"] = []
        if isinstance(line[3],str):
            if line[3].strip() == "IS-A":
                self.classRel[line[0]]["is-a"] += [ self.classRel[line[2]]["path"] ]
                self.classRel[line[0]]["is-a"] +=   self.classRel[line[2]]["is-a"]
            else:
                logging.error( " Unknown D column '" + line[3] + "' " + line_line )
                errCount += 1
        
        return errCount
        #print( self.classes )
        pass # TBoxTools.extractClasses()

    def extractObjProp( self, line, file_line ):

        errCount = self.validateObjLine( line, file_line )

        #self.addObjProp( line )
        self.objProp.append( line )
        self.opNames.append( line[0].strip() )

        line4 = line[4]
        line5 = line[5]

        if not isinstance( line[4], str):
            #logging.warning( " Not a string '" + str(line4) + "' " + file_line + ". Case_1" )
            #print( line[4], line4 )
            if  None == line[4] or math.isnan(line[4]):
                line4 = "owl:Thing"
        elif "" == line[4]:
            line4 = "owl:Thing"

        if not isinstance( line[5], str):
            logging.warning( " Not a string '" + str(line[5]) + "' " + file_line + ". Case_2" )
            if  None == line[5]:
                line5 = "owl:Thing"
        elif "" == line[5]:
            line5 = "owl:Thing"

        if not isinstance( line[8], str):
            logging.error( " Not a string '" + str(line[8]) + "' " + file_line + ". Case_3" )
            errCount += 1

        if isinstance(line4, str):
            for s in line4.split("UNION"):
                self.addTriple( s.strip(), line[8]+line[0], line5, file_line )
            #self.addTriple( line[8] + s.strip(), line[8]+line[0], line[5], file_line )
        else:
            self.addTriple( "", line[8]+line[0], line5, file_line )

        # Domain:
        words = str(line4).split( "UNION" )
        self.objPropDomain = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.objPropDomain:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' in " + file_line + \
                                 " col " + COLUMN_LETTERS[4] + ", it may " \
                                 "be confused with the UNION keyword." )
                errCount += 1

            if w not in self.clNames and w != "":
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[4] + ". Case_1." )
                errCount += 1
            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.objPropDomain )
            
        # Range:
        words = line5.split( "UNION" )
        self.objPropRange = [ w.strip() for w in words ] #line[5].split( "UNION" )
        for w in self.objPropRange:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' in " + file_line + \
                                 " col " + COLUMN_LETTERS[5] + ", it may " \
                                 "be confused with the UNION keyword." )
                errCount += 1

            if w not in self.clNames and w != "":
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[5] + ". Case_2." )
                errCount += 1

            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.objPropRange )

        return errCount
        #print( self.objProp )
        pass # TBoxTools.extractObjProp()

    def extractDataProp( self, line, file_line ):
        errCount = self.validateDataLine( line, file_line )

        #self.addDataProp( line )
        self.datProp.append( line )
        self.dpNames.append( line[0].strip() )

        if not isinstance( line[4], str):
            logging.error( " Not a string '" + str(line[4]) + "' " + file_line + ". Case_4" )
            # TODO use line4 variable, see extractObjProp as example.
            errCount += 1
        if not isinstance( line[5], str):
            logging.error( " Not a string '" + str(line[5]) + "' " + file_line + ". Case_5" )
            errCount += 1
        if not isinstance( line[8], str):
            logging.error( " Not a string '" + str(line[8]) + "' " + file_line + ". Case_6" )
            errCount += 1

        for s in line[4].split("UNION"):
            self.addTriple( s.strip(), line[8]+line[0], line[5], file_line )
           #self.addTriple( line[8] + s.strip(), line[8]+line[0], line[5], file_line )
           #self.addTriple( line[8]+line[4], line[8]+line[0], line[5], file_line )

        # Domain:
        words = line[4].split( "UNION" )
        self.datPropDomain = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.datPropDomain:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' word in " + file_line + \
                                 " col " + COLUMN_LETTERS[4] + ", it may " \
                                 "be confused with the UNION keyword." )
                errCount += 1

            if w not in self.clNames:
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[4] + ". Case_3." )
                errCount += 1

            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.datPropDomain )
            
        # Range:
        words = line[5].split( "UNION" )
        self.datPropRange = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.datPropRange:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' word in " + \
                                 file_line + " col " + COLUMN_LETTERS[5] + \
                                 ", it may be confused with the UNION keyword." )
                errCount += 1
            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.datPropRange )

                # Here no need to check the range(s) for data properties.
 
        return errCount
        #print( self.datProp )
        pass # TBoxTools.extractDataProp()

    def addTriple( self, subj, predicate, obj, file_line ):
        #logging.info( " Starting addTriple() for '" + subj + "','" + \
        #                predicate + "','" + obj + "'." )

        if not isinstance(predicate,str):
            logging.error( " Predicate '" + str(predicate) + \
                           "' is not a string " + file_line )
            return

        # Add predicate to the list of triples:
        #pStr = self.tbox + predicate
        pStr = predicate
        if pStr not in self.triples:
            self.triples[pStr] = {}
            self.triples[pStr]["domain"] = []
            self.triples[pStr]["range" ] = []
        else:
            #logging.warning( " Repeated predicate '" + predicate + "' " + \
            #                 file_line )
            pass

        # Add ?
        if subj in self.classRel:
            if self.classRel[subj]["path"] not in self.triples[pStr]["domain"]:
                self.triples[pStr]["domain"].append( self.classRel[subj]["path"] )
        else:
            logging.error( " Wrong subj name '" + str(subj) + "' " + file_line )

        #self.triples[pStr]["range" ].append(  obj )
        isDT = self.isDataType( obj )
        #if self.isDataType( obj ):
        if isDT[0]:
            #logging.warning( " Detected a datatype '" + obj + "' but not processed." )
            if not isDT[1] in self.classRel:
                self.classRel[isDT[1]] = dict()
                self.classRel[isDT[1]]["path"] = isDT[1]
                self.classRel[isDT[1]]["is-a"] = []

            if self.classRel[isDT[1]]["path"] not in self.triples[pStr]["range"]:
                self.triples[pStr]["range"].append( self.classRel[isDT[1]]["path"] )
            #else:
 
        elif obj in self.classRel:
            #logging.info( " Adding obj to classRel:" + str(obj) )
            if self.classRel[obj]["path"] not in self.triples[pStr]["range"]:
                self.triples[pStr]["range"].append( self.classRel[obj]["path"] )
        else:
            logging.error( " Wrong obj name '" + str(obj) + "' " + file_line )

        pass # TBoxTools.addTriple()


    def isDataType( self, obj ):

        # 1) Convert to short name in low case
        #print( " >>>> ", obj )
        lower = obj.strip().lower()
        if   lower.startswith( PREFIX_XSD.lower() ): # XSD
            pos  = lower.find( PREFIX_XSD.lower() )
            short = lower[ len(PREFIX_XSD) + pos: ]
            if short in DATA_TYPES_XSD: 
                return True, short #, short, DATA_TYPES_XSD
            else:
                logging.error( "XSD prefix, but unknown data type: '" + obj + "'." )
                return False, obj

        elif lower.startswith( PREFIX_RDFS.lower() ): # RDFS
            pos  = lower.find( PREFIX_RDFS.lower() )
            short = lower[ len(PREFIX_RDFS) + pos: ]
            #return True, obj
            if short in DATA_TYPES_RDFS: 
                return True, short #, short, DATA_TYPES_RDFS
            else:
                logging.error( "RDFS prefix, but unknown data type: '" + obj + "'." )
                return False, obj



        elif lower.startswith( PREFIX_RDF.lower() ): # RDF
            pos  = lower.find( PREFIX_RDF.lower() )
            short = lower[ len(PREFIX_RDF) + pos: ]
            #return True, obj

        elif lower.startswith( "xsd:" ): # XSD
            pos  = lower.find( "xsd:" )
            short = lower[4 + pos:]
            #return True, obj
            if short in DATA_TYPES_XSD: 
                return True, short #, short, DATA_TYPES_XSD
            else:
                logging.error( "XSD prefix, but unknown data type: '" + obj + "'." )
                return False, obj

        elif lower.startswith( "rdfs:" ): # RDFS
            pos  = lower.find( "rdfs:" )
            short = lower[5 + pos:]
            #logging.info( " Checking rdfs: " + lower + "  " + str(pos) + "  " + short )
            #return True, obj
            if short in DATA_TYPES_RDFS: 
                return True, short #, short, DATA_TYPES_RDFS
            else:
                logging.error( "RDFS prefix, but unknown data type: '" + obj + "'." )
                return False, obj



        elif lower.startswith( "rdf:" ): # RDF
            pos  = lower.find( "rdf:" )
            short = lower[5 + pos:]
            #return True, obj

        else: 
            # Do nothing?
            short = lower
            pass

        # 2) Check versus the collection of known data types
        #if short in DATA_TYPES:
        #    return True, obj
        if   short in DATA_TYPES_XSD: 
            return True, short #, short, DATA_TYPES_XSD
        elif short in DATA_TYPES_RDF: 
            return True, short #, short, DATA_TYPES_RDF
        elif short in DATA_TYPES_RDFS: 
            return True, short #, short, DATA_TYPES_RDFS
        else:
            #return False, obj
            #logging.error( "XSD prefix, but unknown data type: '" + obj + "'." )
            return False, obj



        return False, obj
        pass # TBoxTools.isDataType()

    def checkTriple( self, subj, predicate, obj, file_line ):
        errCount = 0

        if not isinstance( subj, str ):
            logging.error( " Subj is not a string '" + str(subj) + "' " + \
                           file_line )
            errCount += 1

        if not isinstance( predicate, str ):
            logging.error( " Predicate is not a string '" + str(predicate) + \
                           "' " + file_line )
            errCount += 1

        if not isinstance( obj, str ):
            logging.error( " Obj is not a string '" + str(obj) + "' " + \
                           file_line )
            errCount += 1

        if predicate not in self.triples:
            logging.error( " Predicate '" + predicate + "' is not in TBox " + \
                           file_line )
            #for k in self.triples.keys():
            #    print( "   ", k )
            #print( "  tbox = '" + self.tbox + "'." )
            errCount += 1
            return errCount

        #print( "Inside checkTriple:", subj, "and", obj )
        if subj in self.classRel:
            if self.classRel[subj]["path"] in self.triples[predicate]["domain"]:
                # Do nothing
                pass
            elif self.anyInList( self.classRel[subj]["is-a"], \
                                 self.triples[predicate]["domain"] ):
                # Do nothing
                pass
            else:
                logging.error( " Subject '" + subj + "' is not in TBox " + \
                               file_line )

                """
                print( "     predicate:", predicate )
                print( "       path   :", self.classRel[subj]["path"] )
                print( "       is-a   :" ) #, self.classRel[subj]["is-a"] )
                for d in self.classRel[subj]["is-a"]:
                    print( "               ", d )
                print( "      domain  :" ) #, self.triples[predicate]["domain"] )
                for d in self.triples[predicate]["domain"]:
                    print( "               ", d )
                #print( "   domains:" )
                #for k in self.triples.keys():
                #    print( "    ", self.triples[k]["domain"] )
                """

                errCount += 1

        else:
            logging.error( " Subject '" + str(subj) + "' is not in the list" + \
                           " of classes " + file_line )
            errCount += 1
            pass

        isDT = self.isDataType(obj)
        if isDT[0]:
            # need more verification. (for capital letter)
            pass
            
        elif obj in self.classRel:
        #if obj in self.triples[predicate]["range"]:
            #print( " >>>>>> ", obj, type(obj) )
        #    if obj not in self.classRel:
        #        logging.error( "Missing '" + obj + "' in classRel " + file_line )
        #        errCount += 1

            #else:
            if predicate in DATA_PROPS:
                # Do nothing
                pass

            elif self.classRel[obj]["path"] in self.triples[predicate]["range"]:
                # Do nothing
                pass
            elif self.anyInList( self.classRel[obj]["is-a"], \
                                 self.triples[predicate]["range"] ):
                # Do nothing
                pass
            else:
                logging.error( " Object '" + obj + "' is not in TBox " + \
                               file_line )

                print( "     predicate:", predicate )
                print( "       path   :", self.classRel[obj]["path"] )
                """
                print( "       is-a   :" ) #, self.classRel[obj]["is-a"] )
                for d in self.classRel[obj]["is-a"]:
                    print( "               ", d )
                print( "       range  :" ) #, self.triples[predicate]["domain"] )
                for d in self.triples[predicate]["range"]:
                    print( "               ", d )
                """

                errCount += 1

        else:
            logging.error( " Object '" + obj + "' is not in the list" + \
                           " of classes " + file_line )
            #print( "     predicate:", predicate )
            #print( "     object:   ", obj )
            #for o in self.triples[predicate]["range"]:
            #    print( "   in range: ", o )
            #for rel in self.classRel:
            #    print( "   in class: ", rel )
            errCount += 1

        return errCount
        pass # TBoxTools.checkTriple()
        
    def anyInList( self, list1, list2 ):
        """
        Checks whether any element from list 1 is present in list 2.
        """
        for l1 in list1:
            if l1 in list2:
                return True
        return False

    def addClass( self, line ):
        logging.error( "No implemented addClass() in tboxtools.py." )
        pass # TBoxTools.addClass()

    def addObjProp( self, line ):
        logging.error( "No implemented addObjProp() in tboxtools.py." )
        pass # TBoxTools.addObjProp()

    def addDataProp( self, line ):
        logging.error( "No implemented addDataProp() in tboxtools.py." )
        pass # TBoxTools.addDataProp()


    def mergeObjProp(self):
        logging.error( "Not implemented mergeObjProp() function. in tboxtools.py." )

        pass # TBoxTools.mergeObjProp()

    def mergeDataProp(self):
        logging.error( "Not implemented mergeDataProp() function. in tboxtools.py." )

        pass # TBoxTools.mergeDataProp()

    def saveCsv( self, filename ):

        #print( self.headers )
        #print( self.onto )
        #print( self.classes )
        #print( self.objProp )
        #print( self.datProp )

        # TODO
        self.mergeObjProp()
        self.mergeDataProp()
        # TODO
        #self.CheckClasses()
        #self.CheckObjProp()
        #self.CheckDataProp()

        tmp = self.headers + self.onto    + self.classes + \
              self.objProp + self.datProp

        #print( tmp )
        #print( "===================" )

        self.dataOut = []
        for il, line in enumerate(tmp):
            l = []
            for ic,c in enumerate(line):
                if ic >= self.nCol:
                    logging.warning( " Too many columns for class/property '" + \
                                     line[0] + "' during saving to csv file." + \
                                     " Extra cell '" + str(c) + "'." )
                    continue

                if isinstance( c, float ):
                    if math.isnan( c ):
                        l.append( "" )
                    else:
                        l.append( c )
                elif isinstance( c, str ):
                    if "owl:Thing" == c:
                        l.append( "" )
                    else:
                        l.append( c.strip() )
                else:
                    l.append( c )
                    #logging.warning( " Unknown type '" + str(type(c)) + "' of" + \
                    #                 " '" + str(c) + "' during saving to csv. " )
                    # 
            self.dataOut.append( l )

        for line in self.dataOut:
            # For debugging:
            #print( line[0] )
            #print( line )
            pass

        try:
            writeCsv( filename, self.dataOut )
        except:
            # On failure I try to write line-by-line,
            # to detect the location of the error:
            for s in self.dataOut:
                print( "Saving to csv:", s )
                writeCsv( filename, [s] )

        #tools.writeCsv( filename, self.classes )
        print( "Saved '" + filename + "' file, number of lines =", 
               str(len(self.dataOut)) + "." )
        """
        """
        print( "Number of errors+warnings =", str(self.errCount) + "." )

        pass # TBoxTools.saveCsv()

writeCsvErrCount = 0
def writeCsv( filename, array ):
    global writeCsvErrCount
    try:
        with open( filename, "w", newline = "" ) as f:
            csvw = csv.writer( f, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL )
            for a in array:
                csvw.writerow( a )
    except IOError:
        tmpFile = "test-tmp.csv"
        print( "Error: File '" + filename + "' is protected. " + \
                       "Using temporary instead: '" + tmpFile + "'." )
        writeCsvErrCount += 1
        if 1 == writeCsvErrCount :
            writeCsv( tmpFile, array )
        else:
            print( " Error: I give up. " + "You need to close the files." )
    pass # writeCsv()


if __name__ == "__main__":
    
    fileIn  = "Book2.xlsx"
    fileIn  = "ontocrystal.xlsx"

    fileOut = "default.csv"

    if len( sys.argv ) == 3:
        fileIn  = sys.argv[1]
        fileOut = sys.argv[2]
    elif len( sys.argv ) == 2:
        fileIn = sys.argv[1]
        fileBase, fileExt = os.path.splitext( fileIn )
    elif len( sys.argv ) == 1:
        # Do nothing. Use default file names.
        pass
    else:
        logging.error( " Expecting 1 or 2 command line arguments:" + \
                       " tboxtools.py file_in [file_out] \n" + \
                       ", but got '" + str(len(sys.argv)) + "' program call." )
        sys.exit(0) 

    fileBase, fileExt = os.path.splitext( fileIn )
    if fileExt.lower() == ".xlsx":
        fileOut = fileBase + ".csv"
    else:
        logging.error( " Input must be Excel file with extension .xlsx, " + \
                       "but got file name '" + fileIn + "'." )
        #sys.exit(0)

    fileBase, fileExt = os.path.splitext( fileOut )
    if not fileExt.lower() == ".csv":
        logging.error( " Output must be CSV file with extension .csv, " + \
                       "but got file name '" + fileOut + "'." )
        sys.exit(0)

    logging.info( "File base = '" + fileBase + "', ext = '" + fileExt + "'." )

    tb = TBoxTools()
    #print( tb.anyInList( [1,2.3],[3,2,1] ) )
    #1/0
    tb.readExcel( fileIn )
    tb.parseInputData()
    #print( "   tbox = '" + tb.tbox + "'." )
    """
    print( "All triples:" )
    for predicate in tb.triples:
        print( " pred:", predicate )
        for d in tb.triples[predicate]["domain"]:
            print( "  dom:", d )
        for d in tb.triples[predicate]["range"]:
            print( "  ran:", d )
    """

    # Example of use:
    #tb.checkTriple( subj, predicate, obj, "file and line" )

# old version:
#tb.extractOntology()
#tb.extractClasses()
#tb.extractObjProp()
#tb.extractDataProp()

    tb.saveCsv( fileOut )


