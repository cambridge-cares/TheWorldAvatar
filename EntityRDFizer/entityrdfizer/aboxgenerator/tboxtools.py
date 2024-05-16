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

import sys     # for command line arguments (sys.argv)
import os      # To detect file, to check filename, etc.

import math    # to detect NaN values
import csv     # to save csv files

import logging # For error/waarning messages
#import pandas  # to read xlsx files

logging.basicConfig(level=logging.INFO)
#logging.basicConfig(level=logging.WARNING)
#logging.basicConfig(level=logging.ERROR)
#logging.disable(logging.CRITICAL)

#import tools

COLUMN_LETTERS = "ABCDEFGHIJ"
COLUMN_HEADERS = [ "Source",      "Type",  "Target",   "Relation", "Domain",
                   "Range", "Quantifier", "Comment", "Defined By", "Label" ]


PREFIX_XSD  = "http://www.w3.org/2001/XMLSchema#"
PREFIX_RDFS = "http://www.w3.org/2000/01/rdf-schema#"
#logging.warning(" Not supported PREFIX_RDF ")
PREFIX_RDF  = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

#DATA_TYPES = {}
DATA_TYPES_XSD  = {}
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

DATA_TYPES_RDF  = {}
DATA_TYPES_RDF["langstring"] = "langString"
DATA_TYPES_RDF["plainliteral"] = "PlainLiteral"
DATA_TYPES_RDF["xmlliteral"] = "XMLLiteral"
DATA_TYPES_RDFS = {}
DATA_TYPES_RDFS["literal"] = "Literal"
#DATA_TYPES_RDFS["label"] = "label"

DATA_PROPS = {}
#DATA_TYPES_RDFS["label"]   = "label"
DATA_PROPS['range'       ] = 'range'
DATA_PROPS['domain'      ] = 'domain'
DATA_PROPS['type'        ] = 'type'
DATA_PROPS['subclassof'  ] = 'subClassOf'
DATA_PROPS['subpropertyof'] = 'subPropertyOf'
DATA_PROPS['rdfs:label'  ] = 'label'
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
for i in range(3):
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

def is_http(name):
    if not isinstance(name, str):
        return False
    if name.lower().strip().startswith("http://") or \
       name.lower().strip().startswith("https://"):
        return True
    return False

class TBoxTools:
    __slots__ = ["file_in",  "data_in", "data_out", "classes", "headers",
                 "nCol", "err_count",
                 "cl_names", "op_names", "dp_names", "onto", "onto_names",
                 "obj_prop", "obj_prop_domain", "obj_prop_range",
                 "dat_prop", "dat_prop_domain", "dat_prop_range",
                 "triples", "tbox", "class_rel",
                 "verbose"
                ]
    def __init__(self, verbose = True):
        self.cleanAll()

        self.verbose = verbose

        #self.prepareArrs()

        ### TBoxTools.__init__()

    def is_empty(self):

        if len(self.data_in) == 0:
            return True

        return False

    def cleanAll( self ):
        self.data_in    = []
        self.headers   = []
        self.onto      = []
        self.onto_names = []
        self.classes   = []
        self.cl_names   = []
        self.obj_prop   = []
        self.op_names   = []
        self.dat_prop   = []
        self.dp_names   = []

        self.nCol    = 10
        self.file_in  = ""

        self.dat_prop_domain = []
        self.dat_prop_range  = []
        self.obj_prop_domain = []
        self.obj_prop_range  = []

        self.tbox          = ""  # TBox path

        self.triples       = {}  # Structure of this dictionary is
                                 # self.triples["hasXXX"]["domain"] = []
                                 # self.triples["hasXXX"]["range" ] = []
                                 #

        #self.class_rel     = {}  # Structure of this dictionary is
        #                         # self.class_rel[shortName]["path"] = full path
        #                         # self.class_rel[shortName]["is-a"] = [] all paths
        #                         # For built-in datatypes for Data Properties
        #                         # use low-case without prefix for 'short'
        #                         # and full path for 'path'.

        self.class_rel     = {}  # Structure of this dictionary is
                                 # self.class_rel[fullName]["short"] = class without prefix
                                 # self.class_rel[fullName]["path"] = full path (capital letters?)
                                 # self.class_rel[fullName]["is-a"] = [] all paths
                                 # self.class_rel[fullName]["where"] = file 
                                 #     and line where this class is defined (str),
                                 #     where fullName is prefix + className.
                                 # For built-in datatypes for Data Properties
                                 # use low-case without prefix for 'short'
                                 # and full path for 'path'.

        self.class_rel["owl:Thing"] = {}

        # Global pre-defined classes:
        self.class_rel["owl:Thing"]["path"] = "owl:Thing"
        self.class_rel["owl:Thing"]["is-a"] = []
        self.cl_names.append( "owl:Thing" )

        self.verbose       = False

        ### TBoxTools.cleanAll()

    def readExcelList( self, file_path ):
        err_count = 0

        if isinstance( file_path, str ):
            err_count += self.readExcel( file_path )
        elif isinstance( file_path, list ):
            for file in file_path:
                err_count += self.readExcelList( file )
        else:
            logging.error(" tboxtools.readExcelList() accepts a path or" +
                          " a list of pathes, but got '%s'.", str(file_path))

        return err_count
        ### TBoxTools.readExcelList()

    def readExcel( self, filename ):
        err_count = 0

        _, file_ext = os.path.splitext(filename)
        if ".xlsx" == file_ext.lower():
            err_count += self._read_xlsx(filename)
        elif ".csv" == file_ext.lower():
            print("filename = ", filename)
            err_count += self._read_csv(filename)
        else:
            logging.error(" Unknown file extension in '%s'.", filename)
            err_count += 1

        return err_count
        ### TBoxTools.readExcel()

    def _read_csv(self, filename, encoding="utf-8"):
        err_count = 0
        output = []
        if os.path.exists(filename):
            try:
                if encoding == "utf-8":
                    f = open(filename, "r", encoding="utf-8")
                else:
                    f = open(filename, "r")
                csvr = csv.reader(f)

                for ir, row in enumerate(csvr):
                    output.append(row)
                f.close()

            except Exception as e:
                print("Failed to open file '", filename, "' as utf-8", sep="")
                print("An error:", e)
                err_count += 1
                #raise e
        else:
            print("_read_csv: file does not exist: '" + filename + "'.")
            err_count += 1

        self.data_in += output
        return err_count

    def _read_csv_pandas(self, filename):
        err_count = 0

        if not os.path.isfile( filename ):
            self.file_in = ""
            logging.error(" Input file '%s' does not exist.", filename)
            err_count += 1
            return err_count

        self.file_in = filename
        df = pandas.read_csv(filename, encoding='unicode_escape')
        logging.info("The size of input file '%s' is %s.",
                     filename, str(df.shape))

        self.data_in.append(df.columns)
        for _, row in df.iterrows():
            #print(row)
            #print(row.to_dict())
            r = row.to_dict()
            line = [r[k] for k in r.keys()]
            #print( line )
            self.data_in.append(line)
        #print( self.data_in )

        return err_count
        ### TBoxTools._read_csv()

    def _read_xlsx( self, filename ):
        err_count = 0

        if not os.path.isfile( filename ):
            self.file_in = ""
            logging.error(" Input file '%s' does not exist.", filename)
            err_count += 1
            return err_count

        self.file_in = filename
        df = pandas.read_excel( filename )
        logging.info(" The size of input file '%s' is %s.",
                     filename, str(df.shape))

        self.data_in.append( df.columns )
        for _, row in df.iterrows():
            #print(row)
            #print(row.to_dict())
            r = row.to_dict()
            line = [ r[k] for k in r.keys() ]
            #print( line )
            self.data_in.append( line )
        #print( self.data_in )

        return err_count
        ### TBoxTools._read_xlsx()

    def is_empty_cell(self, cell):
        """ Returns True if the input string 'cell' is empty or nan.
        """
        #print( "cell value = '" + str(cell) + "', type =", type(cell) )
        if isinstance(cell, float):
            if math.isnan(cell):
                return True
        elif isinstance(cell, str):
            short = cell.strip().lower()
            if "nan" == short or "" == short:
                return True
        else:
            logging.warning(" Unknown type of '%s': %s.", cell, str(type(cell)))

        return False
        ### TBoxTools.is_empty_cell()

    def isCellBValue(self, line, value, file_line):
        if len(line) < 2:
            return False

        if not isinstance(line[1], str):
            # Here NaN value of type float if the cell is empty.
            return False

        i = 1
        expect = value

        if expect.lower() == line[i].lower():
            if expect != line[i]:
                logging.warning(" Wrong input '%s' col %s: '%s',"
                                "but expected '%s'.",
                                file_line, COLUMN_LETTERS[i], line[i], expect)
            return True
        else:
            return False
        ### TBoxTools.isCellBValue()

    def isLineOntology(self, line, file_line):
        return self.isCellBValue(line, "TBox", file_line)
        ### TBoxTools.isLineOntology()

    def isLineClass(self, line, file_line):
        return self.isCellBValue(line, "Class", file_line)
        ### TBoxTools.isLineClass()

    def isLineObjProp(self, line, file_line):
        return self.isCellBValue(line, "Object Property", file_line)
        ### TBoxTools.isLineObjProp()

    def isLineDataProp(self, line, file_line ):
        return self.isCellBValue(line, "Data Property", file_line)
        ### TBoxTools.isLineDataProp()

    def isKnownClass(self, value, strict = False):
        return self.isInClassRel(value)

        for c in self.cl_names:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        ### TBoxTools.isKnownClass()

    def isKnownObjProp(self, value, strict=False):
        for c in self.op_names:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        ### TBoxTools.isKnownObjProp()

    def isKnownDataProp(self, value, strict=False):
        for c in self.dp_names:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        ### TBoxTools.isKnownObjProp()

    def validateHeaders(self, headers, file_line):
        """ Function returns number of errors + warnings detected.

        """
        err_count = 0

        # Must be exactly 10 headers:
        if len(headers) != len(COLUMN_HEADERS):
            logging.error(" The header row has size %d but expected %d %s.",
                          len(headers), len(COLUMN_HEADERS), file_line)
            err_count += 1

        # Headers must match the standard values, including capital letters:
        for i in range( min(len(headers), len(COLUMN_HEADERS)) ):

            if   headers[i].strip() == COLUMN_HEADERS[i].strip():
                # Do nothing, this header cell is correct.
                pass
            elif headers[i].strip().lower() != COLUMN_HEADERS[i].strip().lower():
                logging.warning(" Wrong header value %s col %s: '%s', but" +
                                " expected '%s'.", file_line, COLUMN_LETTERS[i],
                                headers[i], COLUMN_HEADERS[i])
                err_count += 1
            else:
                logging.error(" Wrong header value %s col %s: '%s', but" +
                              " expected '%s'.", file_line, COLUMN_LETTERS[i],
                              headers[i], COLUMN_HEADERS[i])
                err_count += 1

        return err_count
        ### TBoxTools.validateHeaders()

    def validateOntoLine(self, line, file_line):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines.

        """
        err_count = 0

        # TODO see function description
        logging.warning(" ontology validation is not implemented")
        err_count += 1

        # Check the exact matching the property type:
        if "class" == line[1].strip().lower():
            if not "TBox" == line[1]:
                logging.warning(" Expected 'TBox' but found '%s' %s.",
                                line[1], file_line)
                err_count += 1

        # TODO Col 0, 1, 2, 3 must be present, and be checked.


        # Other columns are empty:
        for i in [5, 6, 7, 8, 9]:
            if not self.is_empty_cell(line[i]):
                logging.warning(" Expecting empty cell %s but got '%s' %s. E3",
                                COLUMN_LETTERS[i], str(line[i]), file_line)
                err_count += 1

        return err_count
        ### TBoxTools.validateOntoLine()

    def validateClassLine(self, line, file_line):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """

        # DONE class name should not be repeated, and don't match the obj prop and dat prop

        err_count = 0

        # Check the exact matching the property type:
        if "class" == line[1].strip().lower():
            if not "Class" == line[1]:
                logging.warning(" Expected 'Class' but found '%s' %s.",
                                line[1], file_line)
                err_count += 1

        # TODO columns 0,1 are required

        # Check IS-A Relation of the classes (columns 2,3):
        if self.is_empty_cell(line[3]):
            if not self.is_empty_cell(line[2]):
                logging.warning(" Found an 'IS-A' class '%s' without the" +
                                " 'IS-A' keyword %s.", line[2], file_line)
                err_count += 1
            # pass
        else:
            short = str(line[3]).strip()
            if "is-a" == short.lower():
                if "IS-A" != short:
                    logging.warning(" Expected 'IS-A' but found '%s' %s.",
                                    line[3], file_line)
                    err_count += 1
                if self.is_empty_cell(line[2]):
                    logging.error(" Missing relation class for class '%s' " +
                                  " %s col %s.",
                                  line[0], file_line , COLUMN_LETTERS[3])
                    err_count += 1
                else:
                    if not self.isKnownClass(line[2], strict=True):
                        logging.error(" Unknown 'IS-A' class name '%s' %s.",
                                      line[2], file_line )
                        err_count += 1

            else:
                # Make warning for is-a and error for neither is-a nor IS-A:
                logging.error(" Unknown Relation keyword '%s' for class '%s'" +
                              " %s col %s.",
                              short, line[0], file_line, COLUMN_LETTERS[3])
                err_count += 1

        # Check whether 'Comment' exists (column 7):
        if self.is_empty_cell(line[7]):
            logging.error(" Missing 'Comment' for class '%s' %s in col %s.",
                          line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1
        elif isinstance(line[7], str):
            if len(line[7]) < 20:
                logging.warning(" Class '%s' may have incompete 'comment':" +
                                " '%s' %s col %s.",
                                line[0], line[7], file_line, COLUMN_LETTERS[7])
                err_count += 1
        else:
            logging.warning(" Wrong 'Comment' '%s' for class '%s' %s in col" +
                            " %s.",
                            line[7], line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1

        # Check whether 'Defined In' exists (column 8):
        if self.is_empty_cell(line[8]):
            logging.error(" Missing '%s' for class '%s' %s in col %s.",
                          COLUMN_HEADERS[8], line[0], file_line,
                          COLUMN_LETTERS[8])
            err_count += 1
        elif isinstance(line[8], str):
            if len(line[8]) < 20:
                logging.warning(" Class '%s' may have incompete '%s': '%s'" +
                                " %s col %s.", line[0], COLUMN_HEADERS[8],
                                line[8], file_line, COLUMN_LETTERS[8])
                err_count += 1

        # Check whether 'Label' exists (column 9):
        if self.is_empty_cell(line[9]):
            logging.error(" Missing '%s' for class '%s' %s in col %s.",
                          COLUMN_HEADERS[9], str(line[0]), file_line,
                          COLUMN_LETTERS[9])
            err_count += 1
        elif isinstance(line[9], str):
            if len(line[9]) < 4:
                logging.warning(" Class '%s' may have incompete '%s': '%s'" +
                                " %s col %s.", str(line[0]), COLUMN_HEADERS[9],
                                line[9] , file_line, COLUMN_LETTERS[9])
                err_count += 1

        # Other columns are empty:
        for i in [4, 5, 6]:
            if not self.is_empty_cell(line[i]):
                logging.warning(" Expect empty cell %s but got '%s' %s. E1.",
                                COLUMN_LETTERS[i], str(line[i]), file_line)
                err_count += 1


        # Check repeating entries of same category:
        if self.isKnownClass(line[0], strict=True):
            logging.warning(" Repeating class name '%s' %s.",
                            line[0], file_line )
            err_count += 1
        elif self.isKnownClass( line[0], strict=False):
            logging.warning(" Class name '%s' repeats an existing name," +
                            " but uses different capital letters %s.",
                            line[0], file_line )
            err_count += 1

        # Check repeating entries of other category:
        if self.isKnownObjProp (line[0], strict=False) or \
           self.isKnownDataProp(line[0], strict = False):
            logging.warning(" Data Prop name '%s' repeats an existing name " +
                            "in different category (ObjProp/DataProp) %s.",
                            line[0], file_line)
            err_count += 1

        return err_count
        ### TBoxTools.validateClassLine()

    def validateObjLine(self, line, file_line):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

        """

        err_count = 0

        #logging.warning( " object property validation is not implemented" )
        #err_count += 1

        # Check the exact matching the property type:
        if "Object Property" == line[1]:
            # Do nothing, this is correct value
            pass
        elif "object property" == line[1].strip().lower():
            logging.warning(" Expected 'Object Property' but found '%s' %s.",
                            line[1], file_line)
            err_count += 1

        # TODO Columns 0,1,4,5 are required.

        # TODO Column 6 is optional.
        # TODO cell 6 can be empty or "only"
        if self.is_empty_cell(line[6]):
            pass
        elif "only" == line[6]:
            pass
        else:
            logging.error(" Invalid value '%s' in %s in col %s.",
                          line[6], file_line, COLUMN_LETTERS[6])
            err_count += 1

        # Check whether 'Comment' exists (column 7):
        if self.is_empty_cell(line[7]):
            logging.error(" Missing 'Comment' for Obj Property '%s' %s" +
                          " in col %s.",
                          line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1
        elif isinstance( line[7], str ):
            if len( line[7] ) < 20:
                logging.warning(" Obj Property '%s' may have incompete" +
                                "'comment': '%s' %s col %s.",
                                line[0], line[7], file_line, COLUMN_LETTERS[7])
                err_count += 1
        else:
            logging.warning(" Wrong 'Comment' '%s' for Obj Property '%s' %s" +
                            " in col %s.",
                            line[7], line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1

        # Check whether 'Defined In' exists (column 8):
        if self.is_empty_cell(line[8]):
            logging.error(" Missing '%s' for Obj Property '%s' %s in col " +
                          "%s.", COLUMN_HEADERS[8], line[0], file_line,
                          COLUMN_LETTERS[8])
            err_count += 1
        elif isinstance(line[8], str):
            if len(line[8]) < 20:
                logging.warning(" Obj Property '%s' may have incompete '%s':" +
                                " '%s' %s col %s.",
                                line[0], COLUMN_HEADERS[8], line[8], file_line,
                                COLUMN_LETTERS[8])
                err_count += 1

        # Check whether 'Label' exists (column 9):
        if self.is_empty_cell(line[9]):
            logging.error(" Missing '%s' for Obj Property '%s' %s in col %s.",
                          COLUMN_HEADERS[9], line[0], file_line,
                          COLUMN_LETTERS[9])
            err_count += 1
        elif isinstance(line[9], str):
            if len(line[9]) < 4:
                logging.warning(" Obj Property '%s' may have incompete '%s':" +
                                " '%s' %s col %s.", line[0], COLUMN_HEADERS[9],
                                line[9], file_line, COLUMN_LETTERS[9])
                err_count += 1

        # Other columns are empty:
        for i in [ 2, 3 ]:
            if not self.is_empty_cell( line[i] ):
                logging.warning(" Expecting empty cell %s but got '%s' %s. E2",
                                COLUMN_LETTERS[i], str(line[i]), file_line)
                err_count += 1

        # Check repeating entries:

        # Check repeating entries of same category:
        if self.isKnownObjProp(line[0], strict=True):
            logging.warning(" Repeating Obj Prop name '%s' %s.",
                            line[0], file_line)
            err_count += 1
        elif self.isKnownObjProp(line[0], strict=False):
            logging.warning(" Obj Prop name '%s' repeats an existing name," +
                            " but uses different capital letters %s.",
                            line[0], file_line)
            err_count += 1

        # Check repeating entries of other category:
        if self.isKnownClass(   line[0], strict=False) or \
           self.isKnownDataProp(line[0], strict=False):
            logging.warning(" Data Prop name '%s' repeats an existing name" +
                            " in different category (Class/DataProp) %s.",
                            line[0], file_line)
            err_count += 1


        return err_count
        ### TBoxTools.validateObjLine()


    def validateDataLine( self, line, file_line ):
        """ Return number of warnings + errors.
            Checks the expected empty lines and non-empty lines

            Columns 0,1,4,5 are required.
        """
        err_count = 0

        #logging.warning( " data property validation is not implemented" )

        # Check the exact matching the property type:
        if "data property" == line[1].strip().lower():
            if not "Data Property" == line[1]:
                logging.warning(" Expected 'Data Property' but found '%s' %s.",
                                line[1], file_line)
                err_count += 1

        # Check whether 'Comment' exists (column 7):
        if self.is_empty_cell(line[7]):
            logging.error(" Missing 'Comment' for Data Property '%s' %s" +
                          " in col %s.",
                          line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1

        elif isinstance( line[7], str ):
            if len(line[7]) < 20:
                logging.warning(" Data Property '%s' may have incompete" +
                                " 'comment': '%s' %s col %s.",
                                line[0], line[7], file_line, COLUMN_LETTERS[7])
                err_count += 1
        else:
            logging.warning(" Wrong 'Comment' '%s' for Data Property '%s'" +
                            " %s in col %s.",
                            line[7], line[0], file_line, COLUMN_LETTERS[7])
            err_count += 1

        # Check whether 'Defined In' exists (column 8):
        if self.is_empty_cell(line[8]):
            logging.error(" Missing '%s' for Data Property '%s' %s in col %s.",
                          COLUMN_HEADERS[8], line[0], file_line,
                          COLUMN_LETTERS[8])
            err_count += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning(" Data Property '%s' may have incompete '%s':"
                                " '%s' %s col %s"".",
                                line[0], COLUMN_HEADERS[8], line[8], file_line,
                                COLUMN_LETTERS[8])
                err_count += 1

        # Check whether 'Label' exists (column 9):
        if self.is_empty_cell(line[9]):
            logging.error(" Missing '%s' for Data Property '%s' %s in col %s.",
                          COLUMN_HEADERS[9], line[0], file_line,
                          COLUMN_LETTERS[9])
            err_count += 1
        elif isinstance( line[9], str ):
            if len( line[9] ) < 4:
                logging.warning(" Data Property '%s' may have incompete '%s':"
                                " '%s' %s col %s.",
                                line[0], COLUMN_HEADERS[9], line[9], file_line,
                                COLUMN_LETTERS[9])
                err_count += 1

        # Other columns are empty:
        for i in [2, 3, 6]:
            if not self.is_empty_cell(line[i]):
                logging.warning(" Expecting empty cell %s but got '%s' %s. E3.",
                                COLUMN_LETTERS[i], str(line[i]), file_line)
                err_count += 1

        # Check repeating entries of same category:
        if self.isKnownDataProp(line[0], strict=True):
            logging.warning(" Repeating Data Prop name '%s' %s.",
                            line[0], file_line)
            err_count += 1
        elif self.isKnownDataProp(line[0], strict=False):
            logging.warning(" Data Prop name '%s' repeats an existing name," +
                            " but uses different capital letters %s.",
                            line[0], file_line)
            err_count += 1

        # Check repeating entries of other category:
        if self.isKnownClass(  line[0], strict=False) or \
           self.isKnownObjProp(line[0], strict=False):
            logging.warning(" Data Prop name '%s' repeats an existing name" +
                            " in different category (Class/ObjProp) %s.",
                            line[0], file_line)
            err_count += 1

        return err_count
        ### TBoxTools.validateDataLine()


    def prepareArrs( self ):
        # Preparation:

        self.headers = []
        self.onto    = []
        self.classes = []
        self.obj_prop = []
        self.dat_prop = []
        ### TBoxTools.prepareArrs()

    def parseInputData( self ):
        self.err_count = 0

        if len(self.data_in) == 0:
            logging.error( " in extractOntology(): data_in is not loaded, use readExcel()." )
            return

        # TODO Other internal arrays:

        # Header row on Line 0:
        #file_line = self.file_in + " on " + str(0)
        #self.headers = [ self.data_in[0] ]

        # The rest of the input data:
        for il, line in enumerate(self.data_in):
            file_line = "in '" + self.file_in + "' on " + str(il+1)
            #if il > 20:
            #    1/0

            if 0 == il:
                self.extractHeaders(  line, file_line )

            elif self.isLineOntology( line, file_line ):
                self.err_count += self.extractOntology( line, file_line )

            elif self.isLineClass(   line, file_line ):
                self.err_count += self.extractClasses( line, file_line )

            elif self.isLineObjProp( line, file_line ):
                self.err_count += self.extractObjProp( line, file_line )

            elif self.isLineDataProp( line, file_line ):
                self.err_count += self.extractDataProp( line, file_line )

            elif self.is_empty_line( line, file_line ):
                # Do nothing
                #logging.info( " Skipping empty line " + file_line )
                pass
            elif self.isLineComment( line, file_line ):
                # Do nothing
                #logging.info( " Skipping comment line " + file_line )
                pass
            else:
                #logging.warning( " xxxxxxxxxxxxxxxxxxxx " )
                logging.warning(" Failed to identify type, skipping line:" +
                                " '%s' %s.",
                                str(line), file_line
                                 #" Check '" + str( words[1] + "'."
                               )
        ### TBoxTools.parseInputData()

    def isLineComment( self, line, file_line ):
        """ Now the conditions are either:
            - the 1st column is empty or equal to Comment
            - the 2nd column is empty or equal to Comment
            .
        """

        short = self.getLowShort(line[0], file_line)
        #print( "short = '" + short + "'" )
        if short in ("comment", ""):
        #if "comment" == short or "" == short:
            return True

        short = self.getLowShort(line[1], file_line)
        #print( "short = '" + short + "'" )
        #if "comment" == short or "" == short:
        if short in ("comment", ""):
            return True

        return False
        ### TBoxTools.isLineComment()

    def getLowShort(self, word, file_line):
        if isinstance(word, str):
            #print( "type = str" )
            short = word.strip().lower()
        elif isinstance(word, float):
            #print( "type = float" )
            if math.isnan(word):
                short = ""
            else:
                short = str(word)

        else:
            logging.error(" Unknown value '%s', of type %s %s.",
                          str(word), str(type(word)), file_line)
            short = str(word)
        return short
        ### TBoxTools.getShort()

    def is_empty_line(self, line, file_line):
        #isEmpty = True
        for word in line:
            if isinstance(word, float):
                if not math.isnan(word):
                    return False
            elif isinstance(word, str):
                if not word.strip() == "":
                    return False
            else:
                logging.error(" Unknown type %s of '%s': %s.",
                              str(type(word)), str(word), file_line)
        return True
        ### TBoxTools.is_empty_line()

    def extractHeaders(self, line, file_line):
        #print( "sssssssssssssssssssss" )
        #print( "header line =", line )
        self.headers = [line]

        err_count = self.validateHeaders(line, file_line)
        return err_count
        ### TBoxTools.extractHeaders()

    def extractOntology(self, line, file_line):
        err_count = self.validateOntoLine(line, file_line)

        self.onto.append(line)
        self.onto_names.append(line[0])

        #print( "In ontology:", line[3] )
        #print( "In ontology:", line[3].strip().lower() )
        if line[3].strip().lower() == "https://www.w3.org/2007/05/powder-s#hasiri":
            # "http://www.theworldavatar.com/kg/ontocrystal/":
            self.tbox = line[2].strip()

        return err_count
        ### TBoxTools.extractOntology()

    def _add_class_rel(self, line, file_line):
        err_count = 0

        if line[0] in self.class_rel:
            logging.error(" Over-writing an existing class '%s' %s.",
                          str(line), file_line )
        self.class_rel[line[0]] = {}
        if is_http( line[0] ):
            path = line[0]
        else:
            path = line[8] + line[0]

        self.class_rel[line[0]]["path"] = path
        self.class_rel[line[0]]["is-a"] = []
        if isinstance(line[3],str):
            if line[3].strip() == "IS-A":
                if line[2] in self.class_rel:
                    self.class_rel[line[0]]["is-a"] += [ self.class_rel[line[2]]["path"] ]
                    self.class_rel[line[0]]["is-a"] +=   self.class_rel[line[2]]["is-a"]
                else:    
                    logging.error( " Unknown class '%s' in class_rel %s", line[2], file_line)
            else:
                logging.error(" Unrecognized D column '%s' %s.", line[3], file_line)
                err_count += 1

        # Location where this class is defined:
        self.class_rel[line[0]]["where"] = file_line

        return err_count
        ### TBoxTools._add_class_rel()

    def isInClassRel(self, value):
        output = False
        if value in self.class_rel:
            return True
        for k in self.class_rel:
            if value == self.class_rel[k]["path"]:
                return True

        return output
        ### TBoxTools.isInClassRel()

    def getClassRel(self, value):
        """
        Return a dictionary of the class, to which 'value' belongs to.
        If multiple classes match 'value' print an error message.
        """
        count = 0
        output = None
        if value in self.class_rel:
            #print( "value = ", value )
            count += 1
            output = self.class_rel[value]
        for k in self.class_rel:
            if value == self.class_rel[k]["path"] and k != value:
                count += 1
                output = self.class_rel[k]
                #print( "value = ", output["path"]  )
        if count > 1:
            logging.error( " Multiple matches of class '%s'.", value)
            if value in self.class_rel:
                print("   > in class_rel: ", value)
            for k in self.class_rel:
                if value == self.class_rel[k]["path"]:
                    print("   > in its path: ", value, k)

        return output
        ### TBoxTools.getClassRel()

    def extractClasses(self, line, file_line):

        err_count = self.validateClassLine(line, file_line)

        #self.addClass( line )
        self.classes.append(line)
        self.cl_names.append(str(line[0]).strip())

        # Keep all known classes and their relations (parent classes):
        err_count += self._add_class_rel(line, file_line)

        return err_count
        ### TBoxTools.extractClasses()

    def extractObjProp(self, line, file_line):

        err_count = self.validateObjLine(line, file_line)

        #self.addObjProp( line )
        self.obj_prop.append(line)
        self.op_names.append(line[0].strip())

        line4 = line[4]
        line5 = line[5]

        if not isinstance(line[4], str):
            #logging.warning( " Not a string '" + str(line4) + "' " + file_line + ". Case_1" )
            #print( line[4], line4 )
            if (line[4] is None) or math.isnan(line[4]):
                line4 = "owl:Thing"
        elif "" == line[4]:
            line4 = "owl:Thing"

        if not isinstance(line[5], str):
            logging.warning(" Not a string '%s' %s. Case_2",
                            str(line[5]), file_line)
            if line[5] is None:
                line5 = "owl:Thing"
        elif "" == line[5]:
            line5 = "owl:Thing"

        if not isinstance( line[8], str):
            logging.error(" Not a string '%s' %s. Case_3",
                          str(line[8]), file_line)
            err_count += 1

        if isinstance(line4, str):
            for s in line4.split("UNION"):
                self.addTriple( s.strip(), line[8]+line[0], line5, file_line )
            #self.addTriple( line[8] + s.strip(), line[8]+line[0], line[5], file_line )
        else:
            self.addTriple( "", line[8]+line[0], line5, file_line )

        # Domain:
        words = str(line4).split( "UNION" )
        self.obj_prop_domain = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.obj_prop_domain:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning(" Found '%s' in %s col %s, it may be confused"+
                                " with the UNION keyword.",
                                w[pos:pos+5], file_line, COLUMN_LETTERS[4])
                err_count += 1

            if w not in self.cl_names and w != "":
                logging.error(" Unknown class '%s' in property '%s' %s" +
                              " col %s. Case_1.",
                              w, line[0], file_line, COLUMN_LETTERS[4]  )
                err_count += 1
            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.obj_prop_domain )

        # Range:
        words = line5.split( "UNION" )
        self.obj_prop_range = [ w.strip() for w in words ] #line[5].split( "UNION" )
        for w in self.obj_prop_range:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning(" Found '%s' in %s col %s, it may be confused"+
                                " with the UNION keyword.",
                                w[pos:pos+5], file_line, COLUMN_LETTERS[5])
                err_count += 1

            if w not in self.cl_names and w != "":
                logging.error(" Unknown class '%s' in property '%s' %s" +
                              " col %s. Case_2.",
                              w, line[0], file_line, COLUMN_LETTERS[5])
                err_count += 1

            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.obj_prop_range )

        return err_count
        #print( self.obj_prop )
        ### TBoxTools.extractObjProp()

    def extractDataProp(self, line, file_line):
        err_count = self.validateDataLine(line, file_line)

        #self.addDataProp( line )
        self.dat_prop.append(line)
        self.dp_names.append(line[0].strip())

        if not isinstance(line[4], str):
            logging.error(" Not a string '%s' %s. Case_4.", str(line[4]), file_line)
            # TODO use line4 variable, see extractObjProp as example.
            err_count += 1
        if not isinstance(line[5], str):
            logging.error(" Not a string '%s' %s. Case_5.", str(line[5]), file_line)
            err_count += 1
        if not isinstance(line[8], str):
            logging.error(" Not a string '%s' %s. Case_6.", str(line[8]), file_line)
            err_count += 1

        for s in line[4].split("UNION"):
            self.addTriple(s.strip(), line[8]+line[0], line[5], file_line)
           #self.addTriple( line[8] + s.strip(), line[8]+line[0], line[5], file_line )
           #self.addTriple( line[8]+line[4], line[8]+line[0], line[5], file_line )

        # Domain:
        words = line[4].split("UNION")
        self.dat_prop_domain = [w.strip() for w in words]  #line[4].split( "UNION" )
        for w in self.dat_prop_domain:
            pos = w.lower().find("union")
            if pos >= 0:
                logging.warning(" Found '%s' word in %s col %s, it may be" +
                                " confused with the UNION keyword.",
                                w[pos:pos+5], file_line, COLUMN_LETTERS[4])
                err_count += 1

            if w not in self.cl_names:
                logging.error(" Unknown class '%s' in property '%s' %s"
                              " col %s. Case_3.",
                              w, line[0], file_line, COLUMN_LETTERS[4])
                err_count += 1

            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.dat_prop_domain )

        # Range:
        words = line[5].split("UNION")
        self.dat_prop_range = [ w.strip() for w in words ]  # line[4].split( "UNION" )
        for w in self.dat_prop_range:
            pos = w.lower().find("union")
            if pos >= 0:
                logging.warning(" Found '%s' word in %s col %s, it may be" +
                                " confused with the UNION keyword.",
                                w[pos:pos+5], file_line, COLUMN_LETTERS[5] )
                err_count += 1
            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.dat_prop_range )

                # Here no need to check the range(s) for data properties.

        return err_count
        #print( self.dat_prop )
        ### TBoxTools.extractDataProp()

    def addTriple( self, subj, predicate, obj, file_line ):
        #logging.info( " Starting addTriple() for '" + subj + "','" + \
        #                predicate + "','" + obj + "'." )

        if not isinstance(predicate,str):
            logging.error(" Predicate '%s' is not a string %s.",
                          str(predicate), file_line )
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

        # Adding ?
        if subj in self.class_rel:
            if self.class_rel[subj]["path"] not in self.triples[pStr]["domain"]:
                self.triples[pStr]["domain"].append(self.class_rel[subj]["path"])
        else:
            logging.error(" Wrong subj name '%s' %s.", str(subj), file_line)

        #self.triples[pStr]["range" ].append(  obj )
        isDT = self.isDataType( obj )
        #if self.isDataType( obj ):
        if isDT[0]:
            #logging.warning( " Detected a datatype '" + obj + "' but not processed." )
            if not isDT[1] in self.class_rel:
                self.class_rel[isDT[1]] = {}
                self.class_rel[isDT[1]]["path"] = isDT[1]
                self.class_rel[isDT[1]]["is-a"] = []

            if self.class_rel[isDT[1]]["path"] not in self.triples[pStr]["range"]:
                self.triples[pStr]["range"].append(self.class_rel[isDT[1]]["path"])
            #else:

        elif obj in self.class_rel:
            #logging.info( " Adding obj to class_rel:" + str(obj) )
            if self.class_rel[obj]["path"] not in self.triples[pStr]["range"]:
                self.triples[pStr]["range"].append( self.class_rel[obj]["path"])
        else:
            logging.error(" Wrong obj name '%s' %s.", str(obj), file_line)

        ### TBoxTools.addTriple()


    def isDataType( self, obj ):

        # 1) Convert to short name in low case
        #print( " >>>> ", obj )
        lower = obj.strip().lower()
        if   lower.startswith( PREFIX_XSD.lower() ):  # XSD
            pos  = lower.find( PREFIX_XSD.lower() )
            short = lower[ len(PREFIX_XSD) + pos: ]
            if short in DATA_TYPES_XSD:
                return True, short  #, short, DATA_TYPES_XSD
            else:
                logging.error("XSD prefix, but unknown data type: '%s'.", obj)
                return False, obj

        elif lower.startswith( PREFIX_RDFS.lower() ):  # RDFS
            pos  = lower.find( PREFIX_RDFS.lower() )
            short = lower[ len(PREFIX_RDFS) + pos: ]
            #return True, obj
            if short in DATA_TYPES_RDFS:
                return True, short  #, short, DATA_TYPES_RDFS
            else:
                logging.error("RDFS prefix, but unknown data type: '%s'.", obj)
                return False, obj



        elif lower.startswith( PREFIX_RDF.lower() ):  # RDF
            pos  = lower.find( PREFIX_RDF.lower() )
            short = lower[ len(PREFIX_RDF) + pos: ]
            #return True, obj

        elif lower.startswith( "xsd:" ):  # XSD
            pos  = lower.find( "xsd:" )
            short = lower[4 + pos:]
            #return True, obj
            if short in DATA_TYPES_XSD:
                return True, short  #, short, DATA_TYPES_XSD
            else:
                logging.error("XSD prefix, but unknown data type: '%s'.", obj)
                return False, obj

        elif lower.startswith( "rdfs:" ): # RDFS
            pos  = lower.find( "rdfs:" )
            short = lower[5 + pos:]
            #logging.info( " Checking rdfs: " + lower + "  " + str(pos) + "  " + short )
            #return True, obj
            if short in DATA_TYPES_RDFS:
                return True, short #, short, DATA_TYPES_RDFS
            else:
                logging.error("RDFS prefix, but unknown data type: '%s'.", obj)
                return False, obj


        elif lower.startswith("rdf:"): # RDF
            pos  = lower.find("rdf:")
            short = lower[5 + pos:]
            #return True, obj

        else:
            # Do nothing?
            short = lower
            # pass

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
        ### TBoxTools.isDataType()

    def isPredicate(self, pred):
        short = pred.strip().lower()
        #print( " >>> short = ", short )
        if short in DATA_PROPS:
            return True
        for k in DATA_PROPS.keys():
            if short == "rdfs:" + k:
                return True
            if short == "rdfs:" + DATA_PROPS[k]:
                return True

        return False
        ### TBoxTools.isPredicate()

    def checkTriple( self, subj, predicate, obj, file_line ):
        err_count = 0

        if not isinstance( subj, str ):
            logging.error(" Subj is not a string '%s' %s.",
                          str(subj), file_line)
            err_count += 1

        if not isinstance( predicate, str ):
            logging.error(" Predicate is not a string '%s' %s.",
                          str(predicate), file_line)
            err_count += 1

        if not isinstance(obj, str):
            logging.error(" Obj is not a string '%s' %s.",
                          str(obj), file_line)
            err_count += 1

        if predicate in self.triples:
            # Do nothing
            pass

        elif self.isPredicate(predicate):
        #    self.triples[predicate] = { "range": "xsd:string" }
        #    # Do nothing
            pass
        else:
            logging.error(" Predicate '%s' is not in TBox %s.",
                          predicate, file_line)
            #for k in self.triples.keys():
            #    print( "   ", k )
            #print( "  tbox = '" + self.tbox + "'." )
            err_count += 1
            return err_count

        #print( "Inside checkTriple:", subj, "and", obj )
        #if subj in self.class_rel:
        if self.isInClassRel(subj):

            subjDict = self.getClassRel(subj)

            #if self.class_rel[subj]["path"] in self.triples[predicate]["domain"]:
            if self.isPredicate(predicate):
                pass
            elif subjDict["path"] in self.triples[predicate]["domain"]:
                # Do nothing
                pass
            #elif self.anyInList( self.class_rel[subj]["is-a"], \
            #                     self.triples[predicate]["domain"]):
            elif self.anyInList(subjDict["is-a"],
                                self.triples[predicate]["domain"]):
                # Do nothing
                pass
            else:
                logging.error(" Subject '%s' is not in TBox %s.",
                              subj, file_line)

                """
                print( "     predicate:", predicate )
                #print( "       path   :", self.class_rel[subj]["path"] )
                print( "       path   :", subjDict["path"] )
                print( "       is-a   :" ) #, self.class_rel[subj]["is-a"] )
                #for d in self.class_rel[subj]["is-a"]:
                for d in subjDict["is-a"]:
                    print( "               ", d )
                print( "      domain  :" ) #, self.triples[predicate]["domain"] )
                for d in self.triples[predicate]["domain"]:
                    print( "               ", d )
                #print( "   domains:" )
                #for k in self.triples.keys():
                #    print( "    ", self.triples[k]["domain"])
                """

                err_count += 1

        else:
            logging.error(" Subject '%s' is not in the list of classes %s.",
                          str(subj), file_line)
            err_count += 1
            # pass

        isDT = self.isDataType(obj)
        if isDT[0]:
            # TODO need more verification. (for capital letter)
            pass

        #elif obj in self.class_rel:
        elif self.isInClassRel(obj):
        #if obj in self.triples[predicate]["range"]:
            #print( " >>>>>> ", obj, type(obj) )
        #    if obj not in self.class_rel:
        #        logging.error( "Missing '" + obj + "' in class_rel " + file_line )
        #        err_count += 1

            objDict = self.getClassRel(obj)

            #else:
            if predicate in DATA_PROPS:
                # Do nothing
                pass

            #elif self.class_rel[obj]["path"] in self.triples[predicate]["range"]:
            elif objDict["path"] in self.triples[predicate]["range"]:
                # Do nothing
                pass
            #elif self.anyInList(self.class_rel[obj]["is-a"], \
            #                     self.triples[predicate]["range"]):
            elif self.anyInList(objDict["is-a"],
                                self.triples[predicate]["range"]):
                # Do nothing
                pass
            else:
                logging.error(" Object '%s' is not in TBox %s.",
                              obj, file_line)

                print("     predicate:", predicate)
                print("       path   :", self.class_rel[obj]["path"])
                """
                print("       is-a   :") #, self.class_rel[obj]["is-a"])
                for d in self.class_rel[obj]["is-a"]:
                    print("               ", d)
                print("       range  :") #, self.triples[predicate]["domain"])
                for d in self.triples[predicate]["range"]:
                    print("               ", d)
                """

                err_count += 1

        else:
            logging.error(" Object '" + obj + "' is not in the list" + \
                           " of classes " + file_line)
            """
            print( "     predicate:", predicate )
            print( "     object:   '" + obj + "'" )
            for o in self.triples[predicate]["range"]:
                print( "   in range:   '" + o + "'" )
            for rel in self.class_rel:
                print( "   in class:   ", rel, " >>>> ", self.class_rel[rel] )
            """
            err_count += 1

        if err_count > 0:
            #print("  tbox.checkTriple() warnings:", err_count)
            pass

        return err_count
        ### TBoxTools.checkTriple()

    def anyInList(self, list1, list2):
        """
        Checks whether any element from list 1 is present in list 2.
        """
        for l1 in list1:
            if l1 in list2:
                return True
        return False

    def addClass(self, line):
        logging.error("Not implemented addClass() in tboxtools.py.")
        ### TBoxTools.addClass()

    def addObjProp(self, line):
        logging.error("Not implemented addObjProp() in tboxtools.py.")
        ### TBoxTools.addObjProp()

    def addDataProp(self, line):
        logging.error("Not implemented addDataProp() in tboxtools.py.")
        ### TBoxTools.addDataProp()


    def _merge_obj_prop(self):
        logging.error("Not implemented _merge_obj_prop() function. in tboxtools.py.")

        ### TBoxTools._merge_obj_prop()

    def _merge_data_prop(self):
        logging.error("Not implemented _merge_data_prop() function. in tboxtools.py.")

        ### TBoxTools._merge_data_prop()

    def saveCsv(self, filename):

        #print(self.headers)
        #print(self.onto)
        #print(self.classes)
        #print(self.obj_prop)
        #print(self.dat_prop)

        # TODO
        self._merge_obj_prop()
        self._merge_data_prop()
        # TODO
        #self.CheckClasses()
        #self.CheckObjProp()
        #self.CheckDataProp()

        tmp = self.headers + self.onto + self.classes + \
              self.obj_prop + self.dat_prop

        #print(tmp)
        #print("===================")

        self.data_out = []
        for _, line in enumerate(tmp):
            l = []
            for ic,c in enumerate(line):
                if ic >= self.nCol:
                    logging.warning(" Too many columns for class/property" +
                                    " '%s' during saving to csv file." +
                                    " Extra cell '%s'.", line[0], str(c))
                    continue

                if isinstance(c, float):
                    if math.isnan(c):
                        l.append("")
                    else:
                        l.append(c)
                elif isinstance(c, str):
                    if "owl:Thing" == c:
                        l.append("")
                    else:
                        l.append(c.strip())
                else:
                    l.append(c)
                    # logging.warning(" Unknown type '" + str(type(c)) + "' of" + \
                    #                 " '" + str(c) + "' during saving to csv. ")
                    #
            self.data_out.append(l)

        for line in self.data_out:
            # For debugging:
            # print(line[0])
            # print(line)
            pass

        try:
            writeCsv(filename, self.data_out)
        except:
            # On failure I try to write line-by-line,
            # to detect the location of the error:
            for s in self.data_out:
                print("Saving to csv:", s)
                writeCsv(filename, [s])

        #tools.writeCsv(filename, self.classes)
        print("Saved '" + filename + "' file, number of lines =",
              str(len(self.data_out)) + ".")
        """
        """
        print("Number of errors+warnings =", str(self.err_count) + ".")

        ### TBoxTools.saveCsv()

writeCsvErrCount = 0
def writeCsv(filename, array):
    global writeCsvErrCount
    try:
        with open(filename, "w", newline="", encoding="utf-8") as f:
            csvw = csv.writer(f, delimiter=",", quotechar='"',
                              quoting=csv.QUOTE_MINIMAL)
            for a in array:
                csvw.writerow(a)
    except IOError:
        tmp_file = "test-tmp.csv"
        print("Error: File '" + filename + "' is protected. " +
              "Using temporary instead: '" + tmp_file + "'.")
        writeCsvErrCount += 1
        if 1 == writeCsvErrCount:
            writeCsv(tmp_file, array)
        else:
            print(" Error: I give up. " + "You need to close the files.")
    ### writeCsv()


if __name__ == "__main__":

    file_in = "Book2.xlsx"
    file_in = "ontocrystal.xlsx"

    file_out = "default.csv"

    if len(sys.argv) == 3:
        file_in  = sys.argv[1]
        file_out = sys.argv[2]
    elif len(sys.argv) == 2:
        file_in = sys.argv[1]
        fileBase, fileExt = os.path.splitext(file_in)
    elif len(sys.argv) == 1:
        # Do nothing. Use default file names.
        pass
    else:
        logging.error(" Expecting 1 or 2 command line arguments:" +
                      " tboxtools.py file_in [file_out] \n" +
                      ", but got '%s' program call.", len(sys.argv))
        sys.exit(0)

    fileBase, fileExt = os.path.splitext(file_in)
    if fileExt.lower() == ".xlsx":
        file_out = fileBase + ".csv"
    else:
        logging.error(" Input must be Excel file with extension .xlsx, " +
                      "but got file name '%s'.", file_in)
        # sys.exit(0)

    fileBase, fileExt = os.path.splitext(file_out)
    if not fileExt.lower() == ".csv":
        logging.error(" Output must be CSV file with extension .csv, " +
                      "but got file name '%s'.", file_out)
        sys.exit(0)

    logging.info("File base = '%s', ext = '%s'.", fileBase, fileExt)

    tb = TBoxTools()
    # print( tb.anyInList( [1,2.3],[3,2,1]) )
    # 1/0
    tb.readExcel(file_in)
    tb.parseInputData()
    # print( "   tbox = '" + tb.tbox + "'." )
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
    # tb.checkTriple( subj, predicate, obj, "file and line" )

# old version:
# tb.extractOntology()
# tb.extractClasses()
# tb.extractObjProp()
# tb.extractDataProp()

    tb.saveCsv(file_out)
