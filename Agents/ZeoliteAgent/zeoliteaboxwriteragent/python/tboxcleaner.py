"""
TODO
  - Allow to combine several .xlsx files to make a single csv file.
    - There is a problem of property that has the same name but different Domain/Range (use UNION)
      If different parts use the same property this should be addressed properly.
      - One way to fix it: add property "Combined Comment"
    - Need to check the tbox details (header of the file)
    - Does each .xlsx must be valid by itself?
    - Is DataProperty must be defined through UNION? Or is it properly converted to owl/Protege?
      This I need to test. And may be ask Feroz.

  - Check is names or types contain spaces, comma, tab characters.
    - comment is allowed to have different values.

  - Remove warnings related to the code.

  - Add a single file test-case warnings.xlsx to test all the warnings in the code.

  - Fix TODO notes in the code.

  - DONE In validationXXX functions add warning for repeating entry.
  .

"""

import pandas  # to read xlsx files
import csv     # to save csv files
import math    # to detect NaN values
import sys     # for command line arguments (sys.argv)
import os      # To detect file, to check filename, etc.

import logging # For error/waarning messages
logging.basicConfig( level=logging.WARNING )
#logging.basicConfig( level=logging.ERROR )
#logging.disable( logging.CRITICAL )

import tools

COLUMN_LETTERS = "ABCDEFGHIJ"
COLUMN_HEADERS = [ "Source",      "Type",  "Target",   "Relation", "Domain",
                   "Range", "Quantifier", "Comment", "Defined By", "Label" ]


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

class TBoxCleaner:
    __slots__ = [ "fileIn",  "dataIn", "classes", "clNames", "objProp", "opNames", 
                  "datProp", "dpNames", "nCol", "onto", "ontoNames", "errCount",
                  "dataOut", "headers",
                  "objPropDomain", "objPropRange", "datPropDomain", "datPropRange" 
                ]
    def __init__( self ):
        self.cleanAll()
        '''
        self.dataIn = []
        self.headers = []
        self.onto    = []
        self.ontoNames = []
        self.classes = []
        self.clNames = []
        self.objProp = []
        self.opNames = []
        self.datProp = []
        self.dpNames = []

        self.nCol    = 10
        self.fileIn  = ""
        '''

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



        pass # TBoxCleaner.cleanAll()

    def readExcel( self, filename ):
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
        pass # isCellBValue    
 
    def isLineOntology( self, line, file_line ):
        return self.isCellBValue( line, "TBox", file_line )
        pass # isLineOntology()
       
    def isLineClass( self, line, file_line ):
        return self.isCellBValue( line, "Class", file_line )

        pass # isLineClass()

    def isLineObjProp( self, line, file_line ):
        return self.isCellBValue( line, "Object Property", file_line )
        pass # isLineObjProp()

    def isLineDataProp( self, line, file_line ):
        return self.isCellBValue( line, "Data Property", file_line )
        pass # isLineDataProp()

    def isKnownClass( self, value, strict = False ):
        for c in self.clNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # isKnownClass

    def isKnownObjProp( self, value, strict = False ):
        for c in self.opNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # isKnownObjProp

    def isKnownDataProp( self, value, strict = False ):
        for c in self.dpNames:
            if strict:
                if str(value).strip() == c.strip():
                    return True
            else:
                if str(value).lower().strip() == c.lower().strip():
                    return True

        return False
        pass # isKnownObjProp

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
        pass # validateHeaders()

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
        pass


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
            logging.error( " Missing Defined In for class '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Class '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 4, 5, 6, 9 ]:
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
        pass # validateClassLine()

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
            logging.error( " Missing Defined In for Obj Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Obj Property '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3, 9 ]:
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
        pass # validateObjLine()


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
            logging.error( " Missing Defined In for Data Property '" + line[0] + "' " + file_line + \
                           " in col " + COLUMN_LETTERS[8] + "." )
            errCount += 1
        elif isinstance( line[8], str ):
            if len( line[8] ) < 20:
                logging.warning( " Data Property '" + line[0] + "' may have incompete 'defined in': '" + \
                                 line[8] + "' " + file_line + " col " + COLUMN_LETTERS[8] + "." )
                errCount += 1

        # Other columns are empty:
        for i in [ 2, 3, 6, 9 ]:
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
        pass # validateDataLine()


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
                self.extractOntology( line, file_line )

            elif self.isLineClass(   line, file_line ):
                self.extractClasses( line, file_line )

            elif self.isLineObjProp( line, file_line ):
                self.extractObjProp( line, file_line )

            elif self.isLineDataProp( line, file_line ):
                self.extractDataProp( line, file_line )

            elif self.emptyLine( line, file_line ):
                # Do nothing
                pass
            elif self.isLineComment( line, file_line ):
                # Do nothing
                pass
            else:
                #logging.warning( " xxxxxxxxxxxxxxxxxxxx " )
                logging.warning( "Failed to identify type, skipping line: '" + \
                      str(line) + "' " + file_line + "." )
        pass # parseInputData()

    def isLineComment( self, line, file_line ):
        """ Now the conditions are either: 
            - the 1st column is empty or equal to Comment
            - the 2nd column is empty or equal to Comment
            .
        """

        short = self.getShort( line[0], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        short = self.getShort( line[1], file_line )
        #print( "short = '" + short + "'" )
        if "comment" == short or "" == short: 
            return True

        return False
        pass # isLineComment()

    def getShort( self, word, file_line ):
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
            logging.error( " unknown value '" + str(word) + "', type " + \
                           str(type(word)) + " " + file_line )
            short = str(word)
        return short


    def emptyLine( self, line, file_line ):
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
                
    def extractHeaders( self, line, file_line ):
        #print( "sssssssssssssssssssss" )
        #print( "header line =", line )
        self.headers = [line]
        self.errCount += self.validateHeaders( line, file_line )
        pass # extractHeaders()
        
    def extractOntology( self, line, file_line ):

        self.errCount += self.validateOntoLine( line, file_line )

        self.onto.append( line )
        self.ontoNames.append( line[0] )
   
        pass # extractOntology() 

    def extractClasses( self, line, file_line ):

        self.errCount += self.validateClassLine( line, file_line )

        #self.addClass( line )
        self.classes.append( line )
        self.clNames.append( str(line[0]).strip() )

        #print( self.classes )
        pass # extractClasses()

    def addClass( self, line ):
        logging.error( "No implemented 333333" )
        pass # addClass( line )
    def addObjProp( self, line ):
        logging.error( "No implemented 333333" )
        pass # addObjProp( line )
    def addDataProp( self, line ):
        logging.error( "No implemented 333333" )
        pass # addDataProp( line )

    def extractObjProp( self, line, file_line ):

        self.errCount += self.validateObjLine( line, file_line )

        #self.addObjProp( line )
        self.objProp.append( line )
        self.opNames.append( line[0].strip() )

        # Domain:
        words = line[4].split( "UNION" )
        self.objPropDomain = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.objPropDomain:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' in " + file_line + \
                                 " col " + COLUMN_LETTERS[4] + ", it may " \
                                 "be confused with the UNION keyword." )
                self.errCount += 1

            if w not in self.clNames:
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[4] + "." )
                self.errCount += 1
            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.objPropDomain )
            
        # Range:
        words = line[5].split( "UNION" )
        self.objPropRange = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.objPropRange:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' in " + file_line + \
                                 " col " + COLUMN_LETTERS[4] + ", it may " \
                                 "be confused with the UNION keyword." )
                self.errCount += 1

            if w not in self.clNames:
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[5] + "." )
                self.errCount += 1

            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.objPropRange )
 

        #print( self.objProp )
        pass # extractObjProp()

    def extractDataProp( self, line, file_line ):
        self.errCount += self.validateDataLine( line, file_line )

        #self.addDataProp( line )
        self.datProp.append( line )
        self.dpNames.append( line[0].strip() )

        # Domain:
        words = line[4].split( "UNION" )
        self.objPropDomain = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.objPropDomain:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' word in " + file_line + \
                                 " col " + COLUMN_LETTERS[4] + ", it may " \
                                 "be confused with the UNION keyword." )
                self.errCount += 1

            if w not in self.clNames:
                logging.error( " Unknown class '" + w + "' in property " \
                               "'" + line[0] + "' " + file_line + \
                               " col " + COLUMN_LETTERS[4] + "." )
                self.errCount += 1

            #print( ">>>>>>>>>>>>> obj prop Domain(s):", self.objPropDomain )
            
        # Range:
        words = line[5].split( "UNION" )
        self.objPropRange = [ w.strip() for w in words ] #line[4].split( "UNION" )
        for w in self.objPropRange:
            pos = w.lower().find( "union" )
            if pos >= 0:
                logging.warning( " Found '" + w[pos:pos+5] + "' word in " + \
                                 file_line + " col " + COLUMN_LETTERS[4] + \
                                 ", it may be confused with the UNION keyword." )
                self.errCount += 1
            #print( ">>>>>>>>>>>>> obj prop Range(s):", self.objPropRange )

                # Here no need to check the range(s) for data properties.
 
        #print( self.datProp )
        pass # extractDataProp()

    def mergeObjProp(self):
        logging.error( "Not implemented mergeObjProp()" )

        pass # mergeObjProp()

    def mergeDataProp(self):
        logging.error( "Not implemented mergeDataProp()" )

        pass # mergeDataProp()

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

        tools.writeCsv( filename, self.dataOut )
        #tools.writeCsv( filename, self.classes )
        print( "Saved '" + filename + "' file, number of lines =", 
               str(len(self.dataOut)) + "." )
        print( "Number of errors+warnings =", str(self.errCount) + "." )

        pass # saveCsv()

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
                       " tboxcleaner.py file_in [file_out] \n" + \
                       ", but got '" + str(len(sys.argv)) + "' program call." )
        sys.exit(0) 

    fileBase, fileExt = os.path.splitext( fileIn )
    if fileExt.lower() == ".xlsx":
        fileOut = fileBase + ".csv"
    else:
        logging.error( " Input must be Excel file with extension .xlsx, " + \
                       "but got file name '" + fileIn + "'." )
        sys.exit(0)

    fileBase, fileExt = os.path.splitext( fileOut )
    if not fileExt.lower() == ".csv":
        logging.error( " Output must be CSV file with extension .csv, " + \
                       "but got file name '" + fileOut + "'." )
        sys.exit(0)

    #print( fileBase, fileExt )

    tb = TBoxCleaner()
    tb.readExcel( fileIn )
    tb.parseInputData()

# old version:
#tb.extractOntology()
#tb.extractClasses()
#tb.extractObjProp()
#tb.extractDataProp()

    tb.saveCsv( fileOut )


